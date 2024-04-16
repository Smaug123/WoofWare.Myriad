namespace WoofWare.Myriad.Plugins

open System
open System.Net.Http
open System.Text
open Fantomas.FCS.Syntax
open Fantomas.FCS.SyntaxTrivia
open Fantomas.FCS.Xml
open Myriad.Core

[<RequireQualifiedAccess>]
module internal HttpClientGenerator =
    open Fantomas.FCS.Text.Range
    open Myriad.Core.Ast

    [<RequireQualifiedAccess>]
    type PathSpec =
        | Verbatim of string
        | MatchArgName

    type HttpAttribute =
        // TODO: Format parameter to these attrs
        | Query of string option
        | Path of PathSpec
        | Body

    type Parameter =
        {
            Attributes : HttpAttribute list
            IsOptional : bool
            Id : Ident option
            Type : SynType
        }

    [<RequireQualifiedAccess>]
    type BodyParamMethods =
        | StringContent
        | StreamContent
        | ByteArrayContent
        | HttpContent
        | Serialise of SynType

        override this.ToString () =
            match this with
            | BodyParamMethods.Serialise _ -> "ToString"
            | BodyParamMethods.ByteArrayContent -> "ByteArrayContent"
            | BodyParamMethods.StringContent -> "StringContent"
            | BodyParamMethods.StreamContent -> "StreamContent"
            | BodyParamMethods.HttpContent -> "HttpContent"

    type MemberInfo =
        {
            /// E.g. HttpMethod.Get
            HttpMethod : HttpMethod
            /// E.g. SynExpr.Const "v1/gyms/{gym_id}/attendance"
            UrlTemplate : SynExpr
            TaskReturnType : SynType
            Args : Parameter list
            Identifier : Ident
            EnsureSuccessHttpCode : bool
            BaseAddress : SynExpr option
            BasePath : SynExpr option
            Accessibility : SynAccess option
        }

    let httpMethodString (m : HttpMethod) : string =
        if m = HttpMethod.Get then "Get"
        elif m = HttpMethod.Post then "Post"
        elif m = HttpMethod.Delete then "Delete"
        elif m = HttpMethod.Patch then "Post"
        elif m = HttpMethod.Options then "Options"
        elif m = HttpMethod.Head then "Head"
        elif m = HttpMethod.Put then "Put"
        elif m = HttpMethod.Trace then "Trace"
        else failwith $"Unrecognised method: %+A{m}"

    /// E.g. converts `[<Get "blah">]` to (HttpMethod.Get, SynExpr.Const "blah")
    let extractHttpInformation (attrs : SynAttribute list) : HttpMethod * SynExpr =
        let matchingAttrs =
            attrs
            |> List.choose (fun attr ->
                match attr.TypeName.AsString with
                | "Get"
                | "GetAttribute"
                | "RestEase.Get"
                | "RestEase.GetAttribute" -> Some (HttpMethod.Get, attr.ArgExpr)
                | "Post"
                | "PostAttribute"
                | "RestEase.Post"
                | "RestEase.PostAttribute" -> Some (HttpMethod.Post, attr.ArgExpr)
                | "Put"
                | "PutAttribute"
                | "RestEase.Put"
                | "RestEase.PutAttribute" -> Some (HttpMethod.Put, attr.ArgExpr)
                | "Delete"
                | "DeleteAttribute"
                | "RestEase.Delete"
                | "RestEase.DeleteAttribute" -> Some (HttpMethod.Delete, attr.ArgExpr)
                | "Head"
                | "HeadAttribute"
                | "RestEase.Head"
                | "RestEase.HeadAttribute" -> Some (HttpMethod.Head, attr.ArgExpr)
                | "Options"
                | "OptionsAttribute"
                | "RestEase.Options"
                | "RestEase.OptionsAttribute" -> Some (HttpMethod.Options, attr.ArgExpr)
                | "Patch"
                | "PatchAttribute"
                | "RestEase.Patch"
                | "RestEase.PatchAttribute" -> Some (HttpMethod.Patch, attr.ArgExpr)
                | "Trace"
                | "TraceAttribute"
                | "RestEase.Trace"
                | "RestEase.TraceAttribute" -> Some (HttpMethod.Trace, attr.ArgExpr)
                | _ -> None
            )

        match matchingAttrs with
        | [ (meth, arg) ] -> meth, arg
        | [] -> failwith "Required exactly one recognised RestEase attribute on member, but got none"
        | matchingAttrs ->
            failwith $"Required exactly one recognised RestEase attribute on member, but got %i{matchingAttrs.Length}"

    /// Get the args associated with the Header attributes within the list.
    let extractHeaderInformation (attrs : SynAttribute list) : SynExpr list list =
        attrs
        |> List.choose (fun attr ->
            match attr.TypeName.AsString with
            | "Header"
            | "RestEase.Header" ->
                match attr.ArgExpr with
                | SynExpr.Paren (SynExpr.Tuple (_, [ v1 ; v2 ], _, _), _, _, _) ->
                    Some [ SynExpr.stripOptionalParen v1 ; SynExpr.stripOptionalParen v2 ]
                | e -> Some [ SynExpr.stripOptionalParen e ]
            | _ -> None
        )

    let shouldAllowAnyStatusCode (attrs : SynAttribute list) : bool =
        attrs
        |> List.exists (fun attr ->
            match attr.TypeName.AsString with
            | "AllowAnyStatusCode"
            | "AllowAnyStatusCodeAttribute"
            | "RestEase.AllowAnyStatusCode"
            | "RestEase.AllowAnyStatusCodeAttribute" -> true
            | _ -> false
        )

    /// constantHeaders are a list of (headerName, headerValue)
    /// variableHeaders are a list of (headerName, selfPropertyToGetValueOf)
    let constructMember
        (constantHeaders : (SynExpr * SynExpr) list)
        (variableHeaders : (SynExpr * Ident) list)
        (info : MemberInfo)
        : SynMemberDefn
        =
        let valInfo =
            SynValInfo.SynValInfo (
                [
                    [ SynArgInfo.Empty ]
                    [
                        for arg in info.Args do
                            match arg.Id with
                            | None -> yield SynArgInfo.CreateIdString (failwith "TODO: create an arg name")
                            | Some id -> yield SynArgInfo.CreateId id
                    ]
                ],
                SynArgInfo.Empty
            )

        let valData =
            SynValData (
                Some
                    {
                        IsInstance = true
                        IsDispatchSlot = false
                        IsOverrideOrExplicitImpl = true
                        IsFinal = false
                        GetterOrSetterIsCompilerGenerated = false
                        MemberKind = SynMemberKind.Member
                    },
                valInfo,
                None
            )

        let args =
            info.Args
            |> List.map (fun arg ->
                let argName =
                    match arg.Id with
                    | None -> failwith "TODO: create an arg name"
                    | Some id -> id

                let argType =
                    if arg.IsOptional then
                        SynType.CreateApp (
                            SynType.CreateLongIdent (SynLongIdent.CreateString "option"),
                            [ arg.Type ],
                            isPostfix = true
                        )
                    else
                        arg.Type

                argName, SynPat.CreateTyped (SynPat.CreateNamed argName, argType)
            )

        let cancellationTokenArg =
            match List.tryLast args with
            | None -> failwith $"expected an optional cancellation token as final arg in %s{info.Identifier.idText}"
            | Some (arg, _) -> arg

        let argPats =
            let args = args |> List.map snd

            SynPat.Tuple (false, args, List.replicate (args.Length - 1) range0, range0)
            |> SynPat.CreateParen
            |> List.singleton
            |> SynArgPats.Pats

        let headPat =
            let thisIdent = if variableHeaders.IsEmpty then "_" else "this"

            SynPat.LongIdent (
                SynLongIdent.CreateFromLongIdent [ Ident.Create thisIdent ; info.Identifier ],
                None,
                None,
                argPats,
                None,
                range0
            )

        let requestUriTrailer =
            (info.UrlTemplate, info.Args)
            ||> List.fold (fun template arg ->
                (template, arg.Attributes)
                ||> List.fold (fun template attr ->
                    match attr with
                    | HttpAttribute.Path spec ->
                        let varName =
                            match arg.Id with
                            | None -> failwith "TODO: anonymous args"
                            | Some id -> id

                        let substituteId =
                            match spec with
                            | PathSpec.Verbatim s -> s
                            | PathSpec.MatchArgName -> varName.idText

                        template
                        |> SynExpr.callMethodArg
                            "Replace"
                            (SynExpr.CreateParenedTuple
                                [
                                    SynExpr.CreateConstString ("{" + substituteId + "}")
                                    SynExpr.callMethod "ToString" (SynExpr.CreateIdent varName)
                                    |> SynExpr.pipeThroughFunction (
                                        SynExpr.CreateLongIdent (
                                            SynLongIdent.Create [ "System" ; "Web" ; "HttpUtility" ; "UrlEncode" ]
                                        )
                                    )
                                ])
                    | _ -> template
                )
            )

        /// List of (query-param-key, parameter-which-provides-value)
        let queryParams =
            info.Args
            |> List.collect (fun arg ->
                arg.Attributes
                |> List.choose (fun attr ->
                    match attr with
                    | Query None ->
                        let name =
                            match arg.Id with
                            | None ->
                                failwith
                                    "Expected a name for the argument we're trying to use as an anonymous query parameter"
                            | Some name -> name.idText

                        Some (name, arg)
                    | Query (Some name) -> Some (name, arg)
                    | _ -> None
                )
            )

        let requestUriTrailer =
            match queryParams with
            | [] -> requestUriTrailer
            | (firstKey, firstValue) :: queryParams ->
                let firstValueId =
                    match firstValue.Id with
                    | None -> failwith "Unable to get parameter variable name from anonymous parameter"
                    | Some id -> id

                let prefix =
                    SynExpr.CreateIdent firstValueId
                    |> SynExpr.toString firstValue.Type
                    |> SynExpr.CreateParen
                    |> SynExpr.pipeThroughFunction (
                        SynExpr.CreateLongIdent (SynLongIdent.Create [ "System" ; "Web" ; "HttpUtility" ; "UrlEncode" ])
                    )
                    |> SynExpr.CreateParen
                    |> SynExpr.plus (SynExpr.CreateConstString ("?" + firstKey + "="))

                (prefix, queryParams)
                ||> List.fold (fun uri (paramKey, paramValue) ->
                    let paramValueId =
                        match paramValue.Id with
                        | None -> failwith "Unable to get parameter variable name from anonymous parameter"
                        | Some id -> id

                    SynExpr.toString paramValue.Type (SynExpr.CreateIdent paramValueId)
                    |> SynExpr.CreateParen
                    |> SynExpr.pipeThroughFunction (
                        SynExpr.CreateLongIdent (
                            SynLongIdent.Create [ "System" ; "Web" ; "HttpUtility" ; "UrlEncode" ]
                        )
                    )
                    |> SynExpr.CreateParen
                    |> SynExpr.plus (SynExpr.plus uri (SynExpr.CreateConstString ("&" + paramKey + "=")))
                )
                |> SynExpr.plus requestUriTrailer
                |> SynExpr.CreateParen

        let requestUri =
            let uriIdent = SynExpr.CreateLongIdent (SynLongIdent.Create [ "System" ; "Uri" ])

            let baseAddress =
                SynExpr.CreateLongIdent (SynLongIdent.Create [ "client" ; "BaseAddress" ])

            let baseAddress =
                SynExpr.CreateMatch (
                    baseAddress,
                    [
                        SynMatchClause.Create (
                            SynPat.CreateNull,
                            None,
                            match info.BaseAddress with
                            | None ->
                                SynExpr.CreateApp (
                                    SynExpr.CreateIdentString "raise",
                                    SynExpr.CreateParen (
                                        SynExpr.CreateApp (
                                            SynExpr.CreateLongIdent (
                                                SynLongIdent.Create [ "System" ; "ArgumentNullException" ]
                                            ),
                                            SynExpr.CreateParenedTuple
                                                [
                                                    SynExpr.CreateApp (
                                                        SynExpr.CreateIdentString "nameof",
                                                        SynExpr.CreateParen baseAddress
                                                    )
                                                    SynExpr.CreateConstString
                                                        "No base address was supplied on the type, and no BaseAddress was on the HttpClient."
                                                ]
                                        )
                                    )
                                )
                            | Some expr -> SynExpr.CreateApp (uriIdent, expr)
                        )
                        SynMatchClause.Create (
                            SynPat.CreateNamed (Ident.Create "v"),
                            None,
                            SynExpr.CreateIdentString "v"
                        )
                    ]
                )
                |> SynExpr.CreateParen

            SynExpr.App (
                ExprAtomicFlag.Atomic,
                false,
                uriIdent,
                SynExpr.CreateParenedTuple
                    [
                        baseAddress
                        SynExpr.CreateApp (
                            uriIdent,
                            SynExpr.CreateParenedTuple
                                [
                                    requestUriTrailer
                                    SynExpr.CreateLongIdent (SynLongIdent.Create [ "System" ; "UriKind" ; "Relative" ])
                                ]
                        )
                    ],
                range0
            )

        let bodyParams =
            info.Args
            |> List.collect (fun arg ->
                arg.Attributes
                |> List.choose (fun attr ->
                    match attr with
                    | HttpAttribute.Body -> Some arg
                    | _ -> None
                )
            )

        let bodyParam =
            match bodyParams with
            | [] -> None
            | [ x ] ->
                // TODO: body serialisation method
                let paramName =
                    match x.Id with
                    | None -> failwith "Anonymous [<Body>] parameter is unsupported"
                    | Some id -> id

                match x.Type with
                | Stream -> Some (BodyParamMethods.StreamContent, paramName)
                | String -> Some (BodyParamMethods.StringContent, paramName)
                | ArrayType Byte -> Some (BodyParamMethods.ByteArrayContent, paramName)
                | HttpContent -> Some (BodyParamMethods.HttpContent, paramName)
                | ty -> Some (BodyParamMethods.Serialise ty, paramName)
            | _ -> failwith "You can only have at most one [<Body>] parameter on a method."

        let httpReqMessageConstructor =
            [
                SynExpr.equals
                    (SynExpr.CreateIdentString "Method")
                    (SynExpr.CreateLongIdent (
                        SynLongIdent.Create
                            [ "System" ; "Net" ; "Http" ; "HttpMethod" ; httpMethodString info.HttpMethod ]
                    ))
                SynExpr.equals (SynExpr.CreateIdentString "RequestUri") (SynExpr.CreateIdentString "uri")
            ]
            |> SynExpr.CreateParenedTuple

        let returnExpr =
            match info.TaskReturnType with
            | HttpResponseMessage -> SynExpr.CreateIdentString "response"
            | String -> SynExpr.CreateIdentString "responseString"
            | Stream -> SynExpr.CreateIdentString "responseStream"
            | RestEaseResponseType contents ->
                let deserialiser =
                    SynExpr.CreateLambda (
                        [ SynPat.CreateConst SynConst.Unit ],
                        SynExpr.CreateParen (
                            JsonParseGenerator.parseNode
                                None
                                JsonParseGenerator.JsonParseOption.None
                                contents
                                (SynExpr.CreateIdentString "jsonNode")
                        )
                    )
                // new RestEase.Response (content : string, response : HttpResponseMessage, deserialiser : unit -> 'T)
                SynExpr.New (
                    false,
                    SynType.App (
                        SynType.CreateLongIdent (SynLongIdent.Create [ "RestEase" ; "Response" ]),
                        Some range0,
                        [ SynType.Anon range0 ],
                        [],
                        Some range0,
                        false,
                        range0
                    ),
                    SynExpr.CreateParenedTuple
                        [
                            SynExpr.CreateIdentString "responseString"
                            SynExpr.CreateIdentString "response"
                            SynExpr.CreateParen deserialiser
                        ],
                    range0
                )
            | retType ->
                JsonParseGenerator.parseNode
                    None
                    JsonParseGenerator.JsonParseOption.None
                    retType
                    (SynExpr.CreateIdentString "jsonNode")

        let handleBodyParams =
            match bodyParam with
            | None -> []
            | Some (bodyParamType, bodyParamName) ->
                match bodyParamType with
                | BodyParamMethods.StreamContent
                | BodyParamMethods.ByteArrayContent
                | BodyParamMethods.StringContent ->
                    [
                        Let (
                            "queryParams",
                            SynExpr.New (
                                false,
                                SynType.CreateLongIdent (
                                    SynLongIdent.Create
                                        [ "System" ; "Net" ; "Http" ; (bodyParamType : BodyParamMethods).ToString () ]
                                ),
                                SynExpr.CreateParen (SynExpr.CreateIdent bodyParamName),
                                range0
                            )
                        )
                        Do (
                            SynExpr.LongIdentSet (
                                SynLongIdent.Create [ "httpMessage" ; "Content" ],
                                SynExpr.CreateIdentString "queryParams",
                                range0
                            )
                        )
                    ]
                | BodyParamMethods.HttpContent ->
                    [
                        Do (
                            SynExpr.LongIdentSet (
                                SynLongIdent.Create [ "httpMessage" ; "Content" ],
                                SynExpr.CreateIdent bodyParamName,
                                range0
                            )
                        )
                    ]
                | BodyParamMethods.Serialise ty ->
                    [
                        Let (
                            "queryParams",
                            SynExpr.New (
                                false,
                                SynType.CreateLongIdent (
                                    SynLongIdent.Create [ "System" ; "Net" ; "Http" ; "StringContent" ]
                                ),
                                SynExpr.CreateParen (
                                    SynExpr.CreateIdent bodyParamName
                                    |> SynExpr.pipeThroughFunction (JsonSerializeGenerator.serializeNode ty)
                                    |> SynExpr.pipeThroughFunction (
                                        SynExpr.createLambda
                                            "node"
                                            (SynExpr.ifThenElse
                                                (SynExpr.CreateApp (
                                                    SynExpr.CreateIdentString "isNull",
                                                    SynExpr.CreateIdentString "node"
                                                ))
                                                (SynExpr.CreateApp (
                                                    SynExpr.CreateLongIdent (
                                                        SynLongIdent.Create [ "node" ; "ToJsonString" ]
                                                    ),
                                                    SynExpr.CreateConst SynConst.Unit
                                                ))
                                                (SynExpr.CreateConst (SynConst.CreateString "null")))
                                    )
                                ),
                                range0
                            )
                        )
                        Do (
                            SynExpr.LongIdentSet (
                                SynLongIdent.Create [ "httpMessage" ; "Content" ],
                                SynExpr.CreateIdent (Ident.Create "queryParams"),
                                range0
                            )
                        )
                    ]

        let implementation =
            let responseString =
                LetBang (
                    "responseString",
                    SynExpr.awaitTask (
                        SynExpr.CreateApp (
                            SynExpr.CreateLongIdent (
                                SynLongIdent.Create [ "response" ; "Content" ; "ReadAsStringAsync" ]
                            ),
                            SynExpr.CreateIdentString "ct"
                        )
                    )
                )

            let responseStream =
                LetBang (
                    "responseStream",
                    SynExpr.awaitTask (
                        SynExpr.CreateApp (
                            SynExpr.CreateLongIdent (
                                SynLongIdent.Create [ "response" ; "Content" ; "ReadAsStreamAsync" ]
                            ),
                            SynExpr.CreateIdentString "ct"
                        )
                    )
                )

            let jsonNode =
                LetBang (
                    "jsonNode",
                    SynExpr.awaitTask (
                        SynExpr.CreateApp (
                            SynExpr.CreateLongIdent (
                                SynLongIdent.Create [ "System" ; "Text" ; "Json" ; "Nodes" ; "JsonNode" ; "ParseAsync" ]
                            ),
                            SynExpr.CreateParenedTuple
                                [
                                    SynExpr.CreateIdentString "responseStream"
                                    SynExpr.equals
                                        (SynExpr.CreateIdentString "cancellationToken")
                                        (SynExpr.CreateIdentString "ct")
                                ]
                        )
                    )
                )

            let setVariableHeaders =
                variableHeaders
                |> List.map (fun (headerName, callToGetValue) ->
                    Do (
                        SynExpr.CreateApp (
                            SynExpr.CreateLongIdent (SynLongIdent.Create [ "httpMessage" ; "Headers" ; "Add" ]),
                            SynExpr.CreateParenedTuple
                                [
                                    headerName
                                    SynExpr.CreateApp (
                                        SynExpr.CreateLongIdent (
                                            SynLongIdent.CreateFromLongIdent
                                                [ Ident.Create "this" ; callToGetValue ; Ident.Create "ToString" ]
                                        ),
                                        SynExpr.CreateConst SynConst.Unit
                                    )
                                ]
                        )
                    )
                )

            let setConstantHeaders =
                constantHeaders
                |> List.map (fun (headerName, headerValue) ->
                    Do (
                        SynExpr.CreateApp (
                            SynExpr.CreateLongIdent (SynLongIdent.Create [ "httpMessage" ; "Headers" ; "Add" ]),
                            SynExpr.CreateParenedTuple [ headerName ; headerValue ]
                        )
                    )
                )

            [
                yield LetBang ("ct", SynExpr.CreateLongIdent (SynLongIdent.Create [ "Async" ; "CancellationToken" ]))
                yield Let ("uri", requestUri)
                yield
                    Use (
                        "httpMessage",
                        SynExpr.New (
                            false,
                            SynType.CreateLongIdent (
                                SynLongIdent.Create [ "System" ; "Net" ; "Http" ; "HttpRequestMessage" ]
                            ),
                            httpReqMessageConstructor,
                            range0
                        )
                    )

                yield! handleBodyParams

                yield! setVariableHeaders
                yield! setConstantHeaders

                yield
                    LetBang (
                        "response",
                        SynExpr.awaitTask (
                            SynExpr.CreateApp (
                                SynExpr.CreateLongIdent (SynLongIdent.Create [ "client" ; "SendAsync" ]),
                                SynExpr.CreateParenedTuple
                                    [ SynExpr.CreateIdentString "httpMessage" ; SynExpr.CreateIdentString "ct" ]
                            )
                        )
                    )
                if info.EnsureSuccessHttpCode then
                    yield
                        Let (
                            "response",
                            SynExpr.CreateApp (
                                SynExpr.CreateLongIdent (SynLongIdent.Create [ "response" ; "EnsureSuccessStatusCode" ]),
                                SynExpr.CreateConst SynConst.Unit
                            )
                        )
                match info.TaskReturnType with
                | HttpResponseMessage -> ()
                | RestEaseResponseType _ ->
                    yield responseString
                    yield responseStream
                    yield jsonNode
                | String -> yield responseString
                | Stream -> yield responseStream
                | _ ->
                    yield responseStream
                    yield jsonNode
            ]
            |> SynExpr.createCompExpr "async" returnExpr
            |> SynExpr.startAsTask (SynLongIdent.CreateFromLongIdent [ cancellationTokenArg ])

        SynMemberDefn.Member (
            SynBinding.SynBinding (
                info.Accessibility,
                SynBindingKind.Normal,
                false,
                false,
                [],
                PreXmlDoc.Empty,
                valData,
                headPat,
                None,
                implementation,
                range0,
                DebugPointAtBinding.Yes range0,
                SynExpr.synBindingTriviaZero true
            ),
            range0
        )

    let getHttpAttributes (attrs : SynAttribute list) : HttpAttribute list =
        attrs
        |> List.choose (fun attr ->
            match attr.TypeName.AsString with
            | "Query"
            | "QueryAttribute" ->
                match attr.ArgExpr with
                | SynExpr.Const (SynConst.Unit, _) -> Some (HttpAttribute.Query None)
                | SynExpr.Const (SynConst.String (s, SynStringKind.Regular, _), _) ->
                    Some (HttpAttribute.Query (Some s))
                | SynExpr.Const (a, _) -> failwith $"unrecognised constant arg to the Query attribute: %+A{a}"
                | _ -> None
            | "Path"
            | "PathAttribute" ->
                match attr.ArgExpr with
                | SynExpr.Const (SynConst.String (s, SynStringKind.Regular, _), _) ->
                    Some (HttpAttribute.Path (PathSpec.Verbatim s))
                | SynExpr.Const (SynConst.Unit, _) -> Some (HttpAttribute.Path PathSpec.MatchArgName)
                | SynExpr.Const (a, _) -> failwith $"unrecognised constant arg to the Path attribute: %+A{a}"
                | _ -> None
            | "Body"
            | "BodyAttribute" ->
                match attr.ArgExpr with
                | SynExpr.Const (SynConst.Unit, _) -> Some HttpAttribute.Body
                | SynExpr.Const (a, _) -> failwith $"unrecognised constant arg to the Body attribute: %+A{a}"
                | _ -> None
            | _ -> None
        )

    let extractBasePath (attrs : SynAttribute list) : SynExpr option =
        attrs
        |> List.tryPick (fun attr ->
            match attr.TypeName.AsString with
            | "BasePath"
            | "RestEase.BasePath"
            | "BasePathAttribute"
            | "RestEase.BasePathAttribute" -> Some attr.ArgExpr
            | _ -> None
        )

    let extractBaseAddress (attrs : SynAttribute list) : SynExpr option =
        attrs
        |> List.tryPick (fun attr ->
            match attr.TypeName.AsString with
            | "BaseAddress"
            | "RestEase.BaseAddress"
            | "BaseAddressAttribute"
            | "RestEase.BaseAddressAttribute" -> Some attr.ArgExpr
            | _ -> None
        )

    let createModule
        (opens : SynOpenDeclTarget list)
        (ns : LongIdent)
        (interfaceType : SynTypeDefn)
        : SynModuleOrNamespace
        =
        let interfaceType = AstHelper.parseInterface interfaceType

        if not (List.isEmpty interfaceType.Inherits) then
            failwith
                "HttpClientGenerator does not support inheritance. Remove the `inherit` keyword if you want to use this generator."

        let constantHeaders =
            interfaceType.Attributes
            |> extractHeaderInformation
            |> List.map (fun exprs ->
                match exprs with
                | [ key ; value ] -> key, value
                | [] ->
                    failwith
                        "Expected constant header parameters to be of the form [<Header (key, value)>], but got no args"
                | [ _ ] ->
                    failwith
                        "Expected constant header parameters to be of the form [<Header (key, value)>], but got only one arg"
                | _ ->
                    failwith
                        "Expected constant header parameters to be of the form [<Header (key, value)>], but got more than two args"
            )

        let baseAddress = extractBaseAddress interfaceType.Attributes
        let basePath = extractBasePath interfaceType.Attributes

        let properties =
            interfaceType.Properties
            |> List.map (fun pi ->
                let headerInfo =
                    match extractHeaderInformation pi.Attributes with
                    | [ [ x ] ] -> x
                    | [ xs ] ->
                        failwith
                            "Expected exactly one Header parameter on the member, with exactly one arg; got one Header parameter with non-1-many args"
                    | [] ->
                        failwith
                            "Expected exactly one Header parameter on the member, with exactly one arg; got no Header parameters"
                    | _ ->
                        failwith
                            "Expected exactly one Header parameter on the member, with exactly one arg; got multiple Header parameters"

                headerInfo, pi
            )

        let nonPropertyMembers =
            let properties = properties |> List.map (fun (header, pi) -> header, pi.Identifier)

            interfaceType.Members
            |> List.map (fun mem ->
                let httpMethod, url = extractHttpInformation mem.Attributes

                let shouldEnsureSuccess = not (shouldAllowAnyStatusCode mem.Attributes)

                let returnType =
                    match mem.ReturnType with
                    | Task ty -> ty
                    | a -> failwith $"Method must return a generic Task; returned %+A{a}"

                if mem.IsMutable then
                    failwith $"mutable methods not supported (identifier: %+A{mem.Identifier})"

                if mem.IsInline then
                    failwith $"inline methods not supported (identifier: %+A{mem.Identifier})"

                let args =
                    match mem.Args with
                    | [ args ] ->
                        args.Args
                        |> List.map (fun arg ->
                            {
                                Attributes = arg.Attributes |> getHttpAttributes
                                IsOptional = arg.IsOptional
                                Id = arg.Id
                                Type = arg.Type
                            }
                        )
                    | [] -> failwith $"Expected %+A{mem.Identifier} to have tupled args, but it had no args."
                    | _ ->
                        failwith
                            $"Expected %+A{mem.Identifier} to have tupled args, but it was curried: %+A{mem.Args}."

                {
                    HttpMethod = httpMethod
                    UrlTemplate = url
                    TaskReturnType = returnType
                    Args = args
                    Identifier = mem.Identifier
                    EnsureSuccessHttpCode = shouldEnsureSuccess
                    BaseAddress = baseAddress
                    BasePath = basePath
                    Accessibility = mem.Accessibility
                }
            )
            |> List.map (constructMember constantHeaders properties)

        let propertyMembers =
            properties
            |> List.map (fun (_, pi) ->
                SynMemberDefn.Member (
                    SynBinding.SynBinding (
                        pi.Accessibility,
                        SynBindingKind.Normal,
                        pi.IsInline,
                        false,
                        [],
                        PreXmlDoc.Empty,
                        SynValData.SynValData (
                            Some
                                {
                                    IsInstance = true
                                    IsDispatchSlot = false
                                    IsOverrideOrExplicitImpl = true
                                    IsFinal = false
                                    GetterOrSetterIsCompilerGenerated = false
                                    MemberKind = SynMemberKind.Member
                                },
                            SynValInfo.SynValInfo ([ [ SynArgInfo.Empty ] ; [] ], SynArgInfo.Empty),
                            None
                        ),
                        SynPat.CreateLongIdent (
                            SynLongIdent.CreateFromLongIdent [ Ident.Create "_" ; pi.Identifier ],
                            []
                        ),
                        Some (SynBindingReturnInfo.Create pi.Type),
                        SynExpr.CreateApp (
                            SynExpr.CreateLongIdent (
                                SynLongIdent.CreateFromLongIdent [ Ident.lowerFirstLetter pi.Identifier ]
                            ),
                            SynExpr.CreateConst SynConst.Unit
                        ),
                        range0,
                        DebugPointAtBinding.Yes range0,
                        {
                            LeadingKeyword = SynLeadingKeyword.Member range0
                            InlineKeyword = if pi.IsInline then Some range0 else None
                            EqualsRange = Some range0
                        }
                    ),
                    range0
                )
            )

        let members = propertyMembers @ nonPropertyMembers

        let docString = PreXmlDoc.Create " Module for constructing a REST client."

        let interfaceImpl =
            SynExpr.ObjExpr (
                SynType.LongIdent (SynLongIdent.CreateFromLongIdent interfaceType.Name),
                None,
                Some range0,
                [],
                members,
                [],
                range0,
                range0
            )

        let headerArgs =
            properties
            |> List.map (fun (_, pi) ->
                SynPat.CreateTyped (
                    SynPat.CreateNamed (Ident.lowerFirstLetter pi.Identifier),
                    SynType.CreateFun (SynType.CreateLongIdent "unit", pi.Type)
                )
                |> SynPat.CreateParen
            )

        let clientCreationArg =
            SynPat.CreateTyped (
                SynPat.CreateNamed (Ident.Create "client"),
                SynType.CreateLongIdent (SynLongIdent.Create [ "System" ; "Net" ; "Http" ; "HttpClient" ])
            )
            |> SynPat.CreateParen

        let xmlDoc =
            if properties.IsEmpty then
                " Create a REST client."
            else
                " Create a REST client. The input functions will be re-evaluated on every HTTP request to obtain the required values for the corresponding header properties."

        let createFunc =
            SynBinding.SynBinding (
                None,
                SynBindingKind.Normal,
                false,
                false,
                [],
                PreXmlDoc.Create xmlDoc,
                SynValData.SynValData (
                    None,
                    SynValInfo.SynValInfo (
                        [ [ SynArgInfo.SynArgInfo ([], false, Some (Ident.Create "client")) ] ],
                        SynArgInfo.Empty
                    ),
                    None
                ),
                SynPat.CreateLongIdent (SynLongIdent.CreateString "make", headerArgs @ [ clientCreationArg ]),
                Some (
                    SynBindingReturnInfo.Create (
                        SynType.LongIdent (SynLongIdent.CreateFromLongIdent interfaceType.Name)
                    )
                ),
                interfaceImpl,
                range0,
                DebugPointAtBinding.NoneAtLet,
                SynExpr.synBindingTriviaZero false
            )
            |> List.singleton
            |> SynModuleDecl.CreateLet

        let moduleName : LongIdent =
            List.last interfaceType.Name
            |> _.idText
            |> fun s ->
                if s.StartsWith 'I' then
                    s.[1..]
                else
                    failwith $"Expected interface type to start with 'I', but was: %s{s}"
            |> Ident.Create
            |> List.singleton

        let attribs =
            [
                SynAttributeList.Create SynAttribute.compilationRepresentation
                SynAttributeList.Create (SynAttribute.RequireQualifiedAccess ())
            ]

        let modInfo =
            SynComponentInfo.Create (
                moduleName,
                attributes = attribs,
                xmldoc = docString,
                access = interfaceType.Accessibility
            )

        SynModuleOrNamespace.CreateNamespace (
            ns,
            decls =
                [
                    for openStatement in opens do
                        yield SynModuleDecl.CreateOpen openStatement
                    yield SynModuleDecl.CreateNestedModule (modInfo, [ createFunc ])
                ]
        )

/// Myriad generator that provides an HTTP client for an interface type using RestEase annotations.
[<MyriadGenerator("http-client")>]
type HttpClientGenerator () =

    interface IMyriadGenerator with
        member _.ValidInputExtensions = [ ".fs" ]

        member _.Generate (context : GeneratorContext) =
            let ast, _ =
                Ast.fromFilename context.InputFilename |> Async.RunSynchronously |> Array.head

            let types = Ast.extractTypeDefn ast

            let opens = AstHelper.extractOpens ast

            let namespaceAndTypes =
                types
                |> List.choose (fun (ns, types) ->
                    match types |> List.filter Ast.hasAttribute<HttpClientAttribute> with
                    | [] -> None
                    | types -> Some (ns, types)
                )

            let modules =
                namespaceAndTypes
                |> List.collect (fun (ns, types) -> types |> List.map (HttpClientGenerator.createModule opens ns))

            Output.Ast modules
