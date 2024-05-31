namespace WoofWare.Myriad.Plugins

open System.Net.Http
open Fantomas.FCS.Syntax
open Fantomas.FCS.SyntaxTrivia
open Fantomas.FCS.Xml
open Myriad.Core

type internal HttpClientGeneratorOutputSpec =
    {
        ExtensionMethods : bool
    }

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
                match SynLongIdent.toString attr.TypeName with
                | "Get"
                | "GetAttribute"
                | "WoofWare.Myriad.Plugins.RestEase.Get"
                | "WoofWare.Myriad.Plugins.RestEase.GetAttribute"
                | "RestEase.Get"
                | "RestEase.GetAttribute" -> Some (HttpMethod.Get, attr.ArgExpr)
                | "Post"
                | "PostAttribute"
                | "WoofWare.Myriad.Plugins.RestEase.Post"
                | "WoofWare.Myriad.Plugins.RestEase.PostAttribute"
                | "RestEase.Post"
                | "RestEase.PostAttribute" -> Some (HttpMethod.Post, attr.ArgExpr)
                | "Put"
                | "PutAttribute"
                | "WoofWare.Myriad.Plugins.RestEase.Put"
                | "WoofWare.Myriad.Plugins.RestEase.PutAttribute"
                | "RestEase.Put"
                | "RestEase.PutAttribute" -> Some (HttpMethod.Put, attr.ArgExpr)
                | "Delete"
                | "DeleteAttribute"
                | "WoofWare.Myriad.Plugins.RestEase.Delete"
                | "WoofWare.Myriad.Plugins.RestEase.DeleteAttribute"
                | "RestEase.Delete"
                | "RestEase.DeleteAttribute" -> Some (HttpMethod.Delete, attr.ArgExpr)
                | "Head"
                | "HeadAttribute"
                | "WoofWare.Myriad.Plugins.RestEase.Head"
                | "WoofWare.Myriad.Plugins.RestEase.HeadAttribute"
                | "RestEase.Head"
                | "RestEase.HeadAttribute" -> Some (HttpMethod.Head, attr.ArgExpr)
                | "Options"
                | "OptionsAttribute"
                | "WoofWare.Myriad.Plugins.RestEase.Options"
                | "WoofWare.Myriad.Plugins.RestEase.OptionsAttribute"
                | "RestEase.Options"
                | "RestEase.OptionsAttribute" -> Some (HttpMethod.Options, attr.ArgExpr)
                | "Patch"
                | "PatchAttribute"
                | "WoofWare.Myriad.Plugins.RestEase.Patch"
                | "WoofWare.Myriad.Plugins.RestEase.PatchAttribute"
                | "RestEase.Patch"
                | "RestEase.PatchAttribute" -> Some (HttpMethod.Patch, attr.ArgExpr)
                | "Trace"
                | "TraceAttribute"
                | "WoofWare.Myriad.Plugins.RestEase.Trace"
                | "WoofWare.Myriad.Plugins.RestEase.TraceAttribute"
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
            match SynLongIdent.toString attr.TypeName with
            | "Header"
            | "RestEase.Header"
            | "WoofWare.Myriad.Plugins.RestEase.Header" ->
                match attr.ArgExpr with
                | SynExpr.Paren (SynExpr.Tuple (_, [ v1 ; v2 ], _, _), _, _, _) ->
                    Some [ SynExpr.stripOptionalParen v1 ; SynExpr.stripOptionalParen v2 ]
                | e -> Some [ SynExpr.stripOptionalParen e ]
            | _ -> None
        )

    let shouldAllowAnyStatusCode (attrs : SynAttribute list) : bool =
        attrs
        |> List.exists (fun attr ->
            match SynLongIdent.toString attr.TypeName with
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
                        SynType.appPostfix "option" arg.Type
                    else
                        arg.Type

                argName, SynPat.CreateTyped (SynPat.CreateNamed argName, argType)
            )

        let cancellationTokenArg =
            match List.tryLast args with
            | None -> failwith $"expected an optional cancellation token as final arg in %s{info.Identifier.idText}"
            | Some (arg, _) -> arg

        let headPat =
            let thisIdent = if variableHeaders.IsEmpty then "_" else "this"

            args
            |> List.map snd
            |> SynPat.tuple
            |> List.singleton
            |> SynArgPats.Pats
            |> SynPat.identWithArgs [ Ident.create thisIdent ; info.Identifier ]

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
                            (SynExpr.tuple
                                [
                                    SynExpr.CreateConst ("{" + substituteId + "}")
                                    SynExpr.callMethod "ToString" (SynExpr.createIdent' varName)
                                    |> SynExpr.pipeThroughFunction (
                                        SynExpr.createLongIdent [ "System" ; "Web" ; "HttpUtility" ; "UrlEncode" ]
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

                let urlSeparator =
                    // apparent Myriad bug: `IndexOf '?'` gets formatted as `IndexOf ?` which is clearly wrong
                    let questionMark =
                        SynExpr.CreateConst 63
                        |> SynExpr.applyFunction (SynExpr.createIdent "char")
                        |> SynExpr.paren

                    let containsQuestion =
                        info.UrlTemplate
                        |> SynExpr.callMethodArg "IndexOf" questionMark
                        |> SynExpr.greaterThanOrEqual (SynExpr.CreateConst 0)

                    SynExpr.ifThenElse containsQuestion (SynExpr.CreateConst "?") (SynExpr.CreateConst "&")
                    |> SynExpr.paren

                let prefix =
                    SynExpr.createIdent' firstValueId
                    |> SynExpr.toString firstValue.Type
                    |> SynExpr.paren
                    |> SynExpr.pipeThroughFunction (
                        SynExpr.createLongIdent [ "System" ; "Web" ; "HttpUtility" ; "UrlEncode" ]
                    )
                    |> SynExpr.paren
                    |> SynExpr.plus (SynExpr.plus urlSeparator (SynExpr.CreateConst (firstKey + "=")))

                (prefix, queryParams)
                ||> List.fold (fun uri (paramKey, paramValue) ->
                    let paramValueId =
                        match paramValue.Id with
                        | None -> failwith "Unable to get parameter variable name from anonymous parameter"
                        | Some id -> id

                    SynExpr.toString paramValue.Type (SynExpr.createIdent' paramValueId)
                    |> SynExpr.paren
                    |> SynExpr.pipeThroughFunction (
                        SynExpr.createLongIdent [ "System" ; "Web" ; "HttpUtility" ; "UrlEncode" ]
                    )
                    |> SynExpr.paren
                    |> SynExpr.plus (SynExpr.plus uri (SynExpr.CreateConst ("&" + paramKey + "=")))
                )
                |> SynExpr.plus requestUriTrailer
                |> SynExpr.paren

        let requestUri =
            let uriIdent = SynExpr.createLongIdent [ "System" ; "Uri" ]

            let baseAddress = SynExpr.createLongIdent [ "client" ; "BaseAddress" ]

            let baseAddress =
                [
                    SynMatchClause.create
                        SynPat.createNull
                        (match info.BaseAddress with
                         | None ->
                             [
                                 SynExpr.applyFunction (SynExpr.createIdent "nameof") (SynExpr.paren baseAddress)
                                 SynExpr.CreateConst
                                     "No base address was supplied on the type, and no BaseAddress was on the HttpClient."
                             ]
                             |> SynExpr.tuple
                             |> SynExpr.applyFunction (SynExpr.createLongIdent [ "System" ; "ArgumentNullException" ])
                             |> SynExpr.paren
                             |> SynExpr.applyFunction (SynExpr.createIdent "raise")
                         | Some expr -> SynExpr.applyFunction uriIdent expr)
                    SynMatchClause.create (SynPat.named "v") (SynExpr.createIdent "v")
                ]
                |> SynExpr.createMatch baseAddress
                |> SynExpr.paren

            [
                baseAddress
                SynExpr.applyFunction
                    uriIdent
                    (SynExpr.tuple
                        [
                            requestUriTrailer
                            SynExpr.createLongIdent [ "System" ; "UriKind" ; "Relative" ]
                        ])
            ]
            |> SynExpr.tuple
            |> SynExpr.applyFunction uriIdent

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
                    (SynExpr.createIdent "Method")
                    (SynExpr.createLongIdent
                        [ "System" ; "Net" ; "Http" ; "HttpMethod" ; httpMethodString info.HttpMethod ])
                SynExpr.equals (SynExpr.createIdent "RequestUri") (SynExpr.createIdent "uri")
            ]
            |> SynExpr.tupleNoParen

        let returnExpr =
            match info.TaskReturnType with
            | HttpResponseMessage -> SynExpr.createIdent "response"
            | String -> SynExpr.createIdent "responseString"
            | Stream -> SynExpr.createIdent "responseStream"
            | RestEaseResponseType contents ->
                let deserialiser =
                    JsonParseGenerator.parseNode
                        None
                        JsonParseGenerator.JsonParseOption.None
                        contents
                        (SynExpr.createIdent "jsonNode")
                    |> SynExpr.paren
                    |> SynExpr.createThunk

                // new RestEase.Response (content : string, response : HttpResponseMessage, deserialiser : unit -> 'T)
                SynExpr.createNew
                    (SynType.app' (SynType.createLongIdent' [ "RestEase" ; "Response" ]) [ SynType.Anon range0 ])
                    (SynExpr.CreateTuple
                        [
                            SynExpr.createIdent "responseString"
                            SynExpr.createIdent "response"
                            deserialiser
                        ])
            | retType ->
                JsonParseGenerator.parseNode
                    None
                    JsonParseGenerator.JsonParseOption.None
                    retType
                    (SynExpr.createIdent "jsonNode")

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
                            SynExpr.createNew
                                (SynType.createLongIdent'
                                    [ "System" ; "Net" ; "Http" ; (bodyParamType : BodyParamMethods).ToString () ])
                                (SynExpr.createIdent' bodyParamName)
                        )
                        Do (
                            SynExpr.LongIdentSet (
                                SynLongIdent.createS' [ "httpMessage" ; "Content" ],
                                SynExpr.createIdent "queryParams",
                                range0
                            )
                        )
                    ]
                | BodyParamMethods.HttpContent ->
                    [
                        Do (
                            SynExpr.LongIdentSet (
                                SynLongIdent.createS' [ "httpMessage" ; "Content" ],
                                SynExpr.createIdent' bodyParamName,
                                range0
                            )
                        )
                    ]
                | BodyParamMethods.Serialise ty ->
                    [
                        Let (
                            "queryParams",
                            SynExpr.createNew
                                (SynType.createLongIdent' [ "System" ; "Net" ; "Http" ; "StringContent" ])
                                (SynExpr.createIdent' bodyParamName
                                 |> SynExpr.pipeThroughFunction (JsonSerializeGenerator.serializeNode ty)
                                 |> SynExpr.pipeThroughFunction (
                                     SynExpr.createLambda
                                         "node"
                                         (SynExpr.ifThenElse
                                             (SynExpr.applyFunction
                                                 (SynExpr.createIdent "isNull")
                                                 (SynExpr.createIdent "node"))
                                             (SynExpr.applyFunction
                                                 (SynExpr.createLongIdent [ "node" ; "ToJsonString" ])
                                                 (SynExpr.CreateConst ()))
                                             (SynExpr.CreateConst "null"))
                                 ))
                        )
                        Do (
                            SynExpr.LongIdentSet (
                                SynLongIdent.createS' [ "httpMessage" ; "Content" ],
                                SynExpr.createIdent "queryParams",
                                range0
                            )
                        )
                    ]

        let implementation =
            let responseString =
                LetBang (
                    "responseString",
                    SynExpr.awaitTask (
                        SynExpr.applyFunction
                            (SynExpr.createLongIdent [ "response" ; "Content" ; "ReadAsStringAsync" ])
                            (SynExpr.createIdent "ct")
                    )
                )

            let responseStream =
                LetBang (
                    "responseStream",
                    SynExpr.awaitTask (
                        SynExpr.applyFunction
                            (SynExpr.createLongIdent [ "response" ; "Content" ; "ReadAsStreamAsync" ])
                            (SynExpr.createIdent "ct")
                    )
                )

            let jsonNode =
                LetBang (
                    "jsonNode",
                    SynExpr.awaitTask (
                        SynExpr.applyFunction
                            (SynExpr.createLongIdent
                                [ "System" ; "Text" ; "Json" ; "Nodes" ; "JsonNode" ; "ParseAsync" ])
                            (SynExpr.tuple
                                [
                                    SynExpr.createIdent "responseStream"
                                    SynExpr.equals (SynExpr.createIdent "cancellationToken") (SynExpr.createIdent "ct")
                                ])
                    )
                )

            let setVariableHeaders =
                variableHeaders
                |> List.map (fun (headerName, callToGetValue) ->
                    [
                        headerName
                        SynExpr.applyFunction
                            (SynExpr.createLongIdent'
                                [ Ident.create "this" ; callToGetValue ; Ident.create "ToString" ])
                            (SynExpr.CreateConst ())
                    ]
                    |> SynExpr.tuple
                    |> SynExpr.applyFunction (SynExpr.createLongIdent [ "httpMessage" ; "Headers" ; "Add" ])
                    |> Do
                )

            let setConstantHeaders =
                constantHeaders
                |> List.map (fun (headerName, headerValue) ->
                    SynExpr.applyFunction
                        (SynExpr.createLongIdent [ "httpMessage" ; "Headers" ; "Add" ])
                        (SynExpr.tuple [ headerName ; headerValue ])
                    |> Do
                )

            [
                yield LetBang ("ct", SynExpr.createLongIdent [ "Async" ; "CancellationToken" ])
                yield Let ("uri", requestUri)
                yield
                    Use (
                        "httpMessage",
                        SynExpr.createNew
                            (SynType.createLongIdent' [ "System" ; "Net" ; "Http" ; "HttpRequestMessage" ])
                            httpReqMessageConstructor
                    )

                yield! handleBodyParams

                yield! setVariableHeaders
                yield! setConstantHeaders

                yield
                    LetBang (
                        "response",
                        SynExpr.awaitTask (
                            SynExpr.applyFunction
                                (SynExpr.createLongIdent [ "client" ; "SendAsync" ])
                                (SynExpr.tuple [ SynExpr.createIdent "httpMessage" ; SynExpr.createIdent "ct" ])
                        )
                    )
                if info.EnsureSuccessHttpCode then
                    yield
                        Let (
                            "response",
                            SynExpr.applyFunction
                                (SynExpr.createLongIdent [ "response" ; "EnsureSuccessStatusCode" ])
                                (SynExpr.CreateConst ())
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
            |> SynExpr.startAsTask cancellationTokenArg

        SynBinding.SynBinding (
            None,
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
            SynBinding.triviaZero true
        )
        |> SynBinding.withAccessibility info.Accessibility
        |> fun b -> SynMemberDefn.Member (b, range0)

    let getHttpAttributes (attrs : SynAttribute list) : HttpAttribute list =
        attrs
        |> List.choose (fun attr ->
            match SynLongIdent.toString attr.TypeName with
            | "RestEase.Query"
            | "RestEase.QueryAttribute"
            | "WoofWare.Myriad.Plugins.RestEase.Query"
            | "WoofWare.Myriad.Plugins.RestEase.QueryAttribute"
            | "Query"
            | "QueryAttribute" ->
                match attr.ArgExpr with
                | SynExpr.Const (SynConst.Unit, _) -> Some (HttpAttribute.Query None)
                | SynExpr.Const (SynConst.String (s, SynStringKind.Regular, _), _) ->
                    Some (HttpAttribute.Query (Some s))
                | SynExpr.Const (a, _) -> failwith $"unrecognised constant arg to the Query attribute: %+A{a}"
                | _ -> None
            | "RestEase.Path"
            | "RestEase.PathAttribute"
            | "WoofWare.Myriad.Plugins.RestEase.Path"
            | "WoofWare.Myriad.Plugins.RestEase.PathAttribute"
            | "Path"
            | "PathAttribute" ->
                match attr.ArgExpr |> SynExpr.stripOptionalParen with
                | SynExpr.Const (SynConst.String (s, SynStringKind.Regular, _), _) ->
                    Some (HttpAttribute.Path (PathSpec.Verbatim s))
                | SynExpr.Const (SynConst.Unit, _) -> Some (HttpAttribute.Path PathSpec.MatchArgName)
                | SynExpr.Const (a, _) -> failwith $"unrecognised constant arg to the Path attribute: %+A{a}"
                | _ -> None
            | "RestEase.Body"
            | "RestEase.BodyAttribute"
            | "WoofWare.Myriad.Plugins.RestEase.Body"
            | "WoofWare.Myriad.Plugins.RestEase.BodyAttribute"
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
            match SynLongIdent.toString attr.TypeName with
            | "BasePath"
            | "RestEase.BasePath"
            | "WoofWare.Myriad.Plugins.RestEase.BasePath"
            | "BasePathAttribute"
            | "RestEase.BasePathAttribute"
            | "WoofWare.Myriad.Plugins.RestEase.BasePathAttribute" -> Some attr.ArgExpr
            | _ -> None
        )

    let extractBaseAddress (attrs : SynAttribute list) : SynExpr option =
        attrs
        |> List.tryPick (fun attr ->
            match SynLongIdent.toString attr.TypeName with
            | "BaseAddress"
            | "RestEase.BaseAddress"
            | "WoofWare.Myriad.Plugins.RestEase.BaseAddress"
            | "BaseAddressAttribute"
            | "RestEase.BaseAddressAttribute"
            | "WoofWare.Myriad.Plugins.RestEase.BaseAddressAttribute" -> Some attr.ArgExpr
            | _ -> None
        )

    let createModule
        (opens : SynOpenDeclTarget list)
        (ns : LongIdent)
        (interfaceType : SynTypeDefn, spec : HttpClientGeneratorOutputSpec)
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
                        SynPat.CreateLongIdent (SynLongIdent.create [ Ident.create "_" ; pi.Identifier ], []),
                        Some (SynBindingReturnInfo.Create pi.Type),
                        SynExpr.applyFunction
                            (SynExpr.createLongIdent' [ Ident.lowerFirstLetter pi.Identifier ])
                            (SynExpr.CreateConst ()),
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

        let docString =
            (if spec.ExtensionMethods then
                 "Extension methods"
             else
                 "Module")
            |> sprintf "%s for constructing a REST client."
            |> PreXmlDoc.create

        let interfaceImpl =
            SynExpr.ObjExpr (
                SynType.createLongIdent interfaceType.Name,
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
                SynPat.namedI (Ident.lowerFirstLetter pi.Identifier)
                |> SynPat.annotateType (SynType.funFromDomain (SynType.named "unit") pi.Type)
            )

        let clientCreationArg =
            SynPat.named "client"
            |> SynPat.annotateType (SynType.createLongIdent' [ "System" ; "Net" ; "Http" ; "HttpClient" ])

        let xmlDoc =
            if properties.IsEmpty then
                "Create a REST client."
            else
                "Create a REST client. The input functions will be re-evaluated on every HTTP request to obtain the required values for the corresponding header properties."
            |> PreXmlDoc.create

        let functionName = Ident.create "client"

        let valData =
            let memberFlags =
                if spec.ExtensionMethods then
                    {
                        SynMemberFlags.IsInstance = false
                        SynMemberFlags.IsDispatchSlot = false
                        SynMemberFlags.IsOverrideOrExplicitImpl = false
                        SynMemberFlags.IsFinal = false
                        SynMemberFlags.GetterOrSetterIsCompilerGenerated = false
                        SynMemberFlags.MemberKind = SynMemberKind.Member
                    }
                    |> Some
                else
                    None

            SynValData.SynValData (
                memberFlags,
                SynValInfo.SynValInfo ([ [ SynArgInfo.SynArgInfo ([], false, Some functionName) ] ], SynArgInfo.Empty),
                None
            )

        let pattern = SynLongIdent.createS "make"

        let returnInfo = SynType.createLongIdent interfaceType.Name

        let nameWithoutLeadingI =
            List.last interfaceType.Name
            |> _.idText
            |> fun s ->
                if s.StartsWith 'I' then
                    s.Substring 1
                else
                    failwith $"Expected interface type to start with 'I', but was: %s{s}"

        let createFunc =
            if spec.ExtensionMethods then
                let binding =
                    SynBinding.basic [ Ident.create "make" ] (headerArgs @ [ clientCreationArg ]) interfaceImpl
                    |> SynBinding.withXmlDoc xmlDoc
                    |> SynBinding.withReturnAnnotation returnInfo
                    |> SynMemberDefn.staticMember

                let componentInfo =
                    SynComponentInfo.create (Ident.create nameWithoutLeadingI)
                    |> SynComponentInfo.withDocString (PreXmlDoc.create "Extension methods for HTTP clients")

                let containingType =
                    SynTypeDefnRepr.augmentation ()
                    |> SynTypeDefn.create componentInfo
                    |> SynTypeDefn.withMemberDefns [ binding ]

                SynModuleDecl.Types ([ containingType ], range0)

            else
                SynBinding.basic [ Ident.create "make" ] (headerArgs @ [ clientCreationArg ]) interfaceImpl
                |> SynBinding.withXmlDoc xmlDoc
                |> SynBinding.withReturnAnnotation returnInfo
                |> List.singleton
                |> SynModuleDecl.CreateLet

        let moduleName =
            if spec.ExtensionMethods then
                Ident.create (nameWithoutLeadingI + "HttpClientExtension")
            else
                Ident.create nameWithoutLeadingI

        let attribs =
            if spec.ExtensionMethods then
                [ SynAttribute.autoOpen ]
            else
                [ SynAttribute.compilationRepresentation ; SynAttribute.requireQualifiedAccess ]

        let modInfo =
            SynComponentInfo.create moduleName
            |> SynComponentInfo.withDocString docString
            |> SynComponentInfo.addAttributes attribs
            |> SynComponentInfo.setAccessibility interfaceType.Accessibility

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
                    types
                    |> List.choose (fun typeDef ->
                        match Ast.getAttribute<HttpClientAttribute> typeDef with
                        | None -> None
                        | Some attr ->
                            let arg =
                                match SynExpr.stripOptionalParen attr.ArgExpr with
                                | SynExpr.Const (SynConst.Bool value, _) -> value
                                | SynExpr.Const (SynConst.Unit, _) -> JsonParseAttribute.DefaultIsExtensionMethod
                                | arg ->
                                    failwith
                                        $"Unrecognised argument %+A{arg} to [<%s{nameof HttpClientAttribute}>]. Literals are not supported. Use `true` or `false` (or unit) only."

                            let spec =
                                {
                                    ExtensionMethods = arg
                                }

                            Some (typeDef, spec)
                    )
                    |> function
                        | [] -> None
                        | ty -> Some (ns, ty)
                )

            let modules =
                namespaceAndTypes
                |> List.collect (fun (ns, types) -> types |> List.map (HttpClientGenerator.createModule opens ns))

            Output.Ast modules
