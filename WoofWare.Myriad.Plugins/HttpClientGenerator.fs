namespace WoofWare.Myriad.Plugins

open System.Net.Http
open Fantomas.FCS.Syntax
open WoofWare.Whippet.Fantomas

type internal HttpClientGeneratorOutputSpec =
    {
        ExtensionMethods : bool
    }

[<RequireQualifiedAccess>]
module internal HttpClientGenerator =
    open Fantomas.FCS.Text.Range

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
            /// Headers which apply *only* to this endpoint.
            /// For example, SynConst "Authorization" and SynConst "token BLAH".
            Headers : (SynExpr * SynExpr) list
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

                // We'll be tupling these up anyway, so don't need the parens
                // around the type annotations.
                argName, SynPat.annotateTypeNoParen argType (SynPat.namedI argName)
            )

        let cancellationTokenArg =
            match List.tryLast args with
            | None -> failwith $"expected an optional cancellation token as final arg in %s{info.Identifier.idText}"
            | Some (arg, _) -> arg

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
                                        SynExpr.createLongIdent [ "System" ; "Uri" ; "EscapeDataString" ]
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
                    let questionMark = SynExpr.CreateConst '?'

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
                    |> SynExpr.pipeThroughFunction (SynExpr.createLongIdent [ "System" ; "Uri" ; "EscapeDataString" ])
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
                    |> SynExpr.pipeThroughFunction (SynExpr.createLongIdent [ "System" ; "Uri" ; "EscapeDataString" ])
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

            let baseAddress =
                match info.BasePath with
                | None -> baseAddress
                | Some basePath ->
                    [
                        yield baseAddress

                        yield
                            SynExpr.applyFunction
                                uriIdent
                                (SynExpr.tuple
                                    [ basePath ; SynExpr.createLongIdent [ "System" ; "UriKind" ; "Relative" ] ])
                    ]
                    |> SynExpr.tuple
                    |> SynExpr.applyFunction uriIdent

            [
                yield baseAddress

                yield
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
                match JsonNodeWithNullability.Identify contents with
                | CannotBeNull ->
                    let deserialiser =
                        JsonParseGenerator.parseNonNullableNode
                            None
                            JsonParseGenerator.JsonParseOption.None
                            contents
                            (SynExpr.createIdent "jsonNode")
                        |> SynExpr.paren
                        |> SynExpr.createThunk

                    // new RestEase.Response (content : string, response : HttpResponseMessage, deserialiser : unit -> 'T)
                    SynExpr.createNew
                        (SynType.app' (SynType.createLongIdent' [ "RestEase" ; "Response" ]) [ SynType.Anon range0 ])
                        (SynExpr.tupleNoParen
                            [
                                SynExpr.createIdent "responseString"
                                SynExpr.createIdent "response"
                                deserialiser
                            ])
                | Nullable ->
                    let deserialiser =
                        JsonParseGenerator.parseNullableNode
                            None
                            JsonParseGenerator.JsonParseOption.None
                            contents
                            (SynExpr.createIdent "jsonNode")
                        |> SynExpr.paren
                        |> SynExpr.createThunk

                    // new RestEase.Response (content : string, response : HttpResponseMessage, deserialiser : unit -> 'T)
                    SynExpr.createNew
                        (SynType.app' (SynType.createLongIdent' [ "RestEase" ; "Response" ]) [ SynType.Anon range0 ])
                        (SynExpr.tupleNoParen
                            [
                                SynExpr.createIdent "responseString"
                                SynExpr.createIdent "response"
                                deserialiser
                            ])
            | retType ->
                match JsonNodeWithNullability.Identify retType with
                | Nullable ->
                    JsonParseGenerator.parseNullableNode
                        None
                        JsonParseGenerator.JsonParseOption.None
                        retType
                        (SynExpr.createIdent "jsonNode")
                | CannotBeNull ->
                    JsonParseGenerator.parseNonNullableNode
                        None
                        JsonParseGenerator.JsonParseOption.None
                        retType
                        (SynExpr.createIdent "jsonNode")

        let contentTypeHeader, memberHeaders =
            info.Headers
            |> List.partition (fun (headerName, headerValue) ->
                match headerName |> SynExpr.stripOptionalParen with
                | SynExpr.Const (SynConst.String ("Content-Type", _, _), _) -> true
                | _ -> false
            )

        let contentTypeHeader =
            match contentTypeHeader with
            | [] -> None
            | [ _, ct ] -> Some (SynExpr.stripOptionalParen ct)
            | _ -> failwith "Unexpectedly got multiple Content-Type headers"

        let createStringContent (contents : SynExpr) =
            SynExpr.createNew
                (SynType.createLongIdent' [ "System" ; "Net" ; "Http" ; "StringContent" ])
                (SynExpr.tupleNoParen
                    [
                        yield contents
                        match contentTypeHeader with
                        | None -> ()
                        | Some ch ->
                            yield SynExpr.createNull ()
                            // Sigh, Gitea in particular passes "json" here
                            match ch with
                            | SynExpr.Const (SynConst.String ("json", _, _), _) ->
                                yield SynExpr.CreateConst "application/json"
                            | SynExpr.Const (SynConst.String ("html", _, _), _) -> yield SynExpr.CreateConst "text/html"
                            | _ -> yield ch
                    ])

        let handleBodyParams =
            match bodyParam with
            | None -> []
            | Some (bodyParamType, bodyParamName) ->
                match bodyParamType with
                | BodyParamMethods.StringContent ->
                    [
                        Let ("queryParams", createStringContent (SynExpr.createIdent' bodyParamName))
                        Do (
                            SynExpr.assign
                                (SynLongIdent.createS' [ "httpMessage" ; "Content" ])
                                (SynExpr.createIdent "queryParams")
                        )
                    ]
                | BodyParamMethods.StreamContent
                | BodyParamMethods.ByteArrayContent ->
                    [
                        Let (
                            "queryParams",
                            SynExpr.createNew
                                (SynType.createLongIdent'
                                    [ "System" ; "Net" ; "Http" ; (bodyParamType : BodyParamMethods).ToString () ])
                                (SynExpr.createIdent' bodyParamName)
                        )
                        Do (
                            SynExpr.assign
                                (SynLongIdent.createS' [ "httpMessage" ; "Content" ])
                                (SynExpr.createIdent "queryParams")
                        )
                    ]
                | BodyParamMethods.HttpContent ->
                    [
                        Do (
                            SynExpr.assign
                                (SynLongIdent.createS' [ "httpMessage" ; "Content" ])
                                (SynExpr.createIdent' bodyParamName)
                        )
                    ]
                | BodyParamMethods.Serialise ty ->
                    let isNullable =
                        match JsonNodeWithNullability.Identify ty with
                        | CannotBeNull -> false
                        | Nullable -> true

                    [
                        Let (
                            "queryParams",
                            createStringContent (
                                SynExpr.createIdent' bodyParamName
                                |> SynExpr.pipeThroughFunction (
                                    fst (
                                        (if isNullable then
                                             JsonSerializeGenerator.serializeNodeNullable
                                         else
                                             JsonSerializeGenerator.serializeNodeNonNullable)
                                            ty
                                    )
                                )
                                |> SynExpr.pipeThroughFunction (
                                    SynExpr.createLambda
                                        "node"
                                        (if isNullable then
                                             SynExpr.createMatch
                                                 (SynExpr.createIdent "node")
                                                 [
                                                     SynMatchClause.create
                                                         (SynPat.named "None")
                                                         (SynExpr.CreateConst "null")
                                                     SynMatchClause.create
                                                         (SynPat.nameWithArgs "Some" [ SynPat.named "node" ])
                                                         (SynExpr.applyFunction
                                                             (SynExpr.createLongIdent [ "node" ; "ToJsonString" ])
                                                             (SynExpr.CreateConst ()))
                                                 ]
                                         else
                                             (SynExpr.applyFunction
                                                 (SynExpr.createLongIdent [ "node" ; "ToJsonString" ])
                                                 (SynExpr.CreateConst ())))
                                )
                            )
                        )
                        Do (
                            SynExpr.assign
                                (SynLongIdent.createS' [ "httpMessage" ; "Content" ])
                                (SynExpr.createIdent "queryParams")
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

            let jsonNodeWithoutNull =
                match JsonNodeWithNullability.Identify info.TaskReturnType with
                | Nullable ->
                    Let (
                        "jsonNode",
                        SynExpr.createIdent "jsonNode"
                        |> SynExpr.pipeThroughFunction (SynExpr.createLongIdent [ "Option" ; "ofObj" ])
                    )
                | CannotBeNull ->
                    Let (
                        "jsonNode",
                        JsonSerializeGenerator.assertNotNull (Ident.create "jsonNode") (SynExpr.createIdent "jsonNode")
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

            let setMemberHeaders =
                memberHeaders
                |> List.map (fun (headerName, headerValue) ->
                    // Best-effort: assume this is a message header.
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
                yield! setMemberHeaders

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
                    yield jsonNodeWithoutNull
                | String -> yield responseString
                | Stream -> yield responseStream
                | UnitType ->
                    // What we're returning doesn't depend on the content, so don't bother!
                    ()
                | _ ->
                    yield responseStream
                    yield jsonNode
                    yield jsonNodeWithoutNull
            ]
            |> SynExpr.createCompExpr "async" returnExpr
            |> SynExpr.startAsTask cancellationTokenArg

        let thisIdent =
            if variableHeaders.IsEmpty then "_" else "this"
            |> Ident.create

        let args = args |> List.map snd |> SynPat.tuple |> List.singleton

        SynBinding.basic [ thisIdent ; info.Identifier ] args implementation
        |> SynBinding.withAccessibility info.Accessibility
        |> SynMemberDefn.memberImplementation

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

    let insertTrailingSlash (path : SynExpr) : SynExpr =
        match path |> SynExpr.stripOptionalParen with
        | SynExpr.Const (SynConst.String (s, _, _), _) ->
            if s.EndsWith '/' then
                path
            else
                SynExpr.CreateConst (s + "/")
        | _ -> SynExpr.plus (SynExpr.paren path) (SynExpr.CreateConst "/")

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

        let baseAddress =
            extractBaseAddress interfaceType.Attributes
            // We artificially insert a trailing slash because this is almost certainly
            // not meant to be an endpoint itself.
            |> Option.map insertTrailingSlash

        let basePath =
            extractBasePath interfaceType.Attributes
            // We artificially insert a trailing slash because this is almost certainly
            // not meant to be an endpoint itself.
            |> Option.map insertTrailingSlash

        let properties =
            interfaceType.Properties
            |> List.map (fun pi ->
                let headerInfo =
                    match extractHeaderInformation pi.Attributes with
                    | [ [ x ] ] -> x
                    | [ _ ] ->
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

                let specificHeaders =
                    extractHeaderInformation mem.Attributes
                    |> List.map (fun l ->
                        match l with
                        | [ x ; y ] -> x, y
                        | _ ->
                            failwith
                                $"Expected Header attribute on member %s{mem.Identifier.idText} to have exactly two arguments."
                    )

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
                    Headers = specificHeaders
                }
            )
            |> List.map (constructMember constantHeaders properties)

        let propertyMembers =
            properties
            |> List.map (fun (_, pi) ->
                SynExpr.createLongIdent' [ Ident.lowerFirstLetter pi.Identifier ]
                |> SynExpr.applyTo (SynExpr.CreateConst ())
                |> SynBinding.basic [ Ident.create "_" ; pi.Identifier ] []
                |> SynBinding.withReturnAnnotation pi.Type
                |> SynBinding.setInline pi.IsInline
                |> SynBinding.withAccessibility pi.Accessibility
                |> SynMemberDefn.memberImplementation
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

                SynModuleDecl.createTypes [ containingType ]

            else
                SynBinding.basic [ Ident.create "make" ] (headerArgs @ [ clientCreationArg ]) interfaceImpl
                |> SynBinding.withXmlDoc xmlDoc
                |> SynBinding.withReturnAnnotation returnInfo
                |> SynModuleDecl.createLet

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

        [
            for openStatement in opens do
                yield SynModuleDecl.openAny openStatement
            yield SynModuleDecl.nestedModule modInfo [ createFunc ]
        ]
        |> SynModuleOrNamespace.createNamespace ns

open Myriad.Core

/// Myriad generator that provides an HTTP client for an interface type using RestEase annotations.
[<MyriadGenerator("http-client")>]
type HttpClientGenerator () =

    interface IMyriadGenerator with
        member _.ValidInputExtensions = [ ".fs" ]

        member _.Generate (context : GeneratorContext) =
            let targetedTypes =
                MyriadParamParser.render context.AdditionalParameters
                |> Map.map (fun _ v -> v.Split '!' |> Array.toList |> List.map DesiredGenerator.Parse)

            let ast, _ =
                Ast.fromFilename context.InputFilename |> Async.RunSynchronously |> Array.head

            let types = Ast.getTypes ast

            let opens = AstHelper.extractOpens ast

            let namespaceAndTypes =
                types
                |> List.choose (fun (ns, types) ->
                    types
                    |> List.choose (fun typeDef ->
                        match SynTypeDefn.getAttribute typeof<HttpClientAttribute>.Name typeDef with
                        | None ->
                            let name = SynTypeDefn.getName typeDef |> List.map _.idText |> String.concat "."

                            match Map.tryFind name targetedTypes with
                            | Some desired ->
                                desired
                                |> List.tryPick (fun generator ->
                                    match generator with
                                    | DesiredGenerator.HttpClient arg ->
                                        let spec =
                                            {
                                                ExtensionMethods =
                                                    arg
                                                    |> Option.defaultValue
                                                        HttpClientAttribute.DefaultIsExtensionMethod
                                            }

                                        Some (typeDef, spec)
                                    | _ -> None
                                )
                            | _ -> None
                        | Some attr ->
                            let arg =
                                match SynExpr.stripOptionalParen attr.ArgExpr with
                                | SynExpr.Const (SynConst.Bool value, _) -> value
                                | SynExpr.Const (SynConst.Unit, _) -> HttpClientAttribute.DefaultIsExtensionMethod
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
