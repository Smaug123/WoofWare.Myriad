namespace WoofWare.Myriad.Plugins

open System
open System.Net.Http
open Fantomas.FCS.Syntax
open Fantomas.FCS.SyntaxTrivia
open Fantomas.FCS.Xml
open Myriad.Core

/// Attribute indicating a record type to which the "create HTTP client" Myriad
/// generator should apply during build.
type HttpClientAttribute () =
    inherit Attribute ()

[<RequireQualifiedAccess>]
module internal HttpClientGenerator =
    open Fantomas.FCS.Text.Range
    open Myriad.Core.Ast

    type HttpAttribute =
        // TODO: Format parameter to these attrs
        | Query of string option
        | Path of string
        | Body

    type Parameter =
        {
            Attributes : HttpAttribute list
            IsOptional : bool
            Id : Ident option
            Type : SynType
        }

    let synBindingTriviaZero (isMember : bool) =
        {
            SynBindingTrivia.EqualsRange = Some range0
            InlineKeyword = None
            LeadingKeyword =
                if isMember then
                    SynLeadingKeyword.Member range0
                else
                    SynLeadingKeyword.Let range0
        }

    type MemberInfo =
        {
            /// E.g. HttpMethod.Get
            HttpMethod : HttpMethod
            /// E.g. "v1/gyms/{gym_id}/attendance"
            UrlTemplate : string
            ReturnType : SynType
            Arity : SynArgInfo list
            Args : Parameter list
            Identifier : Ident
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

    /// E.g. converts `[<Get "blah">]` to (HttpMethod.Get, "blah")
    let extractHttpInformation (attrs : SynAttribute list) : HttpMethod * string =
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
        | [ (meth, arg) ] ->
            match arg with
            | SynExpr.Const (SynConst.String (text, SynStringKind.Regular, _), _) -> meth, text
            | arg ->
                failwithf "Unrecognised AST member in attribute argument. Only regular strings are supported: %+A" arg
        | [] -> failwith "Required exactly one recognised RestEase attribute on member, but got none"
        | matchingAttrs ->
            failwithf "Required exactly one recognised RestEase attribute on member, but got %i" matchingAttrs.Length

    let constructMember (info : MemberInfo) : SynMemberDefn =
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

        let argPats =
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

                    SynPat.CreateTyped (SynPat.CreateNamed argName, argType)
                )

            SynPat.Tuple (false, args, List.replicate (args.Length - 1) range0, range0)
            |> SynPat.CreateParen
            |> List.singleton
            |> SynArgPats.Pats

        let headPat =
            SynPat.LongIdent (
                SynLongIdent.CreateFromLongIdent [ Ident.Create "_" ; info.Identifier ],
                None,
                None,
                argPats,
                None,
                range0
            )

        let requestUriTrailer =
            (SynExpr.CreateConstString info.UrlTemplate, info.Args)
            ||> List.fold (fun template arg ->
                (template, arg.Attributes)
                ||> List.fold (fun template attr ->
                    match attr with
                    | HttpAttribute.Path s ->
                        let varName =
                            match arg.Id with
                            | None -> failwith "TODO: anonymous args"
                            | Some id -> id

                        template
                        |> SynExpr.callMethodArg
                            "Replace"
                            (SynExpr.CreateParenedTuple
                                [
                                    SynExpr.CreateConstString ("{" + s + "}")
                                    SynExpr.callMethod "ToString" (SynExpr.CreateIdent varName)
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

                let toString (ident : SynExpr) (ty : SynType) =
                    match ty with
                    | DateOnly ->
                        ident
                        |> SynExpr.callMethodArg "ToString" (SynExpr.CreateConstString "yyyy-MM-dd")
                    | DateTime ->
                        ident
                        |> SynExpr.callMethodArg "ToString" (SynExpr.CreateConstString "yyyy-MM-ddTHH:mm:ss")
                    | _ -> SynExpr.callMethod "ToString" ident

                let prefix =
                    toString (SynExpr.CreateIdent firstValueId) firstValue.Type
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

                    toString (SynExpr.CreateIdent paramValueId) paramValue.Type
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

            SynExpr.App (
                ExprAtomicFlag.Atomic,
                false,
                uriIdent,
                SynExpr.CreateParenedTuple
                    [
                        SynExpr.CreateLongIdent (SynLongIdent.Create [ "client" ; "BaseAddress" ])
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
                    | Body -> Some arg
                    | _ -> None
                )
            )

        if not bodyParams.IsEmpty then
            failwith "[<Body>] is not yet supported"

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
            JsonParseGenerator.parseNode
                None
                JsonParseGenerator.JsonParseOption.None
                info.ReturnType
                (SynExpr.CreateIdentString "node")

        let implementation =
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
                (*
                if not bodyParams.IsEmpty then
                    yield
                        Use (
                            "queryParams",
                            SynExpr.New (
                                false,
                                SynType.CreateLongIdent (
                                    SynLongIdent.Create [ "System" ; "Net" ; "Http" ; "StringContent" ]
                                ),
                                SynExpr.CreateParen (failwith "TODO"),
                                range0
                            )
                        )

                    yield
                        Do (
                            SynExpr.LongIdentSet (
                                SynLongIdent.Create [ "httpMessage" ; "Content" ],
                                SynExpr.CreateIdentString "queryParams",
                                range0
                            )
                        )
                *)
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
                yield
                    Let (
                        "response",
                        SynExpr.CreateApp (
                            SynExpr.CreateLongIdent (SynLongIdent.Create [ "response" ; "EnsureSuccessStatusCode" ]),
                            SynExpr.CreateConst SynConst.Unit
                        )
                    )
                yield
                    LetBang (
                        "stream",
                        SynExpr.awaitTask (
                            SynExpr.CreateApp (
                                SynExpr.CreateLongIdent (
                                    SynLongIdent.Create [ "response" ; "Content" ; "ReadAsStreamAsync" ]
                                ),
                                SynExpr.CreateIdentString "ct"
                            )
                        )
                    )
                yield
                    LetBang (
                        "node",
                        SynExpr.awaitTask (
                            SynExpr.CreateApp (
                                SynExpr.CreateLongIdent (
                                    SynLongIdent.Create
                                        [ "System" ; "Text" ; "Json" ; "Nodes" ; "JsonNode" ; "ParseAsync" ]
                                ),
                                SynExpr.CreateParenedTuple
                                    [
                                        SynExpr.CreateIdentString "stream"
                                        SynExpr.equals
                                            (SynExpr.CreateIdentString "cancellationToken")
                                            (SynExpr.CreateIdentString "ct")
                                    ]
                            )
                        )
                    )
            ]
            |> SynExpr.createCompExpr "async" returnExpr
            |> SynExpr.startAsTask

        SynMemberDefn.Member (
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
                synBindingTriviaZero true
            ),
            range0
        )

    let rec convertSigParam (ty : SynType) : Parameter =
        match ty with
        | SynType.Paren (inner, _) -> convertSigParam inner
        | SynType.SignatureParameter (attrs, opt, id, usedType, _) ->
            let attrs =
                attrs
                |> List.collect (fun attrs ->
                    attrs.Attributes
                    |> List.choose (fun attr ->
                        match attr.TypeName.AsString with
                        | "Query"
                        | "QueryAttribute" ->
                            match attr.ArgExpr with
                            | SynExpr.Const (SynConst.Unit, _) -> Some (HttpAttribute.Query None)
                            | SynExpr.Const (SynConst.String (s, SynStringKind.Regular, _), _) ->
                                Some (HttpAttribute.Query (Some s))
                            | SynExpr.Const (a, _) ->
                                failwithf "unrecognised constant arg to the Query attribute: %+A" a
                            | _ -> None
                        | "Path"
                        | "PathAttribute" ->
                            match attr.ArgExpr with
                            | SynExpr.Const (SynConst.String (s, SynStringKind.Regular, _), _) ->
                                Some (HttpAttribute.Path s)
                            | SynExpr.Const (a, _) ->
                                failwithf "unrecognised constant arg to the Path attribute: %+A" a
                            | _ -> None
                        | "Body"
                        | "BodyAttribute" ->
                            match attr.ArgExpr with
                            | SynExpr.Const (SynConst.Unit, _) -> Some (HttpAttribute.Body)
                            | SynExpr.Const (a, _) ->
                                failwithf "unrecognised constant arg to the Body attribute: %+A" a
                            | _ -> None
                        | _ -> None
                    )
                )

            {
                Attributes = attrs
                IsOptional = opt
                Id = id
                Type = usedType
            }
        | _ -> failwithf "expected SignatureParameter, got: %+A" ty

    let rec extractTypes (tupleType : SynTupleTypeSegment list) : Parameter list =
        match tupleType with
        | [] -> []
        | [ SynTupleTypeSegment.Type param ] -> [ convertSigParam param ]
        | SynTupleTypeSegment.Type param :: SynTupleTypeSegment.Star _ :: rest ->
            convertSigParam param :: extractTypes rest
        | _ -> failwithf "Didn't have alternating type-and-star in interface member definition: %+A" tupleType

    let createModule
        (opens : SynOpenDeclTarget list)
        (ns : LongIdent)
        (interfaceType : SynTypeDefn)
        : SynModuleOrNamespace
        =
        let (SynTypeDefn (SynComponentInfo (_, _, _, interfaceName, _, _, _, _), synTypeDefnRepr, _, _, _, _)) =
            interfaceType

        let members =
            match synTypeDefnRepr with
            | SynTypeDefnRepr.ObjectModel (_kind, members, _) ->
                members
                |> List.map (fun defn ->
                    match defn with
                    | SynMemberDefn.AbstractSlot (slotSig, flags, _, _) ->
                        match flags.MemberKind with
                        | SynMemberKind.Member -> ()
                        | kind -> failwithf "Unrecognised member kind: %+A" kind

                        if not flags.IsInstance then
                            failwith "member was not an instance member"

                        match slotSig with
                        | SynValSig (attrs,
                                     SynIdent.SynIdent (ident, _),
                                     _typeParams,
                                     synType,
                                     arity,
                                     isInline,
                                     isMutable,
                                     _xmlDoc,
                                     accessibility,
                                     synExpr,
                                     _,
                                     _) ->
                            if isInline then
                                failwith "inline members not supported"

                            if isMutable then
                                failwith "mutable members not supported"

                            match accessibility with
                            | Some (SynAccess.Internal _)
                            | Some (SynAccess.Private _) -> failwith "only public members are supported"
                            | _ -> ()

                            match synExpr with
                            | Some _ -> failwith "literal members are not supported"
                            | None -> ()

                            let attrs = attrs |> List.collect (fun a -> a.Attributes)

                            let arity =
                                match arity with
                                | SynValInfo ([ curriedArgs ], SynArgInfo ([], false, _)) -> curriedArgs
                                | SynValInfo (curriedArgs, SynArgInfo ([], false, _)) ->
                                    failwithf "only tupled arguments are supported, but got: %+A" curriedArgs
                                | SynValInfo (_, info) ->
                                    failwithf
                                        "only bare return values like `Task<foo>` are supported, but got: %+A"
                                        info

                            let args, ret =
                                match synType with
                                | SynType.Fun (argType, Task returnType, _, _) -> argType, returnType
                                | _ ->
                                    failwithf
                                        "Expected a return type of a generic Task; bad signature was: %+A"
                                        synType

                            let args =
                                match args with
                                | SynType.SignatureParameter _ -> [ convertSigParam args ]
                                | SynType.Tuple (false, path, _) -> extractTypes path
                                | _ -> failwithf "Unrecognised args in interface method declaration: %+A" args

                            let httpMethod, url = extractHttpInformation attrs

                            {
                                HttpMethod = httpMethod
                                UrlTemplate = url
                                ReturnType = ret
                                Arity = arity
                                Args = args
                                Identifier = ident
                            }
                    | _ -> failwithf "Unrecognised member definition: %+A" defn
                )
            | _ -> failwithf "Unrecognised SynTypeDefnRepr: %+A" synTypeDefnRepr

        let constructed = members |> List.map constructMember
        let docString = PreXmlDoc.Create " Module for constructing a REST client."

        let interfaceImpl =
            SynExpr.ObjExpr (
                SynType.LongIdent (SynLongIdent.CreateFromLongIdent interfaceName),
                None,
                Some range0,
                [],
                constructed,
                [],
                range0,
                range0
            )

        let createFunc =
            SynBinding.SynBinding (
                None,
                SynBindingKind.Normal,
                false,
                false,
                [],
                PreXmlDoc.Create " Create a REST client.",
                SynValData.SynValData (
                    None,
                    SynValInfo.SynValInfo (
                        [ [ SynArgInfo.SynArgInfo ([], false, Some (Ident.Create "client")) ] ],
                        SynArgInfo.Empty
                    ),
                    None
                ),
                SynPat.CreateLongIdent (
                    SynLongIdent.CreateString "make",
                    [
                        SynPat.CreateParen (
                            SynPat.CreateTyped (
                                SynPat.CreateNamed (Ident.Create "client"),
                                SynType.CreateLongIdent (
                                    SynLongIdent.Create [ "System" ; "Net" ; "Http" ; "HttpClient" ]
                                )
                            )
                        )
                    ]
                ),
                Some (SynBindingReturnInfo.Create (SynType.LongIdent (SynLongIdent.CreateFromLongIdent interfaceName))),
                interfaceImpl,
                range0,
                DebugPointAtBinding.NoneAtLet,
                synBindingTriviaZero false
            )
            |> List.singleton
            |> SynModuleDecl.CreateLet

        let moduleName : LongIdent =
            List.last interfaceName
            |> fun ident -> ident.idText
            |> fun s ->
                if s.StartsWith 'I' then
                    s.[1..]
                else
                    failwithf "Expected interface type to start with 'I', but was: %s" s
            |> Ident.Create
            |> List.singleton

        let attribs =
            [
                SynAttributeList.Create SynAttribute.compilationRepresentation
                SynAttributeList.Create (SynAttribute.RequireQualifiedAccess ())
            ]

        let modInfo =
            SynComponentInfo.Create (moduleName, attributes = attribs, xmldoc = docString)

        SynModuleOrNamespace.CreateNamespace (
            ns,
            decls =
                [
                    for openStatement in opens do
                        yield SynModuleDecl.CreateOpen openStatement
                    yield SynModuleDecl.CreateNestedModule (modInfo, [ createFunc ])
                ]
        )

    let rec extractOpens (moduleDecls : SynModuleDecl list) : SynOpenDeclTarget list =
        moduleDecls
        |> List.choose (fun moduleDecl ->
            match moduleDecl with
            | SynModuleDecl.Open (target, _) -> Some target
            | other -> None
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

            let opens =
                match ast with
                | ParsedInput.ImplFile (ParsedImplFileInput (_, _, _, _, _, modules, _, _, _)) ->
                    modules
                    |> List.collect (fun (SynModuleOrNamespace (nsId, _, _, decls, _, _, _, _, _)) ->
                        HttpClientGenerator.extractOpens decls
                    )
                | _ -> []

            let namespaceAndTypes =
                types
                |> List.choose (fun (ns, types) ->
                    match types |> List.filter Ast.hasAttribute<HttpClientAttribute> with
                    | [] -> None
                    | types -> Some (ns, types)
                )

            let modules =
                namespaceAndTypes
                |> List.collect (fun (ns, types) ->
                    types
                    |> List.map (fun interfaceType ->
                        let clientModule = HttpClientGenerator.createModule opens ns interfaceType
                        clientModule
                    )
                )

            Output.Ast modules
