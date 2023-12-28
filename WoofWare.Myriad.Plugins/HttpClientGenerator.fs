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

    type Parameter =
        {
            Attributes : SynAttributes
            IsOptional : bool
            Id : Ident option
            Type : SynType
        }

    let synBindingTriviaZero =
        {
            SynBindingTrivia.EqualsRange = Some range0
            InlineKeyword = Some range0
            LeadingKeyword = SynLeadingKeyword.Member range0
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
            info.Args
            |> List.map (fun arg ->
                let argName =
                    match arg.Id with
                    | None -> failwith "TODO: create an arg name"
                    | Some id -> id

                SynPat.Tuple (false, [ SynPat.CreateTyped (SynPat.CreateNamed argName, arg.Type) ], [ range0 ], range0)
                |> SynPat.CreateParen
            )
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

        let requestUri =
            SynExpr.CreateApp (
                SynExpr.CreateIdentString "System.Uri",
                SynExpr.CreateParen (
                    SynExpr.plus
                        (SynExpr.CreateApp (
                            SynExpr.CreateLongIdent (SynLongIdent.Create [ "client" ; "BaseAddress" ; "ToString" ]),
                            SynExpr.CreateConst SynConst.Unit
                        ))
                        (SynExpr.CreateConstString info.UrlTemplate)
                )
            )

        let httpReqMessageConstructor =
            [
                SynExpr.equals
                    (SynExpr.CreateIdentString "Method")
                    (SynExpr.CreateLongIdent (
                        SynLongIdent.Create
                            [ "System" ; "Net" ; "Http" ; "HttpMethod" ; httpMethodString info.HttpMethod ]
                    ))
                SynExpr.equals (SynExpr.CreateIdentString "RequestUri") requestUri
            ]
            |> SynExpr.CreateParenedTuple

        let returnExpr =
            // TODO
            SynExpr.CreateApp (
                SynExpr.CreateLongIdent (SynLongIdent.Create [ "GymAttendance" ; "jsonParse" ]),
                SynExpr.CreateIdentString "node"
            )

        let implementation =
            [
                LetBang ("ct", SynExpr.CreateLongIdent (SynLongIdent.Create [ "Async" ; "CancellationToken" ]))
                Use (
                    "message",
                    SynExpr.New (
                        false,
                        SynType.CreateLongIdent (
                            SynLongIdent.Create [ "System" ; "Net" ; "Http" ; "HttpRequestMessage" ]
                        ),
                        httpReqMessageConstructor,
                        range0
                    )
                )
                LetBang (
                    "response",
                    SynExpr.awaitTask (
                        SynExpr.CreateApp (
                            SynExpr.CreateLongIdent (SynLongIdent.Create [ "client" ; "SendAsync" ]),
                            SynExpr.CreateParenedTuple
                                [ SynExpr.CreateIdentString "message" ; SynExpr.CreateIdentString "ct" ]
                        )
                    )
                )
                Let (
                    "response",
                    SynExpr.CreateApp (
                        SynExpr.CreateLongIdent (SynLongIdent.Create [ "response" ; "EnsureSuccessStatusCode" ]),
                        SynExpr.CreateConst SynConst.Unit
                    )
                )
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
                LetBang (
                    "node",
                    SynExpr.awaitTask (
                        SynExpr.CreateApp (
                            SynExpr.CreateLongIdent (
                                SynLongIdent.Create [ "System" ; "Text" ; "Json" ; "Nodes" ; "JsonNode" ; "ParseAsync" ]
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
                synBindingTriviaZero
            ),
            range0
        )
    (*
        SynMemberDefn.Member(
                                        memberDefn =
                                            SynBinding.SynBinding(
                                                accessibility = None,
                                                kind = SynBindingKind.Normal,
                                                isInline = false,
                                                isMutable = false,
                                                attributes = [],
                                                xmlDoc = PreXmlDoc.Empty,
                                                valData =
                                                    SynValData.SynValData(
                                                        memberFlags =
                                                            Some(
                                                                {
                                                                    IsInstance = true
                                                                    IsDispatchSlot = false
                                                                    IsOverrideOrExplicitImpl = true
                                                                    IsFinal = false
                                                                    GetterOrSetterIsCompilerGenerated = false
                                                                    MemberKind = SynMemberKind.Member
                                                                }
                                                            ),
                                                        valInfo =
                                                            SynValInfo.SynValInfo(
                                                                curriedArgInfos = [
                                                                    [ SynArgInfo.SynArgInfo(attributes = [], optional = false, ident = None) ]
                                                                    [
                                                                        SynArgInfo.SynArgInfo(attributes = [], optional = false, ident = Some(Ident("ct", R("(2,22--2,24)"))))
                                                                    ]
                                                                ],
                                                                returnInfo = SynArgInfo.SynArgInfo(attributes = [], optional = false, ident = None)
                                                            ),
                                                        thisIdOpt = None
                                                    ),
                                                headPat =
                                                    SynPat.LongIdent(
                                                        longDotId =
                                                            SynLongIdent.SynLongIdent(
                                                                id = [ Ident("_", R("(2,11--2,12)")); Ident("GetGyms", R("(2,13--2,20)")) ],
                                                                dotRanges = [ R("(2,12--2,13)") ],
                                                                trivia = [ None; None ]
                                                            ),
                                                        extraId = None,
                                                        typarDecls = None,
                                                        argPats =
                                                            SynArgPats.Pats(
                                                                [
                                                                    SynPat.Paren(
                                                                        pat =
                                                                            SynPat.Typed(
                                                                                pat =
                                                                                    SynPat.Named(
                                                                                        ident = SynIdent.SynIdent(ident = Ident("ct", R("(2,22--2,24)")), trivia = None),
                                                                                        isThisVal = false,
                                                                                        accessibility = None,
                                                                                        range = R("(2,22--2,24)")
                                                                                    ),
                                                                                targetType =
                                                                                    SynType.App(
                                                                                        typeName =
                                                                                            SynType.LongIdent(
                                                                                                SynLongIdent.SynLongIdent(
                                                                                                    id = [ Ident("option", R("(2,45--2,51)")) ],
                                                                                                    dotRanges = [],
                                                                                                    trivia = [ None ]
                                                                                                )
                                                                                            ),
                                                                                        lessRange = None,
                                                                                        typeArgs = [
                                                                                            SynType.LongIdent(
                                                                                                SynLongIdent.SynLongIdent(
                                                                                                    id = [ Ident("CancellationToken", R("(2,27--2,44)")) ],
                                                                                                    dotRanges = [],
                                                                                                    trivia = [ None ]
                                                                                                )
                                                                                            )
                                                                                        ],
                                                                                        commaRanges = [],
                                                                                        greaterRange = None,
                                                                                        isPostfix = true,
                                                                                        range = R("(2,27--2,51)")
                                                                                    ),
                                                                                range = R("(2,22--2,51)")
                                                                            ),
                                                                        range = R("(2,21--2,52)")
                                                                    )
                                                                ]
                                                            ),
                                                        accessibility = None,
                                                        range = R("(2,11--2,52)")
                                                    ),
                                                returnInfo = None,
                                                expr =
                                                    SynExpr.App(
                                                        flag = ExprAtomicFlag.NonAtomic,
                                                        isInfix = false,
                                                        funcExpr = SynExpr.Ident(Ident("failwith", R("(3,8--3,16)"))),
                                                        argExpr =
                                                            SynExpr.Const(
                                                                constant = SynConst.String(text = "", synStringKind = SynStringKind.Regular, range = R("(3,17--3,19)")),
                                                                range = R("(3,17--3,19)")
                                                            ),
                                                        range = R("(3,8--3,19)")
                                                    ),
                                                range = R("(2,11--2,52)"),
                                                debugPoint = DebugPointAtBinding.NoneAtInvisible,
                                                trivia = {
                                                    LeadingKeyword = SynLeadingKeyword.Member(R("(2,4--2,10)"))
                                                    InlineKeyword = None
                                                    EqualsRange = Some(R("(2,53--2,54)"))
                                                }
                                            ),
                                        range = R("(2,4--3,19)")
                                    )
        *)

    let rec convertSigParam (ty : SynType) : Parameter =
        match ty with
        | SynType.Paren (inner, _) -> convertSigParam inner
        | SynType.SignatureParameter (attrs, opt, id, usedType, _) ->
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

    let createModule (ns : LongIdent) (interfaceType : SynTypeDefn) : SynModuleOrNamespace =
        let (SynTypeDefn (SynComponentInfo (_, _, _, interfaceName, _, _, _, _), synTypeDefnRepr, members, _, _, _)) =
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
                SynValData.SynValData (None, SynValInfo.Empty, None),
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
                (DebugPointAtBinding.Yes range0),
                synBindingTriviaZero
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

        SynModuleOrNamespace.CreateModule (
            moduleName,
            decls = [ createFunc ],
            docs = docString,
            attribs =
                [
                    SynAttributeList.Create SynAttribute.compilationRepresentation
                    SynAttributeList.Create (SynAttribute.RequireQualifiedAccess ())
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
                        let clientModule = HttpClientGenerator.createModule ns interfaceType
                        clientModule
                    )
                )

            Output.Ast modules
