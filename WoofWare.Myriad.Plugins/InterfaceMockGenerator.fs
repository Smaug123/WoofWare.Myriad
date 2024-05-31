namespace WoofWare.Myriad.Plugins

open System
open Fantomas.FCS.Syntax
open Fantomas.FCS.SyntaxTrivia
open Fantomas.FCS.Xml
open Myriad.Core

type internal GenerateMockOutputSpec =
    {
        IsInternal : bool
    }

[<RequireQualifiedAccess>]
module internal InterfaceMockGenerator =
    open Fantomas.FCS.Text.Range
    open Myriad.Core.Ast

    let private getName (SynField (_, _, id, _, _, _, _, _, _)) =
        match id with
        | None -> failwith "Expected record field to have a name, but it was somehow anonymous"
        | Some id -> id

    [<RequireQualifiedAccess>]
    type private KnownInheritance = | IDisposable

    let createType
        (spec : GenerateMockOutputSpec)
        (name : string)
        (interfaceType : InterfaceType)
        (xmlDoc : PreXmlDoc)
        (fields : SynField list)
        : SynModuleDecl
        =
        let inherits =
            interfaceType.Inherits
            |> Seq.map (fun ty ->
                match ty with
                | SynType.LongIdent (SynLongIdent.SynLongIdent (name, _, _)) ->
                    match name |> List.map _.idText with
                    | [] -> failwith "Unexpected empty identifier in inheritance declaration"
                    | [ "IDisposable" ]
                    | [ "System" ; "IDisposable" ] -> KnownInheritance.IDisposable
                    | _ -> failwithf "Unrecognised inheritance identifier: %+A" name
                | x -> failwithf "Unrecognised type in inheritance: %+A" x
            )
            |> Set.ofSeq

        let failwithFun =
            SynExpr.createLongIdent [ "System" ; "NotImplementedException" ]
            |> SynExpr.applyTo (SynExpr.CreateConst "Unimplemented mock function")
            |> SynExpr.CreateParen
            |> SynExpr.applyFunction (SynExpr.createIdent "raise")
            |> SynExpr.createLambda "_"

        let constructorReturnType =
            match interfaceType.Generics with
            | None -> SynType.CreateLongIdent name
            | Some generics ->

            let generics =
                generics.TyparDecls
                |> List.map (fun (SynTyparDecl (_, typar)) -> SynType.var typar)

            SynType.app name generics

        let constructorFields =
            let extras =
                if inherits.Contains KnownInheritance.IDisposable then
                    let unitFun = SynExpr.createLambda "_" SynExpr.CreateUnit

                    [ (SynLongIdent.createS "Dispose", true), Some unitFun ]
                else
                    []

            let nonExtras =
                fields
                |> List.map (fun field -> (SynLongIdent.createI (getName field), true), Some failwithFun)

            extras @ nonExtras

        let constructor =
            SynBinding.basic
                (SynLongIdent.createS "Empty")
                (if interfaceType.Generics.IsNone then
                     []
                 else
                     [ SynPat.CreateConst SynConst.Unit ])
                (AstHelper.instantiateRecord constructorFields)
            |> SynBinding.makeStaticMember
            |> SynBinding.withXmlDoc (PreXmlDoc.Create " An implementation where every method throws.")
            |> SynBinding.withReturnAnnotation constructorReturnType
            |> fun m -> SynMemberDefn.Member (m, range0)

        let fields =
            let extras =
                if inherits.Contains KnownInheritance.IDisposable then
                    [
                        SynField.Create (
                            SynType.CreateFun (SynType.CreateUnit, SynType.CreateUnit),
                            Ident.Create "Dispose",
                            xmldoc = PreXmlDoc.Create " Implementation of IDisposable.Dispose"
                        )
                    ]
                else
                    []

            extras @ fields

        let interfaceMembers =
            let members =
                interfaceType.Members
                |> List.map (fun memberInfo ->

                    let synValData =
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
                            valInfo =
                                SynValInfo.SynValInfo (
                                    curriedArgInfos =
                                        [
                                            yield
                                                [
                                                    SynArgInfo.SynArgInfo (
                                                        attributes = [],
                                                        optional = false,
                                                        ident = None
                                                    )
                                                ]
                                            yield!
                                                memberInfo.Args
                                                |> List.mapi (fun i arg ->
                                                    arg.Args
                                                    |> List.mapi (fun j arg ->
                                                        match arg.Type with
                                                        | UnitType -> SynArgInfo.SynArgInfo ([], false, None)
                                                        | _ -> SynArgInfo.CreateIdString $"arg_%i{i}_%i{j}"
                                                    )
                                                )
                                        ],
                                    returnInfo =
                                        SynArgInfo.SynArgInfo (attributes = [], optional = false, ident = None)
                                ),
                            thisIdOpt = None
                        )

                    let headArgs =
                        memberInfo.Args
                        |> List.mapi (fun i tupledArgs ->
                            let args =
                                tupledArgs.Args
                                |> List.mapi (fun j ty ->
                                    match ty.Type with
                                    | UnitType -> SynPat.Const (SynConst.Unit, range0)
                                    | _ -> SynPat.CreateNamed (Ident.Create $"arg_%i{i}_%i{j}")
                                )

                            match args with
                            | [] -> failwith "somehow got no args at all"
                            | [ arg ] -> arg
                            | args ->
                                SynPat.Tuple (false, args, List.replicate (args.Length - 1) range0, range0)
                                |> SynPat.CreateParen
                            |> fun i -> if tupledArgs.HasParen then SynPat.Paren (i, range0) else i
                        )

                    let headPat =
                        SynPat.LongIdent (
                            SynLongIdent.create [ Ident.Create "this" ; memberInfo.Identifier ],
                            None,
                            None,
                            SynArgPats.Pats headArgs,
                            None,
                            range0
                        )

                    let body =
                        let tuples =
                            memberInfo.Args
                            |> List.mapi (fun i args ->
                                args.Args
                                |> List.mapi (fun j arg ->
                                    match arg.Type with
                                    | UnitType -> SynExpr.CreateConst ()
                                    | _ -> SynExpr.createIdent $"arg_%i{i}_%i{j}"
                                )
                                |> SynExpr.CreateParenedTuple
                            )

                        match tuples |> List.rev with
                        | [] -> failwith "expected args but got none"
                        | last :: rest ->

                        (last, rest)
                        ||> List.fold (fun trail next -> SynExpr.CreateApp (next, trail))
                        |> SynExpr.applyFunction (
                            SynExpr.createLongIdent' [ Ident.Create "this" ; memberInfo.Identifier ]
                        )

                    SynMemberDefn.Member (
                        SynBinding.SynBinding (
                            None,
                            SynBindingKind.Normal,
                            false,
                            false,
                            [],
                            PreXmlDoc.Empty,
                            synValData,
                            headPat,
                            None,
                            body,
                            range0,
                            DebugPointAtBinding.Yes range0,
                            {
                                LeadingKeyword = SynLeadingKeyword.Member range0
                                InlineKeyword = None
                                EqualsRange = Some range0
                            }
                        ),
                        range0
                    )
                )

            let interfaceName =
                let baseName = SynType.createLongIdent interfaceType.Name

                match interfaceType.Generics with
                | None -> baseName
                | Some generics ->
                    let generics =
                        match generics with
                        | SynTyparDecls.PostfixList (decls, _, _) -> decls
                        | SynTyparDecls.PrefixList (decls, _) -> decls
                        | SynTyparDecls.SinglePrefix (decl, _) -> [ decl ]
                        |> List.map (fun (SynTyparDecl (_, typar)) -> SynType.var typar)

                    SynType.app' baseName generics

            SynMemberDefn.Interface (interfaceName, Some range0, Some members, range0)

        let access =
            match interfaceType.Accessibility, spec.IsInternal with
            | Some (SynAccess.Public _), true
            | None, true -> SynAccess.Internal range0
            | Some (SynAccess.Public _), false -> SynAccess.Public range0
            | None, false -> SynAccess.Public range0
            | Some (SynAccess.Internal _), _ -> SynAccess.Internal range0
            | Some (SynAccess.Private _), _ -> SynAccess.Private range0

        let extraInterfaces =
            inherits
            |> Seq.map (fun inheritance ->
                match inheritance with
                | KnownInheritance.IDisposable ->
                    let binding =
                        SynBinding.basic
                            (SynLongIdent.createS' [ "this" ; "Dispose" ])
                            [ SynPat.CreateConst SynConst.Unit ]
                            (SynExpr.CreateApp (SynExpr.createLongIdent [ "this" ; "Dispose" ], SynExpr.CreateUnit))
                        |> SynBinding.withReturnAnnotation (SynType.Unit ())
                        |> SynBinding.makeInstanceMember

                    let mem = SynMemberDefn.Member (binding, range0)

                    SynMemberDefn.Interface (
                        SynType.CreateLongIdent (SynLongIdent.createS' [ "System" ; "IDisposable" ]),
                        Some range0,
                        Some [ mem ],
                        range0
                    )
            )
            |> Seq.toList

        let record =
            {
                Name = Ident.Create name
                Fields = fields
                Members = Some ([ constructor ; interfaceMembers ] @ extraInterfaces)
                XmlDoc = Some xmlDoc
                Generics = interfaceType.Generics
                Accessibility = Some access
            }

        let typeDecl = AstHelper.defineRecordType record

        SynModuleDecl.Types ([ typeDecl ], range0)

    let private buildType (x : ParameterInfo) : SynType =
        if x.IsOptional then
            SynType.app "option" [ x.Type ]
        else
            x.Type

    let private constructMemberSinglePlace (tuple : TupledArg) : SynType =
        match tuple.Args |> List.rev |> List.map buildType with
        | [] -> failwith "no-arg functions not supported yet"
        | [ x ] -> x
        | last :: rest ->
            ([ SynTupleTypeSegment.Type last ], rest)
            ||> List.fold (fun ty nextArg -> SynTupleTypeSegment.Type nextArg :: SynTupleTypeSegment.Star range0 :: ty)
            |> fun segs -> SynType.Tuple (false, segs, range0)
        |> fun ty -> if tuple.HasParen then SynType.Paren (ty, range0) else ty

    let constructMember (mem : MemberInfo) : SynField =
        let inputType = mem.Args |> List.map constructMemberSinglePlace

        let funcType = AstHelper.toFun inputType mem.ReturnType

        SynField.SynField (
            [],
            false,
            Some mem.Identifier,
            funcType,
            false,
            mem.XmlDoc |> Option.defaultValue PreXmlDoc.Empty,
            None,
            range0,
            SynFieldTrivia.Zero
        )

    let createRecord
        (namespaceId : LongIdent)
        (opens : SynOpenDeclTarget list)
        (interfaceType : SynTypeDefn, spec : GenerateMockOutputSpec)
        : SynModuleOrNamespace
        =
        let interfaceType = AstHelper.parseInterface interfaceType
        let fields = interfaceType.Members |> List.map constructMember
        let docString = PreXmlDoc.Create " Mock record type for an interface"

        let name =
            List.last interfaceType.Name
            |> _.idText
            |> fun s ->
                if s.StartsWith 'I' && s.Length > 1 && Char.IsUpper s.[1] then
                    s.Substring 1
                else
                    s
            |> fun s -> s + "Mock"

        let typeDecl = createType spec name interfaceType docString fields

        SynModuleOrNamespace.CreateNamespace (
            namespaceId,
            decls = (opens |> List.map SynModuleDecl.CreateOpen) @ [ typeDecl ]
        )

/// Myriad generator that creates a record which implements the given interface,
/// but with every field mocked out.
[<MyriadGenerator("interface-mock")>]
type InterfaceMockGenerator () =

    interface IMyriadGenerator with
        member _.ValidInputExtensions = [ ".fs" ]

        member _.Generate (context : GeneratorContext) =
            let ast, _ =
                Ast.fromFilename context.InputFilename |> Async.RunSynchronously |> Array.head

            let types = Ast.extractTypeDefn ast

            let namespaceAndInterfaces =
                types
                |> List.choose (fun (ns, types) ->
                    types
                    |> List.choose (fun typeDef ->
                        match Ast.getAttribute<GenerateMockAttribute> typeDef with
                        | None -> None
                        | Some attr ->
                            let arg =
                                match SynExpr.stripOptionalParen attr.ArgExpr with
                                | SynExpr.Const (SynConst.Bool value, _) -> value
                                | SynExpr.Const (SynConst.Unit, _) -> GenerateMockAttribute.DefaultIsInternal
                                | arg ->
                                    failwith
                                        $"Unrecognised argument %+A{arg} to [<%s{nameof GenerateMockAttribute}>]. Literals are not supported. Use `true` or `false` (or unit) only."

                            let spec =
                                {
                                    IsInternal = arg
                                }

                            Some (typeDef, spec)
                    )
                    |> function
                        | [] -> None
                        | ty -> Some (ns, ty)
                )

            let opens = AstHelper.extractOpens ast

            let modules =
                namespaceAndInterfaces
                |> List.collect (fun (ns, records) ->
                    records |> List.map (InterfaceMockGenerator.createRecord ns opens)
                )

            Output.Ast modules
