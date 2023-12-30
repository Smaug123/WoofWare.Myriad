namespace WoofWare.Myriad.Plugins

open System
open Fantomas.FCS.Syntax
open Fantomas.FCS.SyntaxTrivia
open Fantomas.FCS.Xml
open Myriad.Core

/// Attribute indicating an interface type for which the "Generate Mock" Myriad
/// generator should apply during build.
/// This generator creates a record which implements the interface,
/// but where each method is represented as a record field, so you can use
/// record update syntax to easily specify partially-implemented mock objects.
type GenerateMockAttribute () =
    inherit Attribute ()

[<RequireQualifiedAccess>]
module internal InterfaceMockGenerator =
    open Fantomas.FCS.Text.Range
    open Myriad.Core.Ast

    let private getName (SynField (_, _, id, _, _, _, _, _, _)) =
        match id with
        | None -> failwith "Expected record field to have a name, but it was somehow anonymous"
        | Some id -> id

    let createType
        (name : string)
        (generics : SynTyparDecls option)
        (xmlDoc : PreXmlDoc)
        (fields : SynField list)
        : SynModuleDecl
        =
        let synValData =
            {
                SynMemberFlags.IsInstance = false
                SynMemberFlags.IsDispatchSlot = false
                SynMemberFlags.IsOverrideOrExplicitImpl = false
                SynMemberFlags.IsFinal = false
                SynMemberFlags.GetterOrSetterIsCompilerGenerated = false
                SynMemberFlags.MemberKind = SynMemberKind.Member
            }

        let failwithFun =
            SynExpr.createLambda
                "x"
                (SynExpr.CreateApp (
                    SynExpr.CreateIdentString "failwith",
                    SynExpr.CreateConstString "Unimplemented function"
                ))

        let constructorIdent =
            let generics =
                generics |> Option.map (fun generics -> SynValTyparDecls (Some generics, false))

            SynPat.LongIdent (
                SynLongIdent.CreateString "Empty",
                None,
                generics,
                SynArgPats.Pats (
                    if generics.IsNone then
                        []
                    else
                        [ SynPat.CreateParen (SynPat.CreateConst SynConst.Unit) ]
                ),
                None,
                range0
            )

        let constructorReturnType =
            match generics with
            | None -> SynType.CreateLongIdent name
            | Some generics ->
                let generics =
                    generics.TyparDecls
                    |> List.map (fun (SynTyparDecl (_, typar)) -> SynType.Var (typar, range0))

                SynType.App (
                    SynType.CreateLongIdent name,
                    Some range0,
                    generics,
                    List.replicate (generics.Length - 1) range0,
                    Some range0,
                    false,
                    range0
                )
            |> SynBindingReturnInfo.Create

        let constructor =
            SynMemberDefn.Member (
                SynBinding.SynBinding (
                    None,
                    SynBindingKind.Normal,
                    false,
                    false,
                    [],
                    PreXmlDoc.Empty,
                    SynValData.SynValData (Some synValData, SynValInfo.Empty, None),
                    constructorIdent,
                    Some constructorReturnType,
                    AstHelper.instantiateRecord (
                        fields
                        |> List.map (fun field ->
                            ((SynLongIdent.CreateFromLongIdent [ getName field ], true), Some failwithFun)
                        )
                    ),
                    range0,
                    DebugPointAtBinding.Yes range0,
                    { SynExpr.synBindingTriviaZero true with
                        LeadingKeyword = SynLeadingKeyword.Static range0
                    }
                ),
                range0
            )

        let record =
            {
                Name = Ident.Create name
                Fields = fields
                Members = Some [ constructor ]
                XmlDoc = Some xmlDoc
                Generics = generics
            }

        let typeDecl = AstHelper.defineRecordType record

        SynModuleDecl.Types ([ typeDecl ], range0)

    let constructMember (mem : MemberInfo) : SynField =
        let inputType =
            match mem.Args |> List.map (fun pi -> pi.Type) |> List.rev with
            | [] -> failwith "no-arg functions not supported yet"
            | [ x ] -> x
            | last :: rest ->
                ([ SynTupleTypeSegment.Type last ], rest)
                ||> List.fold (fun ty nextArg ->
                    SynTupleTypeSegment.Type nextArg :: SynTupleTypeSegment.Star range0 :: ty
                )
                |> fun segs -> SynType.Tuple (false, segs, range0)

        let funcType = SynType.CreateFun (inputType, mem.ReturnType)

        SynField.SynField (
            [],
            true,
            Some mem.Identifier,
            funcType,
            false,
            mem.XmlDoc |> Option.defaultValue PreXmlDoc.Empty,
            None,
            range0,
            SynFieldTrivia.Zero
        )

    let createRecord (namespaceId : LongIdent) (interfaceType : SynTypeDefn) : SynModuleOrNamespace =
        let interfaceType = AstHelper.parseInterface interfaceType
        let fields = interfaceType.Members |> List.map constructMember
        let docString = PreXmlDoc.Create " Mock record type for an interface"

        let name =
            List.last interfaceType.Name
            |> fun s -> s.idText
            |> fun s ->
                if s.StartsWith 'I' && s.Length > 1 && Char.IsUpper s.[1] then
                    s.[1..]
                else
                    s
            |> fun s -> s + "Mock"

        let typeDecl = createType name interfaceType.Generics docString fields

        SynModuleOrNamespace.CreateNamespace (namespaceId, decls = [ typeDecl ])

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
                    match types |> List.filter Ast.hasAttribute<GenerateMockAttribute> with
                    | [] -> None
                    | types -> Some (ns, types)
                )

            let opens = AstHelper.extractOpens ast

            let modules =
                namespaceAndInterfaces
                |> List.collect (fun (ns, records) -> records |> List.map (InterfaceMockGenerator.createRecord ns))

            Output.Ast modules
