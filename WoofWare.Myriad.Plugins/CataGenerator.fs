namespace WoofWare.Myriad.Plugins

open Fantomas.FCS.Syntax
open Fantomas.FCS.SyntaxTrivia
open Fantomas.FCS.Xml
open Myriad.Core

[<RequireQualifiedAccess>]
module internal CataGenerator =
    open Fantomas.FCS.Text.Range
    open Myriad.Core.Ast

    /// Returns a function:
    /// let run{Case} (cata : Cata<{typars}>) (x : {Case}) : {TyPar} =
    ///     let instructions = ResizeArray ()
    ///     instructions.Add (Instruction.Process{Case} e)
    ///     let {typar1}Results, {typar2}Results, ... = loop cata instructions
    ///     { for all non-relevant typars: }
    ///     if {typar}Results.Count > 0 then failwith "logic error"
    ///     Seq.exactlyOne {relevantTypar}Stack
    let createRunFunction (allTypars : SynType list) (relevantTypar : SynType) (unionType : SynTypeDefn) : SynBinding =
        let relevantTypeName =
            match unionType with
            | SynTypeDefn.SynTypeDefn (SynComponentInfo.SynComponentInfo (longId = id), _, _, _, _, _) -> List.last id

        let allTyparNames =
            allTypars
            |> List.map (fun ty ->
                match ty with
                | SynType.Var (SynTypar.SynTypar (ident = ident), _) -> ident
                | _ -> failwith "logic error in generator"
            )

        let relevantTyparName =
            match relevantTypar with
            | SynType.Var (SynTypar.SynTypar (ident = ident), _) -> ident
            | _ -> failwith "logic error in generator"

        SynBinding.SynBinding (
            None,
            SynBindingKind.Normal,
            false,
            false,
            [],
            PreXmlDoc.Create " Execute the catamorphism.",
            SynValData.SynValData (
                None,
                SynValInfo.SynValInfo (
                    [ [ SynArgInfo.CreateIdString "cata" ] ; [ SynArgInfo.CreateIdString "x" ] ],
                    SynArgInfo.SynArgInfo ([], false, None)
                ),
                None
            ),
            SynPat.CreateLongIdent (
                SynLongIdent.CreateString ("run" + relevantTypeName.idText),
                [
                    SynPat.CreateParen (
                        SynPat.CreateTyped (
                            SynPat.CreateNamed (Ident.Create "cata"),
                            SynType.App (
                                SynType.CreateLongIdent "Cata",
                                Some range0,
                                allTypars,
                                List.replicate (allTypars.Length - 1) range0,
                                Some range0,
                                false,
                                range0
                            )
                        )
                    )
                    SynPat.CreateParen (
                        SynPat.CreateTyped (
                            SynPat.CreateNamed (Ident.Create "x"),
                            SynType.CreateLongIdent (SynLongIdent.CreateFromLongIdent [ relevantTypeName ])
                        )
                    )
                ]
            ),
            Some (SynBindingReturnInfo.Create relevantTypar),
            SynExpr.CreateTyped (
                SynExpr.LetOrUse (
                    false,
                    false,
                    [
                        SynBinding.Let (
                            valData = SynValData.SynValData (None, SynValInfo.Empty, None),
                            pattern = SynPat.CreateNamed (Ident.Create "instructions"),
                            expr =
                                SynExpr.CreateApp (
                                    SynExpr.CreateIdentString "ResizeArray",
                                    SynExpr.CreateConst SynConst.Unit
                                )
                        )
                    ],
                    SynExpr.CreateSequential
                        [
                            SynExpr.CreateApp (
                                SynExpr.CreateLongIdent (SynLongIdent.Create [ "instructions" ; "Add" ]),
                                SynExpr.CreateParen (
                                    SynExpr.CreateApp (
                                        SynExpr.CreateLongIdent (
                                            SynLongIdent.Create [ "Instruction" ; "Process" + relevantTypeName.idText ]
                                        ),
                                        SynExpr.CreateLongIdent (SynLongIdent.CreateString "x")
                                    )
                                )
                            )
                            SynExpr.LetOrUse (
                                false,
                                false,
                                [
                                    SynBinding.Let (
                                        valData = SynValData.SynValData (None, SynValInfo.Empty, None),
                                        pattern =
                                            SynPat.Tuple (
                                                false,
                                                List.map
                                                    (fun (t : Ident) ->
                                                        SynPat.CreateNamed (Ident.Create (t.idText + "Stack"))
                                                    )
                                                    allTyparNames,
                                                List.replicate (allTypars.Length - 1) range0,
                                                range0
                                            ),
                                        expr =
                                            SynExpr.CreateApp (
                                                SynExpr.CreateApp (
                                                    SynExpr.CreateIdentString "loop",
                                                    SynExpr.CreateIdentString "cata"
                                                ),
                                                SynExpr.CreateIdentString "instructions"
                                            )
                                    )
                                ],
                                // TODO: add the "all other stacks are empty" sanity checks
                                SynExpr.CreateApp (
                                    SynExpr.CreateLongIdent (SynLongIdent.Create [ "Seq" ; "exactlyOne" ]),
                                    SynExpr.CreateIdent (Ident.Create (relevantTyparName.idText + "Stack"))
                                ),
                                range0,
                                {
                                    SynExprLetOrUseTrivia.InKeyword = None
                                }
                            )
                        ],
                    range0,
                    {
                        InKeyword = None
                    }
                ),
                relevantTypar
            ),
            range0,
            DebugPointAtBinding.NoneAtLet,
            {
                LeadingKeyword = SynLeadingKeyword.Let range0
                InlineKeyword = None
                EqualsRange = Some range0
            }
        )

    let getName (ty : SynTypeDefn) : LongIdent =
        match ty with
        | SynTypeDefn.SynTypeDefn (SynComponentInfo.SynComponentInfo (_, _, _, id, _, _, _, _), _, _, _, _, _) -> id

    let getNameKey (ty : SynTypeDefn) : string =
        getName ty |> List.map _.idText |> String.concat "/"

    let getNameKeyUnion (unionType : SynType) : string =
        match unionType with
        | SynType.LongIdent (SynLongIdent.SynLongIdent (name, _, _)) -> name |> List.map _.idText |> String.concat "/"
        | _ -> failwithf "unrecognised type: %+A" unionType

    type UnionField =
        {
            Type : SynType
            Name : Ident option
        }

    type UnionCase =
        {
            Name : SynIdent
            Fields : UnionField list
        }

    let getCases (SynTypeDefn.SynTypeDefn (_, repr, _, _, _, _)) : UnionCase list =
        match repr with
        | SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.Union (_, cases, _), range0) ->
            cases
            |> List.map (fun (SynUnionCase.SynUnionCase (_, ident, kind, _, _, _, _)) ->
                match kind with
                | SynUnionCaseKind.FullType _ -> failwith "FullType union cases not supported"
                | SynUnionCaseKind.Fields fields ->
                    {
                        Name = ident
                        Fields =
                            fields
                            |> List.map (fun (SynField.SynField (_, _, id, ty, _, _, _, _, _)) ->
                                {
                                    Type = ty
                                    Name = id
                                }
                            )
                    }
            )
        | _ -> failwithf "Failed to get union cases for type that was: %+A" repr

    type FieldDescription =
        | ListSelf of SynType
        /// One of the union types itself
        | Self of SynType
        | NonRecursive of SynType

    let analyse (allUnionTypes : SynTypeDefn list) (case : UnionCase) : FieldDescription list =
        let rec go (ty : SynType) : FieldDescription =
            let stripped = SynType.stripOptionalParen ty

            match stripped with
            | ListType child ->
                let gone = go child

                match gone with
                | FieldDescription.NonRecursive ty -> FieldDescription.NonRecursive stripped
                | FieldDescription.Self ty -> FieldDescription.ListSelf ty
                | FieldDescription.ListSelf _ -> failwith "Deeply nested lists not currently supported"
            | PrimitiveType _ -> NonRecursive stripped
            | SynType.LongIdent (SynLongIdent.SynLongIdent (ty, _, _)) ->
                let key = ty |> List.map _.idText |> String.concat "/"
                let isSelf = allUnionTypes |> List.exists (fun unionTy -> getNameKey unionTy = key)

                if isSelf then
                    FieldDescription.Self stripped
                else
                    FieldDescription.NonRecursive stripped

            | _ -> failwithf "Unrecognised type: %+A" stripped

        case.Fields |> List.map _.Type |> List.map go

    /// Returns whether this type recursively contains a Self, and the emitted TODO
    let rec toInstructionCase (field : FieldDescription) : bool * SynType option =
        match field with
        | FieldDescription.NonRecursive ty -> false, Some ty
        | FieldDescription.Self ty -> true, None
        | FieldDescription.ListSelf ty ->
            // store the length of the list
            true, Some (SynType.Int ())

    /// Given the input `| Pair of Expr * Expr * PairOpKind`,
    /// strips out any members which contain recursive calls.
    /// Stores a list as an int which is "the length of the list".
    /// TODO: support other compound types.
    let createInstructionCases (allUnionTypes : SynTypeDefn list) (case : UnionCase) : UnionField list option =
        let analysed = analyse allUnionTypes case

        let hasRecursion, cases =
            ((false, []), analysed)
            ||> List.fold (fun (hasRecursion, cases) field ->
                let newHasRecursion, case = toInstructionCase field

                let cases =
                    match case with
                    | None -> cases
                    | Some case -> case :: cases

                hasRecursion || newHasRecursion, cases
            )

        if hasRecursion then
            cases
            |> List.rev
            |> List.map (fun ty ->
                {
                    Name = None
                    Type = ty
                }
            )
            |> Some
        else
            None

    let createInstructionType (allUnionTypes : SynTypeDefn list) : SynTypeDefn =
        // One union case for each union type, and then
        // a union case for each union case which contains a recursive reference.
        let casesFromProcess : SynUnionCase list =
            allUnionTypes
            |> List.map (fun unionType ->
                let name = getName unionType

                SynUnionCase.Create (
                    Ident.Create ("Process" + (List.last name).idText),
                    [
                        SynField.Create (SynType.CreateLongIdent (SynLongIdent.CreateFromLongIdent name))
                    ]
                )
            )

        let casesFromCases =
            allUnionTypes
            |> List.collect (fun unionType ->
                getCases unionType
                |> List.choose (fun case ->
                    let fields = createInstructionCases allUnionTypes case

                    match fields with
                    | None -> None
                    | Some fields ->
                        let name =
                            match case.Name with
                            | SynIdent.SynIdent (ident, _) ->
                                (List.last (getName unionType)).idText + ident.idText |> Ident.Create

                        SynUnionCase.Create (name, fields |> List.map (fun field -> SynField.Create field.Type))
                        |> Some
                )
            )

        let cases = casesFromProcess @ casesFromCases

        SynTypeDefn.SynTypeDefn (
            SynComponentInfo.SynComponentInfo (
                [ SynAttributeList.Create [ SynAttribute.RequireQualifiedAccess () ] ],
                None,
                [],
                [ Ident.Create "Instruction" ],
                PreXmlDoc.Empty,
                false,
                Some (SynAccess.Private range0),
                range0
            ),
            SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.Union (None, cases, range0), range0),
            [],
            None,
            range0,
            {
                LeadingKeyword = SynTypeDefnLeadingKeyword.Type range0
                EqualsRange = Some range0
                WithKeyword = None
            }
        )

    let createCataStructure (allUnionTypes : SynTypeDefn list) : SynTypeDefn list =
        let generics =
            allUnionTypes
            |> List.map (fun defn ->
                let name = getName defn |> List.map _.idText |> String.concat "" |> Ident.Create
                SynTypar.SynTypar (name, TyparStaticReq.None, false)
            )

        let tyToGenericMap =
            let names = allUnionTypes |> List.map getNameKey
            List.zip names generics |> Map.ofList

        allUnionTypes
        |> List.map (fun unionType ->
            let name =
                match getName unionType |> List.rev with
                | [] -> failwith "empty name"
                | head :: rest -> Ident.Create (head.idText + "Cata") :: rest |> List.rev

            let componentInfo =
                let generics = generics |> List.map (fun ty -> SynTyparDecl.SynTyparDecl ([], ty))

                SynComponentInfo.SynComponentInfo (
                    [],
                    Some (SynTyparDecls.PostfixList (generics, [], range0)),
                    [],
                    name,
                    // TODO: better docstring
                    PreXmlDoc.Create " Description of how to combine cases during a fold",
                    false,
                    None,
                    range0
                )

            let slots =
                let ourGenericName = tyToGenericMap.[getNameKey unionType]

                let flags =
                    {
                        SynMemberFlags.IsInstance = true
                        SynMemberFlags.IsDispatchSlot = true
                        SynMemberFlags.IsOverrideOrExplicitImpl = false
                        SynMemberFlags.IsFinal = false
                        SynMemberFlags.GetterOrSetterIsCompilerGenerated = false
                        SynMemberFlags.MemberKind = SynMemberKind.Member
                    }

                getCases unionType
                |> List.map (fun case ->
                    let caseName =
                        match case.Name with
                        | SynIdent.SynIdent (name, _) -> name

                    let arity =
                        SynValInfo.SynValInfo (
                            case.Fields |> List.map (fun field -> [ SynArgInfo.Empty ]),
                            SynArgInfo.Empty
                        )

                    let ty =
                        let analysed = analyse allUnionTypes case

                        (SynType.Var (ourGenericName, range0), List.rev analysed)
                        ||> List.fold (fun acc field ->
                            let place : SynType =
                                match field with
                                | FieldDescription.Self ty ->
                                    SynType.Var (tyToGenericMap.[getNameKeyUnion ty], range0)
                                | FieldDescription.ListSelf ty ->
                                    SynType.CreateApp (
                                        SynType.CreateLongIdent "list",
                                        [ SynType.Var (tyToGenericMap.[getNameKeyUnion ty], range0) ],
                                        true
                                    )
                                | FieldDescription.NonRecursive ty -> ty

                            SynType.Fun (
                                place,
                                acc,
                                range0,
                                {
                                    ArrowRange = range0
                                }
                            )
                        )

                    let slot =
                        SynValSig.SynValSig (
                            [],
                            case.Name,
                            SynValTyparDecls.SynValTyparDecls (None, true),
                            ty,
                            arity,
                            false,
                            false,
                            PreXmlDoc.Create $" How to operate on the %s{caseName.idText} case",
                            None,
                            None,
                            range0,
                            {
                                EqualsRange = None
                                WithKeyword = None
                                InlineKeyword = None
                                LeadingKeyword = SynLeadingKeyword.Abstract range0
                            }
                        )

                    SynMemberDefn.AbstractSlot (
                        slot,
                        flags,
                        range0,
                        {
                            GetSetKeywords = None
                        }
                    )
                )

            let repr = SynTypeDefnRepr.ObjectModel (SynTypeDefnKind.Unspecified, slots, range0)

            SynTypeDefn.SynTypeDefn (
                componentInfo,
                repr,
                [],
                None,
                range0,
                {
                    LeadingKeyword = SynTypeDefnLeadingKeyword.Type range0
                    EqualsRange = Some range0
                    WithKeyword = None
                }
            )
        )

    let createCataRecord (allUnionTypes : SynTypeDefn list) : SynTypeDefn =
        let nameForDoc = List.last (getName allUnionTypes.[0]) |> _.idText

        let generics =
            allUnionTypes
            |> List.map (fun defn ->
                let name = getName defn |> List.map _.idText |> String.concat "" |> Ident.Create
                SynTypar.SynTypar (name, TyparStaticReq.None, false)
            )

        let fields =
            allUnionTypes
            |> List.map (fun unionType ->
                let doc = PreXmlDoc.Create " TODO: doc"
                let name = getName unionType

                let ty =
                    SynType.App (
                        SynType.CreateLongIdent (SynLongIdent.CreateString (List.last(name).idText + "Cata")),
                        Some range0,
                        generics |> List.map (fun v -> SynType.Var (v, range0)),
                        List.replicate (generics.Length - 1) range0,
                        Some range0,
                        false,
                        range0
                    )

                SynField.SynField (
                    [],
                    false,
                    Some (List.last name),
                    ty,
                    false,
                    doc,
                    None,
                    range0,
                    {
                        LeadingKeyword = None
                    }
                )
            )

        let componentInfo =
            SynComponentInfo.SynComponentInfo (
                [],
                Some (
                    SynTyparDecls.PostfixList (
                        generics |> List.map (fun ty -> SynTyparDecl.SynTyparDecl ([], ty)),
                        [],
                        range0
                    )
                ),
                [],
                [ Ident.Create "Cata" ], // TODO: better name
                PreXmlDoc.Create $" Specifies how to perform a fold (catamorphism) over the type %s{nameForDoc}.",
                false,
                None,
                range0
            )

        SynTypeDefn.SynTypeDefn (
            componentInfo,
            SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.Record (None, fields, range0), range0),
            [],
            None,
            range0,
            {
                LeadingKeyword = SynTypeDefnLeadingKeyword.Type range0
                WithKeyword = None
                EqualsRange = Some range0
            }
        )

    let createModule
        (opens : SynOpenDeclTarget list)
        (ns : LongIdent)
        (taggedType : SynTypeDefn)
        (allUnionTypes : SynTypeDefn list)
        : SynModuleOrNamespace
        =
        let parentName = List.last (getName taggedType) |> _.idText
        let moduleName : LongIdent = parentName + "Cata" |> Ident.Create |> List.singleton

        let attribs = [ SynAttributeList.Create (SynAttribute.RequireQualifiedAccess ()) ]

        let modInfo =
            SynComponentInfo.Create (
                moduleName,
                attributes = attribs,
                xmldoc = PreXmlDoc.Create $" Methods to perform a catamorphism over the type {parentName}"
            )

        let allTypars =
            allUnionTypes
            |> List.map (fun unionType ->
                List.last (getName unionType)
                |> fun x -> x.idText + "Ret"
                |> Ident.Create
                |> fun x -> SynTypar.SynTypar (x, TyparStaticReq.None, false)
                |> fun x -> SynType.Var (x, range0)
            )

        let runFunctions =
            List.zip allUnionTypes allTypars
            |> List.map (fun (unionType, relevantTypar) -> createRunFunction allTypars relevantTypar unionType)

        let cataStructures =
            createCataStructure allUnionTypes
            |> List.map (fun repr -> SynModuleDecl.Types ([ repr ], range0))

        let cataRecord = SynModuleDecl.Types ([ createCataRecord allUnionTypes ], range0)

        SynModuleOrNamespace.CreateNamespace (
            ns,
            decls =
                [
                    for openStatement in opens do
                        yield SynModuleDecl.CreateOpen openStatement
                    yield! cataStructures
                    yield cataRecord
                    yield
                        SynModuleDecl.CreateNestedModule (
                            modInfo,
                            [
                                SynModuleDecl.Types ([ createInstructionType allUnionTypes ], range0)
                                SynModuleDecl.CreateLet runFunctions
                            ]
                        )
                ]
        )

/// Myriad generator that provides an HTTP client for an interface type using RestEase annotations.
[<MyriadGenerator("create-catamorphism")>]
type CreateCatamorphismGenerator () =

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
                    match types |> List.tryFind Ast.hasAttribute<CreateCatamorphismAttribute> with
                    | Some taggedType ->
                        let anyNonUnion =
                            types
                            |> List.exists (fun (SynTypeDefn.SynTypeDefn (_, repr, _, _, _, _)) ->
                                match repr with
                                | SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.Union _, _) -> false
                                | _ -> true
                            )

                        if anyNonUnion then
                            failwith
                                "Error: all types recursively defined together with a CreateCatamorphism type must be discriminated unions"

                        Some (ns, taggedType, types)
                    | _ -> None
                )

            let modules =
                namespaceAndTypes
                |> List.map (fun (ns, taggedType, types) -> CataGenerator.createModule opens ns taggedType types)

            Output.Ast modules
