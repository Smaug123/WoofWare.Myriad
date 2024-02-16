namespace WoofWare.Myriad.Plugins

open System.Transactions
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
                                            SynLongIdent.Create
                                                [ "Instruction" ; "Process__" + relevantTypeName.idText ]
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
                                                        SynPat.CreateNamed (
                                                            Ident.Create (t.idText + "Stack") |> Ident.lowerFirstLetter
                                                        )
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
                                    SynExpr.CreateIdent (
                                        Ident.Create (relevantTyparName.idText + "Stack") |> Ident.lowerFirstLetter
                                    )
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
            SynExpr.synBindingTriviaZero false
        )

    let getName (ty : SynTypeDefn) : LongIdent =
        match ty with
        | SynTypeDefn.SynTypeDefn (SynComponentInfo.SynComponentInfo (_, _, _, id, _, _, _, _), _, _, _, _, _) -> id

    let getNameUnion (unionType : SynType) : LongIdent =
        match unionType with
        | SynType.LongIdent (SynLongIdent.SynLongIdent (name, _, _)) -> name
        | _ -> failwithf "unrecognised type: %+A" unionType

    let getNameKey (ty : SynTypeDefn) : string =
        getName ty |> List.map _.idText |> String.concat "/"

    let getNameKeyUnion (unionType : SynType) : string =
        match unionType with
        | SynType.LongIdent (SynLongIdent.SynLongIdent (name, _, _)) -> name |> List.map _.idText |> String.concat "/"
        | _ -> failwithf "unrecognised type: %+A" unionType

    type FieldDescription =
        | ListSelf of SynType
        /// One of the union types itself
        | Self of SynType
        | NonRecursive of SynType

    let analyse (allUnionTypes : SynTypeDefn list) (case : UnionCase) : (Ident option * FieldDescription) list =
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

        case.Fields |> List.map (fun x -> x.Name, go x.Type)

    /// Returns whether this type recursively contains a Self, and the emitted TODO
    let rec toInstructionCase (field : FieldDescription) : bool * SynType option =
        match field with
        | FieldDescription.NonRecursive ty -> false, Some ty
        | FieldDescription.Self ty -> true, None
        | FieldDescription.ListSelf ty ->
            // store the length of the list
            true, Some (SynType.Int ())

    type InstructionCase =
        {
            Name : Ident
            Fields : UnionField list
        }

    let getInstructionCaseName (thisUnionType : SynTypeDefn) (case : UnionCase) =
        match case.Name with
        | SynIdent.SynIdent (ident, _) ->
            (List.last (getName thisUnionType)).idText + "_" + ident.idText |> Ident.Create

    /// Given the input `| Pair of Expr * Expr * PairOpKind`,
    /// strips out any members which contain recursive calls.
    /// Stores a list as an int which is "the length of the list".
    /// TODO: support other compound types.
    let getRecursiveInstruction
        (allUnionTypes : SynTypeDefn list)
        (thisUnionType : SynTypeDefn)
        (case : UnionCase)
        : InstructionCase option
        =
        let analysed = analyse allUnionTypes case

        let hasRecursion, cases =
            ((false, []), analysed)
            ||> List.fold (fun (hasRecursion, cases) (fieldName, field) ->
                let newHasRecursion, case = toInstructionCase field

                let cases =
                    match case with
                    | None -> cases
                    | Some case -> (fieldName, case) :: cases

                hasRecursion || newHasRecursion, cases
            )

        let name = getInstructionCaseName thisUnionType case

        if hasRecursion then
            let fields =
                cases
                |> List.rev
                |> List.map (fun (name, ty) ->
                    {
                        Name = name
                        Type = ty
                    }
                )

            {
                Name = name
                Fields = fields
            }
            |> Some
        else
            None

    /// The instruction to "process an Expr"; the loop will have to descend
    /// into this Expr and break it down to discover what recursive calls
    /// and calls to the cata this will imply making.
    let baseCases (allUnionTypes : SynTypeDefn list) : InstructionCase list =
        allUnionTypes
        |> List.map (fun unionType ->
            let name = getName unionType

            {
                Name = Ident.Create ("Process__" + (List.last name).idText)
                Fields =
                    {
                        Name = None
                        Type = SynType.CreateLongIdent (SynLongIdent.CreateFromLongIdent (getName unionType))
                    }
                    |> List.singleton
            }
        )

    let recursiveCases (allUnionTypes : SynTypeDefn list) : InstructionCase list =
        allUnionTypes
        |> List.collect (fun unionType ->
            AstHelper.getUnionCases unionType
            |> List.choose (fun case -> getRecursiveInstruction allUnionTypes unionType case)
        )

    let createInstructionType (allUnionTypes : SynTypeDefn list) : SynTypeDefn =
        // One union case for each union type, and then
        // a union case for each union case which contains a recursive reference.
        let casesFromProcess : SynUnionCase list =
            baseCases allUnionTypes
            |> List.map (fun unionCase ->
                SynUnionCase.Create (unionCase.Name, unionCase.Fields |> List.map (fun f -> SynField.Create f.Type))
            )

        let casesFromCases =
            recursiveCases allUnionTypes
            |> List.map (fun case ->
                SynUnionCase.Create (case.Name, case.Fields |> List.map (fun field -> SynField.Create field.Type))
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

                AstHelper.getUnionCases unionType
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
                        // TODO: we should only have called this once; pass the resulting
                        // data structure in, rather than rederiving it
                        let analysed = analyse allUnionTypes case

                        (SynType.Var (ourGenericName, range0), List.rev analysed)
                        ||> List.fold (fun acc (_name, field) ->
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

    let createLoopFunction (allUnionTypes : SynTypeDefn list) : SynBinding =
        let valData =
            SynValData.SynValData (
                None,
                SynValInfo.SynValInfo (
                    [
                        [ SynArgInfo.SynArgInfo ([], false, Some (Ident.Create "cata")) ]
                        [ SynArgInfo.SynArgInfo ([], false, Some (Ident.Create "instructions")) ]
                    ],
                    SynArgInfo.Empty
                ),
                None
            )

        let headPat =
            SynPat.LongIdent (
                SynLongIdent.CreateString "loop",
                None,
                None,
                SynArgPats.Pats
                    [
                        SynPat.CreateParen (
                            SynPat.CreateTyped (
                                SynPat.CreateNamed (Ident.Create "cata"),
                                SynType.App (
                                    // TODO: better type name
                                    SynType.CreateLongIdent "Cata",
                                    Some range0,
                                    List.replicate allUnionTypes.Length (SynType.Anon range0),
                                    List.replicate (allUnionTypes.Length - 1) range0,
                                    Some range0,
                                    false,
                                    range0
                                )
                            )
                        )
                        SynPat.CreateParen (
                            SynPat.CreateTyped (
                                SynPat.CreateNamed (Ident.Create "instructions"),
                                SynType.App (
                                    SynType.CreateLongIdent "ResizeArray",
                                    Some range0,
                                    [ SynType.CreateLongIdent "Instruction" ],
                                    [],
                                    Some range0,
                                    false,
                                    range0
                                )
                            )
                        )
                    ],
                Some (SynAccess.Private range0),
                range0
            )

        let stackNames =
            allUnionTypes
            |> List.map (fun ty ->
                // TODO this is jank
                List.last(getName ty).idText + "Stack" |> Ident.Create |> Ident.lowerFirstLetter
            )

        // A clause for each type, splitting it into its cases:
        let baseMatchClauses =
            List.zip stackNames allUnionTypes
            |> List.map (fun (stackName, unionType) ->
                let cases = AstHelper.getUnionCases unionType
                let unionTypeName = getName unionType

                let matchCases =
                    cases
                    |> List.map (fun case ->
                        let name =
                            match case.Name with
                            | SynIdent (ident, _) -> ident

                        let analysis = analyse allUnionTypes case

                        let _, nonRecursiveArgs, selfArgs, listSelfArgs =
                            ((0, [], [], []), analysis)
                            ||> List.fold (fun (i, nonRec, self, listSelf) (name, desc) ->
                                let name =
                                    match name with
                                    | Some n -> n
                                    | None -> Ident.Create $"arg%i{i}"

                                match desc with
                                | FieldDescription.NonRecursive ty -> i + 1, (i, name, ty) :: nonRec, self, listSelf
                                | FieldDescription.Self ty -> i + 1, nonRec, (i, name, ty) :: self, listSelf
                                | FieldDescription.ListSelf ty -> i + 1, nonRec, self, (i, name, ty) :: listSelf
                            )

                        let matchBody =
                            if nonRecursiveArgs.Length = analysis.Length then
                                // directly call the cata
                                ((0,
                                  SynExpr.CreateLongIdent (
                                      SynLongIdent.CreateFromLongIdent (
                                          Ident.Create "cata" :: unionTypeName @ [ name ]
                                      )
                                  )),
                                 List.rev case.Fields)
                                ||> List.fold (fun (i, body) field ->
                                    let fieldName =
                                        match field.Name with
                                        | Some n -> n
                                        | None -> Ident.Create $"arg%i{i}"

                                    let body = SynExpr.CreateApp (body, SynExpr.CreateIdent fieldName)
                                    (i + 1, body)
                                )
                                |> snd
                                |> SynExpr.pipeThroughFunction (
                                    SynExpr.CreateLongIdent (
                                        SynLongIdent.CreateFromLongIdent (stackName :: [ Ident.Create "Add" ])
                                    )
                                )
                            else
                            // There's a recursive type in here, so we'll have to make some calls
                            // and then come back.

                            // The instruction to process us again once our inputs are ready:
                            let reprocessCommand =
                                SynExpr.CreateApp (
                                    SynExpr.CreateLongIdent (SynLongIdent.Create [ "instructions" ; "Add" ]),
                                    if selfArgs.Length = analysis.Length then
                                        SynExpr.CreateLongIdent (
                                            SynLongIdent.Create
                                                [
                                                    "Instruction"
                                                    List.last(unionTypeName).idText + "_" + name.idText
                                                ]
                                        )
                                    else
                                        // We need to tell ourselves each non-rec arg, and the length of each input list.
                                        SynExpr.CreateApp (
                                            SynExpr.CreateLongIdent (
                                                SynLongIdent.Create
                                                    [
                                                        "Instruction"
                                                        List.last(unionTypeName).idText + "_" + name.idText
                                                    ]
                                            ),
                                            SynExpr.CreateParenedTuple (
                                                listSelfArgs
                                                |> List.map (fun (i, argName, _) ->
                                                    i,
                                                    SynExpr.CreateParen (
                                                        SynExpr.CreateApp (
                                                            SynExpr.CreateLongIdent (
                                                                SynLongIdent.Create [ "List" ; "length" ]
                                                            ),
                                                            SynExpr.CreateIdent argName
                                                        )
                                                    )
                                                )
                                                |> List.append (
                                                    nonRecursiveArgs
                                                    |> List.map (fun (i, arg, _) -> i, SynExpr.CreateIdent arg)
                                                )
                                                |> List.sortBy fst
                                                |> List.map snd
                                            )
                                        )
                                        |> SynExpr.CreateParen
                                )

                            [
                                yield reprocessCommand

                                for i, (argName, arg) in Seq.rev (Seq.indexed analysis) do
                                    let argName =
                                        match argName with
                                        | Some n -> n
                                        | None -> Ident.Create $"arg%i{i}"

                                    match arg with
                                    | NonRecursive synType ->
                                        // Nothing to do, because we're not calling the cata yet
                                        ()
                                    | ListSelf synType ->
                                        // Tell our future self to process the list elements first.
                                        yield
                                            SynExpr.ForEach (
                                                DebugPointAtFor.Yes range0,
                                                DebugPointAtInOrTo.Yes range0,
                                                SeqExprOnly.SeqExprOnly false,
                                                true,
                                                SynPat.CreateNamed (SynIdent.SynIdent (Ident.Create "elt", None)),
                                                SynExpr.CreateIdent argName,
                                                SynExpr.CreateApp (
                                                    SynExpr.CreateLongIdent (
                                                        SynLongIdent.Create [ "instructions" ; "Add" ]
                                                    ),
                                                    SynExpr.CreateParen (
                                                        SynExpr.CreateApp (
                                                            SynExpr.CreateLongIdent (
                                                                SynLongIdent.Create
                                                                    [
                                                                        "Instruction"
                                                                        "Process__" + List.last(unionTypeName).idText
                                                                    ]
                                                            ),
                                                            SynExpr.CreateIdentString "elt"
                                                        )
                                                    )
                                                ),
                                                range0
                                            )
                                    | Self synType ->
                                        // And push the instruction to process each recursive call
                                        // onto the stack.
                                        yield
                                            SynExpr.CreateApp (
                                                SynExpr.CreateLongIdent (
                                                    SynLongIdent.Create [ "instructions" ; "Add" ]
                                                ),
                                                SynExpr.CreateParen (
                                                    SynExpr.CreateApp (
                                                        SynExpr.CreateLongIdent (
                                                            SynLongIdent.Create
                                                                [
                                                                    "Instruction"
                                                                    "Process"
                                                                    + "__"
                                                                    + List.last(getNameUnion synType).idText
                                                                ]
                                                        ),
                                                        SynExpr.CreateIdent argName
                                                    )
                                                )
                                            )
                            ]
                            |> SynExpr.CreateSequential

                        SynMatchClause.SynMatchClause (
                            SynPat.CreateLongIdent (
                                SynLongIdent.CreateFromLongIdent (unionTypeName @ [ name ]),
                                [
                                    SynPat.CreateParen (
                                        SynPat.Tuple (
                                            false,
                                            case.Fields
                                            |> List.mapi (fun i field ->
                                                let name =
                                                    match field.Name with
                                                    | None -> Ident.Create $"arg%i{i}"
                                                    | Some n -> n

                                                SynPat.CreateNamed name
                                            ),
                                            List.replicate (case.Fields.Length - 1) range0,
                                            range0
                                        )
                                    )
                                ]
                            ),
                            None,
                            matchBody,
                            range0,
                            DebugPointAtTarget.Yes,
                            {
                                ArrowRange = Some range0
                                BarRange = Some range0
                            }
                        )
                    )

                let bodyMatch = SynExpr.CreateMatch (SynExpr.CreateIdentString "x", matchCases)

                SynMatchClause.SynMatchClause (
                    SynPat.LongIdent (
                        // TODO this is also jank; should unify with DU generator
                        SynLongIdent.Create [ "Instruction" ; "Process__" + (List.last (unionTypeName)).idText ],
                        None,
                        None,
                        SynArgPats.Pats [ SynPat.CreateNamed (Ident.Create "x") ],
                        None,
                        range0
                    ),
                    None,
                    bodyMatch,
                    range0,
                    DebugPointAtTarget.Yes,
                    {
                        ArrowRange = Some range0
                        BarRange = Some range0
                    }
                )
            )

        // And a clause for each case with a recursive reference.
        let recMatchClauses : SynMatchClause list =
            allUnionTypes
            |> List.collect (fun unionType ->
                let cases = AstHelper.getUnionCases unionType

                cases
                |> List.choose (fun case ->
                    let analysis = analyse allUnionTypes case
                    // We already know there is a recursive reference somewhere
                    // in `analysis`.
                    if
                        analysis
                        |> List.exists (fun (_, ty) ->
                            match ty with
                            | NonRecursive _ -> false
                            | _ -> true
                        )
                    then
                        Some (case, analysis)
                    else
                        None
                )
                |> List.map (fun (case, analysis) ->
                    let lhsNames =
                        analysis
                        |> Seq.mapi (fun i x -> (i, x))
                        |> Seq.choose (fun (i, (name, desc)) ->
                            match desc with
                            | FieldDescription.NonRecursive _ ->
                                match name with
                                | None -> Ident.Create $"arg%i{i}"
                                | Some name -> name
                                |> SynPat.CreateNamed
                                |> Some
                            | FieldDescription.ListSelf _ -> Ident.Create "n" |> SynPat.CreateNamed |> Some
                            | FieldDescription.Self _ -> None
                        )
                        |> Seq.toList

                    let lhs =
                        match lhsNames with
                        | [] -> []
                        | lhsNames ->
                            SynPat.Tuple (false, lhsNames, List.replicate (lhsNames.Length - 1) range0, range0)
                            |> SynPat.CreateParen
                            |> List.singleton

                    let pat =
                        SynPat.LongIdent (
                            SynLongIdent.CreateFromLongIdent
                                [ Ident.Create "Instruction" ; getInstructionCaseName unionType case ],
                            None,
                            None,
                            SynArgPats.Pats lhs,
                            None,
                            range0
                        )

                    let body = [ SynExpr.CreateConst SynConst.Unit ] |> SynExpr.CreateSequential

                    SynMatchClause.SynMatchClause (
                        pat,
                        None,
                        body,
                        range0,
                        DebugPointAtTarget.Yes,
                        {
                            ArrowRange = Some range0
                            BarRange = Some range0
                        }
                    )
                )
            )

        let matchStatement =
            SynExpr.CreateMatch (SynExpr.CreateIdentString "currentInstruction", baseMatchClauses @ recMatchClauses)

        let body =
            SynExpr.CreateSequential
                [
                    SynExpr.CreateApp (
                        SynExpr.CreateLongIdent (SynLongIdent.Create [ "instructions" ; "RemoveAt" ]),
                        SynExpr.CreateParen (SynExpr.minusN (SynLongIdent.Create [ "instructions" ; "Count" ]) 1)
                    )
                    matchStatement
                ]

        let body =
            SynExpr.LetOrUse (
                false,
                false,
                [
                    SynBinding.SynBinding (
                        None,
                        SynBindingKind.Normal,
                        false,
                        false,
                        [],
                        PreXmlDoc.Empty,
                        SynValData.SynValData (None, SynValInfo.SynValInfo ([], SynArgInfo.Empty), None),
                        SynPat.CreateNamed (Ident.Create "currentInstruction"),
                        None,
                        SynExpr.DotIndexedGet (
                            SynExpr.CreateIdentString "instructions",
                            SynExpr.minusN (SynLongIdent.Create [ "instructions" ; "Count" ]) 1,
                            range0,
                            range0
                        ),
                        range0,
                        DebugPointAtBinding.Yes range0,
                        SynExpr.synBindingTriviaZero false
                    )
                ],
                body,
                range0,
                {
                    InKeyword = None
                }
            )

        let body =
            SynExpr.CreateSequential
                [
                    SynExpr.While (
                        DebugPointAtWhile.Yes range0,
                        SynExpr.CreateApp (
                            SynExpr.CreateAppInfix (
                                SynExpr.CreateLongIdent (
                                    SynLongIdent.SynLongIdent (
                                        [ Ident.Create "op_GreaterThan" ],
                                        [],
                                        [ Some (IdentTrivia.OriginalNotation ">") ]
                                    )
                                ),
                                SynExpr.CreateLongIdent (SynLongIdent.Create [ "instructions" ; "Count" ])
                            ),
                            SynExpr.CreateConst (SynConst.Int32 0)

                        ),
                        body,
                        range0
                    )
                    SynExpr.CreateTuple (
                        stackNames
                        |> List.map (List.singleton >> SynLongIdent.CreateFromLongIdent >> SynExpr.CreateLongIdent)
                    )
                ]

        let body =
            (body, List.zip stackNames allUnionTypes)
            ||> List.fold (fun body (stackName, unionType) ->
                SynExpr.LetOrUse (
                    false,
                    false,
                    [
                        SynBinding.SynBinding (
                            None,
                            SynBindingKind.Normal,
                            false,
                            false,
                            [],
                            PreXmlDoc.Empty,
                            SynValData.SynValData (None, SynValInfo.Empty, None),
                            SynPat.Named (SynIdent.SynIdent (stackName, None), false, None, range0),
                            None,
                            SynExpr.CreateApp (
                                SynExpr.CreateLongIdent (SynLongIdent.CreateString "ResizeArray"),
                                SynExpr.CreateConst SynConst.Unit
                            ),
                            range0,
                            DebugPointAtBinding.Yes range0,
                            SynExpr.synBindingTriviaZero false
                        )
                    ],
                    body,
                    range0,
                    {
                        SynExprLetOrUseTrivia.InKeyword = None
                    }
                )
            )

        SynBinding.SynBinding (
            Some (SynAccess.Private range0),
            SynBindingKind.Normal,
            false,
            false,
            [],
            PreXmlDoc.Empty,
            valData,
            headPat,
            None,
            body,
            range0,
            DebugPointAtBinding.NoneAtLet,
            trivia = SynExpr.synBindingTriviaZero false
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

        let loopFunction = createLoopFunction allUnionTypes

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
                                SynModuleDecl.CreateLet (loopFunction :: runFunctions)
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
