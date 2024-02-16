namespace WoofWare.Myriad.Plugins

open System
open System.Text
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

    /// Given the input `| Pair of Expr * Expr * PairOpKind`,
    /// strips out any members which contain recursive calls.
    /// TODO: support lists and other compound types.
    let createInstructionCases (allUnionTypes : SynTypeDefn list) (case : UnionCase) : UnionField list option =
        let hasRecursion, cases =
            ((false, []), case.Fields)
            ||> List.fold (fun (hasRecursion, cases) field ->
                match SynType.stripOptionalParen field.Type with
                | ListType ty ->
                    match ty with
                    | SynType.LongIdent (SynLongIdent.SynLongIdent (ty, _, _)) ->
                        let isListOfSelf =
                            allUnionTypes
                            |> List.exists (fun unionTy -> List.last(getName unionTy).idText = List.last(ty).idText)

                        if isListOfSelf then
                            // store an int which is the length of the list
                            true, SynType.Int () :: cases
                        else
                            hasRecursion, field.Type :: cases
                    | _ -> hasRecursion, field.Type :: cases
                | PrimitiveType _ -> hasRecursion, field.Type :: cases
                | SynType.LongIdent (SynLongIdent.SynLongIdent (ty, _, _)) ->
                    let isSelf =
                        allUnionTypes
                        |> List.exists (fun unionTy -> List.last(getName unionTy).idText = List.last(ty).idText)

                    if isSelf then
                        true, cases
                    else
                        hasRecursion, field.Type :: cases
                | _ -> failwithf "Unrecognised type: %+A" field.Type
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

    let createModule
        (opens : SynOpenDeclTarget list)
        (ns : LongIdent)
        (taggedType : SynTypeDefn)
        (allUnionTypes : SynTypeDefn list)
        : SynModuleOrNamespace
        =
        let moduleName : LongIdent =
            List.last (getName taggedType)
            |> fun x -> x.idText + "Cata"
            |> Ident.Create
            |> List.singleton

        let attribs = [ SynAttributeList.Create (SynAttribute.RequireQualifiedAccess ()) ]

        let modInfo =
            SynComponentInfo.Create (
                moduleName,
                attributes = attribs,
                xmldoc = PreXmlDoc.Create " Catamorphism" // TODO: better docstring
            )

        let allTypars =
            allUnionTypes
            |> List.map (fun unionType ->
                List.last (getName unionType)
                |> fun x -> x.idText
                |> fun s -> s + "Ret"
                |> Ident.Create
                |> fun x -> SynTypar.SynTypar (x, TyparStaticReq.None, false)
                |> fun x -> SynType.Var (x, range0)
            )

        let runFunctions =
            List.zip allUnionTypes allTypars
            |> List.map (fun (unionType, relevantTypar) -> createRunFunction allTypars relevantTypar unionType)

        SynModuleOrNamespace.CreateNamespace (
            ns,
            decls =
                [
                    for openStatement in opens do
                        yield SynModuleDecl.CreateOpen openStatement
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
