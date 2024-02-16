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

    let createModule
        (opens : SynOpenDeclTarget list)
        (ns : LongIdent)
        (SynTypeDefn.SynTypeDefn (SynComponentInfo.SynComponentInfo (_, _, _, taggedType, _, _, _, _), _, _, _, _, _))
        (allUnionTypes : SynTypeDefn list)
        : SynModuleOrNamespace
        =
        let moduleName : LongIdent =
            List.last taggedType
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
            |> List.map (fun
                             (SynTypeDefn.SynTypeDefn (SynComponentInfo.SynComponentInfo (_, _, _, id, _, _, _, _),
                                                       _,
                                                       _,
                                                       _,
                                                       _,
                                                       _)) ->
                List.last id
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
                    yield SynModuleDecl.CreateNestedModule (modInfo, [ SynModuleDecl.CreateLet runFunctions ])
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
