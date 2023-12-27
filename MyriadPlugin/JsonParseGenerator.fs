namespace MyriadPlugin

open System
open Fantomas.FCS.Syntax
open Fantomas.FCS.SyntaxTrivia
open Fantomas.FCS.Xml
open Myriad.Core

/// Attribute indicating a record type to which the "Add JSON parse" Myriad
/// generator should apply during build.
type JsonParseAttribute () =
    inherit Attribute ()

[<RequireQualifiedAccess>]
module internal JsonParseGenerator =
    open Fantomas.FCS.Text.Range
    open Myriad.Core.Ast

    let createParseLineValue (jsonName : string) (typeName : string) : SynExpr =
        // node.["town"].AsValue().GetValue<string> ()
        SynExpr.CreateApp (
            SynExpr.TypeApp (
                SynExpr.DotGet (
                    SynExpr.CreateApp (
                        SynExpr.DotGet (
                            SynExpr.DotIndexedGet (
                                SynExpr.Ident (Ident.Create "node"),
                                SynExpr.CreateConstString jsonName,
                                range0,
                                range0
                            ),
                            range0,
                            SynLongIdent.SynLongIdent (
                                id = [ Ident.Create "AsValue" ],
                                dotRanges = [],
                                trivia = [ None ]
                            ),
                            range0
                        ),
                        SynExpr.CreateConst (SynConst.Unit)
                    ),
                    range0,
                    SynLongIdent.SynLongIdent (id = [ Ident.Create "GetValue" ], dotRanges = [], trivia = [ None ]),
                    range0
                ),
                range0,
                [
                    SynType.LongIdent (
                        SynLongIdent.SynLongIdent (id = [ Ident.Create typeName ], dotRanges = [], trivia = [ None ])
                    )
                ],
                [],
                Some range0,
                range0,
                range0
            ),
            SynExpr.CreateConst (SynConst.Unit)
        )

    let createParseLineCallThrough (jsonName : string) (fieldType : SynType) : SynExpr =
        // Type.jsonParse node.["town"]
        let typeName =
            match fieldType with
            | SynType.LongIdent ident -> ident.LongIdent
            | _ -> failwith $"Unrecognised type: %+A{fieldType}"

        SynExpr.CreateApp (
            SynExpr.CreateLongIdent (SynLongIdent.CreateFromLongIdent (typeName @ [ Ident.Create "jsonParse" ])),
            SynExpr.DotIndexedGet (SynExpr.CreateIdentString "node", SynExpr.CreateConstString jsonName, range0, range0)
        )

    /// collectionType is e.g. "List"; we'll be calling `ofSeq` on it.
    let createParseLineList (collectionType : string) (jsonName : string) (elementType : string) : SynExpr =
        // node.["openingHours"].AsArray()
        // |> Seq.map (fun elt -> elt.AsValue().GetValue<string> ())
        // |> List.ofSeq

        let parsedDataPat = [ SynPat.CreateNamed (Ident.Create "elt") ]

        let parsedData =
            SynExpr.CreateApp (
                SynExpr.TypeApp (
                    SynExpr.DotGet (
                        SynExpr.CreateLongIdent (SynLongIdent.CreateString "elt"),
                        range0,
                        SynLongIdent.Create [ "GetValue" ],
                        range0
                    ),
                    range0,
                    [ SynType.CreateLongIdent elementType ],
                    [],
                    Some range0,
                    range0,
                    range0
                ),
                SynExpr.CreateConst SynConst.Unit
            )

        SynExpr.CreateApp (
            SynExpr.CreateAppInfix (
                SynExpr.LongIdent (
                    false,
                    SynLongIdent.SynLongIdent (
                        [ Ident.Create "op_PipeRight" ],
                        [],
                        [ Some (IdentTrivia.OriginalNotation "|>") ]
                    ),
                    None,
                    range0
                ),
                SynExpr.CreateApp (
                    SynExpr.CreateAppInfix (
                        SynExpr.LongIdent (
                            false,
                            SynLongIdent.SynLongIdent (
                                [ Ident.Create "op_PipeRight" ],
                                [],
                                [ Some (IdentTrivia.OriginalNotation "|>") ]
                            ),
                            None,
                            range0
                        ),
                        SynExpr.CreateApp (
                            SynExpr.DotGet (
                                SynExpr.DotIndexedGet (
                                    SynExpr.CreateIdent (Ident.Create "node"),
                                    SynExpr.CreateConstString jsonName,
                                    range0,
                                    range0
                                ),
                                range0,
                                SynLongIdent.CreateString "AsArray",
                                range0
                            ),
                            SynExpr.CreateConst SynConst.Unit
                        )
                    ),
                    SynExpr.CreateApp (
                        SynExpr.CreateLongIdent (SynLongIdent.Create [ "Seq" ; "map" ]),
                        SynExpr.CreateParen (
                            SynExpr.Lambda (
                                false,
                                false,
                                SynSimplePats.Create [ SynSimplePat.CreateId (Ident.Create "elt") ],
                                SynExpr.CreateApp (
                                    SynExpr.TypeApp (
                                        SynExpr.DotGet (
                                            SynExpr.CreateApp (
                                                SynExpr.CreateLongIdent (SynLongIdent.CreateString "elt"),
                                                SynExpr.CreateConst SynConst.Unit
                                            ),
                                            range0,
                                            SynLongIdent.CreateString "GetValue",
                                            range0
                                        ),
                                        range0,
                                        [ SynType.CreateLongIdent (SynLongIdent.CreateString elementType) ],
                                        [],
                                        None,
                                        range0,
                                        range0
                                    ),
                                    SynExpr.CreateConst SynConst.Unit
                                ),
                                Some (parsedDataPat, parsedData),
                                range0,
                                {
                                    ArrowRange = Some range0
                                }
                            )
                        )
                    )
                )
            ),
            SynExpr.CreateLongIdent (SynLongIdent.Create [ collectionType ; "ofSeq" ])
        )

    let createParseRhs (varName : string) (jsonName : string) (fieldType : SynType) : SynExpr =
        match fieldType with
        | OptionType ty -> failwith "TODO: options"
        | PrimitiveType typeName -> createParseLineValue jsonName typeName
        | ListType (PrimitiveType typeName) -> createParseLineList "List" jsonName typeName
        // TODO: support recursive lists
        | _ ->
            // Let's just hope that we've also got our own type annotation!
            createParseLineCallThrough jsonName fieldType

    let createMaker (typeName : LongIdent) (fields : SynField list) =
        let xmlDoc = PreXmlDoc.Create " Parse from a JSON node."

        let returnInfo =
            SynBindingReturnInfo.Create (SynType.LongIdent (SynLongIdent.CreateFromLongIdent typeName))

        let inputArg = Ident.Create "node"
        let functionName = Ident.Create "jsonParse"

        let inputVal =
            SynValData.SynValData (
                None,
                SynValInfo.SynValInfo ([ [ SynArgInfo.CreateId functionName ] ], SynArgInfo.Empty),
                Some inputArg
            )

        let assignments =
            fields
            |> List.map (fun (SynField (_, _, id, fieldType, _, _, _, _, _)) ->
                let pattern =
                    SynPat.LongIdent (
                        SynLongIdent.CreateFromLongIdent [ Option.get id ],
                        None,
                        None,
                        SynArgPats.Empty,
                        None,
                        range0
                    )

                SynBinding.Let (
                    isInline = false,
                    isMutable = false,
                    // TODO: id.Value.idText is gross for many reasons
                    expr = createParseRhs (id.ToString ()) id.Value.idText fieldType,
                    valData = inputVal,
                    pattern = pattern
                )
            )

        let finalConstruction =
            fields
            |> List.map (fun (SynField (_, _, id, _, _, _, _, _, _)) ->
                let id =
                    match id with
                    | None -> failwith "Expected record field to have an identifying name"
                    | Some id -> id

                (SynLongIdent.CreateFromLongIdent [ id ], true),
                Some (SynExpr.CreateLongIdent (SynLongIdent.CreateFromLongIdent [ id ]))
            )
            |> AstHelper.constructRecord

        let assignments =
            (finalConstruction, assignments)
            ||> List.fold (fun final assignment ->
                SynExpr.LetOrUse (
                    false,
                    false,
                    [ assignment ],
                    final,
                    range0,
                    {
                        InKeyword = None
                    }
                )
            )

        let pattern =
            SynPat.LongIdent (
                SynLongIdent.CreateFromLongIdent [ functionName ],
                None,
                None,
                SynArgPats.Pats
                    [
                        SynPat.CreateTyped (
                            SynPat.CreateNamed inputArg,
                            SynType.LongIdent (
                                SynLongIdent.Create [ "System" ; "Text" ; "Json" ; "Nodes" ; "JsonNode" ]
                            )
                        )
                        |> SynPat.CreateParen
                    ],
                None,
                range0
            )

        let binding =
            SynBinding.Let (
                isInline = false,
                isMutable = false,
                xmldoc = xmlDoc,
                returnInfo = returnInfo,
                expr = assignments,
                valData = inputVal,
                pattern = pattern
            )

        SynModuleDecl.CreateLet [ binding ]

    let createRecordModule (namespaceId : LongIdent) (typeDefn : SynTypeDefn) =
        let (SynTypeDefn (synComponentInfo, synTypeDefnRepr, _members, _implicitCtor, _, _)) =
            typeDefn

        let (SynComponentInfo (_attributes, _typeParams, _constraints, recordId, doc, _preferPostfix, _access, _)) =
            synComponentInfo

        match synTypeDefnRepr with
        | SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.Record (_accessibility, recordFields, _recordRange), _) ->

            let decls = [ createMaker recordId recordFields ]

            let compilationRepresentation : SynAttribute =
                {
                    TypeName = SynLongIdent.CreateString "CompilationRepresentation"
                    ArgExpr =
                        SynExpr.CreateLongIdent (
                            false,
                            SynLongIdent.Create [ "CompilationRepresentationFlags" ; "ModuleSuffix" ],
                            None
                        )
                        |> SynExpr.CreateParen
                    Target = None
                    AppliesToGetterAndSetter = false
                    Range = range0
                }

            let attributes =
                [
                    SynAttributeList.Create (SynAttribute.RequireQualifiedAccess ())
                    SynAttributeList.Create compilationRepresentation
                ]

            let xmlDoc =
                recordId
                |> Seq.map (fun i -> i.idText)
                |> String.concat "."
                |> sprintf " Module containing JSON parsing methods for the %s type"
                |> PreXmlDoc.Create

            let info =
                SynComponentInfo.Create (recordId, attributes = attributes, xmldoc = xmlDoc)

            let mdl = SynModuleDecl.CreateNestedModule (info, decls)

            SynModuleOrNamespace.CreateNamespace (namespaceId, decls = [ mdl ])
        | _ -> failwithf "Not a record type"

/// Myriad generator that provides a JSON parse function for a record type.
[<MyriadGenerator("json-parse")>]
type JsonParseGenerator () =

    interface IMyriadGenerator with
        member _.ValidInputExtensions = [ ".fs" ]

        member _.Generate (context : GeneratorContext) =
            let ast, _ =
                Ast.fromFilename context.InputFilename |> Async.RunSynchronously |> Array.head

            let records = Ast.extractRecords ast

            let namespaceAndRecords =
                records
                |> List.choose (fun (ns, types) ->
                    match types |> List.filter Ast.hasAttribute<JsonParseAttribute> with
                    | [] -> None
                    | types -> Some (ns, types)
                )

            let modules =
                namespaceAndRecords
                |> List.collect (fun (ns, records) ->
                    records
                    |> List.map (fun record ->
                        let recordModule = JsonParseGenerator.createRecordModule ns record
                        recordModule
                    )
                )

            Output.Ast modules
