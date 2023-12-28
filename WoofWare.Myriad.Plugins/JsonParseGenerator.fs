namespace WoofWare.Myriad.Plugins

open System
open System.Text
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

    type JsonParseOption =
        {
            JsonNumberHandlingArg : SynExpr option
        }

        static member None =
            {
                JsonNumberHandlingArg = None
            }

    /// {node}.AsValue().GetValue<{typeName}> ()
    let asValueGetValue (typeName : string) (node : SynExpr) : SynExpr =
        let asValue =
            SynExpr.CreateApp (
                SynExpr.DotGet (
                    node,
                    range0,
                    SynLongIdent.SynLongIdent (id = [ Ident.Create "AsValue" ], dotRanges = [], trivia = [ None ]),
                    range0
                ),
                SynExpr.CreateConst SynConst.Unit
            )

        SynExpr.CreateApp (
            SynExpr.TypeApp (
                SynExpr.DotGet (
                    asValue,
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
            SynExpr.CreateConst SynConst.Unit
        )

    /// {type}.jsonParse {node}
    let typeJsonParse (typeName : LongIdent) (node : SynExpr) : SynExpr =
        SynExpr.CreateApp (
            SynExpr.CreateLongIdent (SynLongIdent.CreateFromLongIdent (typeName @ [ Ident.Create "jsonParse" ])),
            node
        )

    /// collectionType is e.g. "List"; we'll be calling `ofSeq` on it.
    /// body is the body of a lambda which takes a parameter `elt`.
    /// {node}.AsArray()
    /// |> Seq.map (fun elt -> {body})
    /// |> {collectionType}.ofSeq
    let asArrayMapped (collectionType : string) (node : SynExpr) (body : SynExpr) : SynExpr =
        let parsedDataPat = [ SynPat.CreateNamed (Ident.Create "elt") ]

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
                            SynExpr.DotGet (node, range0, SynLongIdent.CreateString "AsArray", range0),
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
                                body,
                                Some (parsedDataPat, body),
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

    /// match {node} with | null -> None | v -> Some {body}
    /// Use the variable `v` to get access to the `Some`.
    let createParseLineOption (node : SynExpr) (body : SynExpr) : SynExpr =
        let body =
            SynExpr.CreateApp (
                SynExpr.CreateAppInfix (
                    SynExpr.CreateLongIdent (
                        SynLongIdent.SynLongIdent (
                            [ Ident.Create "op_PipeRight" ],
                            [],
                            [ Some (IdentTrivia.OriginalNotation "|>") ]
                        )
                    ),
                    body
                ),
                SynExpr.CreateIdentString "Some"
            )

        SynExpr.CreateMatch (
            node,
            [
                SynMatchClause.Create (SynPat.CreateNull, None, SynExpr.CreateIdent (Ident.Create "None"))
                SynMatchClause.Create (SynPat.CreateNamed (Ident.Create "v"), None, body)
            ]
        )

    let eltGetValue (elementType : string) : SynExpr =
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

    /// {expr} |> DateOnly.Parse
    let pipeThroughFunction (ident : SynLongIdent) (expr : SynExpr) : SynExpr =
        SynExpr.CreateApp (
            SynExpr.CreateAppInfix (
                SynExpr.CreateLongIdent (
                    SynLongIdent.SynLongIdent (
                        [ Ident.Create "op_PipeRight" ],
                        [],
                        [ Some (IdentTrivia.OriginalNotation "|>") ]
                    )
                ),
                expr
            ),
            SynExpr.CreateLongIdent ident
        )

    /// if {cond} then {trueBranch} else {falseBranch}
    let ifThenElse (cond : SynExpr) (trueBranch : SynExpr) (falseBranch : SynExpr) : SynExpr =
        SynExpr.IfThenElse (
            cond,
            trueBranch,
            Some falseBranch,
            DebugPointAtBinding.Yes range0,
            false,
            range0,
            {
                IfKeyword = range0
                IsElif = false
                ThenKeyword = range0
                ElseKeyword = Some range0
                IfToThenRange = range0
            }
        )

    /// try {body} with | {exc} as exc -> {handler}
    let pipeThroughTryWith (exc : SynPat) (handler : SynExpr) (body : SynExpr) : SynExpr =
        let clause =
            SynMatchClause.Create (SynPat.As (exc, SynPat.CreateNamed (Ident.Create "exc"), range0), None, handler)

        SynExpr.TryWith (
            body,
            [ clause ],
            range0,
            DebugPointAtTry.Yes range0,
            DebugPointAtWith.Yes range0,
            {
                TryKeyword = range0
                TryToWithRange = range0
                WithKeyword = range0
                WithToEndRange = range0
            }
        )

    /// {a} = {b}
    let equals (a : SynExpr) (b : SynExpr) =
        SynExpr.CreateApp (
            SynExpr.CreateAppInfix (
                SynExpr.CreateLongIdent (
                    SynLongIdent.SynLongIdent (
                        Ident.CreateLong "op_Equality",
                        [],
                        [ Some (IdentTrivia.OriginalNotation "=") ]
                    )
                ),
                a
            ),
            b
        )

    /// Given e.g. "float", returns "Double.Parse"
    let parseFunction (typeName : string) : LongIdent =
        match typeName with
        | "float32" -> [ "System" ; "Single" ]
        | "float" -> [ "System" ; "Double" ]
        | "byte"
        | "uint8" -> [ "System" ; "Byte" ]
        | "sbyte" -> [ "System" ; "SByte" ]
        | "int16" -> [ "System" ; "Int16" ]
        | "int" -> [ "System" ; "Int32" ]
        | "int64" -> [ "System" ; "Int64" ]
        | "uint16" -> [ "System" ; "UInt16" ]
        | "uint"
        | "uint32" -> [ "System" ; "UInt32" ]
        | "uint64" -> [ "System" ; "UInt64" ]
        | _ -> failwith $"Unable to identify a parsing function `string -> %s{typeName}`"
        |> fun a -> List.append a [ "Parse" ]
        |> List.map Ident.Create

    /// Given `node.["town"]`, for example, choose how to obtain a JSON value from it.
    let rec parseNode (options : JsonParseOption) (fieldType : SynType) (node : SynExpr) : SynExpr =
        // TODO: parsing format for DateTime etc
        match fieldType with
        | DateOnly ->
            asValueGetValue "string" node
            |> pipeThroughFunction (SynLongIdent.Create [ "System" ; "DateOnly" ; "Parse" ])
        | DateTime ->
            asValueGetValue "string" node
            |> pipeThroughFunction (SynLongIdent.Create [ "System" ; "DateTime" ; "Parse" ])
        | NumberType typeName ->
            let basic = asValueGetValue typeName node

            match options.JsonNumberHandlingArg with
            | None -> basic
            | Some option ->
                let reraise =
                    (SynExpr.CreateApp (SynExpr.CreateIdent (Ident.Create "reraise"), SynExpr.CreateConst SynConst.Unit))

                let cond =
                    SynExpr.CreateApp (
                        SynExpr.CreateLongIdent (SynLongIdent.Create [ "exc" ; "Message" ; "Contains" ]),
                        SynExpr.CreateConst (SynConst.CreateString "cannot be converted to")
                    )

                let trueBranch =
                    ifThenElse
                        (equals
                            option
                            (SynExpr.CreateLongIdent (
                                SynLongIdent.Create
                                    [
                                        "System"
                                        "Text"
                                        "Json"
                                        "Serialization"
                                        "JsonNumberHandling"
                                        "AllowReadingFromString"
                                    ]
                            )))
                        (asValueGetValue "string" node
                         |> pipeThroughFunction (SynLongIdent.CreateFromLongIdent (parseFunction typeName)))
                        reraise

                let handler = ifThenElse cond trueBranch reraise

                basic
                |> pipeThroughTryWith
                    (SynPat.IsInst (
                        SynType.LongIdent (SynLongIdent.Create [ "System" ; "InvalidOperationException" ]),
                        range0
                    ))
                    handler
        | PrimitiveType typeName -> asValueGetValue typeName node
        | OptionType ty ->
            parseNode options ty (SynExpr.CreateIdentString "v")
            |> createParseLineOption node
        | ListType ty ->
            parseNode options ty (SynExpr.CreateLongIdent (SynLongIdent.CreateString "elt"))
            |> asArrayMapped "List" node
        | ArrayType ty ->
            parseNode options ty (SynExpr.CreateLongIdent (SynLongIdent.CreateString "elt"))
            |> asArrayMapped "Array" node
        | _ ->
            // Let's just hope that we've also got our own type annotation!
            let typeName =
                match fieldType with
                | SynType.LongIdent ident -> ident.LongIdent
                | _ -> failwith $"Unrecognised type: %+A{fieldType}"

            typeJsonParse typeName node

    /// propertyName is probably a string literal, but it could be a [<Literal>] variable
    /// The result of this function is the body of a let-binding (not including the LHS of that let-binding).
    let createParseRhs (options : JsonParseOption) (propertyName : SynExpr) (fieldType : SynType) : SynExpr =
        SynExpr.DotIndexedGet (SynExpr.CreateIdentString "node", propertyName, range0, range0)
        |> parseNode options fieldType

    let stripOptionalParen (expr : SynExpr) =
        match expr with
        | SynExpr.Paren (expr, _, _, _) -> expr
        | expr -> expr

    let isJsonNumberHandling (literal : LongIdent) : bool =
        match List.rev literal |> List.map (fun ident -> ident.idText) with
        | [ _ ; "JsonNumberHandling" ]
        | [ _ ; "JsonNumberHandling" ; "Serialization" ]
        | [ _ ; "JsonNumberHandling" ; "Serialization" ; "Json" ]
        | [ _ ; "JsonNumberHandling" ; "Serialization" ; "Json" ; "Text" ]
        | [ _ ; "JsonNumberHandling" ; "Serialization" ; "Json" ; "Text" ; "System" ] -> true
        | _ -> false

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
            |> List.map (fun (SynField (attrs, _, id, fieldType, _, _, _, _, _)) ->
                let id =
                    match id with
                    | None -> failwith "didn't get an ID on field"
                    | Some id -> id

                let attrs = attrs |> List.collect (fun l -> l.Attributes)

                let propertyNameAttr =
                    attrs
                    |> List.tryFind (fun attr ->
                        attr.TypeName.AsString.EndsWith ("JsonPropertyName", StringComparison.Ordinal)
                    )

                let options =
                    (JsonParseOption.None, attrs)
                    ||> List.fold (fun options attr ->
                        if attr.TypeName.AsString.EndsWith ("JsonNumberHandling", StringComparison.Ordinal) then
                            let qualifiedEnumValue =
                                match stripOptionalParen attr.ArgExpr with
                                | SynExpr.LongIdent (_, SynLongIdent (ident, _, _), _, _) when
                                    isJsonNumberHandling ident
                                    ->
                                    // Make sure it's fully qualified
                                    SynExpr.CreateLongIdent (
                                        SynLongIdent.Create
                                            [
                                                "System"
                                                "Text"
                                                "Json"
                                                "Serialization"
                                                "JsonNumberHandling"
                                                "AllowReadingFromString"
                                            ]
                                    )
                                | _ -> attr.ArgExpr

                            {
                                JsonNumberHandlingArg = Some qualifiedEnumValue
                            }
                        else
                            options
                    )

                let propertyName =
                    match propertyNameAttr with
                    | None ->
                        let sb = StringBuilder id.idText.Length
                        sb.Append (Char.ToLowerInvariant id.idText.[0]) |> ignore

                        if id.idText.Length > 1 then
                            sb.Append id.idText.[1..] |> ignore

                        sb.ToString () |> SynConst.CreateString |> SynExpr.CreateConst
                    | Some name -> name.ArgExpr

                let pattern =
                    SynPat.LongIdent (
                        SynLongIdent.CreateFromLongIdent [ id ],
                        None,
                        None,
                        SynArgPats.Empty,
                        None,
                        range0
                    )

                SynBinding.Let (
                    isInline = false,
                    isMutable = false,
                    expr = createParseRhs options propertyName fieldType,
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

        let (SynComponentInfo (_attributes, _typeParams, _constraints, recordId, _, _preferPostfix, _access, _)) =
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