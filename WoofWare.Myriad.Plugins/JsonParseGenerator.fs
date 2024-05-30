namespace WoofWare.Myriad.Plugins

open System
open System.Text
open Fantomas.FCS.Syntax
open Fantomas.FCS.SyntaxTrivia
open Fantomas.FCS.Xml
open Myriad.Core

type internal JsonParseOutputSpec =
    {
        ExtensionMethods : bool
    }

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

    /// (match {indexed} with | null -> raise (System.Collections.Generic.KeyNotFoundException ()) | v -> v)
    let assertNotNull (propertyName : SynExpr) (indexed : SynExpr) =
        let raiseExpr =
            SynExpr.CreateApp (
                SynExpr.CreateApp (
                    SynExpr.CreateIdentString "sprintf",
                    SynExpr.CreateConstString "Required key '%s' not found on JSON object"
                ),
                SynExpr.CreateParen propertyName
            )
            |> SynExpr.CreateParen
            |> SynExpr.applyFunction (
                SynExpr.CreateLongIdent (
                    SynLongIdent.Create [ "System" ; "Collections" ; "Generic" ; "KeyNotFoundException" ]
                )
            )
            |> SynExpr.CreateParen
            |> SynExpr.applyFunction (SynExpr.CreateIdentString "raise")

        SynExpr.CreateMatch (
            indexed,
            [
                SynMatchClause.Create (SynPat.CreateNull, None, raiseExpr)
                SynMatchClause.Create (SynPat.CreateNamed (Ident.Create "v"), None, SynExpr.CreateIdentString "v")
            ]
        )
        |> SynExpr.CreateParen

    /// {node}.AsValue().GetValue<{typeName}> ()
    /// If `propertyName` is Some, uses `assertNotNull {node}` instead of `{node}`.
    let asValueGetValue (propertyName : SynExpr option) (typeName : string) (node : SynExpr) : SynExpr =
        match propertyName with
        | None -> node
        | Some propertyName -> assertNotNull propertyName node
        |> SynExpr.callMethod "AsValue"
        |> SynExpr.callGenericMethod' "GetValue" typeName

    let asValueGetValueIdent (propertyName : SynExpr option) (typeName : LongIdent) (node : SynExpr) : SynExpr =
        match propertyName with
        | None -> node
        | Some propertyName -> assertNotNull propertyName node
        |> SynExpr.callMethod "AsValue"
        |> SynExpr.callGenericMethod "GetValue" typeName

    /// {node}.AsObject()
    /// If `propertyName` is Some, uses `assertNotNull {node}` instead of `{node}`.
    let asObject (propertyName : SynExpr option) (node : SynExpr) : SynExpr =
        match propertyName with
        | None -> node
        | Some propertyName -> assertNotNull propertyName node
        |> SynExpr.callMethod "AsObject"

    /// {type}.jsonParse {node}
    let typeJsonParse (typeName : LongIdent) (node : SynExpr) : SynExpr =
        SynExpr.CreateApp (
            SynExpr.CreateLongIdent (SynLongIdent.CreateFromLongIdent (typeName @ [ Ident.Create "jsonParse" ])),
            node
        )

    /// collectionType is e.g. "List"; we'll be calling `ofSeq` on it.
    /// body is the body of a lambda which takes a parameter `elt`.
    /// {assertNotNull node}.AsArray()
    /// |> Seq.map (fun elt -> {body})
    /// |> {collectionType}.ofSeq
    let asArrayMapped
        (propertyName : SynExpr option)
        (collectionType : string)
        (node : SynExpr)
        (body : SynExpr)
        : SynExpr
        =
        match propertyName with
        | None -> node
        | Some propertyName -> assertNotNull propertyName node
        |> SynExpr.callMethod "AsArray"
        |> SynExpr.pipeThroughFunction (
            SynExpr.CreateApp (
                SynExpr.CreateLongIdent (SynLongIdent.Create [ "Seq" ; "map" ]),
                SynExpr.createLambda "elt" body
            )
        )
        |> SynExpr.pipeThroughFunction (SynExpr.CreateLongIdent (SynLongIdent.Create [ collectionType ; "ofSeq" ]))

    /// match {node} with | null -> None | v -> {body} |> Some
    /// Use the variable `v` to get access to the `Some`.
    let createParseLineOption (node : SynExpr) (body : SynExpr) : SynExpr =
        let body = SynExpr.pipeThroughFunction (SynExpr.CreateIdentString "Some") body

        SynExpr.CreateMatch (
            node,
            [
                SynMatchClause.Create (SynPat.CreateNull, None, SynExpr.CreateIdent (Ident.Create "None"))
                SynMatchClause.Create (SynPat.CreateNamed (Ident.Create "v"), None, body)
            ]
        )

    /// Given e.g. "float", returns "System.Double.Parse"
    let parseFunction (typeName : string) : LongIdent =
        let qualified =
            match AstHelper.qualifyPrimitiveType typeName with
            | Some x -> x
            | None -> failwith $"Could not recognise type %s{typeName} as a primitive."

        List.append qualified [ Ident.Create "Parse" ]

    /// fun kvp -> let key = {key(kvp)} in let value = {value(kvp)} in (key, value))
    /// The inputs will be fed with appropriate SynExprs to apply them to the `kvp.Key` and `kvp.Value` args.
    let dictionaryMapper (key : SynExpr -> SynExpr) (value : SynExpr -> SynExpr) : SynExpr =
        let keyArg = SynExpr.createLongIdent [ "kvp" ; "Key" ] |> SynExpr.CreateParen

        let valueArg = SynExpr.createLongIdent [ "kvp" ; "Value" ] |> SynExpr.CreateParen

        SynExpr.CreateTuple [ SynExpr.CreateIdentString "key" ; SynExpr.CreateIdentString "value" ]
        |> SynExpr.createLet
            [
                SynBinding.Let (pattern = SynPat.CreateNamed (Ident.Create "value"), expr = value valueArg)
            ]
        |> SynExpr.createLet
            [
                SynBinding.Let (pattern = SynPat.CreateNamed (Ident.Create "key"), expr = key keyArg)
            ]
        |> SynExpr.createLambda "kvp"

    /// A conforming JSON object has only strings as keys. But it would be reasonable to allow the user
    /// to parse these as URIs, for example.
    let parseKeyString (desiredType : SynType) (key : SynExpr) : SynExpr =
        match desiredType with
        | String -> key
        | Uri ->
            key
            |> SynExpr.pipeThroughFunction (SynExpr.CreateLongIdent (SynLongIdent.Create [ "System" ; "Uri" ]))
        | _ ->
            failwithf
                $"Unable to parse the key type %+A{desiredType} of a JSON object. Keys are strings, and this plugin does not know how to convert to that from a string."

    /// Given `node.["town"]`, for example, choose how to obtain a JSON value from it.
    /// The property name is used in error messages at runtime to show where a JSON
    /// parse error occurred; supply `None` to indicate "don't validate".
    let rec parseNode
        (propertyName : SynExpr option)
        (options : JsonParseOption)
        (fieldType : SynType)
        (node : SynExpr)
        : SynExpr
        =
        // TODO: parsing format for DateTime etc
        match fieldType with
        | DateOnly ->
            node
            |> asValueGetValue propertyName "string"
            |> SynExpr.pipeThroughFunction (SynExpr.createLongIdent [ "System" ; "DateOnly" ; "Parse" ])
        | Uri ->
            node
            |> asValueGetValue propertyName "string"
            |> SynExpr.pipeThroughFunction (SynExpr.createLongIdent [ "System" ; "Uri" ])
        | Guid ->
            node
            |> asValueGetValue propertyName "string"
            |> SynExpr.pipeThroughFunction (SynExpr.createLongIdent [ "System" ; "Guid" ; "Parse" ])
        | DateTime ->
            node
            |> asValueGetValue propertyName "string"
            |> SynExpr.pipeThroughFunction (SynExpr.createLongIdent [ "System" ; "DateTime" ; "Parse" ])
        | NumberType typeName ->
            let basic = asValueGetValue propertyName typeName node

            match options.JsonNumberHandlingArg with
            | None -> basic
            | Some option ->
                let cond =
                    SynExpr.DotGet (
                        SynExpr.CreateIdentString "exc",
                        range0,
                        SynLongIdent.CreateString "Message",
                        range0
                    )
                    |> SynExpr.callMethodArg
                        "Contains"
                        (SynExpr.CreateConst (SynConst.CreateString "cannot be converted to"))

                let handler =
                    asValueGetValue propertyName "string" node
                    |> SynExpr.pipeThroughFunction (SynExpr.createLongIdent' (parseFunction typeName))
                    |> SynExpr.ifThenElse
                        (SynExpr.equals
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
                        SynExpr.reraise
                    |> SynExpr.ifThenElse cond SynExpr.reraise

                basic
                |> SynExpr.pipeThroughTryWith
                    (SynPat.IsInst (
                        SynType.LongIdent (SynLongIdent.Create [ "System" ; "InvalidOperationException" ]),
                        range0
                    ))
                    handler
        | PrimitiveType typeName -> asValueGetValueIdent propertyName typeName node
        | OptionType ty ->
            parseNode None options ty (SynExpr.CreateIdentString "v")
            |> createParseLineOption node
        | ListType ty ->
            parseNode None options ty (SynExpr.CreateLongIdent (SynLongIdent.CreateString "elt"))
            |> asArrayMapped propertyName "List" node
        | ArrayType ty ->
            parseNode None options ty (SynExpr.CreateLongIdent (SynLongIdent.CreateString "elt"))
            |> asArrayMapped propertyName "Array" node
        | IDictionaryType (keyType, valueType) ->
            node
            |> asObject propertyName
            |> SynExpr.pipeThroughFunction (
                SynExpr.CreateApp (
                    SynExpr.CreateLongIdent (SynLongIdent.Create [ "Seq" ; "map" ]),
                    dictionaryMapper (parseKeyString keyType) (parseNode None options valueType)
                )
            )
            |> SynExpr.pipeThroughFunction (SynExpr.CreateLongIdent (SynLongIdent.Create [ "dict" ]))
        | DictionaryType (keyType, valueType) ->
            node
            |> asObject propertyName
            |> SynExpr.pipeThroughFunction (
                SynExpr.CreateApp (
                    SynExpr.CreateLongIdent (SynLongIdent.Create [ "Seq" ; "map" ]),
                    dictionaryMapper (parseKeyString keyType) (parseNode None options valueType)
                )
            )
            |> SynExpr.pipeThroughFunction (
                SynExpr.CreateApp (
                    SynExpr.CreateLongIdent (SynLongIdent.Create [ "Seq" ; "map" ]),
                    SynExpr.CreateLongIdent (
                        SynLongIdent.Create [ "System" ; "Collections" ; "Generic" ; "KeyValuePair" ]
                    )
                )
            )
            |> SynExpr.pipeThroughFunction (
                SynExpr.CreateLongIdent (SynLongIdent.Create [ "System" ; "Collections" ; "Generic" ; "Dictionary" ])
            )
        | IReadOnlyDictionaryType (keyType, valueType) ->
            node
            |> asObject propertyName
            |> SynExpr.pipeThroughFunction (
                SynExpr.CreateApp (
                    SynExpr.CreateLongIdent (SynLongIdent.Create [ "Seq" ; "map" ]),
                    dictionaryMapper (parseKeyString keyType) (parseNode None options valueType)
                )
            )
            |> SynExpr.pipeThroughFunction (SynExpr.CreateLongIdent (SynLongIdent.Create [ "readOnlyDict" ]))
        | MapType (keyType, valueType) ->
            node
            |> asObject propertyName
            |> SynExpr.pipeThroughFunction (
                SynExpr.CreateApp (
                    SynExpr.CreateLongIdent (SynLongIdent.Create [ "Seq" ; "map" ]),
                    dictionaryMapper (parseKeyString keyType) (parseNode None options valueType)
                )
            )
            |> SynExpr.pipeThroughFunction (SynExpr.CreateLongIdent (SynLongIdent.Create [ "Map" ; "ofSeq" ]))
        | BigInt ->
            node
            |> SynExpr.callMethod "ToJsonString"
            |> SynExpr.CreateParen
            |> SynExpr.applyFunction (SynExpr.createLongIdent [ "System" ; "Numerics" ; "BigInteger" ; "Parse" ])
        | _ ->
            // Let's just hope that we've also got our own type annotation!
            let typeName =
                match fieldType with
                | SynType.LongIdent ident -> ident.LongIdent
                | _ -> failwith $"Unrecognised type: %+A{fieldType}"

            match propertyName with
            | None -> node
            | Some propertyName -> assertNotNull propertyName node
            |> typeJsonParse typeName

    /// propertyName is probably a string literal, but it could be a [<Literal>] variable
    /// The result of this function is the body of a let-binding (not including the LHS of that let-binding).
    let createParseRhs (options : JsonParseOption) (propertyName : SynExpr) (fieldType : SynType) : SynExpr =
        let objectToParse = SynExpr.CreateIdentString "node" |> SynExpr.index propertyName
        parseNode (Some propertyName) options fieldType objectToParse

    let isJsonNumberHandling (literal : LongIdent) : bool =
        match List.rev literal |> List.map (fun ident -> ident.idText) with
        | [ _ ; "JsonNumberHandling" ]
        | [ _ ; "JsonNumberHandling" ; "Serialization" ]
        | [ _ ; "JsonNumberHandling" ; "Serialization" ; "Json" ]
        | [ _ ; "JsonNumberHandling" ; "Serialization" ; "Json" ; "Text" ]
        | [ _ ; "JsonNumberHandling" ; "Serialization" ; "Json" ; "Text" ; "System" ] -> true
        | _ -> false

    /// `populateNode` will be inserted before we return the `node` variable.
    ///
    /// That is, we give you access to a `JsonNode` called `node`,
    /// and you must return a `typeName`.
    let scaffolding (spec : JsonParseOutputSpec) (typeName : LongIdent) (functionBody : SynExpr) : SynModuleDecl =
        let xmlDoc = PreXmlDoc.Create " Parse from a JSON node."

        let returnInfo = SynType.LongIdent (SynLongIdent.CreateFromLongIdent typeName)

        let inputArg = Ident.Create "node"
        let functionName = Ident.Create "jsonParse"

        let arg =
            SynPat.CreateNamed inputArg
            |> SynPat.annotateType (
                SynType.LongIdent (SynLongIdent.Create [ "System" ; "Text" ; "Json" ; "Nodes" ; "JsonNode" ])
            )

        if spec.ExtensionMethods then
            let binding =
                SynBinding.basic (SynLongIdent.CreateFromLongIdent [ functionName ]) [ arg ] functionBody
                |> SynBinding.makeStaticMember
                |> SynBinding.withXmlDoc xmlDoc
                |> SynBinding.withReturnAnnotation returnInfo

            let mem = SynMemberDefn.Member (binding, range0)

            let containingType =
                SynTypeDefn.SynTypeDefn (
                    SynComponentInfo.Create (typeName, xmldoc = PreXmlDoc.Create " Extension methods for JSON parsing"),
                    SynTypeDefnRepr.ObjectModel (SynTypeDefnKind.Augmentation range0, [], range0),
                    [ mem ],
                    None,
                    range0,
                    {
                        LeadingKeyword = SynTypeDefnLeadingKeyword.Type range0
                        EqualsRange = None
                        WithKeyword = None
                    }
                )

            SynModuleDecl.Types ([ containingType ], range0)
        else
            SynBinding.basic (SynLongIdent.CreateFromLongIdent [ functionName ]) [ arg ] functionBody
            |> SynBinding.withXmlDoc xmlDoc
            |> SynBinding.withReturnAnnotation returnInfo
            |> List.singleton
            |> SynModuleDecl.CreateLet

    let getParseOptions (fieldAttrs : SynAttribute list) =
        (JsonParseOption.None, fieldAttrs)
        ||> List.fold (fun options attr ->
            if attr.TypeName.AsString.EndsWith ("JsonNumberHandling", StringComparison.Ordinal) then
                let qualifiedEnumValue =
                    match SynExpr.stripOptionalParen attr.ArgExpr with
                    | SynExpr.LongIdent (_, SynLongIdent (ident, _, _), _, _) when isJsonNumberHandling ident ->
                        // Make sure it's fully qualified
                        SynExpr.createLongIdent
                            [
                                "System"
                                "Text"
                                "Json"
                                "Serialization"
                                "JsonNumberHandling"
                                "AllowReadingFromString"
                            ]
                    | _ -> attr.ArgExpr

                {
                    JsonNumberHandlingArg = Some qualifiedEnumValue
                }
            else
                options
        )


    let createRecordMaker (spec : JsonParseOutputSpec) (typeName : LongIdent) (fields : SynFieldData<Ident> list) =
        let assignments =
            fields
            |> List.mapi (fun i fieldData ->
                let propertyNameAttr =
                    fieldData.Attrs
                    |> List.tryFind (fun attr ->
                        attr.TypeName.AsString.EndsWith ("JsonPropertyName", StringComparison.Ordinal)
                    )

                let options = getParseOptions fieldData.Attrs

                let propertyName =
                    match propertyNameAttr with
                    | None ->
                        let sb = StringBuilder fieldData.Ident.idText.Length

                        sb.Append (Char.ToLowerInvariant fieldData.Ident.idText.[0])
                        |> ignore<StringBuilder>

                        if fieldData.Ident.idText.Length > 1 then
                            sb.Append (fieldData.Ident.idText.Substring 1) |> ignore<StringBuilder>

                        sb.ToString () |> SynConst.CreateString |> SynExpr.CreateConst
                    | Some name -> name.ArgExpr

                createParseRhs options propertyName fieldData.Type
                |> SynBinding.basic (SynLongIdent.CreateString $"arg_%i{i}") []
            )

        let finalConstruction =
            fields
            |> List.mapi (fun i fieldData ->
                (SynLongIdent.CreateFromLongIdent [ fieldData.Ident ], true),
                Some (SynExpr.CreateLongIdent (SynLongIdent.CreateString $"arg_%i{i}"))
            )
            |> AstHelper.instantiateRecord

        (finalConstruction, assignments)
        ||> List.fold (fun final assignment -> SynExpr.createLet [ assignment ] final)

    let createUnionMaker (spec : JsonParseOutputSpec) (typeName : LongIdent) (fields : UnionCase<Ident> list) =
        fields
        |> List.map (fun case ->
            let propertyName = JsonSerializeGenerator.getPropertyName case.Ident case.Attrs

            let body =
                if case.Fields.IsEmpty then
                    SynExpr.createLongIdent' (typeName @ [ case.Ident ])
                else
                    case.Fields
                    |> List.map (fun field ->
                        let propertyName = JsonSerializeGenerator.getPropertyName field.Ident field.Attrs
                        let options = getParseOptions field.Attrs
                        createParseRhs options propertyName field.Type
                    )
                    |> SynExpr.CreateParenedTuple
                    |> SynExpr.applyFunction (SynExpr.createLongIdent' (typeName @ [ case.Ident ]))
                    |> SynExpr.createLet
                        [
                            SynExpr.index (SynExpr.CreateConstString "data") (SynExpr.CreateIdentString "node")
                            |> assertNotNull (SynExpr.CreateConstString "data")
                            |> SynBinding.basic (SynLongIdent.CreateString "node") []
                        ]

            match propertyName with
            | SynExpr.Const (synConst, _) ->
                SynMatchClause.SynMatchClause (
                    SynPat.CreateConst synConst,
                    None,
                    body,
                    range0,
                    DebugPointAtTarget.Yes,
                    {
                        ArrowRange = Some range0
                        BarRange = Some range0
                    }
                )
            | _ ->
                SynMatchClause.SynMatchClause (
                    SynPat.CreateNamed (Ident.Create "x"),
                    Some (SynExpr.equals (SynExpr.CreateIdentString "x") propertyName),
                    body,
                    range0,
                    DebugPointAtTarget.Yes,
                    {
                        ArrowRange = Some range0
                        BarRange = Some range0
                    }
                )
        )
        |> fun l ->
            l
            @ [
                let fail =
                    SynExpr.plus
                        (SynExpr.CreateConstString "Unrecognised 'type' field value: ")
                        (SynExpr.CreateIdentString "v")
                    |> SynExpr.CreateParen
                    |> SynExpr.applyFunction (SynExpr.CreateIdentString "failwith")

                SynMatchClause.SynMatchClause (
                    SynPat.CreateNamed (Ident.Create "v"),
                    None,
                    fail,
                    range0,
                    DebugPointAtTarget.Yes,
                    {
                        ArrowRange = Some range0
                        BarRange = Some range0
                    }
                )
            ]
        |> SynExpr.createMatch (SynExpr.CreateIdentString "ty")
        |> SynExpr.createLet
            [
                let property = SynExpr.CreateConstString "type"

                SynExpr.CreateIdentString "node"
                |> SynExpr.index property
                |> assertNotNull property
                |> SynExpr.pipeThroughFunction (
                    SynExpr.createLambda
                        "v"
                        (SynExpr.callGenericMethod "GetValue" [ Ident.Create "string" ] (SynExpr.CreateIdentString "v"))
                )
                |> SynBinding.basic (SynLongIdent.CreateString "ty") []
            ]
    (*
            let ty =
                match node.["type"] with
                | null -> raise (System.Collections.Generic.KeyNotFoundException ())
                | v -> v.GetValue<string> ()
            match ty with
            | "emptyCase" -> FirstDu.EmptyCase
            | "case1" ->
                FirstDu.Case1
            | "case2" -> FirstDu.Case2
            | _ -> failwithf "Unrecognised case name: %s" ty
            *)


    let createModule (namespaceId : LongIdent) (spec : JsonParseOutputSpec) (typeDefn : SynTypeDefn) =
        let (SynTypeDefn (synComponentInfo, synTypeDefnRepr, _members, _implicitCtor, _, _)) =
            typeDefn

        let (SynComponentInfo (_attributes, _typeParams, _constraints, ident, _, _preferPostfix, _access, _)) =
            synComponentInfo

        let attributes =
            if spec.ExtensionMethods then
                [ SynAttributeList.Create SynAttribute.autoOpen ]
            else
                [
                    SynAttributeList.Create (SynAttribute.RequireQualifiedAccess ())
                    SynAttributeList.Create SynAttribute.compilationRepresentation
                ]

        let xmlDoc =
            let fullyQualified = ident |> Seq.map (fun i -> i.idText) |> String.concat "."

            let description =
                if spec.ExtensionMethods then
                    "extension members"
                else
                    "methods"

            $" Module containing JSON parsing %s{description} for the %s{fullyQualified} type"
            |> PreXmlDoc.Create

        let moduleName =
            if spec.ExtensionMethods then
                match ident with
                | [] -> failwith "unexpectedly got an empty identifier for record name"
                | ident ->
                    let expanded =
                        List.last ident
                        |> fun i -> i.idText
                        |> fun s -> s + "JsonParseExtension"
                        |> Ident.Create

                    List.take (List.length ident - 1) ident @ [ expanded ]
            else
                ident

        let info =
            SynComponentInfo.Create (moduleName, attributes = attributes, xmldoc = xmlDoc)

        let decl =
            match synTypeDefnRepr with
            | SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.Record (_accessibility, fields, _range), _) ->
                let fields = fields |> List.map SynField.extractWithIdent
                createRecordMaker spec ident fields
            | SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.Union (_accessibility, cases, _range), _) ->
                let optionGet (i : Ident option) =
                    match i with
                    | None -> failwith "WoofWare.Myriad requires union cases to have identifiers on each field."
                    | Some i -> i

                let cases =
                    cases
                    |> List.map SynUnionCase.extract
                    |> List.map (UnionCase.mapIdentFields optionGet)

                createUnionMaker spec ident cases
            | _ -> failwithf "Not a record or union type"

        let mdl =
            [ scaffolding spec ident decl ]
            |> fun d -> SynModuleDecl.CreateNestedModule (info, d)

        SynModuleOrNamespace.CreateNamespace (namespaceId, decls = [ mdl ])

/// Myriad generator that provides a method (possibly an extension method) for a record type,
/// containing a JSON parse function.
[<MyriadGenerator("json-parse")>]
type JsonParseGenerator () =

    interface IMyriadGenerator with
        member _.ValidInputExtensions = [ ".fs" ]

        member _.Generate (context : GeneratorContext) =
            let ast, _ =
                Ast.fromFilename context.InputFilename |> Async.RunSynchronously |> Array.head

            let recordsAndUnions =
                Ast.extractTypeDefn ast
                |> List.map (fun (name, defns) ->
                    defns
                    |> List.choose (fun defn ->
                        if Ast.isRecord defn then Some defn
                        elif Ast.isDu defn then Some defn
                        else None
                    )
                    |> fun defns -> name, defns
                )

            let namespaceAndTypes =
                recordsAndUnions
                |> List.choose (fun (ns, types) ->
                    types
                    |> List.choose (fun typeDef ->
                        match Ast.getAttribute<JsonParseAttribute> typeDef with
                        | None -> None
                        | Some attr ->
                            let arg =
                                match SynExpr.stripOptionalParen attr.ArgExpr with
                                | SynExpr.Const (SynConst.Bool value, _) -> value
                                | SynExpr.Const (SynConst.Unit, _) -> JsonParseAttribute.DefaultIsExtensionMethod
                                | arg ->
                                    failwith
                                        $"Unrecognised argument %+A{arg} to [<%s{nameof JsonParseAttribute}>]. Literals are not supported. Use `true` or `false` (or unit) only."

                            let spec =
                                {
                                    ExtensionMethods = arg
                                }

                            Some (typeDef, spec)
                    )
                    |> function
                        | [] -> None
                        | ty -> Some (ns, ty)
                )

            let modules =
                namespaceAndTypes
                |> List.collect (fun (ns, types) ->
                    types |> List.map (fun (ty, spec) -> JsonParseGenerator.createModule ns spec ty)
                )

            Output.Ast modules
