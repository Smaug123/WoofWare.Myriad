namespace WoofWare.Myriad.Plugins

open System
open System.Text
open Fantomas.FCS.Syntax
open Fantomas.FCS.SyntaxTrivia

type internal JsonParseOutputSpec =
    {
        ExtensionMethods : bool
    }

[<RequireQualifiedAccess>]
module internal JsonParseGenerator =
    open Fantomas.FCS.Text.Range

    type JsonParseOption =
        {
            JsonNumberHandlingArg : SynExpr option
        }

        static member None =
            {
                JsonNumberHandlingArg = None
            }

    /// (match {indexed} with | null -> raise (System.Collections.Generic.KeyNotFoundException ({propertyName} not found)) | v -> v)
    let assertNotNull (propertyName : SynExpr) (indexed : SynExpr) =
        let raiseExpr =
            SynExpr.applyFunction
                (SynExpr.createIdent "sprintf")
                (SynExpr.CreateConst "Required key '%s' not found on JSON object")
            |> SynExpr.applyTo (SynExpr.paren propertyName)
            |> SynExpr.paren
            |> SynExpr.applyFunction (
                SynExpr.createLongIdent [ "System" ; "Collections" ; "Generic" ; "KeyNotFoundException" ]
            )
            |> SynExpr.paren
            |> SynExpr.applyFunction (SynExpr.createIdent "raise")

        [
            SynMatchClause.create SynPat.createNull raiseExpr
            SynMatchClause.create (SynPat.named "v") (SynExpr.createIdent "v")
        ]
        |> SynExpr.createMatch indexed
        |> SynExpr.paren

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
        |> SynExpr.callGenericMethod (SynLongIdent.createS "GetValue") [ SynType.createLongIdent typeName ]

    /// {node}.AsObject()
    /// If `propertyName` is Some, uses `assertNotNull {node}` instead of `{node}`.
    let asObject (propertyName : SynExpr option) (node : SynExpr) : SynExpr =
        match propertyName with
        | None -> node
        | Some propertyName -> assertNotNull propertyName node
        |> SynExpr.callMethod "AsObject"

    /// {type}.jsonParse {node}
    let typeJsonParse (typeName : LongIdent) (node : SynExpr) : SynExpr =
        node
        |> SynExpr.applyFunction (SynExpr.createLongIdent' (typeName @ [ Ident.create "jsonParse" ]))

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
            SynExpr.applyFunction (SynExpr.createLongIdent [ "Seq" ; "map" ]) (SynExpr.createLambda "elt" body)
        )
        |> SynExpr.pipeThroughFunction (SynExpr.createLongIdent [ collectionType ; "ofSeq" ])

    let dotParse (typeName : LongIdent) : LongIdent =
        List.append typeName [ Ident.create "Parse" ]

    /// fun kvp -> let key = {key(kvp)} in let value = {value(kvp)} in (key, value))
    /// The inputs will be fed with appropriate SynExprs to apply them to the `kvp.Key` and `kvp.Value` args.
    let dictionaryMapper (key : SynExpr -> SynExpr) (value : SynExpr -> SynExpr) : SynExpr =
        let keyArg = SynExpr.createLongIdent [ "kvp" ; "Key" ] |> SynExpr.paren

        let valueArg = SynExpr.createLongIdent [ "kvp" ; "Value" ] |> SynExpr.paren

        // No need to paren here, we're on the LHS of a `let`
        SynExpr.tupleNoParen [ SynExpr.createIdent "key" ; SynExpr.createIdent "value" ]
        |> SynExpr.createLet [ SynBinding.basic [ Ident.create "value" ] [] (value valueArg) ]
        |> SynExpr.createLet [ SynBinding.basic [ Ident.create "key" ] [] (key keyArg) ]
        |> SynExpr.createLambda "kvp"

    /// A conforming JSON object has only strings as keys. But it would be reasonable to allow the user
    /// to parse these as URIs, for example.
    let parseKeyString (desiredType : SynType) (key : SynExpr) : SynExpr =
        match desiredType with
        | String -> key
        | Uri ->
            key
            |> SynExpr.pipeThroughFunction (SynExpr.createLongIdent [ "System" ; "Uri" ])
        | _ ->
            failwithf
                $"Unable to parse the key type %+A{desiredType} of a JSON object. Keys are strings, and this plugin does not know how to convert to that from a string."

    let private parseNumberType
        (options : JsonParseOption)
        (propertyName : SynExpr option)
        (node : SynExpr)
        (typeName : LongIdent)
        =
        let basic = asValueGetValueIdent propertyName typeName node

        match options.JsonNumberHandlingArg with
        | None -> basic
        | Some option ->
            let cond =
                SynExpr.DotGet (SynExpr.createIdent "exc", range0, SynLongIdent.createS "Message", range0)
                |> SynExpr.callMethodArg "Contains" (SynExpr.CreateConst "cannot be converted to")

            let handler =
                asValueGetValue propertyName "string" node
                |> SynExpr.pipeThroughFunction (SynExpr.createLongIdent' (typeName |> dotParse))
                |> SynExpr.ifThenElse
                    (SynExpr.equals
                        option
                        (SynExpr.createLongIdent
                            [
                                "System"
                                "Text"
                                "Json"
                                "Serialization"
                                "JsonNumberHandling"
                                "AllowReadingFromString"
                            ]))
                    SynExpr.reraise
                |> SynExpr.ifThenElse cond SynExpr.reraise

            basic
            |> SynExpr.pipeThroughTryWith
                (SynPat.IsInst (
                    SynType.LongIdent (SynLongIdent.createS' [ "System" ; "InvalidOperationException" ]),
                    range0
                ))
                handler

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
        | DateTimeOffset ->
            node
            |> asValueGetValue propertyName "string"
            |> SynExpr.pipeThroughFunction (SynExpr.createLongIdent [ "System" ; "DateTimeOffset" ; "Parse" ])
        | NumberType typeName -> parseNumberType options propertyName node typeName
        | PrimitiveType typeName -> asValueGetValueIdent propertyName typeName node
        | OptionType ty ->
            let someClause =
                parseNode None options ty (SynExpr.createIdent "v")
                |> SynExpr.pipeThroughFunction (SynExpr.createIdent "Some")
                |> SynMatchClause.create (SynPat.named "v")

            [
                SynMatchClause.create SynPat.createNull (SynExpr.createIdent "None")
                someClause
            ]
            |> SynExpr.createMatch node
        | NullableType ty ->
            let someClause =
                parseNode None options ty (SynExpr.createIdent "v")
                |> SynExpr.pipeThroughFunction (SynExpr.createLongIdent [ "System" ; "Nullable" ])
                |> SynMatchClause.create (SynPat.named "v")

            [
                SynMatchClause.create
                    SynPat.createNull
                    (SynExpr.applyFunction (SynExpr.createLongIdent [ "System" ; "Nullable" ]) (SynExpr.CreateConst ()))
                someClause
            ]
            |> SynExpr.createMatch node
        | ListType ty ->
            parseNode None options ty (SynExpr.createIdent "elt")
            |> asArrayMapped propertyName "List" node
        | ArrayType ty ->
            parseNode None options ty (SynExpr.createIdent "elt")
            |> asArrayMapped propertyName "Array" node
        | IDictionaryType (keyType, valueType) ->
            node
            |> asObject propertyName
            |> SynExpr.pipeThroughFunction (
                SynExpr.applyFunction
                    (SynExpr.createLongIdent [ "Seq" ; "map" ])
                    (dictionaryMapper (parseKeyString keyType) (parseNode None options valueType))
            )
            |> SynExpr.pipeThroughFunction (SynExpr.createIdent "dict")
        | DictionaryType (keyType, valueType) ->
            node
            |> asObject propertyName
            |> SynExpr.pipeThroughFunction (
                SynExpr.applyFunction
                    (SynExpr.createLongIdent [ "Seq" ; "map" ])
                    (dictionaryMapper (parseKeyString keyType) (parseNode None options valueType))
            )
            |> SynExpr.pipeThroughFunction (
                SynExpr.applyFunction
                    (SynExpr.createLongIdent [ "Seq" ; "map" ])
                    (SynExpr.createLongIdent [ "System" ; "Collections" ; "Generic" ; "KeyValuePair" ])
            )
            |> SynExpr.pipeThroughFunction (
                SynExpr.createLongIdent [ "System" ; "Collections" ; "Generic" ; "Dictionary" ]
            )
        | IReadOnlyDictionaryType (keyType, valueType) ->
            node
            |> asObject propertyName
            |> SynExpr.pipeThroughFunction (
                SynExpr.applyFunction
                    (SynExpr.createLongIdent [ "Seq" ; "map" ])
                    (dictionaryMapper (parseKeyString keyType) (parseNode None options valueType))
            )
            |> SynExpr.pipeThroughFunction (SynExpr.createIdent "readOnlyDict")
        | MapType (keyType, valueType) ->
            node
            |> asObject propertyName
            |> SynExpr.pipeThroughFunction (
                SynExpr.applyFunction
                    (SynExpr.createLongIdent [ "Seq" ; "map" ])
                    (dictionaryMapper (parseKeyString keyType) (parseNode None options valueType))
            )
            |> SynExpr.pipeThroughFunction (SynExpr.createLongIdent [ "Map" ; "ofSeq" ])
        | BigInt ->
            node
            |> SynExpr.callMethod "ToJsonString"
            |> SynExpr.paren
            |> SynExpr.applyFunction (SynExpr.createLongIdent [ "System" ; "Numerics" ; "BigInteger" ; "Parse" ])
        | Measure (_measure, primType) ->
            parseNumberType options propertyName node primType
            |> SynExpr.pipeThroughFunction (Measure.getLanguagePrimitivesMeasure primType)
        | JsonNode -> node
        | Unit -> SynExpr.CreateConst ()
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
        let objectToParse = SynExpr.createIdent "node" |> SynExpr.index propertyName
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
        let xmlDoc = PreXmlDoc.create "Parse from a JSON node."

        let returnInfo = SynType.createLongIdent typeName

        let inputArg = "node"
        let functionName = Ident.create "jsonParse"

        let arg =
            SynPat.named inputArg
            |> SynPat.annotateType (SynType.createLongIdent' [ "System" ; "Text" ; "Json" ; "Nodes" ; "JsonNode" ])

        if spec.ExtensionMethods then
            let binding =
                SynBinding.basic [ functionName ] [ arg ] functionBody
                |> SynBinding.withXmlDoc xmlDoc
                |> SynBinding.withReturnAnnotation returnInfo
                |> SynMemberDefn.staticMember

            let componentInfo =
                SynComponentInfo.createLong typeName
                |> SynComponentInfo.withDocString (PreXmlDoc.create "Extension methods for JSON parsing")

            let containingType =
                SynTypeDefnRepr.augmentation ()
                |> SynTypeDefn.create componentInfo
                |> SynTypeDefn.withMemberDefns [ binding ]

            SynModuleDecl.Types ([ containingType ], range0)
        else
            SynBinding.basic [ functionName ] [ arg ] functionBody
            |> SynBinding.withXmlDoc xmlDoc
            |> SynBinding.withReturnAnnotation returnInfo
            |> SynModuleDecl.createLet

    let getParseOptions (fieldAttrs : SynAttribute list) =
        (JsonParseOption.None, fieldAttrs)
        ||> List.fold (fun options attr ->
            if
                (SynLongIdent.toString attr.TypeName)
                    .EndsWith ("JsonNumberHandling", StringComparison.Ordinal)
            then
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

    let createRecordMaker (spec : JsonParseOutputSpec) (fields : SynFieldData<Ident> list) =
        let propertyFields =
            fields
            |> List.map (fun fieldData ->
                let propertyNameAttr =
                    fieldData.Attrs
                    |> List.tryFind (fun attr ->
                        (SynLongIdent.toString attr.TypeName)
                            .EndsWith ("JsonPropertyName", StringComparison.Ordinal)
                    )

                let extensionDataAttr =
                    fieldData.Attrs
                    |> List.tryFind (fun attr ->
                        (SynLongIdent.toString attr.TypeName)
                            .EndsWith ("JsonExtensionData", StringComparison.Ordinal)
                    )

                let propertyName =
                    match propertyNameAttr with
                    | None ->
                        let sb = StringBuilder fieldData.Ident.idText.Length

                        sb.Append (Char.ToLowerInvariant fieldData.Ident.idText.[0])
                        |> ignore<StringBuilder>

                        if fieldData.Ident.idText.Length > 1 then
                            sb.Append (fieldData.Ident.idText.Substring 1) |> ignore<StringBuilder>

                        sb.ToString () |> SynExpr.CreateConst
                    | Some name -> name.ArgExpr

                propertyName, extensionDataAttr
            )

        let namedPropertyFields =
            propertyFields
            |> List.choose (fun (name, extension) ->
                match extension with
                | Some _ -> None
                | None -> Some name
            )

        let isNamedPropertyField =
            match namedPropertyFields with
            | [] -> SynExpr.CreateConst false
            | _ ->
                namedPropertyFields
                |> List.map (fun fieldName -> SynExpr.equals (SynExpr.createIdent "key") fieldName)
                |> List.reduce SynExpr.booleanOr

        let assignments =
            List.zip fields propertyFields
            |> List.mapi (fun i (fieldData, (propertyName, extensionDataAttr)) ->
                let options = getParseOptions fieldData.Attrs

                let accIdent = Ident.create $"arg_%i{i}"

                match extensionDataAttr with
                | Some _ ->
                    // Can't go through the usual parse logic here, because that will try and identify the node that's
                    // been labelled. The whole point of JsonExtensionData is that there is no such node!
                    let valType =
                        match fieldData.Type with
                        | DictionaryType (String, v) -> v
                        | _ -> failwith "Expected JsonExtensionData to be Dictionary<string, _>"

                    SynExpr.ifThenElse
                        isNamedPropertyField
                        (SynExpr.callMethodArg
                            "Add"
                            (SynExpr.tuple
                                [
                                    SynExpr.createIdent "key"
                                    createParseRhs options (SynExpr.createIdent "key") valType
                                ])
                            (SynExpr.createIdent "result"))
                        (SynExpr.CreateConst ())
                    |> SynExpr.createForEach
                        (SynPat.nameWithArgs "KeyValue" [ SynPat.named "key" ; SynPat.named "value" ])
                        (SynExpr.createIdent "node")
                    |> fun forEach -> [ forEach ; SynExpr.createIdent "result" ]
                    |> SynExpr.sequential
                    |> SynExpr.createLet
                        [
                            SynBinding.basic
                                [ Ident.create "result" ]
                                []
                                (SynExpr.typeApp
                                    [ SynType.string ; valType ]
                                    (SynExpr.createLongIdent [ "System" ; "Collections" ; "Generic" ; "Dictionary" ])
                                 |> SynExpr.applyTo (SynExpr.CreateConst ()))

                            SynBinding.basic
                                [ Ident.create "node" ]
                                []
                                (SynExpr.createIdent "node" |> SynExpr.callMethod "AsObject")
                        ]
                    |> SynBinding.basic [ accIdent ] []
                | None ->

                createParseRhs options propertyName fieldData.Type
                |> SynBinding.basic [ accIdent ] []
            )

        let finalConstruction =
            fields
            |> List.mapi (fun i fieldData -> SynLongIdent.createI fieldData.Ident, SynExpr.createIdent $"arg_%i{i}")
            |> AstHelper.instantiateRecord

        (finalConstruction, assignments)
        ||> List.fold (fun final assignment -> SynExpr.createLet [ assignment ] final)

    let createUnionMaker (spec : JsonParseOutputSpec) (typeName : LongIdent) (fields : UnionCase<Ident> list) =
        fields
        |> List.map (fun case ->
            let propertyName = JsonSerializeGenerator.getPropertyName case.Name case.Attributes

            let body =
                if case.Fields.IsEmpty then
                    SynExpr.createLongIdent' (typeName @ [ case.Name ])
                else
                    case.Fields
                    |> List.map (fun field ->
                        let propertyName = JsonSerializeGenerator.getPropertyName field.Ident field.Attrs
                        let options = getParseOptions field.Attrs
                        createParseRhs options propertyName field.Type
                    )
                    |> SynExpr.tuple
                    |> SynExpr.applyFunction (SynExpr.createLongIdent' (typeName @ [ case.Name ]))
                    |> SynExpr.createLet
                        [
                            SynExpr.index (SynExpr.CreateConst "data") (SynExpr.createIdent "node")
                            |> assertNotNull (SynExpr.CreateConst "data")
                            |> SynBinding.basic [ Ident.create "node" ] []
                        ]

            match propertyName with
            | SynExpr.Const (synConst, _) ->
                SynMatchClause.SynMatchClause (
                    SynPat.createConst synConst,
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
                SynMatchClause.create (SynPat.named "x") body
                |> SynMatchClause.withWhere (SynExpr.equals (SynExpr.createIdent "x") propertyName)
        )
        |> fun l ->
            l
            @ [
                let fail =
                    SynExpr.plus (SynExpr.CreateConst "Unrecognised 'type' field value: ") (SynExpr.createIdent "v")
                    |> SynExpr.paren
                    |> SynExpr.applyFunction (SynExpr.createIdent "failwith")

                SynMatchClause.SynMatchClause (
                    SynPat.named "v",
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
        |> SynExpr.createMatch (SynExpr.createIdent "ty")
        |> SynExpr.createLet
            [
                let property = SynExpr.CreateConst "type"

                SynExpr.createIdent "node"
                |> SynExpr.index property
                |> assertNotNull property
                |> SynExpr.pipeThroughFunction (
                    SynExpr.createLambda "v" (SynExpr.callGenericMethod' "GetValue" "string" (SynExpr.createIdent "v"))
                )
                |> SynBinding.basic [ Ident.create "ty" ] []
            ]

    let createEnumMaker
        (spec : JsonParseOutputSpec)
        (typeName : LongIdent)
        (fields : (Ident * SynExpr) list)
        : SynExpr
        =
        let numberKind =
            [ "System" ; "Text" ; "Json" ; "JsonValueKind" ; "Number" ]
            |> List.map Ident.create

        let stringKind =
            [ "System" ; "Text" ; "Json" ; "JsonValueKind" ; "String" ]
            |> List.map Ident.create

        let fail =
            SynExpr.plus
                (SynExpr.CreateConst "Unrecognised kind for enum of type: ")
                (SynExpr.CreateConst (typeName |> List.map _.idText |> String.concat "."))
            |> SynExpr.paren
            |> SynExpr.applyFunction (SynExpr.createIdent "failwith")

        let failString =
            SynExpr.plus (SynExpr.CreateConst "Unrecognised value for enum: %i") (SynExpr.createIdent "v")
            |> SynExpr.paren
            |> SynExpr.applyFunction (SynExpr.createIdent "failwith")

        let parseString =
            fields
            |> List.map (fun (ident, _) ->
                SynMatchClause.create
                    (SynPat.createConst (
                        SynConst.String (ident.idText.ToLowerInvariant (), SynStringKind.Regular, range0)
                    ))
                    (SynExpr.createLongIdent' (typeName @ [ ident ]))
            )
            |> fun l -> l @ [ SynMatchClause.create (SynPat.named "v") failString ]
            |> SynExpr.createMatch (
                asValueGetValue None "string" (SynExpr.createIdent "node")
                |> SynExpr.callMethod "ToLowerInvariant"
            )

        [
            SynMatchClause.create
                (SynPat.identWithArgs numberKind (SynArgPats.create []))
                (asValueGetValue None "int" (SynExpr.createIdent "node")
                 |> SynExpr.pipeThroughFunction (
                     SynExpr.typeApp [ SynType.createLongIdent typeName ] (SynExpr.createIdent "enum")
                 ))
            SynMatchClause.create (SynPat.identWithArgs stringKind (SynArgPats.create [])) parseString
            SynMatchClause.create (SynPat.named "_") fail
        ]
        |> SynExpr.createMatch (SynExpr.callMethod "GetValueKind" (SynExpr.createIdent "node"))

    let createModule (namespaceId : LongIdent) (spec : JsonParseOutputSpec) (typeDefn : SynTypeDefn) =
        let (SynTypeDefn (synComponentInfo, synTypeDefnRepr, _members, _implicitCtor, _, _)) =
            typeDefn

        let (SynComponentInfo (_attributes, _typeParams, _constraints, ident, _, _preferPostfix, access, _)) =
            synComponentInfo

        let attributes =
            if spec.ExtensionMethods then
                [ SynAttribute.autoOpen ]
            else
                [ SynAttribute.requireQualifiedAccess ; SynAttribute.compilationRepresentation ]

        let xmlDoc =
            let fullyQualified = ident |> Seq.map (fun i -> i.idText) |> String.concat "."

            let description =
                if spec.ExtensionMethods then
                    "extension members"
                else
                    "methods"

            $"Module containing JSON parsing %s{description} for the %s{fullyQualified} type"
            |> PreXmlDoc.create

        let moduleName =
            if spec.ExtensionMethods then
                match ident with
                | [] -> failwith "unexpectedly got an empty identifier for record name"
                | ident ->
                    let expanded =
                        List.last ident
                        |> fun i -> i.idText
                        |> fun s -> s + "JsonParseExtension"
                        |> Ident.create

                    List.take (List.length ident - 1) ident @ [ expanded ]
            else
                ident

        let info =
            SynComponentInfo.createLong moduleName
            |> SynComponentInfo.withDocString xmlDoc
            |> SynComponentInfo.setAccessibility access
            |> SynComponentInfo.addAttributes attributes

        let decl =
            match synTypeDefnRepr with
            | SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.Record (_accessibility, fields, _range), _) ->
                fields |> List.map SynField.extractWithIdent |> createRecordMaker spec
            | SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.Union (_accessibility, cases, _range), _) ->
                let optionGet (i : Ident option) =
                    match i with
                    | None -> failwith "WoofWare.Myriad requires union cases to have identifiers on each field."
                    | Some i -> i

                cases
                |> List.map UnionCase.ofSynUnionCase
                |> List.map (UnionCase.mapIdentFields optionGet)
                |> createUnionMaker spec ident
            | SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.Enum (cases, _range), _) ->
                cases
                |> List.map (fun c ->
                    match c with
                    | SynEnumCase.SynEnumCase (_, SynIdent.SynIdent (ident, _), value, _, _, _) -> ident, value
                )
                |> createEnumMaker spec ident
            | _ -> failwithf "Not a record or union type"

        [ scaffolding spec ident decl ]
        |> SynModuleDecl.nestedModule info
        |> List.singleton
        |> SynModuleOrNamespace.createNamespace namespaceId

open Myriad.Core

/// Myriad generator that provides a method (possibly an extension method) for a record type,
/// containing a JSON parse function.
[<MyriadGenerator("json-parse")>]
type JsonParseGenerator () =

    interface IMyriadGenerator with
        member _.ValidInputExtensions = [ ".fs" ]

        member _.Generate (context : GeneratorContext) =
            let targetedTypes =
                MyriadParamParser.render context.AdditionalParameters
                |> Map.map (fun _ v -> v.Split '!' |> Array.toList |> List.map DesiredGenerator.Parse)

            let ast, _ =
                Ast.fromFilename context.InputFilename |> Async.RunSynchronously |> Array.head

            let relevantTypes =
                Ast.extractTypeDefn ast
                |> List.map (fun (name, defns) ->
                    defns
                    |> List.choose (fun defn ->
                        if Ast.isRecord defn then Some defn
                        elif Ast.isDu defn then Some defn
                        elif AstHelper.isEnum defn then Some defn
                        else None
                    )
                    |> fun defns -> name, defns
                )

            let namespaceAndTypes =
                relevantTypes
                |> List.choose (fun (ns, types) ->
                    types
                    |> List.choose (fun typeDef ->
                        match SynTypeDefn.getAttribute typeof<JsonParseAttribute>.Name typeDef with
                        | None ->
                            let name = SynTypeDefn.getName typeDef |> List.map _.idText |> String.concat "."

                            match Map.tryFind name targetedTypes with
                            | Some desired ->
                                desired
                                |> List.tryPick (fun generator ->
                                    match generator with
                                    | DesiredGenerator.JsonParse arg ->
                                        let spec =
                                            {
                                                ExtensionMethods =
                                                    arg
                                                    |> Option.defaultValue
                                                        JsonParseAttribute.DefaultIsExtensionMethod
                                            }

                                        Some (typeDef, spec)
                                    | _ -> None
                                )
                            | _ -> None

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
