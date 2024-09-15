namespace WoofWare.Myriad.Plugins

open System
open System.Text
open Fantomas.FCS.Syntax

type internal JsonSerializeOutputSpec =
    {
        ExtensionMethods : bool
    }

[<RequireQualifiedAccess>]
module internal JsonSerializeGenerator =
    open Fantomas.FCS.Text.Range


    // The absolutely galaxy-brained implementation of JsonValue has `JsonValue.Parse "null"`
    // identically equal to null. We have to work around this later, but we might as well just
    // be efficient here and whip up the null directly.
    let private jsonNull () =
        SynExpr.createNull ()
        |> SynExpr.upcast' (SynType.createLongIdent' [ "System" ; "Text" ; "Json" ; "Nodes" ; "JsonNode" ])

    /// Given `input.Ident`, for example, choose how to add it to the ambient `node`.
    /// The result is a line like `(fun ident -> InnerType.toJsonNode ident)` or `(fun ident -> JsonValue.Create ident)`.
    /// Returns also a bool which is true if the resulting SynExpr represents something of type JsonNode.
    let rec serializeNode (fieldType : SynType) : SynExpr * bool =
        // TODO: serialization format for DateTime etc
        match fieldType with
        | DateOnly
        | DateTime
        | NumberType _
        | Measure _
        | PrimitiveType _
        | Guid
        | Uri ->
            // JsonValue.Create<type>
            SynExpr.createLongIdent [ "System" ; "Text" ; "Json" ; "Nodes" ; "JsonValue" ; "Create" ]
            |> SynExpr.typeApp [ fieldType ]
            |> fun e -> e, false
        | DateTimeOffset ->
            // fun field -> field.ToString("o") |> JsonValue.Create<string>
            let create =
                SynExpr.createLongIdent [ "System" ; "Text" ; "Json" ; "Nodes" ; "JsonValue" ; "Create" ]
                |> SynExpr.typeApp [ SynType.named "string" ]

            SynExpr.createIdent "field"
            |> SynExpr.callMethodArg "ToString" (SynExpr.CreateConst "o")
            |> SynExpr.pipeThroughFunction create
            |> SynExpr.createLambda "field"
            |> fun e -> e, false
        | NullableType ty ->
            // fun field -> if field.HasValue then {serializeNode ty} field.Value else JsonValue.Create null
            let inner, innerIsJsonNode = serializeNode ty

            SynExpr.applyFunction inner (SynExpr.createLongIdent [ "field" ; "Value" ])
            |> SynExpr.upcast' (SynType.createLongIdent' [ "System" ; "Text" ; "Json" ; "Nodes" ; "JsonNode" ])
            |> SynExpr.ifThenElse (SynExpr.createLongIdent [ "field" ; "HasValue" ]) (jsonNull ())
            |> SynExpr.createLambda "field"
            |> fun e -> e, innerIsJsonNode
        | OptionType ty ->
            // fun field -> match field with | None -> JsonValue.Create null | Some v -> {serializeNode ty} field
            let noneClause = jsonNull () |> SynMatchClause.create (SynPat.named "None")

            let someClause =
                let inner, innerIsJsonNode = serializeNode ty
                let target = SynExpr.applyFunction inner (SynExpr.createIdent "field")

                if innerIsJsonNode then
                    target
                else
                    target
                    |> SynExpr.paren
                    |> SynExpr.upcast' (SynType.createLongIdent' [ "System" ; "Text" ; "Json" ; "Nodes" ; "JsonNode" ])
                |> SynMatchClause.create (SynPat.nameWithArgs "Some" [ SynPat.named "field" ])

            [ noneClause ; someClause ]
            |> SynExpr.createMatch (SynExpr.createIdent "field")
            |> SynExpr.createLambda "field"
            |> fun e -> e, true
        | ArrayType ty
        | ListType ty ->
            // fun field ->
            //     let arr = JsonArray ()
            //     for mem in field do arr.Add ({serializeNode} mem)
            //     arr
            [
                SynExpr.ForEach (
                    DebugPointAtFor.Yes range0,
                    DebugPointAtInOrTo.Yes range0,
                    SeqExprOnly.SeqExprOnly false,
                    true,
                    SynPat.named "mem",
                    SynExpr.createIdent "field",
                    SynExpr.applyFunction
                        (SynExpr.createLongIdent [ "arr" ; "Add" ])
                        (SynExpr.paren (SynExpr.applyFunction (fst (serializeNode ty)) (SynExpr.createIdent "mem"))),
                    range0
                )
                SynExpr.createIdent "arr"
            ]
            |> SynExpr.sequential
            |> SynExpr.createLet
                [
                    SynExpr.createLongIdent [ "System" ; "Text" ; "Json" ; "Nodes" ; "JsonArray" ]
                    |> SynExpr.applyTo (SynExpr.CreateConst ())
                    |> SynBinding.basic [ Ident.create "arr" ] []
                ]
            |> SynExpr.createLambda "field"
            |> fun e -> e, false
        | IDictionaryType (_keyType, valueType)
        | DictionaryType (_keyType, valueType)
        | IReadOnlyDictionaryType (_keyType, valueType)
        | MapType (_keyType, valueType) ->
            // fun field ->
            //    let ret = JsonObject ()
            //    for (KeyValue(key, value)) in field do
            //        ret.Add (key.ToString (), {serializeNode} value)
            //    ret
            [
                SynExpr.ForEach (
                    DebugPointAtFor.Yes range0,
                    DebugPointAtInOrTo.Yes range0,
                    SeqExprOnly.SeqExprOnly false,
                    true,
                    SynPat.paren (SynPat.nameWithArgs "KeyValue" [ SynPat.named "key" ; SynPat.named "value" ]),
                    SynExpr.createIdent "field",
                    SynExpr.applyFunction
                        (SynExpr.createLongIdent [ "ret" ; "Add" ])
                        (SynExpr.tuple
                            [
                                SynExpr.createLongIdent [ "key" ; "ToString" ]
                                |> SynExpr.applyTo (SynExpr.CreateConst ())
                                SynExpr.applyFunction (fst (serializeNode valueType)) (SynExpr.createIdent "value")
                            ]),
                    range0
                )
                SynExpr.createIdent "ret"
            ]
            |> SynExpr.sequential
            |> SynExpr.createLet
                [
                    SynExpr.createLongIdent [ "System" ; "Text" ; "Json" ; "Nodes" ; "JsonObject" ]
                    |> SynExpr.applyTo (SynExpr.CreateConst ())
                    |> SynBinding.basic [ Ident.create "ret" ] []
                ]
            |> SynExpr.createLambda "field"
            |> fun e -> e, false
        | JsonNode -> SynExpr.createIdent "id", true
        | Unit ->
            SynExpr.createLambda
                "value"
                (SynExpr.createLongIdent [ "System" ; "Text" ; "Json" ; "Nodes" ; "JsonObject" ]
                 |> SynExpr.applyTo (SynExpr.CreateConst ())),
            false
        | _ ->
            // {type}.toJsonNode
            let typeName =
                match fieldType with
                | SynType.LongIdent ident -> ident.LongIdent
                | _ -> failwith $"Unrecognised type: %+A{fieldType}"

            SynExpr.createLongIdent' (typeName @ [ Ident.create "toJsonNode" ]), true

    /// propertyName is probably a string literal, but it could be a [<Literal>] variable
    /// `node.Add ({propertyName}, {toJsonNode})`
    let createSerializeRhsRecord (propertyName : SynExpr) (fieldId : Ident) (fieldType : SynType) : SynExpr =
        [
            propertyName
            SynExpr.pipeThroughFunction
                (fst (serializeNode fieldType))
                (SynExpr.createLongIdent' [ Ident.create "input" ; fieldId ])
            |> SynExpr.paren
        ]
        |> SynExpr.tuple
        |> SynExpr.applyFunction (SynExpr.createLongIdent [ "node" ; "Add" ])

    let getPropertyName (fieldId : Ident) (attrs : SynAttribute list) : SynExpr =
        let propertyNameAttr =
            attrs
            |> List.tryFind (fun attr ->
                (SynLongIdent.toString attr.TypeName)
                    .EndsWith ("JsonPropertyName", StringComparison.Ordinal)
            )

        match propertyNameAttr with
        | None ->
            let sb = StringBuilder fieldId.idText.Length
            sb.Append (Char.ToLowerInvariant fieldId.idText.[0]) |> ignore

            if fieldId.idText.Length > 1 then
                sb.Append fieldId.idText.[1..] |> ignore

            sb.ToString () |> SynExpr.CreateConst
        | Some name -> name.ArgExpr

    let getIsJsonExtension (attrs : SynAttribute list) : bool =
        attrs
        |> List.tryFind (fun attr ->
            (SynLongIdent.toString attr.TypeName)
                .EndsWith ("JsonExtensionData", StringComparison.Ordinal)
        )
        |> Option.isSome

    /// `populateNode` will be inserted before we return the `node` variable.
    ///
    /// That is, we give you access to a `JsonObject` called `node`,
    /// and you have access to a variable `inputArgName` which is of type `typeName`.
    /// Your job is to provide a `populateNode` expression which has the side effect
    /// of mutating `node` to faithfully reflect the value of `inputArgName`.
    let scaffolding
        (spec : JsonSerializeOutputSpec)
        (typeName : LongIdent)
        (inputArgName : Ident)
        (populateNode : SynExpr)
        : SynModuleDecl
        =
        let xmlDoc = PreXmlDoc.create "Serialize to a JSON node"

        let returnInfo =
            SynLongIdent.createS' [ "System" ; "Text" ; "Json" ; "Nodes" ; "JsonNode" ]
            |> SynType.LongIdent

        let functionName = Ident.create "toJsonNode"

        let assignments =
            [
                populateNode
                SynExpr.Upcast (SynExpr.createIdent "node", SynType.Anon range0, range0)
            ]
            |> SynExpr.sequential
            |> SynExpr.createLet
                [
                    SynExpr.createLongIdent [ "System" ; "Text" ; "Json" ; "Nodes" ; "JsonObject" ]
                    |> SynExpr.applyTo (SynExpr.CreateConst ())
                    |> SynBinding.basic [ Ident.create "node" ] []
                ]

        let pattern =
            SynPat.namedI inputArgName
            |> SynPat.annotateType (SynType.LongIdent (SynLongIdent.create typeName))

        if spec.ExtensionMethods then
            let componentInfo =
                SynComponentInfo.createLong typeName
                |> SynComponentInfo.withDocString (PreXmlDoc.create "Extension methods for JSON parsing")

            let memberDef =
                assignments
                |> SynBinding.basic [ functionName ] [ pattern ]
                |> SynBinding.withXmlDoc xmlDoc
                |> SynBinding.withReturnAnnotation returnInfo
                |> SynMemberDefn.staticMember

            let containingType =
                SynTypeDefnRepr.augmentation ()
                |> SynTypeDefn.create componentInfo
                |> SynTypeDefn.withMemberDefns [ memberDef ]

            SynModuleDecl.Types ([ containingType ], range0)
        else
            assignments
            |> SynBinding.basic [ functionName ] [ pattern ]
            |> SynBinding.withReturnAnnotation returnInfo
            |> SynBinding.withXmlDoc xmlDoc
            |> SynModuleDecl.createLet

    let recordModule (spec : JsonSerializeOutputSpec) (_typeName : LongIdent) (fields : SynField list) =
        let fields = fields |> List.map SynField.extractWithIdent

        fields
        |> List.map (fun fieldData ->
            let propertyName = getPropertyName fieldData.Ident fieldData.Attrs
            let isJsonExtension = getIsJsonExtension fieldData.Attrs

            if isJsonExtension then
                let valType =
                    match fieldData.Type with
                    | DictionaryType (String, v) -> v
                    | _ -> failwith "Expected JsonExtensionData to be a Dictionary<string, something>"

                let serialise = fst (serializeNode valType)

                SynExpr.createIdent "node"
                |> SynExpr.callMethodArg
                    "Add"
                    (SynExpr.tuple
                        [
                            SynExpr.createIdent "key"
                            SynExpr.applyFunction serialise (SynExpr.createIdent "value")
                        ])
                |> SynExpr.createForEach
                    (SynPat.identWithArgs
                        [ Ident.create "KeyValue" ]
                        (SynArgPats.create [ SynPat.named "key" ; SynPat.named "value" ]))
                    (SynExpr.createLongIdent' [ Ident.create "input" ; fieldData.Ident ])
            else
                createSerializeRhsRecord propertyName fieldData.Ident fieldData.Type
        )
        |> SynExpr.sequential
        |> fun expr -> SynExpr.Do (expr, range0)

    let unionModule (spec : JsonSerializeOutputSpec) (typeName : LongIdent) (cases : SynUnionCase list) =
        let inputArg = Ident.create "input"
        let fields = cases |> List.map UnionCase.ofSynUnionCase

        fields
        |> List.map (fun unionCase ->
            let propertyName = getPropertyName unionCase.Name unionCase.Attributes

            let caseNames = unionCase.Fields |> List.mapi (fun i _ -> $"arg%i{i}")

            let argPats = SynArgPats.createNamed caseNames

            let pattern =
                SynPat.LongIdent (
                    SynLongIdent.create (typeName @ [ unionCase.Name ]),
                    None,
                    None,
                    argPats,
                    None,
                    range0
                )

            let typeLine =
                [
                    SynExpr.CreateConst "type"
                    SynExpr.applyFunction
                        (SynExpr.createLongIdent [ "System" ; "Text" ; "Json" ; "Nodes" ; "JsonValue" ; "Create" ])
                        propertyName
                ]
                |> SynExpr.tuple
                |> SynExpr.applyFunction (SynExpr.createLongIdent [ "node" ; "Add" ])

            let dataNode =
                SynExpr.createLongIdent [ "System" ; "Text" ; "Json" ; "Nodes" ; "JsonObject" ]
                |> SynExpr.applyTo (SynExpr.CreateConst ())
                |> SynBinding.basic [ Ident.create "dataNode" ] []

            let dataBindings =
                (unionCase.Fields, caseNames)
                ||> List.zip
                |> List.map (fun (fieldData, caseName) ->
                    let propertyName = getPropertyName (Option.get fieldData.Ident) fieldData.Attrs

                    let node =
                        SynExpr.applyFunction (fst (serializeNode fieldData.Type)) (SynExpr.createIdent caseName)

                    [ propertyName ; node ]
                    |> SynExpr.tuple
                    |> SynExpr.applyFunction (SynExpr.createLongIdent [ "dataNode" ; "Add" ])
                )

            let assignToNode =
                [ SynExpr.CreateConst "data" ; SynExpr.createIdent "dataNode" ]
                |> SynExpr.tuple
                |> SynExpr.applyFunction (SynExpr.createLongIdent [ "node" ; "Add" ])

            let dataNode =
                SynExpr.sequential (dataBindings @ [ assignToNode ])
                |> SynExpr.createLet [ dataNode ]

            let action =
                [
                    yield typeLine
                    if not dataBindings.IsEmpty then
                        yield dataNode
                ]
                |> SynExpr.sequential

            SynMatchClause.create pattern action
        )
        |> SynExpr.createMatch (SynExpr.createIdent' inputArg)

    let enumModule
        (spec : JsonSerializeOutputSpec)
        (typeName : LongIdent)
        (cases : (Ident * SynExpr) list)
        : SynModuleDecl
        =
        let fail =
            SynExpr.CreateConst "Unrecognised value for enum: %O"
            |> SynExpr.applyFunction (SynExpr.createIdent "sprintf")
            |> SynExpr.applyTo (SynExpr.createIdent "v")
            |> SynExpr.paren
            |> SynExpr.applyFunction (SynExpr.createIdent "failwith")

        let body =
            cases
            |> List.map (fun (caseName, value) ->
                value
                |> SynExpr.applyFunction (
                    SynExpr.createLongIdent [ "System" ; "Text" ; "Json" ; "Nodes" ; "JsonValue" ; "Create" ]
                )
                |> SynMatchClause.create (SynPat.identWithArgs (typeName @ [ caseName ]) (SynArgPats.create []))
            )
            |> fun l -> l @ [ SynMatchClause.create (SynPat.named "v") fail ]
            |> SynExpr.createMatch (SynExpr.createIdent "input")

        let xmlDoc = PreXmlDoc.create "Serialize to a JSON node"

        let returnInfo =
            SynLongIdent.createS' [ "System" ; "Text" ; "Json" ; "Nodes" ; "JsonNode" ]
            |> SynType.LongIdent

        let functionName = Ident.create "toJsonNode"

        let pattern =
            SynPat.named "input"
            |> SynPat.annotateType (SynType.LongIdent (SynLongIdent.create typeName))

        if spec.ExtensionMethods then
            let componentInfo =
                SynComponentInfo.createLong typeName
                |> SynComponentInfo.withDocString (PreXmlDoc.create "Extension methods for JSON parsing")

            let memberDef =
                body
                |> SynBinding.basic [ functionName ] [ pattern ]
                |> SynBinding.withXmlDoc xmlDoc
                |> SynBinding.withReturnAnnotation returnInfo
                |> SynMemberDefn.staticMember

            let containingType =
                SynTypeDefnRepr.augmentation ()
                |> SynTypeDefn.create componentInfo
                |> SynTypeDefn.withMemberDefns [ memberDef ]

            SynModuleDecl.Types ([ containingType ], range0)
        else
            body
            |> SynBinding.basic [ functionName ] [ pattern ]
            |> SynBinding.withReturnAnnotation returnInfo
            |> SynBinding.withXmlDoc xmlDoc
            |> SynModuleDecl.createLet

    let createModule
        (namespaceId : LongIdent)
        (opens : SynOpenDeclTarget list)
        (spec : JsonSerializeOutputSpec)
        (typeDefn : SynTypeDefn)
        =
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

            $"Module containing JSON serializing %s{description} for the %s{fullyQualified} type"
            |> PreXmlDoc.create

        let moduleName =
            if spec.ExtensionMethods then
                match ident with
                | [] -> failwith "unexpectedly got an empty identifier for type name"
                | ident ->
                    let expanded =
                        List.last ident
                        |> fun i -> i.idText
                        |> fun s -> s + "JsonSerializeExtension"
                        |> Ident.create

                    List.take (List.length ident - 1) ident @ [ expanded ]
            else
                ident

        let info =
            SynComponentInfo.createLong moduleName
            |> SynComponentInfo.addAttributes attributes
            |> SynComponentInfo.setAccessibility access
            |> SynComponentInfo.withDocString xmlDoc

        let decls =
            match synTypeDefnRepr with
            | SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.Record (_accessibility, recordFields, _range), _) ->
                recordModule spec ident recordFields
                |> scaffolding spec ident (Ident.create "input")
            | SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.Union (_accessibility, unionFields, _range), _) ->
                unionModule spec ident unionFields
                |> scaffolding spec ident (Ident.create "input")
            | SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.Enum (cases, _range), _) ->
                cases
                |> List.map (fun c ->
                    match c with
                    | SynEnumCase.SynEnumCase (_, SynIdent.SynIdent (ident, _), value, _, _, _) -> ident, value
                )
                |> enumModule spec ident
            | ty -> failwithf "Unsupported type: got %O" ty

        [
            yield! opens |> List.map SynModuleDecl.openAny
            yield decls |> List.singleton |> SynModuleDecl.nestedModule info
        ]
        |> SynModuleOrNamespace.createNamespace namespaceId

open Myriad.Core

/// Myriad generator that provides a method (possibly an extension method) for a record type,
/// containing a JSON serialization function.
[<MyriadGenerator("json-serialize")>]
type JsonSerializeGenerator () =

    interface IMyriadGenerator with
        member _.ValidInputExtensions = [ ".fs" ]

        member _.Generate (context : GeneratorContext) =
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
                        match Ast.getAttribute<JsonSerializeAttribute> typeDef with
                        | None -> None
                        | Some attr ->
                            let arg =
                                match SynExpr.stripOptionalParen attr.ArgExpr with
                                | SynExpr.Const (SynConst.Bool value, _) -> value
                                | SynExpr.Const (SynConst.Unit, _) -> JsonSerializeAttribute.DefaultIsExtensionMethod
                                | arg ->
                                    failwith
                                        $"Unrecognised argument %+A{arg} to [<%s{nameof JsonSerializeAttribute}>]. Literals are not supported. Use `true` or `false` (or unit) only."

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

            let opens = AstHelper.extractOpens ast

            let modules =
                namespaceAndTypes
                |> List.collect (fun (ns, types) ->
                    types
                    |> List.map (fun (ty, spec) -> JsonSerializeGenerator.createModule ns opens spec ty)
                )

            Output.Ast modules
