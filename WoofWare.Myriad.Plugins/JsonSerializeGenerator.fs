namespace WoofWare.Myriad.Plugins

open System
open System.Text
open Fantomas.FCS.Syntax
open Fantomas.FCS.SyntaxTrivia
open Fantomas.FCS.Xml
open Myriad.Core

type internal JsonSerializeOutputSpec =
    {
        ExtensionMethods : bool
    }

[<RequireQualifiedAccess>]
module internal JsonSerializeGenerator =
    open Fantomas.FCS.Text.Range
    open Myriad.Core.Ast

    /// Given `input.Ident`, for example, choose how to add it to the ambient `node`.
    /// The result is a line like `(fun ident -> InnerType.toJsonNode ident)` or `(fun ident -> JsonValue.Create ident)`.
    let rec serializeNode (fieldType : SynType) : SynExpr =
        // TODO: serialization format for DateTime etc
        match fieldType with
        | DateOnly
        | DateTime
        | NumberType _
        | PrimitiveType _
        | Guid
        | Uri ->
            // JsonValue.Create<type>
            SynExpr.TypeApp (
                SynExpr.createLongIdent [ "System" ; "Text" ; "Json" ; "Nodes" ; "JsonValue" ; "Create" ],
                range0,
                [ fieldType ],
                [],
                Some range0,
                range0,
                range0
            )
        | OptionType ty ->
            // fun field -> match field with | None -> JsonValue.Create null | Some v -> {serializeNode ty} field
            [
                SynMatchClause.Create (
                    SynPat.CreateLongIdent (SynLongIdent.CreateString "None", []),
                    None,
                    // The absolutely galaxy-brained implementation of JsonValue has `JsonValue.Parse "null"`
                    // identically equal to null. We have to work around this later, but we might as well just
                    // be efficient here and whip up the null directly.
                    SynExpr.CreateNull
                    |> SynExpr.upcast' (
                        SynType.CreateLongIdent (
                            SynLongIdent.Create [ "System" ; "Text" ; "Json" ; "Nodes" ; "JsonNode" ]
                        )
                    )
                )

                SynMatchClause.Create (
                    SynPat.CreateLongIdent (
                        SynLongIdent.CreateString "Some",
                        [ SynPat.CreateNamed (Ident.Create "field") ]
                    ),
                    None,
                    SynExpr.CreateApp (serializeNode ty, SynExpr.CreateIdentString "field")
                    |> SynExpr.CreateParen
                    |> SynExpr.upcast' (
                        SynType.CreateLongIdent (
                            SynLongIdent.Create [ "System" ; "Text" ; "Json" ; "Nodes" ; "JsonNode" ]
                        )
                    )
                )
            ]
            |> SynExpr.createMatch (SynExpr.CreateIdentString "field")
            |> SynExpr.createLambda "field"
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
                    SynPat.CreateNamed (Ident.Create "mem"),
                    SynExpr.CreateIdent (Ident.Create "field"),
                    SynExpr.CreateApp (
                        SynExpr.CreateLongIdent (SynLongIdent.Create [ "arr" ; "Add" ]),
                        SynExpr.CreateParen (SynExpr.CreateApp (serializeNode ty, SynExpr.CreateIdentString "mem"))
                    ),
                    range0
                )
                SynExpr.CreateIdentString "arr"
            ]
            |> SynExpr.CreateSequential
            |> SynExpr.createLet
                [
                    SynBinding.Let (
                        pattern = SynPat.CreateNamed (Ident.Create "arr"),
                        expr =
                            SynExpr.CreateApp (
                                SynExpr.createLongIdent [ "System" ; "Text" ; "Json" ; "Nodes" ; "JsonArray" ],
                                SynExpr.CreateConst SynConst.Unit
                            )
                    )
                ]
            |> SynExpr.createLambda "field"
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
                    SynPat.CreateParen (
                        SynPat.CreateLongIdent (
                            SynLongIdent.CreateString "KeyValue",
                            [
                                SynPat.CreateParen (
                                    SynPat.Tuple (
                                        false,
                                        [
                                            SynPat.CreateNamed (Ident.Create "key")
                                            SynPat.CreateNamed (Ident.Create "value")
                                        ],
                                        [ range0 ],
                                        range0
                                    )
                                )
                            ]
                        )
                    ),
                    SynExpr.CreateIdent (Ident.Create "field"),
                    SynExpr.CreateApp (
                        SynExpr.createLongIdent [ "ret" ; "Add" ],
                        SynExpr.CreateParenedTuple
                            [
                                SynExpr.CreateApp (
                                    SynExpr.createLongIdent [ "key" ; "ToString" ],
                                    SynExpr.CreateConst SynConst.Unit
                                )
                                SynExpr.CreateApp (serializeNode valueType, SynExpr.CreateIdentString "value")
                            ]
                    ),
                    range0
                )
                SynExpr.CreateIdentString "ret"
            ]
            |> SynExpr.CreateSequential
            |> SynExpr.createLet
                [
                    SynBinding.Let (
                        pattern = SynPat.CreateNamed (Ident.Create "ret"),
                        expr =
                            SynExpr.CreateApp (
                                SynExpr.createLongIdent [ "System" ; "Text" ; "Json" ; "Nodes" ; "JsonObject" ],
                                SynExpr.CreateConst SynConst.Unit
                            )
                    )
                ]
            |> SynExpr.createLambda "field"
        | _ ->
            // {type}.toJsonNode
            let typeName =
                match fieldType with
                | SynType.LongIdent ident -> ident.LongIdent
                | _ -> failwith $"Unrecognised type: %+A{fieldType}"

            SynExpr.createLongIdent' (typeName @ [ Ident.Create "toJsonNode" ])

    /// propertyName is probably a string literal, but it could be a [<Literal>] variable
    /// `node.Add ({propertyName}, {toJsonNode})`
    let createSerializeRhsRecord (propertyName : SynExpr) (fieldId : Ident) (fieldType : SynType) : SynExpr =
        [
            propertyName
            SynExpr.CreateApp (serializeNode fieldType, SynExpr.createLongIdent' [ Ident.Create "input" ; fieldId ])
        ]
        |> SynExpr.CreateParenedTuple
        |> SynExpr.applyFunction (SynExpr.createLongIdent [ "node" ; "Add" ])

    let getPropertyName (fieldId : Ident) (attrs : SynAttribute list) : SynExpr =
        let propertyNameAttr =
            attrs
            |> List.tryFind (fun attr -> attr.TypeName.AsString.EndsWith ("JsonPropertyName", StringComparison.Ordinal))

        match propertyNameAttr with
        | None ->
            let sb = StringBuilder fieldId.idText.Length
            sb.Append (Char.ToLowerInvariant fieldId.idText.[0]) |> ignore

            if fieldId.idText.Length > 1 then
                sb.Append fieldId.idText.[1..] |> ignore

            sb.ToString () |> SynConst.CreateString |> SynExpr.CreateConst
        | Some name -> name.ArgExpr

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
        let xmlDoc = PreXmlDoc.Create " Serialize to a JSON node"

        let returnInfo =
            SynBindingReturnInfo.Create (
                SynType.LongIdent (SynLongIdent.Create [ "System" ; "Text" ; "Json" ; "Nodes" ; "JsonNode" ])
            )

        let functionName = Ident.Create "toJsonNode"

        let inputVal =
            let memberFlags =
                if spec.ExtensionMethods then
                    {
                        SynMemberFlags.IsInstance = false
                        SynMemberFlags.IsDispatchSlot = false
                        SynMemberFlags.IsOverrideOrExplicitImpl = false
                        SynMemberFlags.IsFinal = false
                        SynMemberFlags.GetterOrSetterIsCompilerGenerated = false
                        SynMemberFlags.MemberKind = SynMemberKind.Member
                    }
                    |> Some
                else
                    None

            let thisIdOpt = if spec.ExtensionMethods then None else Some inputArgName

            SynValData.SynValData (
                memberFlags,
                SynValInfo.SynValInfo ([ [ SynArgInfo.CreateId functionName ] ], SynArgInfo.Empty),
                thisIdOpt
            )

        let assignments =
            [
                populateNode
                SynExpr.Upcast (SynExpr.CreateIdentString "node", SynType.Anon range0, range0)
            ]
            |> SynExpr.CreateSequential
            |> SynExpr.createLet
                [
                    SynBinding.Let (
                        pattern = SynPat.CreateNamed (Ident.Create "node"),
                        expr =
                            SynExpr.CreateApp (
                                SynExpr.createLongIdent [ "System" ; "Text" ; "Json" ; "Nodes" ; "JsonObject" ],
                                SynExpr.CreateConst SynConst.Unit
                            )
                    )
                ]

        let pattern =
            SynPat.LongIdent (
                SynLongIdent.CreateFromLongIdent [ functionName ],
                None,
                None,
                SynArgPats.Pats
                    [
                        SynPat.CreateTyped (
                            SynPat.CreateNamed inputArgName,
                            SynType.LongIdent (SynLongIdent.CreateFromLongIdent typeName)
                        )
                        |> SynPat.CreateParen
                    ],
                None,
                range0
            )

        if spec.ExtensionMethods then
            let binding =
                SynBinding.SynBinding (
                    None,
                    SynBindingKind.Normal,
                    false,
                    false,
                    [],
                    xmlDoc,
                    inputVal,
                    pattern,
                    Some returnInfo,
                    assignments,
                    range0,
                    DebugPointAtBinding.NoneAtInvisible,
                    {
                        LeadingKeyword = SynLeadingKeyword.StaticMember (range0, range0)
                        InlineKeyword = None
                        EqualsRange = Some range0
                    }
                )

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

    let recordModule (spec : JsonSerializeOutputSpec) (typeName : LongIdent) (fields : SynField list) =
        let inputArg = Ident.Create "input"
        let fields = fields |> List.map SynField.extractWithIdent

        fields
        |> List.map (fun fieldData ->
            let propertyName = getPropertyName fieldData.Ident fieldData.Attrs
            createSerializeRhsRecord propertyName fieldData.Ident fieldData.Type
        )
        |> SynExpr.CreateSequential
        |> fun expr -> SynExpr.Do (expr, range0)
        |> scaffolding spec typeName inputArg

    let unionModule (spec : JsonSerializeOutputSpec) (typeName : LongIdent) (cases : SynUnionCase list) =
        let inputArg = Ident.Create "input"
        let fields = cases |> List.map SynUnionCase.extract

        fields
        |> List.map (fun unionCase ->
            let propertyName = getPropertyName unionCase.Ident unionCase.Attrs

            let caseNames = unionCase.Fields |> List.mapi (fun i _ -> Ident.Create $"arg%i{i}")

            let argPats = SynArgPats.create caseNames

            let pattern =
                SynPat.LongIdent (
                    SynLongIdent.CreateFromLongIdent (typeName @ [ unionCase.Ident ]),
                    None,
                    None,
                    argPats,
                    None,
                    range0
                )

            let typeLine =
                [
                    SynExpr.CreateConstString "type"
                    SynExpr.CreateApp (
                        SynExpr.createLongIdent [ "System" ; "Text" ; "Json" ; "Nodes" ; "JsonValue" ; "Create" ],
                        propertyName
                    )
                ]
                |> SynExpr.CreateParenedTuple
                |> SynExpr.applyFunction (SynExpr.createLongIdent [ "node" ; "Add" ])

            let dataNode =
                SynBinding.Let (
                    pattern = SynPat.CreateNamed (Ident.Create "dataNode"),
                    expr =
                        SynExpr.CreateApp (
                            SynExpr.CreateLongIdent (
                                SynLongIdent.Create [ "System" ; "Text" ; "Json" ; "Nodes" ; "JsonObject" ]
                            ),
                            SynExpr.CreateConst SynConst.Unit
                        )
                )

            let dataBindings =
                (unionCase.Fields, caseNames)
                ||> List.zip
                |> List.map (fun (fieldData, caseName) ->
                    let propertyName = getPropertyName (Option.get fieldData.Ident) fieldData.Attrs

                    let node =
                        SynExpr.CreateApp (serializeNode fieldData.Type, SynExpr.CreateIdent caseName)

                    [ propertyName ; node ]
                    |> SynExpr.CreateParenedTuple
                    |> SynExpr.applyFunction (SynExpr.createLongIdent [ "dataNode" ; "Add" ])
                )

            let assignToNode =
                [ SynExpr.CreateConstString "data" ; SynExpr.CreateIdentString "dataNode" ]
                |> SynExpr.CreateParenedTuple
                |> SynExpr.applyFunction (SynExpr.createLongIdent [ "node" ; "Add" ])

            let dataNode =
                SynExpr.CreateSequential (dataBindings @ [ assignToNode ])
                |> SynExpr.createLet [ dataNode ]

            let action =
                [
                    yield typeLine
                    if not dataBindings.IsEmpty then
                        yield dataNode
                ]
                |> SynExpr.CreateSequential

            SynMatchClause.Create (pattern, None, action)
        )
        |> fun clauses -> SynExpr.CreateMatch (SynExpr.CreateIdent inputArg, clauses)
        |> scaffolding spec typeName inputArg

    let createModule
        (namespaceId : LongIdent)
        (opens : SynOpenDeclTarget list)
        (spec : JsonSerializeOutputSpec)
        (typeDefn : SynTypeDefn)
        =
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

            $" Module containing JSON serializing %s{description} for the %s{fullyQualified} type"
            |> PreXmlDoc.Create

        let moduleName =
            if spec.ExtensionMethods then
                match ident with
                | [] -> failwith "unexpectedly got an empty identifier for type name"
                | ident ->
                    let expanded =
                        List.last ident
                        |> fun i -> i.idText
                        |> fun s -> s + "JsonSerializeExtension"
                        |> Ident.Create

                    List.take (List.length ident - 1) ident @ [ expanded ]
            else
                ident

        let info =
            SynComponentInfo.Create (moduleName, attributes = attributes, xmldoc = xmlDoc)

        let decls =
            match synTypeDefnRepr with
            | SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.Record (_accessibility, recordFields, _range), _) ->
                [ recordModule spec ident recordFields ]
            | SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.Union (_accessibility, unionFields, _range), _) ->
                [ unionModule spec ident unionFields ]
            | _ -> failwithf "Only record types currently supported."

        let mdl = SynModuleDecl.CreateNestedModule (info, decls)

        SynModuleOrNamespace.CreateNamespace (
            namespaceId,
            decls = (opens |> List.map SynModuleDecl.CreateOpen) @ [ mdl ]
        )

/// Myriad generator that provides a method (possibly an extension method) for a record type,
/// containing a JSON serialization function.
[<MyriadGenerator("json-serialize")>]
type JsonSerializeGenerator () =

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
