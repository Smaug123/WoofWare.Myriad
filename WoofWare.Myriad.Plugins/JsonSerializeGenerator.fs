namespace WoofWare.Myriad.Plugins

open System
open System.Text
open Fantomas.FCS.Syntax
open Fantomas.FCS.SyntaxTrivia
open Fantomas.FCS.Xml
open Myriad.Core

/// Attribute indicating a record type to which the "Add JSON serializer" Myriad
/// generator should apply during build.
/// The purpose of this generator is to create methods (possibly extension methods) of the form
/// `{TypeName}.toJsonNode : {TypeName} -> System.Text.Json.Nodes.JsonNode`.
///
/// If you supply isExtensionMethod = true, you will get extension methods.
/// These can only be consumed from F#, but the benefit is that they don't use up the module name
/// (since by default we create a module called "{TypeName}").
type JsonSerializeAttribute (isExtensionMethod : bool) =
    inherit Attribute ()

    /// If changing this, *adjust the documentation strings*
    static member internal DefaultIsExtensionMethod = false

    /// Shorthand for the "isExtensionMethod = false" constructor; see documentation there for details.
    new () = JsonSerializeAttribute JsonSerializeAttribute.DefaultIsExtensionMethod

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
        | Uri ->
            // JsonValue.Create<{type}>
            SynExpr.TypeApp (
                SynExpr.CreateLongIdent (
                    SynLongIdent.Create [ "System" ; "Text" ; "Json" ; "Nodes" ; "JsonValue" ; "Create" ]
                ),
                range0,
                [ fieldType ],
                [],
                Some range0,
                range0,
                range0
            )
        | OptionType ty ->
            // fun field -> match field with | None -> JsonValue.Create null | Some v -> {serializeNode ty} field
            SynExpr.CreateMatch (
                SynExpr.CreateIdentString "field",
                [
                    SynMatchClause.Create (
                        SynPat.CreateLongIdent (SynLongIdent.CreateString "None", []),
                        None,
                        SynExpr.CreateApp (
                            SynExpr.CreateLongIdent (
                                SynLongIdent.Create [ "System" ; "Text" ; "Json" ; "Nodes" ; "JsonValue" ; "Create" ]
                            ),
                            SynExpr.CreateNull
                        )
                    )

                    SynMatchClause.Create (
                        SynPat.CreateLongIdent (
                            SynLongIdent.CreateString "Some",
                            [ SynPat.CreateNamed (Ident.Create "field") ]
                        ),
                        None,
                        SynExpr.CreateApp (serializeNode ty, SynExpr.CreateIdentString "field")
                    )
                ]
            )
            |> SynExpr.createLambda "field"
        | ArrayType ty
        | ListType ty ->
            // fun field ->
            //     let arr = JsonArray ()
            //     for mem in field do arr.Add ({serializeNode} mem)
            //     arr
            SynExpr.LetOrUse (
                false,
                false,
                [
                    SynBinding.Let (
                        pattern = SynPat.CreateNamed (Ident.Create "arr"),
                        expr =
                            SynExpr.CreateApp (
                                SynExpr.CreateLongIdent (
                                    SynLongIdent.Create [ "System" ; "Text" ; "Json" ; "Nodes" ; "JsonArray" ]
                                ),
                                SynExpr.CreateConst SynConst.Unit
                            )
                    )
                ],
                SynExpr.CreateSequential
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
                                SynExpr.CreateParen (
                                    SynExpr.CreateApp (serializeNode ty, SynExpr.CreateIdentString "mem")
                                )
                            ),
                            range0
                        )
                        SynExpr.CreateIdentString "arr"
                    ],
                range0,
                {
                    InKeyword = None
                }
            )
            |> SynExpr.createLambda "field"
        | IDictionaryType (keyType, valueType)
        | DictionaryType (keyType, valueType)
        | IReadOnlyDictionaryType (keyType, valueType)
        | MapType (keyType, valueType) ->
            // fun field ->
            //    let ret = JsonObject ()
            //    for (KeyValue(key, value)) in field do
            //        ret.Add (key.ToString (), {serializeNode} value)
            //    ret
            SynExpr.LetOrUse (
                false,
                false,
                [
                    SynBinding.Let (
                        pattern = SynPat.CreateNamed (Ident.Create "ret"),
                        expr =
                            SynExpr.CreateApp (
                                SynExpr.CreateLongIdent (
                                    SynLongIdent.Create [ "System" ; "Text" ; "Json" ; "Nodes" ; "JsonObject" ]
                                ),
                                SynExpr.CreateConst SynConst.Unit
                            )
                    )
                ],
                SynExpr.CreateSequential
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
                                SynExpr.CreateLongIdent (SynLongIdent.Create [ "ret" ; "Add" ]),
                                SynExpr.CreateParenedTuple
                                    [
                                        SynExpr.CreateApp (
                                            SynExpr.CreateLongIdent (SynLongIdent.Create [ "key" ; "ToString" ]),
                                            SynExpr.CreateConst SynConst.Unit
                                        )
                                        SynExpr.CreateApp (serializeNode valueType, SynExpr.CreateIdentString "value")
                                    ]
                            ),
                            range0
                        )
                        SynExpr.CreateIdentString "ret"
                    ],
                range0,
                {
                    InKeyword = None
                }
            )
            |> SynExpr.createLambda "field"
        | _ ->
            // {type}.toJsonNode
            let typeName =
                match fieldType with
                | SynType.LongIdent ident -> ident.LongIdent
                | _ -> failwith $"Unrecognised type: %+A{fieldType}"

            SynExpr.CreateLongIdent (SynLongIdent.CreateFromLongIdent (typeName @ [ Ident.Create "toJsonNode" ]))

    /// propertyName is probably a string literal, but it could be a [<Literal>] variable
    /// `node.Add ({propertyName}, {toJsonNode})`
    let createSerializeRhs (propertyName : SynExpr) (fieldId : Ident) (fieldType : SynType) : SynExpr =
        let func = SynExpr.CreateLongIdent (SynLongIdent.Create [ "node" ; "Add" ])

        let args =
            SynExpr.CreateParenedTuple
                [
                    propertyName
                    SynExpr.CreateApp (
                        serializeNode fieldType,
                        SynExpr.CreateLongIdent (SynLongIdent.CreateFromLongIdent [ Ident.Create "input" ; fieldId ])
                    )
                ]

        SynExpr.CreateApp (func, args)

    let createMaker (spec : JsonSerializeOutputSpec) (typeName : LongIdent) (fields : SynField list) =
        let xmlDoc = PreXmlDoc.Create " Serialize to a JSON node"

        let returnInfo =
            SynBindingReturnInfo.Create (
                SynType.LongIdent (SynLongIdent.Create [ "System" ; "Text" ; "Json" ; "Nodes" ; "JsonNode" ])
            )

        let inputArg = Ident.Create "input"
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

            let thisIdOpt = if spec.ExtensionMethods then None else Some inputArg

            SynValData.SynValData (
                memberFlags,
                SynValInfo.SynValInfo ([ [ SynArgInfo.CreateId functionName ] ], SynArgInfo.Empty),
                thisIdOpt
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

                createSerializeRhs propertyName id fieldType
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
            |> AstHelper.instantiateRecord

        let assignments = assignments |> SynExpr.CreateSequential

        let assignments =
            SynExpr.LetOrUse (
                false,
                false,
                [
                    SynBinding.Let (
                        pattern = SynPat.CreateNamed (Ident.Create "node"),
                        expr =
                            SynExpr.CreateApp (
                                SynExpr.CreateLongIdent (
                                    SynLongIdent.Create [ "System" ; "Text" ; "Json" ; "Nodes" ; "JsonObject" ]
                                ),
                                SynExpr.CreateConst SynConst.Unit
                            )
                    )
                ],
                SynExpr.CreateSequential
                    [
                        SynExpr.Do (assignments, range0)
                        SynExpr.Upcast (SynExpr.CreateIdentString "node", SynType.Anon range0, range0)
                    ],
                range0,
                {
                    InKeyword = None
                }
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

    let createRecordModule
        (namespaceId : LongIdent)
        (opens : SynOpenDeclTarget list)
        (spec : JsonSerializeOutputSpec)
        (typeDefn : SynTypeDefn)
        =
        let (SynTypeDefn (synComponentInfo, synTypeDefnRepr, _members, _implicitCtor, _, _)) =
            typeDefn

        let (SynComponentInfo (_attributes, _typeParams, _constraints, recordId, _, _preferPostfix, _access, _)) =
            synComponentInfo

        match synTypeDefnRepr with
        | SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.Record (_accessibility, recordFields, _recordRange), _) ->

            let decls = [ createMaker spec recordId recordFields ]

            let attributes =
                if spec.ExtensionMethods then
                    [ SynAttributeList.Create SynAttribute.autoOpen ]
                else
                    [
                        SynAttributeList.Create (SynAttribute.RequireQualifiedAccess ())
                        SynAttributeList.Create SynAttribute.compilationRepresentation
                    ]

            let xmlDoc =
                let fullyQualified = recordId |> Seq.map (fun i -> i.idText) |> String.concat "."

                let description =
                    if spec.ExtensionMethods then
                        "extension members"
                    else
                        "methods"

                $" Module containing JSON serializing %s{description} for the %s{fullyQualified} type"
                |> PreXmlDoc.Create

            let moduleName =
                if spec.ExtensionMethods then
                    match recordId with
                    | [] -> failwith "unexpectedly got an empty identifier for record name"
                    | recordId ->
                        let expanded =
                            List.last recordId
                            |> fun i -> i.idText
                            |> fun s -> s + "JsonSerializeExtension"
                            |> Ident.Create

                        List.take (List.length recordId - 1) recordId @ [ expanded ]
                else
                    recordId

            let info =
                SynComponentInfo.Create (moduleName, attributes = attributes, xmldoc = xmlDoc)

            let mdl = SynModuleDecl.CreateNestedModule (info, decls)

            SynModuleOrNamespace.CreateNamespace (
                namespaceId,
                decls = (opens |> List.map SynModuleDecl.CreateOpen) @ [ mdl ]
            )
        | _ -> failwithf "Not a record type"

/// Myriad generator that provides a method (possibly an extension method) for a record type,
/// containing a JSON serialization function.
[<MyriadGenerator("json-serialize")>]
type JsonSerializeGenerator () =

    interface IMyriadGenerator with
        member _.ValidInputExtensions = [ ".fs" ]

        member _.Generate (context : GeneratorContext) =
            let ast, _ =
                Ast.fromFilename context.InputFilename |> Async.RunSynchronously |> Array.head

            let records = Ast.extractRecords ast

            let namespaceAndRecords =
                records
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
                namespaceAndRecords
                |> List.collect (fun (ns, records) ->
                    records
                    |> List.map (fun (record, spec) ->
                        let recordModule = JsonSerializeGenerator.createRecordModule ns opens spec record
                        recordModule
                    )
                )

            Output.Ast modules
