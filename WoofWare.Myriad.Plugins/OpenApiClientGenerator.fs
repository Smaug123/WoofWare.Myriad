namespace WoofWare.Myriad.Plugins

open System
open System.Collections.Generic
open System.Text.Json
open System.Text.Json.Nodes
open System.Text.RegularExpressions
open Fantomas.FCS.Syntax
open Fantomas.FCS.Text.Range
open Fantomas.FCS.Xml
open Myriad.Core
open WoofWare.Whippet.Fantomas

type internal OpenApiGenerationDiagnosticCode =
    | InvalidJson
    | InvalidDocument
    | UnsupportedVersion
    | UnresolvedReference
    | UnsupportedSchema
    | UnsupportedParameter
    | UnsupportedOperation
    | AmbiguousSuccessResponse

type internal OpenApiGenerationDiagnostic =
    {
        Code : OpenApiGenerationDiagnosticCode
        Location : string
        Message : string
    }

type internal OpenApiPrimitive =
    | String
    | Boolean
    | Int32
    | Int64
    | BigInteger
    | Float32
    | Float
    | Decimal
    | Date
    | DateTime
    | Guid

type internal OpenApiPlannedType =
    | Primitive of OpenApiPrimitive
    | Named of string
    | List of OpenApiPlannedType
    | Optional of OpenApiPlannedType
    | JsonNode
    | Stream
    | Unit

type internal OpenApiPlannedField =
    {
        JsonName : string
        FSharpName : string
        Type : OpenApiPlannedType
        Required : bool
    }

type internal OpenApiPlannedTypeDefinition =
    {
        SourceName : string
        FSharpName : string
        Description : string option
        Fields : OpenApiPlannedField list
        AdditionalProperties : OpenApiPlannedType option
    }

type internal OpenApiParameterLocation =
    | Path
    | Query
    | Body

type internal OpenApiPlannedParameter =
    {
        WireName : string
        FSharpName : string
        Location : OpenApiParameterLocation
        Type : OpenApiPlannedType
        Required : bool
    }

type internal OpenApiPlannedOperation =
    {
        OperationId : string
        FSharpName : string
        Description : string option
        Method : HttpMethod
        Path : string
        Parameters : OpenApiPlannedParameter list
        ReturnType : OpenApiPlannedType
        Accept : string option
        RequestContentType : string option
    }

type internal OpenApiServerBase =
    | BaseAddress of string
    | BasePath of string

type internal OpenApiClientPlan =
    {
        Namespace : string
        InterfaceName : string
        Description : string option
        CreateMock : bool option
        ServerBase : OpenApiServerBase
        Types : OpenApiPlannedTypeDefinition list
        Operations : OpenApiPlannedOperation list
    }

[<RequireQualifiedAccess>]
module internal OpenApiClientGenerator =

    type private LocatedObject =
        {
            Value : JsonObject
            Location : string
        }

    type private SchemaReference =
        {
            Name : string
            Location : string
        }

    type private CanonicalSchema =
        {
            Location : string
            Description : string option
            Nullable : bool
            Shape : CanonicalSchemaShape
        }

    and private CanonicalSchemaShape =
        | Reference of SchemaReference
        | Any
        | Primitive of OpenApiPrimitive
        | Array of CanonicalSchema option
        | Object of DirectObjectShape
        | AllOf of
            outerAllowsNull : bool *
            hasSiblingObjectKeywords : bool *
            allBranchesValid : bool *
            branches : CanonicalSchema list
        | UnsupportedNumber of format : string option
        | UnsupportedType of typeName : string
        | Invalid

    and private DirectObjectShape =
        {
            Properties : Map<string, CanonicalSchema option>
            Required : Set<string>
            AdditionalProperties : AdditionalProperties
        }

    and private AdditionalProperties =
        | Any
        | Forbidden
        | Typed of CanonicalSchema

    type private ObjectShape =
        {
            Description : string option
            Properties : Map<string, CanonicalSchema option>
            Required : Set<string>
            AdditionalProperties : AdditionalProperties
        }

    type private SchemaIdentity =
        | Json
        | Primitive of OpenApiPrimitive
        | Named of componentName : string
        | List of SchemaIdentity
        | Optional of SchemaIdentity
        | Object of ObjectShapeIdentity

    and private ObjectShapeIdentity =
        {
            Properties : Map<string, SchemaIdentity>
            Required : Set<string>
            AdditionalProperties : AdditionalPropertiesIdentity
        }

    and private AdditionalPropertiesIdentity =
        | Forbidden
        | Value of SchemaIdentity

    type private ResolvedParameterLocation =
        | Path
        | Query
        | Header
        | Cookie

    type private ResolvedParameter =
        {
            Name : string
            Location : ResolvedParameterLocation
            Required : bool
            Schema : CanonicalSchema
            SourceLocation : string
        }

    let private pointerToken (value : string) : string =
        let value = value.Replace ("~", "~0", StringComparison.Ordinal)
        value.Replace ("/", "~1", StringComparison.Ordinal)

    let private diagnostic
        (code : OpenApiGenerationDiagnosticCode)
        (location : string)
        (message : string)
        : OpenApiGenerationDiagnostic
        =
        {
            Code = code
            Location = location
            Message = message
        }

    let private normaliseParameters (parameters : Map<string, string>) : Map<string, string> =
        parameters
        |> Map.toSeq
        |> Seq.map (fun (key, value) -> key.ToUpperInvariant (), value)
        |> Map.ofSeq

    let private allocateUniqueName
        (used : HashSet<string>)
        (fallback : string)
        (sanitise : string -> string)
        (source : string)
        : string
        =
        let baseName =
            let result = sanitise source

            if String.IsNullOrWhiteSpace result then
                fallback
            else
                result

        let mutable suffix = 1
        let mutable candidate = baseName

        while not (used.Add candidate) do
            suffix <- suffix + 1
            candidate <- $"%s{baseName}%i{suffix}"

        candidate

    let private sanitiseTypeName (value : string) : string =
        (Ident.createSanitisedTypeName value).idText

    let private sanitiseParameterName (value : string) : string =
        (Ident.createSanitisedParamName value).idText

    let private report
        (diagnostics : ResizeArray<OpenApiGenerationDiagnostic>)
        (code : OpenApiGenerationDiagnosticCode)
        (location : string)
        (message : string)
        =
        diagnostics.Add (diagnostic code location message)

    let private tryProperty
        (diagnostics : ResizeArray<OpenApiGenerationDiagnostic>)
        (location : string)
        (node : JsonObject)
        (name : string)
        : JsonNode option
        =
        let propertyLocation = $"%s{location}/%s{pointerToken name}"

        match node.TryGetPropertyValue name with
        | false, _ -> None
        | true, value when isNull value ->
            report diagnostics InvalidDocument propertyLocation "An optional property cannot be null."
            None
        | true, value -> Some value

    let private tryString
        (diagnostics : ResizeArray<OpenApiGenerationDiagnostic>)
        (location : string)
        (node : JsonNode)
        : string option
        =
        try
            Some (node.GetValue<string> ())
        with
        | :? InvalidOperationException
        | :? FormatException ->
            report diagnostics InvalidDocument location "Expected a JSON string."
            None

    let private optionalString diagnostics location (node : JsonObject) name : string option =
        tryProperty diagnostics location node name
        |> Option.bind (tryString diagnostics ($"%s{location}/%s{pointerToken name}"))

    let private requiredString diagnostics location (node : JsonObject) name : string option =
        let propertyLocation = $"%s{location}/%s{pointerToken name}"

        match node.TryGetPropertyValue name with
        | false, _ ->
            report diagnostics InvalidDocument propertyLocation "A required string property is missing."
            None
        | true, value when isNull value ->
            report diagnostics InvalidDocument propertyLocation "A required string property cannot be null."
            None
        | true, value -> tryString diagnostics propertyLocation value

    let private tryBool
        (diagnostics : ResizeArray<OpenApiGenerationDiagnostic>)
        (location : string)
        (node : JsonNode)
        : bool option
        =
        try
            Some (node.GetValue<bool> ())
        with
        | :? InvalidOperationException
        | :? FormatException ->
            report diagnostics InvalidDocument location "Expected a JSON boolean."
            None

    let private optionalBool diagnostics location (node : JsonObject) name : bool option =
        tryProperty diagnostics location node name
        |> Option.bind (tryBool diagnostics ($"%s{location}/%s{pointerToken name}"))

    let private tryObject
        (diagnostics : ResizeArray<OpenApiGenerationDiagnostic>)
        (location : string)
        (node : JsonNode)
        : LocatedObject option
        =
        match node with
        | :? JsonObject as value ->
            Some
                {
                    Value = value
                    Location = location
                }
        | _ ->
            report diagnostics InvalidDocument location "Expected a JSON object."
            None

    let private optionalObject diagnostics location (node : JsonObject) name : LocatedObject option =
        tryProperty diagnostics location node name
        |> Option.bind (tryObject diagnostics ($"%s{location}/%s{pointerToken name}"))

    let private tryArray
        (diagnostics : ResizeArray<OpenApiGenerationDiagnostic>)
        (location : string)
        (node : JsonNode)
        : JsonArray option
        =
        match node with
        | :? JsonArray as value -> Some value
        | _ ->
            report diagnostics InvalidDocument location "Expected a JSON array."
            None

    let private optionalArray diagnostics location (node : JsonObject) name : JsonArray option =
        tryProperty diagnostics location node name
        |> Option.bind (tryArray diagnostics ($"%s{location}/%s{pointerToken name}"))

    let private objectMap diagnostics location (node : JsonObject) : Map<string, LocatedObject> =
        node
        |> Seq.choose (fun (KeyValue (name, value)) ->
            tryObject diagnostics ($"%s{location}/%s{pointerToken name}") value
            |> Option.map (fun value -> name, value)
        )
        |> Map.ofSeq

    let private componentMap diagnostics (components : LocatedObject option) name : Map<string, LocatedObject> =
        match
            components
            |> Option.bind (fun value -> optionalObject diagnostics value.Location value.Value name)
        with
        | None -> Map.empty
        | Some values -> objectMap diagnostics values.Location values.Value

    let private decodePointerToken diagnostics code location (value : string) : string option =
        let value = Uri.UnescapeDataString value

        if Regex.IsMatch (value, "~(?:[^01]|$)") then
            report diagnostics code location $"Reference token '%s{value}' contains an invalid JSON Pointer escape."
            None
        else
            let value = value.Replace ("~1", "/", StringComparison.Ordinal)
            value.Replace ("~0", "~", StringComparison.Ordinal) |> Some

    let private referenceName diagnostics code expectedPrefix referenceLocation (reference : string) : string option =
        if reference.StartsWith (expectedPrefix, StringComparison.Ordinal) then
            reference.Substring expectedPrefix.Length
            |> decodePointerToken diagnostics code referenceLocation
        else
            report
                diagnostics
                code
                referenceLocation
                $"Only local references below '%s{expectedPrefix}' are supported; got '%s{reference}'."

            None

    let rec private analyzeSchema
        (diagnostics : ResizeArray<OpenApiGenerationDiagnostic>)
        (cache : Dictionary<string, CanonicalSchema>)
        (schema : LocatedObject)
        : CanonicalSchema
        =
        match cache.TryGetValue schema.Location with
        | true, analyzed -> analyzed
        | false, _ ->
            let reference = optionalString diagnostics schema.Location schema.Value "$ref"

            let analyzed =
                match reference with
                | Some reference ->
                    let referenceLocation = $"%s{schema.Location}/$ref"

                    let shape =
                        referenceName
                            diagnostics
                            UnresolvedReference
                            "#/components/schemas/"
                            referenceLocation
                            reference
                        |> Option.map (fun name ->
                            CanonicalSchemaShape.Reference
                                {
                                    Name = name
                                    Location = referenceLocation
                                }
                        )
                        |> Option.defaultValue CanonicalSchemaShape.Invalid

                    {
                        Location = schema.Location
                        Description = None
                        Nullable = false
                        Shape = shape
                    }
                | None ->
                    let description =
                        optionalString diagnostics schema.Location schema.Value "description"

                    let nullable =
                        optionalBool diagnostics schema.Location schema.Value "nullable"
                        |> Option.defaultValue false

                    let typeName = optionalString diagnostics schema.Location schema.Value "type"
                    let format = optionalString diagnostics schema.Location schema.Value "format"

                    let unsupportedKeywords =
                        [ "oneOf" ; "anyOf" ; "not" ; "discriminator" ]
                        |> List.filter schema.Value.ContainsKey

                    if not unsupportedKeywords.IsEmpty then
                        let keywordList = String.concat ", " unsupportedKeywords

                        report
                            diagnostics
                            UnsupportedSchema
                            schema.Location
                            $"Unsupported shape-changing schema keyword(s): %s{keywordList}."

                    match
                        optionalBool diagnostics schema.Location schema.Value "readOnly",
                        optionalBool diagnostics schema.Location schema.Value "writeOnly"
                    with
                    | Some true, _
                    | _, Some true ->
                        report
                            diagnostics
                            UnsupportedSchema
                            schema.Location
                            "readOnly/writeOnly schemas require separate request and response projections."
                    | _ -> ()

                    let hasObjectKeywords =
                        schema.Value.ContainsKey "properties"
                        || schema.Value.ContainsKey "required"
                        || schema.Value.ContainsKey "additionalProperties"
                        || schema.Value.ContainsKey "allOf"

                    match typeName with
                    | Some value when value <> "object" && hasObjectKeywords ->
                        report
                            diagnostics
                            UnsupportedSchema
                            ($"%s{schema.Location}/type")
                            $"Schema type '%s{value}' contradicts its object-shape keywords."
                    | _ -> ()

                    let isObject = typeName = Some "object" || (typeName.IsNone && hasObjectKeywords)

                    // Parse every nested schema occurrence once, even if contradictory keywords mean that
                    // the occurrence cannot contribute to the generated type. Diagnostics must not depend on
                    // which planning branch happens to consume the canonical schema.
                    let properties : Map<string, CanonicalSchema option> =
                        match optionalObject diagnostics schema.Location schema.Value "properties" with
                        | None -> Map.empty
                        | Some properties ->
                            properties.Value
                            |> Seq.choose (fun (KeyValue (name, value)) ->
                                tryObject diagnostics ($"%s{properties.Location}/%s{pointerToken name}") value
                                |> Option.map (fun value -> name, Some (analyzeSchema diagnostics cache value))
                            )
                            |> Map.ofSeq

                    let required =
                        match optionalArray diagnostics schema.Location schema.Value "required" with
                        | None -> Set.empty
                        | Some values ->
                            values
                            |> Seq.mapi (fun index value ->
                                tryString diagnostics ($"%s{schema.Location}/required/%i{index}") value
                            )
                            |> Seq.choose id
                            |> Set.ofSeq

                    let additionalProperties : AdditionalProperties =
                        match tryProperty diagnostics schema.Location schema.Value "additionalProperties" with
                        | None -> AdditionalProperties.Any
                        | Some (:? JsonObject as value) ->
                            {
                                Value = value
                                Location = $"%s{schema.Location}/additionalProperties"
                            }
                            |> analyzeSchema diagnostics cache
                            |> AdditionalProperties.Typed
                        | Some value ->
                            match tryBool diagnostics ($"%s{schema.Location}/additionalProperties") value with
                            | Some true -> AdditionalProperties.Any
                            | Some false -> AdditionalProperties.Forbidden
                            | None -> AdditionalProperties.Any

                    let items =
                        optionalObject diagnostics schema.Location schema.Value "items"
                        |> Option.map (analyzeSchema diagnostics cache)

                    let allOf =
                        optionalArray diagnostics schema.Location schema.Value "allOf"
                        |> Option.map (fun values ->
                            let branches =
                                values
                                |> Seq.mapi (fun index value ->
                                    tryObject diagnostics ($"%s{schema.Location}/allOf/%i{index}") value
                                    |> Option.map (analyzeSchema diagnostics cache)
                                )
                                |> Seq.toList

                            branches, values.Count
                        )

                    let directObject : DirectObjectShape =
                        {
                            Properties = properties
                            Required = required
                            AdditionalProperties = additionalProperties
                        }

                    let shape =
                        if isObject && schema.Value.ContainsKey "allOf" then
                            match allOf with
                            | None -> CanonicalSchemaShape.Object directObject
                            | Some (branches, branchCount) ->
                                let parsedBranches = branches |> List.choose id
                                let allBranchesValid = parsedBranches.Length = branchCount

                                let hasSiblingObjectKeywords =
                                    schema.Value.ContainsKey "properties"
                                    || schema.Value.ContainsKey "required"
                                    || schema.Value.ContainsKey "additionalProperties"

                                CanonicalSchemaShape.AllOf (
                                    typeName.IsNone || nullable,
                                    hasSiblingObjectKeywords,
                                    allBranchesValid,
                                    parsedBranches
                                )
                        elif isObject then
                            CanonicalSchemaShape.Object directObject
                        else
                            match typeName with
                            | None -> CanonicalSchemaShape.Any
                            | Some "string" ->
                                match format with
                                | Some "date" -> CanonicalSchemaShape.Primitive OpenApiPrimitive.Date
                                | Some "date-time" -> CanonicalSchemaShape.Primitive OpenApiPrimitive.DateTime
                                | Some "uuid" -> CanonicalSchemaShape.Primitive OpenApiPrimitive.Guid
                                | _ -> CanonicalSchemaShape.Primitive OpenApiPrimitive.String
                            | Some "boolean" -> CanonicalSchemaShape.Primitive OpenApiPrimitive.Boolean
                            | Some "integer" ->
                                match format with
                                | Some "int32" -> CanonicalSchemaShape.Primitive OpenApiPrimitive.Int32
                                | Some "int64" -> CanonicalSchemaShape.Primitive OpenApiPrimitive.Int64
                                | _ -> CanonicalSchemaShape.Primitive OpenApiPrimitive.BigInteger
                            | Some "number" ->
                                match format with
                                | Some "float" -> CanonicalSchemaShape.Primitive OpenApiPrimitive.Float32
                                | Some "double" -> CanonicalSchemaShape.Primitive OpenApiPrimitive.Float
                                | Some "decimal" -> CanonicalSchemaShape.Primitive OpenApiPrimitive.Decimal
                                | format -> CanonicalSchemaShape.UnsupportedNumber format
                            | Some "array" -> CanonicalSchemaShape.Array items
                            | Some other -> CanonicalSchemaShape.UnsupportedType other

                    {
                        Location = schema.Location
                        Description = description
                        Nullable = nullable
                        Shape = shape
                    }

            cache.Add (schema.Location, analyzed)
            analyzed

    type private SchemaResolutionContext =
        {
            Diagnostics : ResizeArray<OpenApiGenerationDiagnostic>
            Components : Map<string, CanonicalSchema>
        }

    let private trySchemaTarget
        (context : SchemaResolutionContext)
        (reportMissing : bool)
        (reference : SchemaReference)
        : CanonicalSchema option
        =
        match Map.tryFind reference.Name context.Components with
        | Some target -> Some target
        | None ->
            if reportMissing then
                report
                    context.Diagnostics
                    UnresolvedReference
                    reference.Location
                    $"Schema component '%s{reference.Name}' does not exist."

            None

    let rec private resolveSchemaReferences
        (context : SchemaResolutionContext)
        (reportFailures : bool)
        (visited : Set<string>)
        (schema : CanonicalSchema)
        : (Set<string> * CanonicalSchema) option
        =
        match schema.Shape with
        | CanonicalSchemaShape.Reference reference when Set.contains reference.Name visited ->
            if reportFailures then
                report
                    context.Diagnostics
                    UnsupportedSchema
                    reference.Location
                    $"Schema reference cycle involving '%s{reference.Name}' is unsupported."

            None
        | CanonicalSchemaShape.Reference reference ->
            trySchemaTarget context reportFailures reference
            |> Option.bind (resolveSchemaReferences context reportFailures (Set.add reference.Name visited))
        | _ -> Some (visited, schema)

    let private schemaIsObjectLike (context : SchemaResolutionContext) (schema : CanonicalSchema) : bool =
        match resolveSchemaReferences context false Set.empty schema with
        | Some (_,
                {
                    Shape = CanonicalSchemaShape.Object _
                })
        | Some (_,
                {
                    Shape = CanonicalSchemaShape.AllOf _
                }) -> true
        | _ -> false

    let rec private schemaAllowsNullInner
        (context : SchemaResolutionContext)
        (visited : Set<string>)
        (schema : CanonicalSchema)
        : bool
        =
        match resolveSchemaReferences context false visited schema with
        | None -> false
        | Some (visited, resolved) ->
            match resolved.Shape with
            | CanonicalSchemaShape.Any -> true
            | CanonicalSchemaShape.AllOf (outerAllowsNull, _, allBranchesValid, branches) ->
                outerAllowsNull
                && allBranchesValid
                && (branches |> List.forall (schemaAllowsNullInner context visited))
            | _ -> resolved.Nullable

    let private schemaAllowsNull (context : SchemaResolutionContext) (schema : CanonicalSchema) : bool =
        schemaAllowsNullInner context Set.empty schema

    type private SchemaPlanningContext =
        {
            Resolution : SchemaResolutionContext
            UsedTypeNames : HashSet<string>
            ComponentTypeNames : Map<string, string>
            Definitions : ResizeArray<OpenApiPlannedTypeDefinition>
            LiftedObjectTypes : Dictionary<ObjectShapeIdentity, string>
        }

    let private emptyObjectShape description : ObjectShape =
        {
            Description = description
            Properties = Map.empty
            Required = Set.empty
            AdditionalProperties = AdditionalProperties.Any
        }

    let rec private typeForSchema
        (context : SchemaPlanningContext)
        (aliasStack : Set<string>)
        (suggestedName : string)
        (schema : CanonicalSchema)
        : OpenApiPlannedType
        =
        match schema.Shape with
        | CanonicalSchemaShape.Reference reference when Map.containsKey reference.Name context.ComponentTypeNames ->
            match trySchemaTarget context.Resolution true reference with
            | Some target ->
                let typeName = context.ComponentTypeNames.[reference.Name]
                let result = OpenApiPlannedType.Named typeName

                if schemaAllowsNull context.Resolution target then
                    OpenApiPlannedType.Optional result
                else
                    result
            | None -> OpenApiPlannedType.JsonNode
        | CanonicalSchemaShape.Reference _ ->
            match resolveSchemaReferences context.Resolution true aliasStack schema with
            | None -> OpenApiPlannedType.JsonNode
            | Some (aliasStack, target) -> typeForSchema context aliasStack suggestedName target
        | shape ->
            let baseType =
                match shape with
                | CanonicalSchemaShape.Any -> OpenApiPlannedType.JsonNode
                | CanonicalSchemaShape.Primitive primitive -> OpenApiPlannedType.Primitive primitive
                | CanonicalSchemaShape.Array None ->
                    report
                        context.Resolution.Diagnostics
                        UnsupportedSchema
                        ($"%s{schema.Location}/items")
                        "Array schemas must specify items."

                    OpenApiPlannedType.List OpenApiPlannedType.JsonNode
                | CanonicalSchemaShape.Array (Some items) ->
                    typeForSchema context aliasStack ($"%s{suggestedName}Item") items
                    |> OpenApiPlannedType.List
                | CanonicalSchemaShape.Object _
                | CanonicalSchemaShape.AllOf _ -> liftObject context suggestedName schema
                | CanonicalSchemaShape.UnsupportedNumber format ->
                    report
                        context.Resolution.Diagnostics
                        UnsupportedSchema
                        ($"%s{schema.Location}/format")
                        (match format with
                         | None -> "Unformatted JSON numbers have no lossless built-in F# representation."
                         | Some format -> $"Number format '%s{format}' has no lossless built-in F# representation.")

                    OpenApiPlannedType.JsonNode
                | CanonicalSchemaShape.UnsupportedType value ->
                    report
                        context.Resolution.Diagnostics
                        UnsupportedSchema
                        ($"%s{schema.Location}/type")
                        $"Schema type '%s{value}' is unsupported."

                    OpenApiPlannedType.JsonNode
                | CanonicalSchemaShape.Invalid
                | CanonicalSchemaShape.Reference _ -> OpenApiPlannedType.JsonNode

            if schemaAllowsNull context.Resolution schema then
                OpenApiPlannedType.Optional baseType
            else
                baseType

    and private liftObject
        (context : SchemaPlanningContext)
        (suggestedName : string)
        (schema : CanonicalSchema)
        : OpenApiPlannedType
        =
        let shape = collectObjectShape context Set.empty schema
        let identity = objectIdentity context shape

        match context.LiftedObjectTypes.TryGetValue identity with
        | true, typeName -> OpenApiPlannedType.Named typeName
        | false, _ ->
            let typeName =
                allocateUniqueName context.UsedTypeNames "AnonymousType" sanitiseTypeName suggestedName

            context.LiftedObjectTypes.Add (identity, typeName)

            buildDefinition context suggestedName schema.Location typeName shape
            |> context.Definitions.Add

            OpenApiPlannedType.Named typeName

    and private buildDefinition
        (context : SchemaPlanningContext)
        sourceName
        sourceLocation
        typeName
        (shape : ObjectShape)
        : OpenApiPlannedTypeDefinition
        =
        let allProperties =
            (shape.Properties, shape.Required)
            ||> Set.fold (fun properties requiredName ->
                if Map.containsKey requiredName properties then
                    properties
                else
                    Map.add requiredName None properties
            )

        if
            allProperties.IsEmpty
            && shape.AdditionalProperties = AdditionalProperties.Forbidden
        then
            report
                context.Resolution.Diagnostics
                UnsupportedSchema
                sourceLocation
                "A closed object with no properties has no faithful non-null F# record representation."

        let usedFieldNames = HashSet<string> (StringComparer.Ordinal)

        if shape.AdditionalProperties <> AdditionalProperties.Forbidden then
            usedFieldNames.Add "AdditionalProperties" |> ignore

        let fields =
            allProperties
            |> Map.toList
            |> List.map (fun (jsonName, propertySchema) ->
                let required = Set.contains jsonName shape.Required
                let fsharpName = allocateUniqueName usedFieldNames "Field" sanitiseTypeName jsonName

                let fieldType =
                    match propertySchema with
                    | None -> OpenApiPlannedType.Optional OpenApiPlannedType.JsonNode
                    | Some propertySchema ->
                        let allowsNull = schemaAllowsNull context.Resolution propertySchema

                        if allowsNull && not required then
                            report
                                context.Resolution.Diagnostics
                                UnsupportedSchema
                                propertySchema.Location
                                "An optional property whose schema allows null has three wire states (missing, null, value), which this generated API does not conflate."

                        let result =
                            typeForSchema context Set.empty ($"%s{typeName}%s{fsharpName}") propertySchema

                        if required || allowsNull then
                            result
                        else
                            OpenApiPlannedType.Optional result

                {
                    JsonName = jsonName
                    FSharpName = fsharpName
                    Type = fieldType
                    Required = required
                }
            )

        let additionalProperties =
            match shape.AdditionalProperties with
            | AdditionalProperties.Forbidden -> None
            | AdditionalProperties.Any -> Some (OpenApiPlannedType.Optional OpenApiPlannedType.JsonNode)
            | AdditionalProperties.Typed schema ->
                typeForSchema context Set.empty ($"%s{typeName}AdditionalProperty") schema
                |> Some

        {
            SourceName = sourceName
            FSharpName = typeName
            Description = shape.Description
            Fields = fields
            AdditionalProperties = additionalProperties
        }

    and private collectObjectShape
        (context : SchemaPlanningContext)
        (compositionStack : Set<string>)
        (schema : CanonicalSchema)
        : ObjectShape
        =
        match resolveSchemaReferences context.Resolution true compositionStack schema with
        | None -> emptyObjectShape None
        | Some (compositionStack, resolved) ->
            match resolved.Shape with
            | CanonicalSchemaShape.Object shape ->
                {
                    Description = resolved.Description
                    Properties = shape.Properties
                    Required = shape.Required
                    AdditionalProperties = shape.AdditionalProperties
                }
            | CanonicalSchemaShape.AllOf (_, hasSiblingObjectKeywords, _, branches) ->
                if hasSiblingObjectKeywords then
                    report
                        context.Resolution.Diagnostics
                        UnsupportedSchema
                        resolved.Location
                        "An allOf schema with sibling object-shape keywords is not currently supported."

                let shapes =
                    branches
                    |> List.map (fun branch ->
                        if not (schemaIsObjectLike context.Resolution branch) then
                            report
                                context.Resolution.Diagnostics
                                UnsupportedSchema
                                branch.Location
                                "Only object-shaped allOf branches can be represented as an F# record."

                        collectObjectShape context compositionStack branch
                    )

                let merge (left : ObjectShape) (right : ObjectShape) : ObjectShape =
                    let ensureNoForbiddenIntroductions (constrained : ObjectShape) (other : ObjectShape) =
                        match constrained.AdditionalProperties with
                        | AdditionalProperties.Any -> ()
                        | AdditionalProperties.Forbidden
                        | AdditionalProperties.Typed _ ->
                            let introduced =
                                Set.difference
                                    (other.Properties |> Map.toSeq |> Seq.map fst |> Set.ofSeq)
                                    (constrained.Properties |> Map.toSeq |> Seq.map fst |> Set.ofSeq)

                            if not introduced.IsEmpty then
                                report
                                    context.Resolution.Diagnostics
                                    UnsupportedSchema
                                    resolved.Location
                                    "allOf cannot merge fields introduced outside a branch with constrained additionalProperties."

                    ensureNoForbiddenIntroductions left right
                    ensureNoForbiddenIntroductions right left

                    let properties =
                        (left.Properties, right.Properties)
                        ||> Map.fold (fun current name propertySchema ->
                            match Map.tryFind name current, propertySchema with
                            | None, _ -> Map.add name propertySchema current
                            | Some None, Some value -> Map.add name (Some value) current
                            | Some None, None
                            | Some (Some _), None -> current
                            | Some (Some existing), Some value ->
                                if
                                    schemaIdentity context Set.empty existing
                                    <> schemaIdentity context Set.empty value
                                then
                                    report
                                        context.Resolution.Diagnostics
                                        UnsupportedSchema
                                        value.Location
                                        $"allOf gives property '%s{name}' incompatible schemas."

                                current
                        )

                    let additionalProperties =
                        match left.AdditionalProperties, right.AdditionalProperties with
                        | AdditionalProperties.Any, value
                        | value, AdditionalProperties.Any -> value
                        | AdditionalProperties.Forbidden, AdditionalProperties.Forbidden ->
                            AdditionalProperties.Forbidden
                        | AdditionalProperties.Typed left, AdditionalProperties.Typed right when
                            schemaIdentity context Set.empty left = schemaIdentity context Set.empty right
                            ->
                            AdditionalProperties.Typed left
                        | _ ->
                            report
                                context.Resolution.Diagnostics
                                UnsupportedSchema
                                resolved.Location
                                "allOf branches have incompatible additionalProperties constraints."

                            AdditionalProperties.Forbidden

                    {
                        Description = None
                        Properties = properties
                        Required = Set.union left.Required right.Required
                        AdditionalProperties = additionalProperties
                    }

                match shapes with
                | [] -> emptyObjectShape resolved.Description
                | head :: tail ->
                    { List.fold merge head tail with
                        Description = resolved.Description
                    }
            | _ ->
                report
                    context.Resolution.Diagnostics
                    UnsupportedSchema
                    resolved.Location
                    "Expected an object-shaped schema."

                emptyObjectShape resolved.Description

    and private schemaIdentity
        (context : SchemaPlanningContext)
        (aliasStack : Set<string>)
        (schema : CanonicalSchema)
        : SchemaIdentity
        =
        match schema.Shape with
        | CanonicalSchemaShape.Reference reference when Map.containsKey reference.Name context.ComponentTypeNames ->
            match trySchemaTarget context.Resolution false reference with
            | None -> SchemaIdentity.Json
            | Some target ->
                let core = SchemaIdentity.Named reference.Name

                if schemaAllowsNull context.Resolution target then
                    SchemaIdentity.Optional core
                else
                    core
        | CanonicalSchemaShape.Reference _ ->
            match resolveSchemaReferences context.Resolution false aliasStack schema with
            | None -> SchemaIdentity.Json
            | Some (aliasStack, target) -> schemaIdentity context aliasStack target
        | shape ->
            let core =
                match shape with
                | CanonicalSchemaShape.Any -> SchemaIdentity.Json
                | CanonicalSchemaShape.Primitive primitive -> SchemaIdentity.Primitive primitive
                | CanonicalSchemaShape.Array None -> SchemaIdentity.List SchemaIdentity.Json
                | CanonicalSchemaShape.Array (Some items) ->
                    SchemaIdentity.List (schemaIdentity context aliasStack items)
                | CanonicalSchemaShape.Object _
                | CanonicalSchemaShape.AllOf _ ->
                    collectObjectShape context Set.empty schema
                    |> objectIdentity context
                    |> SchemaIdentity.Object
                | CanonicalSchemaShape.UnsupportedNumber _
                | CanonicalSchemaShape.UnsupportedType _
                | CanonicalSchemaShape.Invalid
                | CanonicalSchemaShape.Reference _ -> SchemaIdentity.Json

            if schemaAllowsNull context.Resolution schema then
                SchemaIdentity.Optional core
            else
                core

    and private objectIdentity (context : SchemaPlanningContext) (shape : ObjectShape) : ObjectShapeIdentity =
        let allProperties =
            (shape.Properties, shape.Required)
            ||> Set.fold (fun properties requiredName ->
                if Map.containsKey requiredName properties then
                    properties
                else
                    Map.add requiredName None properties
            )

        let properties =
            allProperties
            |> Map.map (fun _ schema ->
                match schema with
                | None -> SchemaIdentity.Optional SchemaIdentity.Json
                | Some schema -> schemaIdentity context Set.empty schema
            )

        let additionalProperties =
            match shape.AdditionalProperties with
            | AdditionalProperties.Forbidden -> AdditionalPropertiesIdentity.Forbidden
            | AdditionalProperties.Any ->
                SchemaIdentity.Optional SchemaIdentity.Json
                |> AdditionalPropertiesIdentity.Value
            | AdditionalProperties.Typed schema ->
                schemaIdentity context Set.empty schema |> AdditionalPropertiesIdentity.Value

        {
            Properties = properties
            Required = shape.Required
            AdditionalProperties = additionalProperties
        }

    let private addComponentDefinition
        (context : SchemaPlanningContext)
        (sourceName : string)
        (schema : CanonicalSchema)
        : unit
        =
        let shape = collectObjectShape context (Set.singleton sourceName) schema
        let typeName = context.ComponentTypeNames.[sourceName]

        buildDefinition context sourceName schema.Location typeName shape
        |> context.Definitions.Add

    let rec private resolveComponentReference
        (diagnostics : ResizeArray<OpenApiGenerationDiagnostic>)
        (diagnosticCode : OpenApiGenerationDiagnosticCode)
        (prefix : string)
        (components : Map<string, LocatedObject>)
        (visited : Set<string>)
        (value : LocatedObject)
        : LocatedObject option
        =
        match optionalString diagnostics value.Location value.Value "$ref" with
        | None -> Some value
        | Some reference ->
            let location = $"%s{value.Location}/$ref"

            match referenceName diagnostics diagnosticCode prefix location reference with
            | None -> None
            | Some name when Set.contains name visited ->
                report diagnostics diagnosticCode location $"Reference cycle involving '%s{name}' is unsupported here."
                None
            | Some name ->
                match Map.tryFind name components with
                | None ->
                    report diagnostics diagnosticCode location $"Component '%s{name}' does not exist."
                    None
                | Some target ->
                    resolveComponentReference diagnostics diagnosticCode prefix components (Set.add name visited) target

    let private parseServerBase
        (diagnostics : ResizeArray<OpenApiGenerationDiagnostic>)
        (root : JsonObject)
        : OpenApiServerBase
        =
        let report = report diagnostics
        let optionalArray = optionalArray diagnostics
        let tryObject = tryObject diagnostics
        let requiredString = requiredString diagnostics
        let optionalObject = optionalObject diagnostics
        let objectMap = objectMap diagnostics

        match optionalArray "#" root "servers" with
        | None -> OpenApiServerBase.BasePath "/"
        | Some servers when servers.Count = 0 -> OpenApiServerBase.BasePath "/"
        | Some servers ->
            if servers.Count > 1 then
                report
                    UnsupportedOperation
                    "#/servers"
                    "Only the first document-level server is supported; additional server entries would be ignored."

            match tryObject "#/servers/0" servers.[0] with
            | None -> OpenApiServerBase.BasePath "/"
            | Some server ->
                let mutable url =
                    requiredString server.Location server.Value "url" |> Option.defaultValue "/"

                let variables =
                    match optionalObject server.Location server.Value "variables" with
                    | None -> Map.empty
                    | Some variables -> objectMap variables.Location variables.Value

                for found in Regex.Matches (url, "\\{([^{}]+)\\}") |> Seq.cast<Match> do
                    let variableName = found.Groups.[1].Value

                    match Map.tryFind variableName variables with
                    | None ->
                        report
                            InvalidDocument
                            ($"%s{server.Location}/url")
                            $"Server variable '%s{variableName}' has no definition."
                    | Some variable ->
                        match requiredString variable.Location variable.Value "default" with
                        | None -> ()
                        | Some value -> url <- url.Replace (found.Value, value, StringComparison.Ordinal)

                match Uri.TryCreate (url, UriKind.Absolute) with
                | true, _ -> OpenApiServerBase.BaseAddress url
                | false, _ -> OpenApiServerBase.BasePath url

    type private OperationPlanningContext =
        {
            Diagnostics : ResizeArray<OpenApiGenerationDiagnostic>
            SchemaCache : Dictionary<string, CanonicalSchema>
            SchemaPlanning : SchemaPlanningContext
            ParameterComponents : Map<string, LocatedObject>
            RequestBodyComponents : Map<string, LocatedObject>
            ResponseComponents : Map<string, LocatedObject>
        }

    let private parseParameter (context : OperationPlanningContext) (value : LocatedObject) : ResolvedParameter option =
        let diagnostics = context.Diagnostics
        let report = report diagnostics
        let requiredString = requiredString diagnostics
        let optionalString = optionalString diagnostics
        let optionalBool = optionalBool diagnostics
        let optionalObject = optionalObject diagnostics

        resolveComponentReference
            diagnostics
            UnresolvedReference
            "#/components/parameters/"
            context.ParameterComponents
            Set.empty
            value
        |> Option.bind (fun value ->
            let name = requiredString value.Location value.Value "name"

            let location =
                requiredString value.Location value.Value "in"
                |> Option.bind (fun location ->
                    match location with
                    | "path" -> Some ResolvedParameterLocation.Path
                    | "query" -> Some ResolvedParameterLocation.Query
                    | "header" -> Some ResolvedParameterLocation.Header
                    | "cookie" -> Some ResolvedParameterLocation.Cookie
                    | other ->
                        report
                            UnsupportedParameter
                            ($"%s{value.Location}/in")
                            $"Parameter location '%s{other}' is unsupported."

                        None
                )

            let schema =
                optionalObject value.Location value.Value "schema"
                |> Option.map (analyzeSchema diagnostics context.SchemaCache)

            let hasSchema = value.Value.ContainsKey "schema"
            let hasContent = value.Value.ContainsKey "content"

            if hasContent then
                report
                    UnsupportedParameter
                    ($"%s{value.Location}/content")
                    "Content-based parameters are not supported."

            if not hasSchema && not hasContent then
                report InvalidDocument value.Location "A parameter must contain exactly one of schema or content."

            if hasSchema && schema.IsNone then
                report
                    InvalidDocument
                    ($"%s{value.Location}/schema")
                    "A parameter schema must be a non-null JSON object."

            match name, location, schema with
            | Some name, Some location, Some schema ->
                let required =
                    optionalBool value.Location value.Value "required" |> Option.defaultValue false

                match location with
                | ResolvedParameterLocation.Path when not required ->
                    report UnsupportedParameter value.Location "Path parameters must specify required: true."
                | ResolvedParameterLocation.Query when not (Regex.IsMatch (name, "^[A-Za-z0-9._~-]+$")) ->
                    report
                        UnsupportedParameter
                        ($"%s{value.Location}/name")
                        "Query parameter names must contain only RFC 3986 unreserved characters."
                | ResolvedParameterLocation.Header
                | ResolvedParameterLocation.Cookie ->
                    report
                        UnsupportedParameter
                        value.Location
                        "Header and cookie parameters are not representable by the generated HTTP client."
                | _ -> ()

                let expectedStyle =
                    match location with
                    | ResolvedParameterLocation.Path -> Some "simple"
                    | ResolvedParameterLocation.Query -> Some "form"
                    | _ -> None

                match expectedStyle, optionalString value.Location value.Value "style" with
                | Some expected, Some actual when actual <> expected ->
                    report
                        UnsupportedParameter
                        ($"%s{value.Location}/style")
                        $"Only the default '%s{expected}' parameter style is supported."
                | _ -> ()

                match optionalBool value.Location value.Value "allowReserved" with
                | Some true ->
                    report
                        UnsupportedParameter
                        ($"%s{value.Location}/allowReserved")
                        "allowReserved parameters require a different URI-escaping strategy."
                | _ -> ()

                {
                    Name = name
                    Location = location
                    Required = required
                    Schema = schema
                    SourceLocation = value.Location
                }
                |> Some
            | _ -> None
        )

    let private parseParameterList
        (context : OperationPlanningContext)
        (owner : LocatedObject)
        : ResolvedParameter list
        =
        let diagnostics = context.Diagnostics
        let report = report diagnostics
        let optionalArray = optionalArray diagnostics
        let tryObject = tryObject diagnostics

        match optionalArray owner.Location owner.Value "parameters" with
        | None -> []
        | Some values ->
            let parsed =
                values
                |> Seq.mapi (fun index value ->
                    tryObject ($"%s{owner.Location}/parameters/%i{index}") value
                    |> Option.bind (parseParameter context)
                )
                |> Seq.choose id
                |> Seq.toList

            parsed
            |> List.groupBy (fun parameter -> parameter.Name, parameter.Location)
            |> List.iter (fun ((name, _), values) ->
                if values.Length > 1 then
                    report InvalidDocument owner.Location $"Parameter '%s{name}' is duplicated at the same location."
            )

            parsed

    let private mergeParameters
        (inherited : ResolvedParameter list)
        (operation : ResolvedParameter list)
        : ResolvedParameter list
        =
        let overrides =
            operation
            |> List.map (fun parameter -> (parameter.Name, parameter.Location), parameter)
            |> Map

        [
            for parameter in inherited do
                match Map.tryFind (parameter.Name, parameter.Location) overrides with
                | Some replacement -> yield replacement
                | None -> yield parameter

            let inheritedKeys =
                inherited
                |> List.map (fun parameter -> parameter.Name, parameter.Location)
                |> Set

            for parameter in operation do
                if not (Set.contains (parameter.Name, parameter.Location) inheritedKeys) then
                    yield parameter
        ]

    let private mediaTypeWithoutParameters (name : string) : string =
        match name.IndexOf ';' with
        | -1 -> name.Trim ()
        | separator -> (name.Substring (0, separator)).Trim ()

    let private mediaTypeEquals (expected : string) (actual : string) : bool =
        (mediaTypeWithoutParameters actual).Equals (expected, StringComparison.OrdinalIgnoreCase)

    let private selectMedia
        (context : OperationPlanningContext)
        (purpose : string)
        (content : LocatedObject)
        : (string * CanonicalSchema option) option
        =
        let diagnostics = context.Diagnostics
        let report = report diagnostics
        let tryObject = tryObject diagnostics
        let optionalObject = optionalObject diagnostics

        let rank (name : string) =
            let name = mediaTypeWithoutParameters name

            if name.Equals ("application/json", StringComparison.OrdinalIgnoreCase) then
                0
            elif name.EndsWith ("+json", StringComparison.OrdinalIgnoreCase) then
                1
            elif name.Equals ("text/plain", StringComparison.OrdinalIgnoreCase) then
                2
            elif name.Equals ("application/octet-stream", StringComparison.OrdinalIgnoreCase) then
                3
            else
                100

        let candidates =
            content.Value
            |> Seq.choose (fun (KeyValue (name, node)) ->
                let rank = rank name

                if rank = 100 then
                    None
                else
                    tryObject ($"%s{content.Location}/%s{pointerToken name}") node
                    |> Option.map (fun media -> rank, name, media)
            )
            |> Seq.sortBy (fun (rank, name, _) -> rank, name)
            |> Seq.toList

        match candidates with
        | [] ->
            let keys =
                content.Value
                |> Seq.map (fun (KeyValue (name, _)) -> name)
                |> Seq.sort
                |> String.concat "', '"
                |> fun value ->
                    if String.IsNullOrEmpty value then
                        "(none)"
                    else
                        $"'%s{value}'"

            report
                UnsupportedOperation
                content.Location
                $"No supported media type was found for %s{purpose}. Content keys: %s{keys}."

            None
        | (_, selectedName, selected) :: _ ->
            let selectedSchema =
                optionalObject selected.Location selected.Value "schema"
                |> Option.map (analyzeSchema diagnostics context.SchemaCache)

            Some (mediaTypeWithoutParameters selectedName, selectedSchema)

    let rec private isJsonStringType (plannedType : OpenApiPlannedType) : bool =
        match plannedType with
        | OpenApiPlannedType.Primitive OpenApiPrimitive.String -> true
        | OpenApiPlannedType.Optional inner -> isJsonStringType inner
        | _ -> false

    let private isJsonMediaType (mediaType : string) : bool =
        let mediaType = mediaTypeWithoutParameters mediaType

        mediaType.Equals ("application/json", StringComparison.OrdinalIgnoreCase)
        || mediaType.EndsWith ("+json", StringComparison.OrdinalIgnoreCase)

    let private responseShape
        (context : OperationPlanningContext)
        (operationName : string)
        (value : LocatedObject)
        : OpenApiPlannedType * string option
        =
        let diagnostics = context.Diagnostics
        let report = report diagnostics
        let optionalObject = optionalObject diagnostics

        let value =
            resolveComponentReference
                diagnostics
                UnresolvedReference
                "#/components/responses/"
                context.ResponseComponents
                Set.empty
                value

        match value with
        | None -> OpenApiPlannedType.JsonNode, None
        | Some value ->
            match optionalObject value.Location value.Value "content" with
            | None -> OpenApiPlannedType.Unit, None
            | Some content when content.Value.Count = 0 -> OpenApiPlannedType.Unit, None
            | Some content ->
                match selectMedia context "a response" content with
                | None -> OpenApiPlannedType.JsonNode, None
                | Some (mediaType, schema) ->
                    let result =
                        if mediaTypeEquals "application/octet-stream" mediaType then
                            match schema with
                            | None -> ()
                            | Some schema ->
                                match
                                    typeForSchema
                                        context.SchemaPlanning
                                        Set.empty
                                        ($"%s{operationName}BinaryResponse")
                                        schema
                                with
                                | OpenApiPlannedType.Primitive OpenApiPrimitive.String -> ()
                                | _ ->
                                    report
                                        UnsupportedOperation
                                        schema.Location
                                        "application/octet-stream responses require a non-null string/binary schema."

                            OpenApiPlannedType.Stream
                        elif mediaTypeEquals "text/plain" mediaType then
                            match schema with
                            | None -> ()
                            | Some schema ->
                                match
                                    typeForSchema
                                        context.SchemaPlanning
                                        Set.empty
                                        ($"%s{operationName}TextResponse")
                                        schema
                                with
                                | OpenApiPlannedType.Primitive OpenApiPrimitive.String -> ()
                                | _ ->
                                    report
                                        UnsupportedOperation
                                        schema.Location
                                        "text/plain responses require a non-null string schema."

                            OpenApiPlannedType.Primitive OpenApiPrimitive.String
                        else
                            match schema with
                            | None -> OpenApiPlannedType.Optional OpenApiPlannedType.JsonNode
                            | Some schema ->
                                typeForSchema context.SchemaPlanning Set.empty ($"%s{operationName}Response") schema

                    if isJsonMediaType mediaType && isJsonStringType result then
                        report
                            UnsupportedOperation
                            content.Location
                            "JSON string responses need JSON unquoting, which the generated HTTP shell cannot distinguish from text/plain."

                    result, Some mediaType

    let private successfulResponses
        (context : OperationPlanningContext)
        (operationName : string)
        (responses : LocatedObject)
        : OpenApiPlannedType * string option
        =
        let diagnostics = context.Diagnostics
        let report = report diagnostics
        let tryObject = tryObject diagnostics

        let declaredSuccesses =
            responses.Value
            |> Seq.choose (fun (KeyValue (status, node)) ->
                let successful =
                    if status.Equals ("2XX", StringComparison.OrdinalIgnoreCase) then
                        true
                    else
                        match Int32.TryParse status with
                        | true, value -> 200 <= value && value < 300
                        | false, _ -> false

                if successful then
                    tryObject ($"%s{responses.Location}/%s{pointerToken status}") node
                    |> Option.map (fun response -> status, response)
                else
                    None
            )
            |> Seq.sortBy fst
            |> Seq.toList

        let rangeCoversEverySuccess =
            declaredSuccesses
            |> List.exists (fun (status, _) -> status.Equals ("2XX", StringComparison.OrdinalIgnoreCase))

        let candidates =
            if rangeCoversEverySuccess then
                declaredSuccesses
            else
                match responses.Value.TryGetPropertyValue "default" with
                | false, _ -> declaredSuccesses
                | true, value ->
                    match tryObject ($"%s{responses.Location}/default") value with
                    | None -> declaredSuccesses
                    | Some response -> declaredSuccesses @ [ "default", response ]

        match candidates with
        | [] ->
            report
                AmbiguousSuccessResponse
                responses.Location
                "At least one exact 2xx, 2XX, or default response is required to describe success."

            OpenApiPlannedType.Unit, None
        | (_, first) :: rest ->
            let firstShape = responseShape context operationName first

            for status, response in rest do
                let otherShape = responseShape context operationName response

                if otherShape <> firstShape then
                    report
                        AmbiguousSuccessResponse
                        ($"%s{responses.Location}/%s{pointerToken status}")
                        "All possible successful responses must have the same body type and media type."

            firstShape

    let private requestBodyParameter
        (context : OperationPlanningContext)
        (operationName : string)
        (operation : LocatedObject)
        : (OpenApiPlannedParameter * string) option
        =
        let diagnostics = context.Diagnostics
        let report = report diagnostics
        let optionalBool = optionalBool diagnostics
        let optionalObject = optionalObject diagnostics

        match optionalObject operation.Location operation.Value "requestBody" with
        | None -> None
        | Some body ->
            let body =
                resolveComponentReference
                    diagnostics
                    UnresolvedReference
                    "#/components/requestBodies/"
                    context.RequestBodyComponents
                    Set.empty
                    body

            body
            |> Option.bind (fun body ->
                let required =
                    optionalBool body.Location body.Value "required" |> Option.defaultValue false

                if not required then
                    report
                        UnsupportedOperation
                        body.Location
                        "Optional request bodies cannot be represented without conflating omission and JSON null."

                match optionalObject body.Location body.Value "content" with
                | None ->
                    report InvalidDocument ($"%s{body.Location}/content") "Request bodies require content."
                    None
                | Some content ->
                    selectMedia context "a request body" content
                    |> Option.map (fun (mediaType, schema) ->
                        let plannedType =
                            if mediaTypeEquals "application/octet-stream" mediaType then
                                OpenApiPlannedType.Stream
                            elif mediaTypeEquals "text/plain" mediaType then
                                match schema with
                                | None -> ()
                                | Some schema ->
                                    match
                                        typeForSchema
                                            context.SchemaPlanning
                                            Set.empty
                                            ($"%s{operationName}TextRequest")
                                            schema
                                    with
                                    | OpenApiPlannedType.Primitive OpenApiPrimitive.String -> ()
                                    | _ ->
                                        report
                                            UnsupportedOperation
                                            schema.Location
                                            "text/plain request bodies require a non-null string schema."

                                OpenApiPlannedType.Primitive OpenApiPrimitive.String
                            else
                                match schema with
                                | None -> OpenApiPlannedType.Optional OpenApiPlannedType.JsonNode
                                | Some schema ->
                                    typeForSchema
                                        context.SchemaPlanning
                                        Set.empty
                                        ($"%s{operationName}Request")
                                        schema

                        if mediaTypeEquals "application/octet-stream" mediaType then
                            report
                                UnsupportedOperation
                                content.Location
                                "Binary request content types are not emitted correctly by the generated HTTP shell."

                        if isJsonMediaType mediaType && isJsonStringType plannedType then
                            report
                                UnsupportedOperation
                                content.Location
                                "JSON string request bodies need JSON quoting, which the generated HTTP shell cannot distinguish from text/plain."

                        {
                            WireName = "body"
                            FSharpName = "body"
                            Location = OpenApiParameterLocation.Body
                            Type = plannedType
                            Required = required
                        },
                        mediaType
                    )
            )

    let private planOperations (context : OperationPlanningContext) (root : JsonObject) : OpenApiPlannedOperation list =
        let diagnostics = context.Diagnostics
        let report = report diagnostics
        let optionalString = optionalString diagnostics
        let optionalObject = optionalObject diagnostics
        let optionalArray = optionalArray diagnostics
        let tryObject = tryObject diagnostics
        let schemaResolution = context.SchemaPlanning.Resolution
        let typeForSchema = typeForSchema context.SchemaPlanning

        let paths = optionalObject "#" root "paths"
        let operations = ResizeArray<OpenApiPlannedOperation> ()
        let usedOperationIds = HashSet<string> (StringComparer.Ordinal)
        let usedMethodNames = HashSet<string> (StringComparer.Ordinal)

        let methodEntries (pathItem : LocatedObject) =
            [
                "get", HttpMethod.Get
                "put", HttpMethod.Put
                "post", HttpMethod.Post
                "delete", HttpMethod.Delete
                "options", HttpMethod.Options
                "head", HttpMethod.Head
                "patch", HttpMethod.Patch
                "trace", HttpMethod.Trace
            ]
            |> List.choose (fun (name, method) ->
                optionalObject pathItem.Location pathItem.Value name
                |> Option.map (fun operation -> method, operation)
            )

        match paths with
        | None -> report InvalidDocument "#/paths" "The OpenAPI paths object is required for client generation."
        | Some paths ->
            for path, pathItemNode in
                paths.Value
                |> Seq.map (fun (KeyValue (key, value)) -> key, value)
                |> Seq.filter (fun (key, _) -> not (key.StartsWith ("x-", StringComparison.OrdinalIgnoreCase)))
                |> Seq.sortBy fst do
                if not (path.StartsWith ('/')) then
                    report
                        InvalidDocument
                        ($"%s{paths.Location}/%s{pointerToken path}")
                        "OpenAPI paths must start with '/'."
                elif path.StartsWith ("//", StringComparison.Ordinal) then
                    report
                        InvalidDocument
                        ($"%s{paths.Location}/%s{pointerToken path}")
                        "Paths beginning with multiple slashes cannot be preserved by the generated URI composition."

                match tryObject ($"%s{paths.Location}/%s{pointerToken path}") pathItemNode with
                | None -> ()
                | Some pathItem ->
                    if pathItem.Value.ContainsKey "$ref" then
                        report UnsupportedOperation pathItem.Location "Referenced Path Item Objects are unsupported."

                    match optionalArray pathItem.Location pathItem.Value "servers" with
                    | Some values when values.Count > 0 ->
                        report
                            UnsupportedOperation
                            ($"%s{pathItem.Location}/servers")
                            "Path-specific servers are unsupported."
                    | _ -> ()

                    let inheritedParameters = parseParameterList context pathItem

                    for httpMethod, operation in methodEntries pathItem do
                        match optionalArray operation.Location operation.Value "servers" with
                        | Some values when values.Count > 0 ->
                            report
                                UnsupportedOperation
                                ($"%s{operation.Location}/servers")
                                "Operation-specific servers are unsupported."
                        | _ -> ()

                        let operationId, operationIdLocation =
                            match optionalString operation.Location operation.Value "operationId" with
                            | Some value -> value, $"%s{operation.Location}/operationId"
                            | None ->
                                let methodName = httpMethod.ToString().ToLowerInvariant ()
                                $"%s{methodName}-%s{path}", operation.Location

                        if not (usedOperationIds.Add operationId) then
                            report InvalidDocument operationIdLocation $"Operation id '%s{operationId}' is duplicated."

                        let operationFSharpName =
                            allocateUniqueName usedMethodNames "Operation" sanitiseTypeName operationId

                        let mergedParameters =
                            parseParameterList context operation |> mergeParameters inheritedParameters

                        let templateNames =
                            Regex.Matches (path, "\\{([^{}]+)\\}")
                            |> Seq.cast<Match>
                            |> Seq.map (fun value -> value.Groups.[1].Value)
                            |> Set.ofSeq

                        let pathParameters =
                            mergedParameters
                            |> List.filter (fun parameter -> parameter.Location = ResolvedParameterLocation.Path)

                        for templateName in templateNames do
                            if not (pathParameters |> List.exists (fun parameter -> parameter.Name = templateName)) then
                                report
                                    UnsupportedParameter
                                    operation.Location
                                    $"Path template variable '%s{templateName}' has no path parameter."

                        for parameter in pathParameters do
                            if not (Set.contains parameter.Name templateNames) then
                                report
                                    UnsupportedParameter
                                    parameter.SourceLocation
                                    $"Path parameter '%s{parameter.Name}' does not occur in the path template."

                        let usedParameterNames = HashSet<string> (StringComparer.Ordinal)
                        usedParameterNames.Add "ct" |> ignore
                        usedParameterNames.Add "body" |> ignore
                        usedParameterNames.Add "client" |> ignore

                        let plannedParameters =
                            mergedParameters
                            |> List.choose (fun parameter ->
                                let location =
                                    match parameter.Location with
                                    | ResolvedParameterLocation.Path -> Some OpenApiParameterLocation.Path
                                    | ResolvedParameterLocation.Query -> Some OpenApiParameterLocation.Query
                                    | ResolvedParameterLocation.Header
                                    | ResolvedParameterLocation.Cookie -> None

                                location
                                |> Option.map (fun location ->
                                    let parameterFSharpName =
                                        allocateUniqueName
                                            usedParameterNames
                                            "parameter"
                                            sanitiseParameterName
                                            parameter.Name

                                    let parameterType =
                                        typeForSchema
                                            Set.empty
                                            ($"%s{operationFSharpName}%s{parameterFSharpName}")
                                            parameter.Schema

                                    if schemaAllowsNull schemaResolution parameter.Schema then
                                        report
                                            UnsupportedParameter
                                            parameter.Schema.Location
                                            "Path/query parameter schemas which allow null cannot be represented distinctly from omission."

                                    match parameterType with
                                    | OpenApiPlannedType.Primitive OpenApiPrimitive.String
                                    | OpenApiPlannedType.Primitive OpenApiPrimitive.Int32
                                    | OpenApiPlannedType.Primitive OpenApiPrimitive.Int64 -> ()
                                    | _ ->
                                        report
                                            UnsupportedParameter
                                            parameter.Schema.Location
                                            "Only string, int32, and int64 path/query parameter schemas currently have an exact wire encoding."

                                    let parameterType =
                                        if parameter.Required then
                                            parameterType
                                        else
                                            match parameterType with
                                            | OpenApiPlannedType.Optional _ -> parameterType
                                            | _ -> OpenApiPlannedType.Optional parameterType

                                    {
                                        WireName = parameter.Name
                                        FSharpName = parameterFSharpName
                                        Location = location
                                        Type = parameterType
                                        Required = parameter.Required
                                    }
                                )
                            )

                        let body = requestBodyParameter context operationFSharpName operation

                        let plannedParameters =
                            match body with
                            | None -> plannedParameters
                            | Some (parameter, _) -> plannedParameters @ [ parameter ]

                        let responses = optionalObject operation.Location operation.Value "responses"

                        let returnType, accept =
                            match responses with
                            | None ->
                                report
                                    InvalidDocument
                                    ($"%s{operation.Location}/responses")
                                    "Operations require responses."

                                OpenApiPlannedType.Unit, None
                            | Some responses -> successfulResponses context operationFSharpName responses

                        operations.Add
                            {
                                OperationId = operationId
                                FSharpName = operationFSharpName
                                Description =
                                    optionalString operation.Location operation.Value "summary"
                                    |> Option.orElseWith (fun () ->
                                        optionalString operation.Location operation.Value "description"
                                    )
                                Method = httpMethod
                                Path = path
                                Parameters = plannedParameters
                                ReturnType = returnType
                                Accept = accept
                                RequestContentType = body |> Option.map snd
                            }

        operations |> Seq.sortBy _.FSharpName |> Seq.toList

    let private orderDefinitions
        (diagnostics : ResizeArray<OpenApiGenerationDiagnostic>)
        (definitions : OpenApiPlannedTypeDefinition seq)
        : OpenApiPlannedTypeDefinition list
        =
        let report = report diagnostics

        let rec namedDependencies (plannedType : OpenApiPlannedType) : Set<string> =
            match plannedType with
            | OpenApiPlannedType.Named name -> Set.singleton name
            | OpenApiPlannedType.List inner
            | OpenApiPlannedType.Optional inner -> namedDependencies inner
            | OpenApiPlannedType.Primitive _
            | OpenApiPlannedType.JsonNode
            | OpenApiPlannedType.Stream
            | OpenApiPlannedType.Unit -> Set.empty

        let definitionsByName =
            definitions
            |> Seq.map (fun definition -> definition.FSharpName, definition)
            |> Map.ofSeq

        let definitionDependencies (definition : OpenApiPlannedTypeDefinition) : Set<string> =
            [
                yield!
                    definition.Fields
                    |> List.collect (fun field -> namedDependencies field.Type |> Set.toList)

                match definition.AdditionalProperties with
                | None -> ()
                | Some value -> yield! namedDependencies value
            ]
            |> Set.ofList
            // A record can recursively refer to itself; its generated codec member can recursively call itself too.
            |> Set.remove definition.FSharpName
            |> Set.filter (fun name -> Map.containsKey name definitionsByName)

        let visitedDefinitions = HashSet<string> (StringComparer.Ordinal)
        let visitingDefinitions = HashSet<string> (StringComparer.Ordinal)
        let reportedCycles = HashSet<string> (StringComparer.Ordinal)
        let orderedDefinitions = ResizeArray<OpenApiPlannedTypeDefinition> ()

        let rec visitDefinition (name : string) =
            if not (visitedDefinitions.Contains name) then
                if not (visitingDefinitions.Add name) then
                    if reportedCycles.Add name then
                        report
                            UnsupportedSchema
                            "#/components/schemas"
                            $"Mutually recursive schema components involving '%s{name}' cannot use the generated JSON codecs."
                else
                    let definition = definitionsByName.[name]

                    for dependency in definitionDependencies definition |> Set.toList |> List.sort do
                        visitDefinition dependency

                    visitingDefinitions.Remove name |> ignore

                    if visitedDefinitions.Add name then
                        orderedDefinitions.Add definition

        for name in definitionsByName |> Map.toList |> List.map fst do
            visitDefinition name

        orderedDefinitions |> Seq.toList

    let private parseDocument
        (parameters : Map<string, string>)
        (root : JsonObject)
        : Result<OpenApiClientPlan, OpenApiGenerationDiagnostic list>
        =
        let diagnostics = ResizeArray<OpenApiGenerationDiagnostic> ()
        let report = report diagnostics
        let optionalString = optionalString diagnostics
        let requiredString = requiredString diagnostics
        let optionalObject = optionalObject diagnostics
        let componentMap = componentMap diagnostics

        let version = requiredString "#" root "openapi"

        match version with
        | Some value ->
            let parts = value.Split '.'

            if parts.Length < 2 || parts.[0] <> "3" || parts.[1] <> "0" then
                report UnsupportedVersion "#/openapi" $"Expected an OpenAPI 3.0.x document, but got '%s{value}'."
        | None -> ()

        let parameters = normaliseParameters parameters

        let className =
            match Map.tryFind "CLASSNAME" parameters with
            | Some value when not (String.IsNullOrWhiteSpace value) -> value
            | _ ->
                report InvalidDocument "#/$parameters/ClassName" "The ClassName Myriad parameter is required."
                "GeneratedClient"

        if sanitiseTypeName className <> className then
            report
                InvalidDocument
                "#/$parameters/ClassName"
                "ClassName must already be a valid PascalCase F# identifier."

        let createMock =
            match Map.tryFind "GENERATEMOCKVISIBILITY" parameters with
            | None -> None
            | Some value ->
                match value.ToLowerInvariant () with
                | "internal" -> Some true
                | "public" -> Some false
                | _ ->
                    report
                        InvalidDocument
                        "#/$parameters/GenerateMockVisibility"
                        "GenerateMockVisibility must be 'internal' or 'public'."

                    None

        let info = optionalObject "#" root "info"

        let description =
            info
            |> Option.bind (fun value -> optionalString value.Location value.Value "description")

        match info with
        | None -> report InvalidDocument "#/info" "The OpenAPI info object is required."
        | Some value -> requiredString value.Location value.Value "title" |> ignore

        let components = optionalObject "#" root "components"
        let schemaCache = Dictionary<string, CanonicalSchema> (StringComparer.Ordinal)

        let schemaComponents =
            componentMap components "schemas"
            |> Map.map (fun _ -> analyzeSchema diagnostics schemaCache)

        let parameterComponents = componentMap components "parameters"
        let requestBodyComponents = componentMap components "requestBodies"
        let responseComponents = componentMap components "responses"

        let schemaResolution =
            {
                Diagnostics = diagnostics
                Components = schemaComponents
            }

        let usedTypeNames = HashSet<string> (StringComparer.Ordinal)

        for reservedTypeName in
            [
                className
                "I" + className
                "System"
                "RestEase"
                "WoofWare"
                "GenerateMockAttribute"
                "HttpClientAttribute"
                "JsonParseAttribute"
                "JsonSerializeAttribute"
            ] do
            usedTypeNames.Add reservedTypeName |> ignore

        let objectComponentNames =
            schemaComponents
            |> Map.toList
            |> List.choose (fun (name, schema) ->
                if schemaIsObjectLike schemaResolution schema then
                    Some name
                else
                    None
            )

        let componentTypeNames =
            objectComponentNames
            |> List.map (fun sourceName ->
                let fsharpName =
                    allocateUniqueName usedTypeNames "GeneratedType" sanitiseTypeName sourceName

                sourceName, fsharpName
            )
            |> Map.ofList

        let definitions = ResizeArray<OpenApiPlannedTypeDefinition> ()

        let schemaPlanning =
            {
                Resolution = schemaResolution
                UsedTypeNames = usedTypeNames
                ComponentTypeNames = componentTypeNames
                Definitions = definitions
                LiftedObjectTypes = Dictionary<ObjectShapeIdentity, string> ()
            }

        for sourceName in objectComponentNames do
            addComponentDefinition schemaPlanning sourceName schemaComponents.[sourceName]

        let operationPlanning =
            {
                Diagnostics = diagnostics
                SchemaCache = schemaCache
                SchemaPlanning = schemaPlanning
                ParameterComponents = parameterComponents
                RequestBodyComponents = requestBodyComponents
                ResponseComponents = responseComponents
            }

        let serverBase = parseServerBase diagnostics root
        let operations = planOperations operationPlanning root
        let orderedDefinitions = orderDefinitions diagnostics definitions

        let plan =
            {
                Namespace = className
                InterfaceName = "I" + className
                Description = description
                CreateMock = createMock
                ServerBase = serverBase
                Types = orderedDefinitions
                Operations = operations
            }

        if diagnostics.Count = 0 then
            Ok plan
        else
            diagnostics |> Seq.toList |> Error

    let parseAndPlan
        (parameters : Map<string, string>)
        (source : string)
        : Result<OpenApiClientPlan, OpenApiGenerationDiagnostic list>
        =
        try
            match JsonNode.Parse source with
            | :? JsonObject as root -> parseDocument parameters root
            | _ ->
                Error
                    [
                        diagnostic InvalidDocument "#" "The OpenAPI document root must be a JSON object."
                    ]
        with :? JsonException as ex ->
            Error [ diagnostic InvalidJson "#" ex.Message ]

    let rec private renderType (plannedType : OpenApiPlannedType) : SynType =
        match plannedType with
        | OpenApiPlannedType.Primitive primitive ->
            match primitive with
            | OpenApiPrimitive.String -> SynType.string
            | OpenApiPrimitive.Boolean -> SynType.bool
            | OpenApiPrimitive.Int32 -> SynType.int
            | OpenApiPrimitive.Int64 -> SynType.createLongIdent' [ "int64" ]
            | OpenApiPrimitive.BigInteger -> SynType.createLongIdent' [ "System" ; "Numerics" ; "BigInteger" ]
            | OpenApiPrimitive.Float32 -> SynType.createLongIdent' [ "float32" ]
            | OpenApiPrimitive.Float -> SynType.createLongIdent' [ "float" ]
            | OpenApiPrimitive.Decimal -> SynType.createLongIdent' [ "decimal" ]
            | OpenApiPrimitive.Date -> SynType.createLongIdent' [ "System" ; "DateOnly" ]
            | OpenApiPrimitive.DateTime -> SynType.createLongIdent' [ "System" ; "DateTimeOffset" ]
            | OpenApiPrimitive.Guid -> SynType.createLongIdent' [ "System" ; "Guid" ]
        | OpenApiPlannedType.Named name -> SynType.named name
        | OpenApiPlannedType.List element -> renderType element |> SynType.list
        | OpenApiPlannedType.Optional value -> renderType value |> SynType.option
        | OpenApiPlannedType.JsonNode -> SynType.createLongIdent' [ "System" ; "Text" ; "Json" ; "Nodes" ; "JsonNode" ]
        | OpenApiPlannedType.Stream -> SynType.createLongIdent' [ "System" ; "IO" ; "Stream" ]
        | OpenApiPlannedType.Unit -> SynType.unit

    let private renderRecord (definition : OpenApiPlannedTypeDefinition) : SynTypeDefn =
        let fields =
            [
                match definition.AdditionalProperties with
                | None -> ()
                | Some additionalProperties ->
                    yield
                        {
                            Attrs =
                                [
                                    SynAttribute.create
                                        (SynLongIdent.createS'
                                            [ "System" ; "Text" ; "Json" ; "Serialization" ; "JsonExtensionData" ])
                                        (SynExpr.CreateConst ())
                                ]
                            Ident = Some (Ident.create "AdditionalProperties")
                            Type =
                                SynType.app'
                                    (SynType.createLongIdent' [ "System" ; "Collections" ; "Generic" ; "Dictionary" ])
                                    [ SynType.string ; renderType additionalProperties ]
                        }
                        |> SynField.make

                for field in definition.Fields do
                    yield
                        {
                            Attrs =
                                [
                                    SynAttribute.create
                                        (SynLongIdent.createS'
                                            [ "System" ; "Text" ; "Json" ; "Serialization" ; "JsonPropertyName" ])
                                        (SynExpr.CreateConst field.JsonName)
                                ]
                            Ident = Some (Ident.create field.FSharpName)
                            Type = renderType field.Type
                        }
                        |> SynField.make
            ]

        let fields =
            if fields.IsEmpty then
                [
                    {
                        Attrs = []
                        Ident = Some (Ident.create "_SchemaUnspecified")
                        Type = SynType.obj
                    }
                    |> SynField.make
                ]
            else
                fields

        let componentInfo =
            SynComponentInfo.create (Ident.create definition.FSharpName)
            |> SynComponentInfo.withDocString (
                definition.Description
                |> Option.defaultValue $"Generated representation of the '%s{definition.SourceName}' OpenAPI schema."
                |> PreXmlDoc.create
            )
            |> SynComponentInfo.addAttributes
                [
                    SynAttribute.create
                        (SynLongIdent.createS' [ "WoofWare" ; "Myriad" ; "Plugins" ; "JsonParse" ])
                        (SynExpr.CreateConst true)

                    SynAttribute.create
                        (SynLongIdent.createS' [ "WoofWare" ; "Myriad" ; "Plugins" ; "JsonSerialize" ])
                        (SynExpr.CreateConst true)
                ]

        fields |> SynTypeDefnRepr.record |> SynTypeDefn.create componentInfo

    let private renderOperation (operation : OpenApiPlannedOperation) : SynMemberDefn =
        let cancellationToken =
            SynType.signatureParamOfType
                []
                (SynType.createLongIdent' [ "System" ; "Threading" ; "CancellationToken" ])
                true
                (Some (Ident.create "ct"))

        let parameterType (parameter : OpenApiPlannedParameter) =
            let attributes =
                match parameter.Location with
                | OpenApiParameterLocation.Path ->
                    [
                        SynAttribute.create
                            (SynLongIdent.createS' [ "RestEase" ; "Path" ])
                            (SynExpr.CreateConst parameter.WireName)
                    ]
                | OpenApiParameterLocation.Query ->
                    [
                        SynAttribute.create
                            (SynLongIdent.createS' [ "RestEase" ; "Query" ])
                            (SynExpr.CreateConst parameter.WireName)
                    ]
                | OpenApiParameterLocation.Body ->
                    [
                        SynAttribute.create (SynLongIdent.createS' [ "RestEase" ; "Body" ]) (SynExpr.CreateConst ())
                    ]

            SynType.signatureParamOfType
                attributes
                (renderType parameter.Type)
                false
                (Some (Ident.create parameter.FSharpName))

        let domain =
            operation.Parameters
            |> List.map parameterType
            |> fun parameters -> parameters @ [ cancellationToken ]
            |> SynType.tupleNoParen
            |> Option.get

        let arity =
            SynValInfo.SynValInfo (
                [
                    [
                        for parameter in operation.Parameters do
                            yield SynArgInfo.SynArgInfo ([], false, Some (Ident.create parameter.FSharpName))

                        yield SynArgInfo.SynArgInfo ([], true, Some (Ident.create "ct"))
                    ]
                ],
                SynArgInfo.SynArgInfo ([], false, None)
            )

        let attributes =
            [
                yield
                    SynAttribute.create
                        (SynLongIdent.createS' [ "RestEase" ; operation.Method.ToString () ])
                        (SynExpr.CreateConst (operation.Path.TrimStart '/'))

                match operation.Accept with
                | None -> ()
                | Some mediaType ->
                    yield
                        SynAttribute.create
                            (SynLongIdent.createS' [ "RestEase" ; "Header" ])
                            (SynExpr.tuple [ SynExpr.CreateConst "Accept" ; SynExpr.CreateConst mediaType ])

                match operation.RequestContentType with
                | None -> ()
                | Some mediaType ->
                    yield
                        SynAttribute.create
                            (SynLongIdent.createS' [ "RestEase" ; "Header" ])
                            (SynExpr.tuple [ SynExpr.CreateConst "Content-Type" ; SynExpr.CreateConst mediaType ])
            ]

        renderType operation.ReturnType
        |> SynType.task
        |> SynType.toFun [ domain ]
        |> SynMemberDefn.abstractMember
            attributes
            (SynIdent.createS operation.FSharpName)
            None
            arity
            (operation.Description
             |> Option.defaultValue $"Invoke the '%s{operation.OperationId}' OpenAPI operation."
             |> PreXmlDoc.create)

    let private renderPlan (plan : OpenApiClientPlan) : Output =
        let typeDeclarations =
            plan.Types |> List.map renderRecord |> SynModuleDecl.createTypes

        let interfaceType =
            plan.Operations
            |> List.map renderOperation
            |> SynTypeDefnRepr.interfaceType
            |> SynTypeDefn.create (
                let attributes =
                    [
                        yield
                            SynAttribute.create
                                (SynLongIdent.createS' [ "WoofWare" ; "Myriad" ; "Plugins" ; "HttpClient" ])
                                (SynExpr.CreateConst false)

                        match plan.ServerBase with
                        | OpenApiServerBase.BaseAddress address ->
                            yield
                                SynAttribute.create
                                    (SynLongIdent.createS' [ "RestEase" ; "BaseAddress" ])
                                    (SynExpr.CreateConst address)
                        | OpenApiServerBase.BasePath path ->
                            yield
                                SynAttribute.create
                                    (SynLongIdent.createS' [ "RestEase" ; "BasePath" ])
                                    (SynExpr.CreateConst path)

                        match plan.CreateMock with
                        | None -> ()
                        | Some isInternal ->
                            yield
                                SynAttribute.create
                                    (SynLongIdent.createS' [ "WoofWare" ; "Myriad" ; "Plugins" ; "GenerateMock" ])
                                    (SynExpr.CreateConst isInternal)
                    ]

                SynComponentInfo.create (Ident.create plan.InterfaceName)
                |> SynComponentInfo.withDocString (
                    plan.Description
                    |> Option.defaultValue "HTTP client generated from an OpenAPI 3.0 document."
                    |> PreXmlDoc.create
                )
                |> SynComponentInfo.addAttributes attributes
            )

        [
            SynModuleDecl.Open (
                SynOpenDeclTarget.ModuleOrNamespace (
                    SynLongIdent.createS' [ "WoofWare" ; "Myriad" ; "Plugins" ],
                    range0
                ),
                range0
            )
            typeDeclarations
            SynModuleDecl.createTypes [ interfaceType ]
        ]
        |> SynModuleOrNamespace.createNamespace [ Ident.create plan.Namespace ]
        |> List.singleton
        |> Output.Ast

    let generate (parameters : Map<string, string>) (source : string) : Output =
        match parseAndPlan parameters source with
        | Ok plan -> renderPlan plan
        | Error diagnostics ->
            diagnostics
            |> List.map (fun value -> $"[%O{value.Code}] %s{value.Location}: %s{value.Message}")
            |> String.concat Environment.NewLine
            |> failwith
