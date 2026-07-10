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

    type private AdditionalProperties =
        | Any
        | Forbidden
        | Typed of LocatedObject

    type private ObjectShape =
        {
            Description : string option
            Properties : Map<string, LocatedObject option>
            Required : Set<string>
            AdditionalProperties : AdditionalProperties
        }

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
            Schema : LocatedObject
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

    let rec private canonicalJson (node : JsonNode) : string =
        if isNull node then
            "null"
        else
            match node with
            | :? JsonObject as value ->
                value
                |> Seq.map (fun (KeyValue (name, child)) ->
                    let name = JsonSerializer.Serialize<string> name
                    $"%s{name}:%s{canonicalJson child}"
                )
                |> Seq.sort
                |> String.concat ","
                |> fun contents -> "{" + contents + "}"
            | :? JsonArray as value ->
                value
                |> Seq.map canonicalJson
                |> String.concat ","
                |> fun contents -> $"[%s{contents}]"
            | value -> value.ToJsonString ()

    /// A canonical description of the F# type shape which this generator emits for a schema.
    /// OpenAPI annotations and validation constraints are intentionally absent: until the generated
    /// type represents them, they cannot make two generated record definitions distinct.
    let rec private schemaShapeKey (includeTopLevelNullable : bool) (node : JsonNode) : string =
        let quoted (value : string) = JsonSerializer.Serialize<string> value

        let tryNode (node : JsonObject) (name : string) =
            match node.TryGetPropertyValue name with
            | true, value when not (isNull value) -> Some value
            | _ -> None

        let tryStringValue (node : JsonNode) =
            try
                Some (node.GetValue<string> ())
            with
            | :? InvalidOperationException
            | :? FormatException -> None

        let tryBoolValue (node : JsonNode) =
            try
                Some (node.GetValue<bool> ())
            with
            | :? InvalidOperationException
            | :? FormatException -> None

        match node with
        | :? JsonObject as value ->
            match tryNode value "$ref" |> Option.bind tryStringValue with
            | Some reference ->
                let schemaPrefix = "#/components/schemas/"

                let reference =
                    if reference.StartsWith (schemaPrefix, StringComparison.Ordinal) then
                        reference.Substring schemaPrefix.Length
                        |> Uri.UnescapeDataString
                        |> fun value -> value.Replace ("~1", "/", StringComparison.Ordinal)
                        |> fun value -> value.Replace ("~0", "~", StringComparison.Ordinal)
                        |> fun value -> schemaPrefix + value
                    else
                        reference

                $"ref(%s{quoted reference})"
            | None ->
                let typeName = tryNode value "type" |> Option.bind tryStringValue

                let isObject =
                    typeName = Some "object"
                    || (typeName.IsNone
                        && (value.ContainsKey "properties"
                            || value.ContainsKey "required"
                            || value.ContainsKey "additionalProperties"
                            || value.ContainsKey "allOf"))

                let nullable =
                    includeTopLevelNullable
                    && ((tryNode value "nullable" |> Option.bind tryBoolValue = Some true)
                        || (typeName.IsNone && not isObject))

                let core =
                    if isObject && value.ContainsKey "allOf" then
                        match tryNode value "allOf" with
                        | Some (:? JsonArray as branches) ->
                            branches
                            |> Seq.map (schemaShapeKey true)
                            |> Seq.sort
                            |> String.concat ","
                            |> fun branches -> $"allOf[%s{branches}]"
                        | Some invalid -> $"invalidAllOf(%s{canonicalJson invalid})"
                        | None -> "invalidAllOf(null)"
                    elif isObject then
                        let properties =
                            match tryNode value "properties" with
                            | Some (:? JsonObject as properties) ->
                                properties
                                |> Seq.map (fun (KeyValue (name, schema)) ->
                                    let schema = if isNull schema then "json" else schemaShapeKey true schema

                                    $"%s{quoted name}:%s{schema}"
                                )
                                |> Seq.sort
                                |> String.concat ","
                            | Some invalid -> $"invalid(%s{canonicalJson invalid})"
                            | None -> ""

                        let required =
                            match tryNode value "required" with
                            | Some (:? JsonArray as required) ->
                                required
                                |> Seq.map (fun item ->
                                    if isNull item then
                                        "null"
                                    else
                                        match tryStringValue item with
                                        | Some item -> quoted item
                                        | None -> canonicalJson item
                                )
                                |> Seq.distinct
                                |> Seq.sort
                                |> String.concat ","
                            | Some invalid -> $"invalid(%s{canonicalJson invalid})"
                            | None -> ""

                        let additionalProperties =
                            match tryNode value "additionalProperties" with
                            | None -> "any"
                            | Some (:? JsonObject as schema) -> $"typed(%s{schemaShapeKey true schema})"
                            | Some other ->
                                match tryBoolValue other with
                                | Some true -> "any"
                                | Some false -> "forbidden"
                                | None -> $"invalid(%s{canonicalJson other})"

                        $"object(properties=(%s{properties});required=[%s{required}];additional=%s{additionalProperties})"
                    else
                        match typeName with
                        | None -> "json"
                        | Some "string" ->
                            match tryNode value "format" |> Option.bind tryStringValue with
                            | Some "date" -> "date"
                            | Some "date-time" -> "date-time"
                            | Some "uuid" -> "guid"
                            | _ -> "string"
                        | Some "boolean" -> "bool"
                        | Some "integer" ->
                            match tryNode value "format" |> Option.bind tryStringValue with
                            | Some "int32" -> "int32"
                            | Some "int64" -> "int64"
                            | _ -> "bigint"
                        | Some "number" ->
                            match tryNode value "format" |> Option.bind tryStringValue with
                            | Some "float" -> "float32"
                            | Some "double" -> "float"
                            | Some "decimal" -> "decimal"
                            | Some format -> $"unsupported-number(%s{quoted format})"
                            | None -> "unsupported-number(unformatted)"
                        | Some "array" ->
                            match tryNode value "items" with
                            | Some items -> $"list(%s{schemaShapeKey true items})"
                            | None -> "list(json)"
                        | Some other -> $"unsupported(%s{quoted other})"

                if nullable then $"optional(%s{core})" else core
        | invalid -> $"invalid(%s{canonicalJson invalid})"

    let private objectShapeKey (shape : ObjectShape) : string =
        let quoted (value : string) = JsonSerializer.Serialize<string> value

        let properties =
            shape.Properties
            |> Map.toSeq
            |> Seq.map (fun (name, schema) ->
                let schema =
                    match schema with
                    | None -> "optional(json)"
                    | Some schema -> schemaShapeKey true schema.Value

                $"%s{quoted name}:%s{schema}"
            )
            |> String.concat ","

        let required = shape.Required |> Seq.map quoted |> String.concat ","

        let additionalProperties =
            match shape.AdditionalProperties with
            | AdditionalProperties.Any -> "any"
            | AdditionalProperties.Forbidden -> "forbidden"
            | AdditionalProperties.Typed schema -> $"typed(%s{schemaShapeKey true schema.Value})"

        $"object(properties=(%s{properties});required=[%s{required}];additional=%s{additionalProperties})"

    let private parseDocument
        (parameters : Map<string, string>)
        (root : JsonObject)
        : Result<OpenApiClientPlan, OpenApiGenerationDiagnostic list>
        =
        let diagnostics = ResizeArray<OpenApiGenerationDiagnostic> ()

        let report code location message =
            diagnostics.Add (diagnostic code location message)

        let tryProperty (location : string) (node : JsonObject) (name : string) : JsonNode option =
            let propertyLocation = $"%s{location}/%s{pointerToken name}"

            match node.TryGetPropertyValue name with
            | false, _ -> None
            | true, value when isNull value ->
                report InvalidDocument propertyLocation "An optional property cannot be null."
                None
            | true, value -> Some value

        let tryString (location : string) (node : JsonNode) : string option =
            try
                Some (node.GetValue<string> ())
            with
            | :? InvalidOperationException
            | :? FormatException ->
                report InvalidDocument location "Expected a JSON string."
                None

        let optionalString (location : string) (node : JsonObject) (name : string) : string option =
            tryProperty location node name
            |> Option.bind (tryString ($"%s{location}/%s{pointerToken name}"))

        let requiredString (location : string) (node : JsonObject) (name : string) : string option =
            let propertyLocation = $"%s{location}/%s{pointerToken name}"

            match node.TryGetPropertyValue name with
            | false, _ ->
                report InvalidDocument propertyLocation "A required string property is missing."
                None
            | true, value when isNull value ->
                report InvalidDocument propertyLocation "A required string property cannot be null."
                None
            | true, value -> tryString propertyLocation value

        let tryBool (location : string) (node : JsonNode) : bool option =
            try
                Some (node.GetValue<bool> ())
            with
            | :? InvalidOperationException
            | :? FormatException ->
                report InvalidDocument location "Expected a JSON boolean."
                None

        let optionalBool (location : string) (node : JsonObject) (name : string) : bool option =
            tryProperty location node name
            |> Option.bind (tryBool ($"%s{location}/%s{pointerToken name}"))

        let tryObject (location : string) (node : JsonNode) : LocatedObject option =
            match node with
            | :? JsonObject as value ->
                {
                    Value = value
                    Location = location
                }
                |> Some
            | _ ->
                report InvalidDocument location "Expected a JSON object."
                None

        let optionalObject (location : string) (node : JsonObject) (name : string) : LocatedObject option =
            tryProperty location node name
            |> Option.bind (tryObject ($"%s{location}/%s{pointerToken name}"))

        let tryArray (location : string) (node : JsonNode) : JsonArray option =
            match node with
            | :? JsonArray as value -> Some value
            | _ ->
                report InvalidDocument location "Expected a JSON array."
                None

        let optionalArray (location : string) (node : JsonObject) (name : string) : JsonArray option =
            tryProperty location node name
            |> Option.bind (tryArray ($"%s{location}/%s{pointerToken name}"))

        let objectMap (location : string) (node : JsonObject) : Map<string, LocatedObject> =
            node
            |> Seq.choose (fun (KeyValue (name, value)) ->
                tryObject ($"%s{location}/%s{pointerToken name}") value
                |> Option.map (fun value -> name, value)
            )
            |> Map.ofSeq

        let componentMap (components : LocatedObject option) (name : string) : Map<string, LocatedObject> =
            match
                components
                |> Option.bind (fun value -> optionalObject value.Location value.Value name)
            with
            | None -> Map.empty
            | Some values -> objectMap values.Location values.Value

        let decodePointerToken
            (code : OpenApiGenerationDiagnosticCode)
            (location : string)
            (value : string)
            : string option
            =
            let value = Uri.UnescapeDataString value

            if Regex.IsMatch (value, "~(?:[^01]|$)") then
                report code location $"Reference token '%s{value}' contains an invalid JSON Pointer escape."
                None
            else
                let value = value.Replace ("~1", "/", StringComparison.Ordinal)
                value.Replace ("~0", "~", StringComparison.Ordinal) |> Some

        let referenceName
            (code : OpenApiGenerationDiagnosticCode)
            (expectedPrefix : string)
            (referenceLocation : string)
            (reference : string)
            : string option
            =
            if reference.StartsWith (expectedPrefix, StringComparison.Ordinal) then
                reference.Substring expectedPrefix.Length
                |> decodePointerToken code referenceLocation
            else
                report
                    code
                    referenceLocation
                    $"Only local references below '%s{expectedPrefix}' are supported; got '%s{reference}'."

                None

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
        let schemaComponents = componentMap components "schemas"
        let parameterComponents = componentMap components "parameters"
        let requestBodyComponents = componentMap components "requestBodies"
        let responseComponents = componentMap components "responses"

        let rawReference (schema : LocatedObject) : (string * string) option =
            optionalString schema.Location schema.Value "$ref"
            |> Option.map (fun value -> $"%s{schema.Location}/$ref", value)

        let rec isObjectLike (visited : Set<string>) (schema : LocatedObject) : bool =
            match rawReference schema with
            | Some (location, reference) ->
                match referenceName UnsupportedSchema "#/components/schemas/" location reference with
                | None -> false
                | Some name when Set.contains name visited -> false
                | Some name ->
                    match Map.tryFind name schemaComponents with
                    | None -> false
                    | Some target -> isObjectLike (Set.add name visited) target
            | None ->
                match optionalString schema.Location schema.Value "type" with
                | Some "object" -> true
                | Some _ -> false
                | None ->
                    schema.Value.ContainsKey "allOf"
                    || schema.Value.ContainsKey "properties"
                    || schema.Value.ContainsKey "required"
                    || schema.Value.ContainsKey "additionalProperties"

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
            |> List.choose (fun (name, schema) -> if isObjectLike Set.empty schema then Some name else None)

        let componentTypeNames =
            objectComponentNames
            |> List.map (fun sourceName ->
                let fsharpName =
                    allocateUniqueName usedTypeNames "GeneratedType" sanitiseTypeName sourceName

                sourceName, fsharpName
            )
            |> Map.ofList

        let definitions = ResizeArray<OpenApiPlannedTypeDefinition> ()

        let liftedObjectTypes =
            System.Collections.Generic.Dictionary<string, string> (StringComparer.Ordinal)

        let rec schemaNullableInner (visited : Set<string>) (schema : LocatedObject) : bool =
            match rawReference schema with
            | Some (location, reference) ->
                match referenceName UnresolvedReference "#/components/schemas/" location reference with
                | None -> false
                | Some name when Set.contains name visited -> false
                | Some name ->
                    match Map.tryFind name schemaComponents with
                    | None -> false
                    | Some target -> schemaNullableInner (Set.add name visited) target
            | None ->
                optionalBool schema.Location schema.Value "nullable"
                |> Option.defaultValue false

        let schemaNullable (schema : LocatedObject) : bool = schemaNullableInner Set.empty schema

        let rec schemaAllowsNullInner (visited : Set<string>) (schema : LocatedObject) : bool =
            match rawReference schema with
            | Some (location, reference) ->
                match referenceName UnresolvedReference "#/components/schemas/" location reference with
                | None -> false
                | Some name when Set.contains name visited -> false
                | Some name ->
                    match Map.tryFind name schemaComponents with
                    | None -> false
                    | Some target -> schemaAllowsNullInner (Set.add name visited) target
            | None ->
                match optionalArray schema.Location schema.Value "allOf" with
                | Some branches ->
                    let outerAllowsNull =
                        match optionalString schema.Location schema.Value "type" with
                        | None -> true
                        | Some _ -> schemaNullable schema

                    let branchNullability =
                        branches
                        |> Seq.mapi (fun index branch ->
                            tryObject ($"%s{schema.Location}/allOf/%i{index}") branch
                            |> Option.map (schemaAllowsNullInner visited)
                        )
                        |> Seq.toList

                    outerAllowsNull
                    && branchNullability.Length = branches.Count
                    && (branchNullability |> List.forall (Option.defaultValue false))
                | None ->
                    if not (schema.Value.ContainsKey "type") && not (isObjectLike Set.empty schema) then
                        // An unconstrained OpenAPI 3.0 Schema Object accepts every JSON value, including null.
                        true
                    else
                        schemaNullable schema

        let schemaAllowsNull (schema : LocatedObject) : bool = schemaAllowsNullInner Set.empty schema

        let reportedUnsupportedSchemaKeywords = HashSet<string> (StringComparer.Ordinal)

        let validateSchemaKeywords (schema : LocatedObject) =
            let unsupportedKeywords =
                [ "oneOf" ; "anyOf" ; "not" ; "discriminator" ]
                |> List.filter schema.Value.ContainsKey

            if not unsupportedKeywords.IsEmpty then
                let keywordKey = String.concat "," unsupportedKeywords
                let reportKey = $"%s{schema.Location}|%s{keywordKey}"

                if reportedUnsupportedSchemaKeywords.Add reportKey then
                    let unsupportedKeywords = String.concat ", " unsupportedKeywords

                    report
                        UnsupportedSchema
                        schema.Location
                        $"Unsupported shape-changing schema keyword(s): %s{unsupportedKeywords}."

            match
                optionalBool schema.Location schema.Value "readOnly",
                optionalBool schema.Location schema.Value "writeOnly"
            with
            | Some true, _
            | _, Some true ->
                let reportKey = $"%s{schema.Location}|readOnly/writeOnly"

                if reportedUnsupportedSchemaKeywords.Add reportKey then
                    report
                        UnsupportedSchema
                        schema.Location
                        "readOnly/writeOnly schemas require separate request and response projections."
            | _ -> ()

            let hasObjectKeywords =
                schema.Value.ContainsKey "properties"
                || schema.Value.ContainsKey "required"
                || schema.Value.ContainsKey "additionalProperties"
                || schema.Value.ContainsKey "allOf"

            match optionalString schema.Location schema.Value "type" with
            | Some value when value <> "object" && hasObjectKeywords ->
                let reportKey = $"%s{schema.Location}|contradictory-type"

                if reportedUnsupportedSchemaKeywords.Add reportKey then
                    report
                        UnsupportedSchema
                        ($"%s{schema.Location}/type")
                        $"Schema type '%s{value}' contradicts its object-shape keywords."
            | _ -> ()

        let validatedSchemaLocations = HashSet<string> (StringComparer.Ordinal)

        let rec validateSchemaTree (schema : LocatedObject) =
            if validatedSchemaLocations.Add schema.Location then
                match rawReference schema with
                | Some _ -> ()
                | None ->
                    validateSchemaKeywords schema
                    optionalString schema.Location schema.Value "format" |> ignore
                    optionalString schema.Location schema.Value "description" |> ignore
                    optionalBool schema.Location schema.Value "nullable" |> ignore

                    match optionalObject schema.Location schema.Value "properties" with
                    | None -> ()
                    | Some properties ->
                        for KeyValue (name, value) in properties.Value do
                            tryObject ($"%s{properties.Location}/%s{pointerToken name}") value
                            |> Option.iter validateSchemaTree

                    match optionalArray schema.Location schema.Value "required" with
                    | None -> ()
                    | Some required ->
                        required
                        |> Seq.iteri (fun index value ->
                            tryString ($"%s{schema.Location}/required/%i{index}") value |> ignore
                        )

                    match optionalObject schema.Location schema.Value "items" with
                    | None -> ()
                    | Some items -> validateSchemaTree items

                    match tryProperty schema.Location schema.Value "additionalProperties" with
                    | None -> ()
                    | Some (:? JsonObject as value) ->
                        validateSchemaTree
                            {
                                Value = value
                                Location = $"%s{schema.Location}/additionalProperties"
                            }
                    | Some value -> tryBool ($"%s{schema.Location}/additionalProperties") value |> ignore

                    match optionalArray schema.Location schema.Value "allOf" with
                    | None -> ()
                    | Some branches ->
                        branches
                        |> Seq.iteri (fun index value ->
                            tryObject ($"%s{schema.Location}/allOf/%i{index}") value
                            |> Option.iter validateSchemaTree
                        )

        for KeyValue (_, schema) in schemaComponents do
            validateSchemaTree schema

        let rec typeForSchema
            (aliasStack : Set<string>)
            (suggestedName : string)
            (schema : LocatedObject)
            : OpenApiPlannedType
            =
            validateSchemaTree schema

            match rawReference schema with
            | Some (location, reference) ->
                match referenceName UnresolvedReference "#/components/schemas/" location reference with
                | None -> OpenApiPlannedType.JsonNode
                | Some name ->
                    match Map.tryFind name schemaComponents with
                    | None ->
                        report UnresolvedReference location $"Schema component '%s{name}' does not exist."
                        OpenApiPlannedType.JsonNode
                    | Some target ->
                        match Map.tryFind name componentTypeNames with
                        | Some typeName ->
                            let result = OpenApiPlannedType.Named typeName

                            if schemaAllowsNull target then
                                OpenApiPlannedType.Optional result
                            else
                                result
                        | None when Set.contains name aliasStack ->
                            report
                                UnsupportedSchema
                                location
                                $"Non-object schema reference cycle involving '%s{name}' is unsupported."

                            OpenApiPlannedType.JsonNode
                        | None -> typeForSchema (Set.add name aliasStack) suggestedName target
            | None ->
                validateSchemaKeywords schema

                let baseType =
                    if isObjectLike Set.empty schema then
                        liftObject suggestedName schema
                    else
                        match optionalString schema.Location schema.Value "type" with
                        | None -> OpenApiPlannedType.JsonNode
                        | Some "string" ->
                            match optionalString schema.Location schema.Value "format" with
                            | Some "date" -> OpenApiPlannedType.Primitive OpenApiPrimitive.Date
                            | Some "date-time" -> OpenApiPlannedType.Primitive OpenApiPrimitive.DateTime
                            | Some "uuid" -> OpenApiPlannedType.Primitive OpenApiPrimitive.Guid
                            | _ -> OpenApiPlannedType.Primitive OpenApiPrimitive.String
                        | Some "boolean" -> OpenApiPlannedType.Primitive OpenApiPrimitive.Boolean
                        | Some "integer" ->
                            match optionalString schema.Location schema.Value "format" with
                            | Some "int32" -> OpenApiPlannedType.Primitive OpenApiPrimitive.Int32
                            | Some "int64" -> OpenApiPlannedType.Primitive OpenApiPrimitive.Int64
                            | _ -> OpenApiPlannedType.Primitive OpenApiPrimitive.BigInteger
                        | Some "number" ->
                            match optionalString schema.Location schema.Value "format" with
                            | Some "float" -> OpenApiPlannedType.Primitive OpenApiPrimitive.Float32
                            | Some "double" -> OpenApiPlannedType.Primitive OpenApiPrimitive.Float
                            | Some "decimal" -> OpenApiPlannedType.Primitive OpenApiPrimitive.Decimal
                            | format ->
                                report
                                    UnsupportedSchema
                                    ($"%s{schema.Location}/format")
                                    (match format with
                                     | None -> "Unformatted JSON numbers have no lossless built-in F# representation."
                                     | Some format ->
                                         $"Number format '%s{format}' has no lossless built-in F# representation.")

                                OpenApiPlannedType.JsonNode
                        | Some "array" ->
                            match optionalObject schema.Location schema.Value "items" with
                            | None ->
                                report
                                    UnsupportedSchema
                                    ($"%s{schema.Location}/items")
                                    "Array schemas must specify items."

                                OpenApiPlannedType.List OpenApiPlannedType.JsonNode
                            | Some items ->
                                typeForSchema aliasStack ($"%s{suggestedName}Item") items
                                |> OpenApiPlannedType.List
                        | Some value ->
                            report
                                UnsupportedSchema
                                ($"%s{schema.Location}/type")
                                $"Schema type '%s{value}' is unsupported."

                            OpenApiPlannedType.JsonNode

                if schemaAllowsNull schema then
                    OpenApiPlannedType.Optional baseType
                else
                    baseType

        and liftObject (suggestedName : string) (schema : LocatedObject) : OpenApiPlannedType =
            // Planning every occurrence preserves diagnostics even when its emitted definition is shared.
            let shape = collectObjectShape Set.empty schema
            // Nullability and annotations wrap or document each use; the flattened record shape is shared.
            let key = objectShapeKey shape

            match liftedObjectTypes.TryGetValue key with
            | true, typeName -> OpenApiPlannedType.Named typeName
            | false, _ ->
                let typeName =
                    allocateUniqueName usedTypeNames "AnonymousType" sanitiseTypeName suggestedName

                liftedObjectTypes.Add (key, typeName)
                buildDefinition suggestedName schema.Location typeName shape |> definitions.Add
                OpenApiPlannedType.Named typeName

        and buildDefinition
            (sourceName : string)
            (sourceLocation : string)
            (typeName : string)
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
                            let allowsNull = schemaAllowsNull propertySchema

                            if allowsNull && not required then
                                report
                                    UnsupportedSchema
                                    propertySchema.Location
                                    "An optional property whose schema allows null has three wire states (missing, null, value), which this generated API does not conflate."

                            let result = typeForSchema Set.empty ($"%s{typeName}%s{fsharpName}") propertySchema

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
                    typeForSchema Set.empty ($"%s{typeName}AdditionalProperty") schema |> Some

            {
                SourceName = sourceName
                FSharpName = typeName
                Description = shape.Description
                Fields = fields
                AdditionalProperties = additionalProperties
            }

        and collectObjectShape (compositionStack : Set<string>) (schema : LocatedObject) : ObjectShape =
            match rawReference schema with
            | Some (location, reference) ->
                match referenceName UnresolvedReference "#/components/schemas/" location reference with
                | None -> emptyObjectShape None
                | Some name when Set.contains name compositionStack ->
                    report UnsupportedSchema location $"Object composition cycle involving '%s{name}' is unsupported."
                    emptyObjectShape None
                | Some name ->
                    match Map.tryFind name schemaComponents with
                    | None ->
                        report UnresolvedReference location $"Schema component '%s{name}' does not exist."
                        emptyObjectShape None
                    | Some target -> collectObjectShape (Set.add name compositionStack) target
            | None ->
                validateSchemaKeywords schema

                match optionalArray schema.Location schema.Value "allOf" with
                | Some branches ->
                    let shapes =
                        branches
                        |> Seq.mapi (fun index node ->
                            tryObject ($"%s{schema.Location}/allOf/%i{index}") node
                            |> Option.map (fun branch ->
                                if not (isObjectLike Set.empty branch) then
                                    report
                                        UnsupportedSchema
                                        branch.Location
                                        "Only object-shaped allOf branches can be represented as an F# record."

                                collectObjectShape compositionStack branch
                            )
                        )
                        |> Seq.choose id
                        |> Seq.toList

                    if
                        schema.Value.ContainsKey "properties"
                        || schema.Value.ContainsKey "required"
                        || schema.Value.ContainsKey "additionalProperties"
                    then
                        report
                            UnsupportedSchema
                            schema.Location
                            "An allOf schema with sibling object-shape keywords is not currently supported."

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
                                        UnsupportedSchema
                                        schema.Location
                                        "allOf cannot merge fields introduced outside a branch with constrained additionalProperties."

                        ensureNoForbiddenIntroductions left right
                        ensureNoForbiddenIntroductions right left

                        let properties =
                            (left.Properties, right.Properties)
                            ||> Map.fold (fun current name schema ->
                                match Map.tryFind name current, schema with
                                | None, _ -> Map.add name schema current
                                | Some None, Some value -> Map.add name (Some value) current
                                | Some None, None -> current
                                | Some (Some _), None -> current
                                | Some (Some existing), Some value ->
                                    if schemaShapeKey true existing.Value <> schemaShapeKey true value.Value then
                                        report
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
                                schemaShapeKey true left.Value = schemaShapeKey true right.Value
                                ->
                                AdditionalProperties.Typed left
                            | _ ->
                                report
                                    UnsupportedSchema
                                    schema.Location
                                    "allOf branches have incompatible additionalProperties constraints."

                                AdditionalProperties.Forbidden

                        {
                            Description = None
                            Properties = properties
                            Required = Set.union left.Required right.Required
                            AdditionalProperties = additionalProperties
                        }

                    let description = optionalString schema.Location schema.Value "description"

                    match shapes with
                    | [] -> emptyObjectShape description
                    | head :: tail ->
                        { List.fold merge head tail with
                            Description = description
                        }
                | None ->
                    let properties =
                        match optionalObject schema.Location schema.Value "properties" with
                        | None -> Map.empty
                        | Some properties ->
                            properties.Value
                            |> Seq.choose (fun (KeyValue (name, value)) ->
                                tryObject ($"%s{properties.Location}/%s{pointerToken name}") value
                                |> Option.map (fun value -> name, Some value)
                            )
                            |> Map.ofSeq

                    let required =
                        match optionalArray schema.Location schema.Value "required" with
                        | None -> Set.empty
                        | Some values ->
                            values
                            |> Seq.mapi (fun index value -> tryString ($"%s{schema.Location}/required/%i{index}") value)
                            |> Seq.choose id
                            |> Set.ofSeq

                    let additionalProperties =
                        match tryProperty schema.Location schema.Value "additionalProperties" with
                        | None -> AdditionalProperties.Any
                        | Some (:? JsonObject as value) ->
                            AdditionalProperties.Typed
                                {
                                    Value = value
                                    Location = $"%s{schema.Location}/additionalProperties"
                                }
                        | Some value ->
                            match tryBool ($"%s{schema.Location}/additionalProperties") value with
                            | Some true -> AdditionalProperties.Any
                            | Some false -> AdditionalProperties.Forbidden
                            | None -> AdditionalProperties.Any

                    {
                        Description = optionalString schema.Location schema.Value "description"
                        Properties = properties
                        Required = required
                        AdditionalProperties = additionalProperties
                    }

        and emptyObjectShape (description : string option) : ObjectShape =
            {
                Description = description
                Properties = Map.empty
                Required = Set.empty
                AdditionalProperties = AdditionalProperties.Any
            }

        for sourceName in objectComponentNames do
            let schema = schemaComponents.[sourceName]
            let shape = collectObjectShape (Set.singleton sourceName) schema
            let typeName = componentTypeNames.[sourceName]
            buildDefinition sourceName schema.Location typeName shape |> definitions.Add

        let rec resolveComponentReference
            (diagnosticCode : OpenApiGenerationDiagnosticCode)
            (prefix : string)
            (components : Map<string, LocatedObject>)
            (visited : Set<string>)
            (value : LocatedObject)
            : LocatedObject option
            =
            match rawReference value with
            | None -> Some value
            | Some (location, reference) ->
                match referenceName diagnosticCode prefix location reference with
                | None -> None
                | Some name when Set.contains name visited ->
                    report diagnosticCode location $"Reference cycle involving '%s{name}' is unsupported here."
                    None
                | Some name ->
                    match Map.tryFind name components with
                    | None ->
                        report diagnosticCode location $"Component '%s{name}' does not exist."
                        None
                    | Some target ->
                        resolveComponentReference diagnosticCode prefix components (Set.add name visited) target

        let parseParameter (value : LocatedObject) : ResolvedParameter option =
            resolveComponentReference UnresolvedReference "#/components/parameters/" parameterComponents Set.empty value
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

                let schema = optionalObject value.Location value.Value "schema"
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

        let parseParameterList (owner : LocatedObject) : ResolvedParameter list =
            match optionalArray owner.Location owner.Value "parameters" with
            | None -> []
            | Some values ->
                let parsed =
                    values
                    |> Seq.mapi (fun index value ->
                        tryObject ($"%s{owner.Location}/parameters/%i{index}") value
                        |> Option.bind parseParameter
                    )
                    |> Seq.choose id
                    |> Seq.toList

                parsed
                |> List.groupBy (fun parameter -> parameter.Name, parameter.Location)
                |> List.iter (fun ((name, _), values) ->
                    if values.Length > 1 then
                        report
                            InvalidDocument
                            owner.Location
                            $"Parameter '%s{name}' is duplicated at the same location."
                )

                parsed

        let mergeParameters
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

        let mediaTypeWithoutParameters (name : string) : string =
            match name.IndexOf ';' with
            | -1 -> name.Trim ()
            | separator -> (name.Substring (0, separator)).Trim ()

        let mediaTypeEquals (expected : string) (actual : string) : bool =
            (mediaTypeWithoutParameters actual).Equals (expected, StringComparison.OrdinalIgnoreCase)

        let selectMedia (purpose : string) (content : LocatedObject) : (string * LocatedObject option) option =
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
                let selectedSchema = optionalObject selected.Location selected.Value "schema"
                Some (mediaTypeWithoutParameters selectedName, selectedSchema)

        let rec isJsonStringType (plannedType : OpenApiPlannedType) : bool =
            match plannedType with
            | OpenApiPlannedType.Primitive OpenApiPrimitive.String -> true
            | OpenApiPlannedType.Optional inner -> isJsonStringType inner
            | _ -> false

        let isJsonMediaType (mediaType : string) : bool =
            let mediaType = mediaTypeWithoutParameters mediaType

            mediaType.Equals ("application/json", StringComparison.OrdinalIgnoreCase)
            || mediaType.EndsWith ("+json", StringComparison.OrdinalIgnoreCase)

        let responseShape (operationName : string) (value : LocatedObject) : OpenApiPlannedType * string option =
            let value =
                resolveComponentReference
                    UnresolvedReference
                    "#/components/responses/"
                    responseComponents
                    Set.empty
                    value

            match value with
            | None -> OpenApiPlannedType.JsonNode, None
            | Some value ->
                match optionalObject value.Location value.Value "content" with
                | None -> OpenApiPlannedType.Unit, None
                | Some content when content.Value.Count = 0 -> OpenApiPlannedType.Unit, None
                | Some content ->
                    match selectMedia "a response" content with
                    | None -> OpenApiPlannedType.JsonNode, None
                    | Some (mediaType, schema) ->
                        let result =
                            if mediaTypeEquals "application/octet-stream" mediaType then
                                match schema with
                                | None -> ()
                                | Some schema ->
                                    match typeForSchema Set.empty ($"%s{operationName}BinaryResponse") schema with
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
                                    match typeForSchema Set.empty ($"%s{operationName}TextResponse") schema with
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
                                | Some schema -> typeForSchema Set.empty ($"%s{operationName}Response") schema

                        if isJsonMediaType mediaType && isJsonStringType result then
                            report
                                UnsupportedOperation
                                content.Location
                                "JSON string responses need JSON unquoting, which the generated HTTP shell cannot distinguish from text/plain."

                        result, Some mediaType

        let successfulResponses (operationName : string) (responses : LocatedObject) =
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
                let firstShape = responseShape operationName first

                for status, response in rest do
                    let otherShape = responseShape operationName response

                    if otherShape <> firstShape then
                        report
                            AmbiguousSuccessResponse
                            ($"%s{responses.Location}/%s{pointerToken status}")
                            "All possible successful responses must have the same body type and media type."

                firstShape

        let requestBodyParameter
            (operationName : string)
            (operation : LocatedObject)
            : (OpenApiPlannedParameter * string) option
            =
            match optionalObject operation.Location operation.Value "requestBody" with
            | None -> None
            | Some body ->
                let body =
                    resolveComponentReference
                        UnresolvedReference
                        "#/components/requestBodies/"
                        requestBodyComponents
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
                        selectMedia "a request body" content
                        |> Option.map (fun (mediaType, schema) ->
                            let plannedType =
                                if mediaTypeEquals "application/octet-stream" mediaType then
                                    OpenApiPlannedType.Stream
                                elif mediaTypeEquals "text/plain" mediaType then
                                    match schema with
                                    | None -> ()
                                    | Some schema ->
                                        match typeForSchema Set.empty ($"%s{operationName}TextRequest") schema with
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
                                    | Some schema -> typeForSchema Set.empty ($"%s{operationName}Request") schema

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

        let parseServerBase () : OpenApiServerBase =
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

                    let inheritedParameters = parseParameterList pathItem

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
                            parseParameterList operation |> mergeParameters inheritedParameters

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

                                    if schemaAllowsNull parameter.Schema then
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

                        let body = requestBodyParameter operationFSharpName operation

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
                            | Some responses -> successfulResponses operationFSharpName responses

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

        let plan =
            {
                Namespace = className
                InterfaceName = "I" + className
                Description = description
                CreateMock = createMock
                ServerBase = parseServerBase ()
                Types = orderedDefinitions |> Seq.toList
                Operations = operations |> Seq.sortBy _.FSharpName |> Seq.toList
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
