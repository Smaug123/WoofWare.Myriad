namespace WoofWare.Myriad.Plugins.Test

open System
open System.Collections.Generic
open System.Text.Json.Nodes
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.Myriad.Plugins

[<TestFixture>]
module TestOpenApi3Generator =

    type private GeneratedScalar =
        | String
        | Boolean
        | Int32
        | Int64
        | Float32
        | Float
        | Decimal
        | Date
        | DateTime
        | Guid

    type private GeneratedField =
        {
            Name : string
            Scalar : GeneratedScalar
            Required : bool
            Nullable : bool
        }

    let private jsonString (value : string) : JsonNode = JsonValue.Create value
    let private jsonBool (value : bool) : JsonNode = JsonValue.Create value

    let private jsonObject (properties : (string * JsonNode) list) : JsonNode =
        let result = JsonObject ()

        for name, value in properties do
            result.Add (name, value)

        result :> JsonNode

    let private jsonArray (values : JsonNode list) : JsonNode =
        JsonArray (values |> List.toArray) :> JsonNode

    let private schemaForScalar (scalar : GeneratedScalar) (nullable : bool) : JsonNode =
        let typeName, format =
            match scalar with
            | GeneratedScalar.String -> "string", None
            | GeneratedScalar.Boolean -> "boolean", None
            | GeneratedScalar.Int32 -> "integer", Some "int32"
            | GeneratedScalar.Int64 -> "integer", Some "int64"
            | GeneratedScalar.Float32 -> "number", Some "float"
            | GeneratedScalar.Float -> "number", Some "double"
            | GeneratedScalar.Decimal -> "number", Some "decimal"
            | GeneratedScalar.Date -> "string", Some "date"
            | GeneratedScalar.DateTime -> "string", Some "date-time"
            | GeneratedScalar.Guid -> "string", Some "uuid"

        jsonObject
            [
                "type", jsonString typeName
                match format with
                | None -> ()
                | Some value -> "format", jsonString value
                if nullable then
                    "nullable", jsonBool true
            ]

    let private reference (target : string) : JsonNode =
        jsonObject [ "$ref", jsonString ("#/components/schemas/" + target) ]

    let private responseWithSchema (schema : JsonNode) : JsonNode =
        jsonObject
            [
                "description", jsonString "success"
                "content", jsonObject [ "application/json", jsonObject [ "schema", schema ] ]
            ]

    let private noContentResponse () : JsonNode =
        jsonObject [ "description", jsonString "success" ]

    let private parameter (name : string) (location : string) (required : bool) (schema : JsonNode) : JsonNode =
        jsonObject
            [
                "name", jsonString name
                "in", jsonString location
                "required", jsonBool required
                "schema", schema
            ]

    let private document
        (version : string)
        (schemas : (string * JsonNode) list)
        (path : string)
        (pathItem : JsonNode)
        : string
        =
        jsonObject
            [
                "openapi", jsonString version
                "info",
                jsonObject
                    [
                        "title", jsonString "Generated API"
                        "description", jsonString "Generated test API"
                        "version", jsonString "1.0.0"
                    ]
                "servers", jsonArray [ jsonObject [ "url", jsonString "/api/v1" ] ]
                "paths", jsonObject [ path, pathItem ]
                "components", jsonObject [ "schemas", jsonObject schemas ]
            ]
        |> _.ToJsonString()

    let private standardPathItem
        (response : JsonNode)
        (pathParameters : JsonNode list)
        (operationParameters : JsonNode list)
        : JsonNode
        =
        jsonObject
            [
                if not pathParameters.IsEmpty then
                    "parameters", jsonArray pathParameters
                "get",
                jsonObject
                    [
                        "operationId", jsonString "getThing"
                        if not operationParameters.IsEmpty then
                            "parameters", jsonArray operationParameters
                        "responses", jsonObject [ "200", response ]
                    ]
            ]

    let private config = Map [ "CLASSNAME", "GeneratedClient" ]

    let private plan (source : string) : OpenApiClientPlan =
        match OpenApiClientGenerator.parseAndPlan config source with
        | Ok value -> value
        | Error diagnostics ->
            diagnostics
            |> List.map (fun diagnostic -> $"%s{diagnostic.Location}: %s{diagnostic.Message}")
            |> String.concat Environment.NewLine
            |> failwith

    let private expectedPrimitive (scalar : GeneratedScalar) : OpenApiPrimitive =
        match scalar with
        | GeneratedScalar.String -> OpenApiPrimitive.String
        | GeneratedScalar.Boolean -> OpenApiPrimitive.Boolean
        | GeneratedScalar.Int32 -> OpenApiPrimitive.Int32
        | GeneratedScalar.Int64 -> OpenApiPrimitive.Int64
        | GeneratedScalar.Float32 -> OpenApiPrimitive.Float32
        | GeneratedScalar.Float -> OpenApiPrimitive.Float
        | GeneratedScalar.Decimal -> OpenApiPrimitive.Decimal
        | GeneratedScalar.Date -> OpenApiPrimitive.Date
        | GeneratedScalar.DateTime -> OpenApiPrimitive.DateTime
        | GeneratedScalar.Guid -> OpenApiPrimitive.Guid

    let private generatedFields : Gen<GeneratedField list> =
        gen {
            let! count = Gen.choose (1, 8)

            let field =
                gen {
                    let! scalar =
                        Gen.elements
                            [
                                GeneratedScalar.String
                                GeneratedScalar.Boolean
                                GeneratedScalar.Int32
                                GeneratedScalar.Int64
                                GeneratedScalar.Float32
                                GeneratedScalar.Float
                                GeneratedScalar.Decimal
                                GeneratedScalar.Date
                                GeneratedScalar.DateTime
                                GeneratedScalar.Guid
                            ]

                    let! required = ArbMap.generate<bool> ArbMap.defaults

                    let! nullable =
                        if required then
                            ArbMap.generate<bool> ArbMap.defaults
                        else
                            Gen.constant false

                    return
                        {
                            Name = "field"
                            Scalar = scalar
                            Required = required
                            Nullable = nullable
                        }
                }

            let! fields = Gen.listOfLength count field

            return
                fields
                |> List.mapi (fun index value ->
                    { value with
                        Name = $"field%i{index}"
                    }
                )
        }

    let private renderObjectDocument (fields : GeneratedField list) : string =
        let properties =
            fields
            |> List.map (fun field -> field.Name, schemaForScalar field.Scalar field.Nullable)

        let required =
            fields
            |> List.choose (fun field ->
                if field.Required then
                    Some (jsonString field.Name)
                else
                    None
            )

        let thingSchema =
            jsonObject
                [
                    "type", jsonString "object"
                    "properties", jsonObject properties
                    if not required.IsEmpty then
                        "required", jsonArray required
                ]

        let pathParameter =
            parameter "id" "path" true (schemaForScalar GeneratedScalar.Int64 false)

        standardPathItem (responseWithSchema (reference "Thing")) [ pathParameter ] []
        |> fun pathItem -> document "3.0.3" [ "Thing", thingSchema ] "/things/{id}" pathItem

    [<Test>]
    let ``Object planning agrees with an independent field oracle across generated schemas`` () =
        let scalarCounts = System.Collections.Generic.Dictionary<GeneratedScalar, int> ()
        let mutable requiredCount = 0
        let mutable optionalCount = 0
        let mutable nullableCount = 0
        let mutable noRequiredDocuments = 0
        let mutable allRequiredDocuments = 0
        let mutable mixedRequiredDocuments = 0
        let mutable documentsWithNullableFields = 0

        let property (fields : GeneratedField list) =
            let requiredInDocument = fields |> List.filter _.Required |> List.length

            if requiredInDocument = 0 then
                noRequiredDocuments <- noRequiredDocuments + 1
            elif requiredInDocument = fields.Length then
                allRequiredDocuments <- allRequiredDocuments + 1
            else
                mixedRequiredDocuments <- mixedRequiredDocuments + 1

            if fields |> List.exists _.Nullable then
                documentsWithNullableFields <- documentsWithNullableFields + 1

            for field in fields do
                scalarCounts.[field.Scalar] <-
                    match scalarCounts.TryGetValue field.Scalar with
                    | true, count -> count + 1
                    | false, _ -> 1

                if field.Required then
                    requiredCount <- requiredCount + 1
                else
                    optionalCount <- optionalCount + 1

                if field.Nullable then
                    nullableCount <- nullableCount + 1

            let actual = renderObjectDocument fields |> plan
            let actualType = actual.Types |> List.exactlyOne
            actualType.SourceName |> shouldEqual "Thing"

            let actualFields =
                actualType.Fields |> List.map (fun field -> field.JsonName, field) |> Map

            actualType.Fields |> List.length |> shouldEqual fields.Length

            actualType.Fields
            |> List.map _.JsonName
            |> Set.ofList
            |> shouldEqual (fields |> List.map _.Name |> Set.ofList)

            for expected in fields do
                let actualField = actualFields.[expected.Name]
                actualField.Required |> shouldEqual expected.Required

                let expectedType =
                    OpenApiPlannedType.Primitive (expectedPrimitive expected.Scalar)
                    |> if expected.Required && not expected.Nullable then
                           id
                       else
                           OpenApiPlannedType.Optional

                actualField.Type |> shouldEqual expectedType

            let reordered = fields |> List.rev |> renderObjectDocument |> plan
            reordered |> shouldEqual actual

        property
        |> Prop.forAll (Arb.fromGen generatedFields)
        |> Check.QuickThrowOnFailure

        for scalar in
            [
                GeneratedScalar.String
                GeneratedScalar.Boolean
                GeneratedScalar.Int32
                GeneratedScalar.Int64
                GeneratedScalar.Float32
                GeneratedScalar.Float
                GeneratedScalar.Decimal
                GeneratedScalar.Date
                GeneratedScalar.DateTime
                GeneratedScalar.Guid
            ] do
            let observed = scalarCounts.GetValueOrDefault scalar

            if observed < 8 then
                failwith $"Generator only exercised %A{scalar} %i{observed} times"

        if requiredCount < 80 || optionalCount < 80 || nullableCount < 30 then
            failwith
                $"Insufficient generated distribution: required=%i{requiredCount}, optional=%i{optionalCount}, nullable=%i{nullableCount}"

        if
            noRequiredDocuments < 3
            || allRequiredDocuments < 3
            || mixedRequiredDocuments < 20
            || documentsWithNullableFields < 10
        then
            failwith
                $"Insufficient document distribution: none-required=%i{noRequiredDocuments}, all-required=%i{allRequiredDocuments}, mixed=%i{mixedRequiredDocuments}, nullable=%i{documentsWithNullableFields}"

    [<Test>]
    let ``Operation parameters override inherited parameters by name and location`` () =
        for scalar in [ GeneratedScalar.String ; GeneratedScalar.Int32 ; GeneratedScalar.Int64 ] do
            for required in [ false ; true ] do
                let inheritedPath =
                    parameter "id" "path" true (schemaForScalar GeneratedScalar.Int64 false)

                let inheritedQuery =
                    parameter "limit" "query" false (schemaForScalar GeneratedScalar.String false)

                let overridingQuery =
                    parameter "limit" "query" required (schemaForScalar scalar false)

                let pathItem =
                    standardPathItem (noContentResponse ()) [ inheritedPath ; inheritedQuery ] [ overridingQuery ]

                let actual = document "3.0.3" [] "/things/{id}" pathItem |> plan
                let operation = actual.Operations |> List.exactlyOne

                operation.Parameters |> List.map _.WireName |> shouldEqual [ "id" ; "limit" ]

                let path = operation.Parameters.[0]
                path.Location |> shouldEqual OpenApiParameterLocation.Path
                path.Required |> shouldEqual true
                path.Type |> shouldEqual (OpenApiPlannedType.Primitive OpenApiPrimitive.Int64)

                let limit = operation.Parameters.[1]
                limit.Location |> shouldEqual OpenApiParameterLocation.Query
                limit.Required |> shouldEqual required

                let expected =
                    OpenApiPlannedType.Primitive (expectedPrimitive scalar)
                    |> if required then id else OpenApiPlannedType.Optional

                limit.Type |> shouldEqual expected

    [<Test>]
    let ``Parameters cannot shadow the generated HTTP client binding`` () =
        let clientParameter =
            parameter "client" "path" true (schemaForScalar GeneratedScalar.String false)

        let pathItem = standardPathItem (noContentResponse ()) [ clientParameter ] []
        let actual = document "3.0.3" [] "/things/{client}" pathItem |> plan

        let plannedParameter =
            actual.Operations |> List.exactlyOne |> _.Parameters |> List.exactlyOne

        plannedParameter.WireName |> shouldEqual "client"
        plannedParameter.FSharpName |> shouldEqual "client2"

    let rec private referencedTypeNames (plannedType : OpenApiPlannedType) : Set<string> =
        match plannedType with
        | OpenApiPlannedType.Named name -> Set.singleton name
        | OpenApiPlannedType.List inner
        | OpenApiPlannedType.Optional inner -> referencedTypeNames inner
        | OpenApiPlannedType.Primitive _
        | OpenApiPlannedType.JsonNode
        | OpenApiPlannedType.Stream
        | OpenApiPlannedType.Unit -> Set.empty

    [<Test>]
    let ``Every planned named reference is bound, including self references`` () =
        let mutable shortChains = 0
        let mutable longChains = 0

        let property count =
            if count <= 4 then
                shortChains <- shortChains + 1

            if count >= 9 then
                longChains <- longChains + 1

            let chain =
                [
                    for index in 0..count do
                        let properties =
                            if index = 0 then
                                [ "value", schemaForScalar GeneratedScalar.Int32 false ]
                            else
                                [ "previous", reference $"Type%i{index - 1}" ]

                        let schema =
                            jsonObject [ "type", jsonString "object" ; "properties", jsonObject properties ]

                        $"Type%i{index}", schema
                ]

            let node =
                jsonObject
                    [
                        "type", jsonString "object"
                        "properties", jsonObject [ "next", reference "Node" ]
                    ]

            let pathParameter =
                parameter "id" "path" true (schemaForScalar GeneratedScalar.Int64 false)

            let pathItem =
                standardPathItem (responseWithSchema (reference "Node")) [ pathParameter ] []

            let actual =
                document "3.0.3" (("Node", node) :: chain) "/things/{id}" pathItem |> plan

            let bound = actual.Types |> List.map _.FSharpName |> Set.ofList

            let referenced =
                actual.Types
                |> List.collect (fun definition ->
                    [
                        yield!
                            definition.Fields
                            |> List.collect (fun field -> referencedTypeNames field.Type |> Set.toList)

                        match definition.AdditionalProperties with
                        | None -> ()
                        | Some value -> yield! referencedTypeNames value
                    ]
                )
                |> Set.ofList

            Set.isSubset referenced bound |> shouldEqual true

            let bySource =
                actual.Types
                |> List.map (fun definition -> definition.SourceName, definition)
                |> Map

            let positions =
                actual.Types
                |> List.mapi (fun index definition -> definition.FSharpName, index)
                |> Map

            for index in 1..count do
                let current = bySource.[$"Type%i{index}"]
                let previous = bySource.[$"Type%i{index - 1}"]
                let field = current.Fields |> List.exactlyOne

                field.Type
                |> shouldEqual (OpenApiPlannedType.Optional (OpenApiPlannedType.Named previous.FSharpName))

                positions.[previous.FSharpName] < positions.[current.FSharpName]
                |> shouldEqual true

            let node = bySource.["Node"]

            node.Fields
            |> List.exactlyOne
            |> _.Type
            |> shouldEqual (OpenApiPlannedType.Optional (OpenApiPlannedType.Named node.FSharpName))

        property
        |> Prop.forAll (Arb.fromGen (Gen.choose (1, 12)))
        |> Check.QuickThrowOnFailure

        if shortChains < 10 || longChains < 10 then
            failwith $"Insufficient chain distribution: short=%i{shortChains}, long=%i{longChains}"

    [<Test>]
    let ``Mutually recursive record components fail explicitly before codec generation`` () =
        let schemaA =
            jsonObject
                [
                    "type", jsonString "object"
                    "properties", jsonObject [ "b", reference "B" ]
                ]

        let schemaB =
            jsonObject
                [
                    "type", jsonString "object"
                    "properties", jsonObject [ "a", reference "A" ]
                ]

        let pathItem = standardPathItem (responseWithSchema (reference "A")) [] []

        let source = document "3.0.3" [ "A", schemaA ; "B", schemaB ] "/things" pathItem

        match OpenApiClientGenerator.parseAndPlan config source with
        | Ok _ -> failwith "Planning unexpectedly accepted mutually recursive generated codecs"
        | Error diagnostics ->
            diagnostics
            |> List.exists (fun diagnostic -> diagnostic.Code = OpenApiGenerationDiagnosticCode.UnsupportedSchema)
            |> shouldEqual true

    [<Test>]
    let ``Dangling references always produce a located diagnostic`` () =
        let property (PositiveInt suffix) =
            let missing = $"Missing%i{suffix}"

            let schema =
                jsonObject
                    [
                        "type", jsonString "object"
                        "properties", jsonObject [ "child", reference missing ]
                    ]

            let pathParameter =
                parameter "id" "path" true (schemaForScalar GeneratedScalar.Int64 false)

            let pathItem =
                standardPathItem (responseWithSchema (reference "Thing")) [ pathParameter ] []

            let source = document "3.0.3" [ "Thing", schema ] "/things/{id}" pathItem

            match OpenApiClientGenerator.parseAndPlan config source with
            | Ok _ -> failwith $"Planning unexpectedly accepted dangling reference %s{missing}"
            | Error diagnostics ->
                diagnostics
                |> List.exists (fun diagnostic ->
                    diagnostic.Code = OpenApiGenerationDiagnosticCode.UnresolvedReference
                    && diagnostic.Location.Contains ("/properties/child", StringComparison.Ordinal)
                    && diagnostic.Message.Contains (missing, StringComparison.Ordinal)
                )
                |> shouldEqual true

        Check.QuickThrowOnFailure property

    [<Test>]
    let ``Required properties without schemas remain required nullable JsonNode fields`` () =
        let schema =
            jsonObject
                [
                    "type", jsonString "object"
                    "properties", jsonObject []
                    "required", jsonArray [ jsonString "ghost" ]
                ]

        let pathParameter =
            parameter "id" "path" true (schemaForScalar GeneratedScalar.Int64 false)

        let pathItem =
            standardPathItem (responseWithSchema (reference "Thing")) [ pathParameter ] []

        let source = document "3.0.3" [ "Thing", schema ] "/things/{id}" pathItem

        let actual = source |> plan
        let thing = actual.Types |> List.exactlyOne
        let ghost = thing.Fields |> List.exactlyOne
        ghost.JsonName |> shouldEqual "ghost"
        ghost.Required |> shouldEqual true

        ghost.Type
        |> shouldEqual (OpenApiPlannedType.Optional OpenApiPlannedType.JsonNode)

    [<Test>]
    let ``Optional nullable fields fail explicitly because their three states are not representable`` () =
        let schema =
            jsonObject
                [
                    "type", jsonString "object"
                    "properties", jsonObject [ "value", schemaForScalar GeneratedScalar.String true ]
                ]

        let pathParameter =
            parameter "id" "path" true (schemaForScalar GeneratedScalar.Int64 false)

        let pathItem =
            standardPathItem (responseWithSchema (reference "Thing")) [ pathParameter ] []

        let source = document "3.0.3" [ "Thing", schema ] "/things/{id}" pathItem

        match OpenApiClientGenerator.parseAndPlan config source with
        | Ok _ -> failwith "Planning unexpectedly conflated missing, null, and present values"
        | Error diagnostics ->
            diagnostics
            |> List.exists (fun diagnostic ->
                diagnostic.Code = OpenApiGenerationDiagnosticCode.UnsupportedSchema
                && diagnostic.Location.Contains ("/properties/value", StringComparison.Ordinal)
            )
            |> shouldEqual true

    [<Test>]
    let ``Unconstrained JSON request and response schemas preserve null as an explicit value`` () =
        let unconstrained () = jsonObject []

        let requestBody =
            jsonObject
                [
                    "required", jsonBool true
                    "content", jsonObject [ "application/json", jsonObject [ "schema", unconstrained () ] ]
                ]

        let pathItem =
            jsonObject
                [
                    "post",
                    jsonObject
                        [
                            "operationId", jsonString "echoAnything"
                            "requestBody", requestBody
                            "responses", jsonObject [ "200", responseWithSchema (unconstrained ()) ]
                        ]
                ]

        let actual = document "3.0.3" [] "/anything" pathItem |> plan
        let operation = actual.Operations |> List.exactlyOne
        let body = operation.Parameters |> List.exactlyOne
        body.Location |> shouldEqual OpenApiParameterLocation.Body
        body.Required |> shouldEqual true

        body.Type
        |> shouldEqual (OpenApiPlannedType.Optional OpenApiPlannedType.JsonNode)

        operation.ReturnType
        |> shouldEqual (OpenApiPlannedType.Optional OpenApiPlannedType.JsonNode)

    [<Test>]
    let ``Optional unconstrained properties fail because all three wire states are legal`` () =
        let schema =
            jsonObject
                [
                    "type", jsonString "object"
                    "properties", jsonObject [ "value", jsonObject [] ]
                ]

        let pathItem = standardPathItem (responseWithSchema (reference "Thing")) [] []
        let source = document "3.0.3" [ "Thing", schema ] "/things" pathItem

        match OpenApiClientGenerator.parseAndPlan config source with
        | Ok _ -> failwith "Planning unexpectedly conflated a missing property with a present JSON null"
        | Error diagnostics ->
            diagnostics
            |> List.exists (fun diagnostic ->
                diagnostic.Code = OpenApiGenerationDiagnosticCode.UnsupportedSchema
                && diagnostic.Location.EndsWith ("/properties/value", StringComparison.Ordinal)
            )
            |> shouldEqual true

    [<Test>]
    let ``Unbounded and extension-formatted integers use BigInteger rather than narrowing`` () =
        for format in [ None ; Some "uint64" ] do
            let schema =
                jsonObject
                    [
                        "type", jsonString "integer"
                        match format with
                        | None -> ()
                        | Some value -> "format", jsonString value
                    ]

            let pathItem = standardPathItem (responseWithSchema schema) [] []
            let actual = document "3.0.3" [] "/integer" pathItem |> plan

            actual.Operations
            |> List.exactlyOne
            |> _.ReturnType
            |> shouldEqual (OpenApiPlannedType.Primitive OpenApiPrimitive.BigInteger)

    [<Test>]
    let ``Unformatted and unknown-format numbers fail instead of narrowing to double`` () =
        for format in [ None ; Some "arbitrary-precision" ] do
            let schema =
                jsonObject
                    [
                        "type", jsonString "number"
                        match format with
                        | None -> ()
                        | Some value -> "format", jsonString value
                    ]

            let pathItem = standardPathItem (responseWithSchema schema) [] []
            let source = document "3.0.3" [] "/number" pathItem

            match OpenApiClientGenerator.parseAndPlan config source with
            | Ok _ -> failwith "Planning unexpectedly narrowed an arbitrary JSON number to double"
            | Error diagnostics ->
                diagnostics
                |> List.exists (fun diagnostic ->
                    diagnostic.Code = OpenApiGenerationDiagnosticCode.UnsupportedSchema
                    && diagnostic.Location.EndsWith ("/format", StringComparison.Ordinal)
                )
                |> shouldEqual true

    [<Test>]
    let ``Nullable component schemas stay nullable through references`` () =
        let maybe =
            jsonObject
                [
                    "type", jsonString "object"
                    "nullable", jsonBool true
                    "properties", jsonObject [ "id", schemaForScalar GeneratedScalar.Int64 false ]
                ]

        let holder =
            jsonObject
                [
                    "type", jsonString "object"
                    "required", jsonArray [ jsonString "value" ]
                    "properties", jsonObject [ "value", reference "Maybe" ]
                ]

        let pathItem = standardPathItem (responseWithSchema (reference "Holder")) [] []

        let actual =
            document "3.0.3" [ "Maybe", maybe ; "Holder", holder ] "/things" pathItem
            |> plan

        let maybeType =
            actual.Types |> List.find (fun definition -> definition.SourceName = "Maybe")

        let holderType =
            actual.Types |> List.find (fun definition -> definition.SourceName = "Holder")

        let value = holderType.Fields |> List.find (fun field -> field.JsonName = "value")

        value.Type
        |> shouldEqual (OpenApiPlannedType.Optional (OpenApiPlannedType.Named maybeType.FSharpName))

    [<Test>]
    let ``AllOf permits null exactly when its outer constraint and every branch permit it`` () =
        let branch name nullable =
            jsonObject
                [
                    "type", jsonString "object"
                    "nullable", jsonBool nullable
                    "properties", jsonObject [ name, schemaForScalar GeneratedScalar.String false ]
                ]

        let bothNullable =
            jsonObject [ "allOf", jsonArray [ branch "left" true ; branch "right" true ] ]

        let onlyOuterNullable =
            jsonObject
                [
                    "nullable", jsonBool true
                    "allOf", jsonArray [ branch "left" false ; branch "right" false ]
                ]

        let responseFor name schema =
            let pathItem = standardPathItem (responseWithSchema (reference name)) [] []
            document "3.0.3" [ name, schema ] "/things" pathItem |> plan

        let bothPlan = responseFor "Both" bothNullable
        let bothType = bothPlan.Types |> List.exactlyOne

        bothPlan.Operations
        |> List.exactlyOne
        |> _.ReturnType
        |> shouldEqual (OpenApiPlannedType.Optional (OpenApiPlannedType.Named bothType.FSharpName))

        let outerPlan = responseFor "Outer" onlyOuterNullable
        let outerType = outerPlan.Types |> List.exactlyOne

        outerPlan.Operations
        |> List.exactlyOne
        |> _.ReturnType
        |> shouldEqual (OpenApiPlannedType.Named outerType.FSharpName)

    [<Test>]
    let ``Additional-properties modes map exhaustively to their generated value types`` () =
        let cases : (string * JsonNode option * OpenApiPlannedType option) list =
            [
                "omitted", None, Some (OpenApiPlannedType.Optional OpenApiPlannedType.JsonNode)
                "allowed", Some (jsonBool true), Some (OpenApiPlannedType.Optional OpenApiPlannedType.JsonNode)
                "forbidden", Some (jsonBool false), None
                "typed",
                Some (schemaForScalar GeneratedScalar.String false),
                Some (OpenApiPlannedType.Primitive OpenApiPrimitive.String)
                "typed-nullable",
                Some (schemaForScalar GeneratedScalar.String true),
                Some (OpenApiPlannedType.Optional (OpenApiPlannedType.Primitive OpenApiPrimitive.String))
                "typed-any", Some (jsonObject []), Some (OpenApiPlannedType.Optional OpenApiPlannedType.JsonNode)
            ]

        for caseName, additionalProperties, expected in cases do
            let schema =
                jsonObject
                    [
                        "type", jsonString "object"
                        "required", jsonArray [ jsonString "id" ]
                        "properties", jsonObject [ "id", schemaForScalar GeneratedScalar.Int64 false ]
                        match additionalProperties with
                        | None -> ()
                        | Some value -> "additionalProperties", value
                    ]

            let pathItem = standardPathItem (responseWithSchema (reference "Thing")) [] []

            let actual =
                document "3.0.3" [ "Thing", schema ] $"/things/%s{caseName}" pathItem |> plan

            actual.Types
            |> List.exactlyOne
            |> _.AdditionalProperties
            |> shouldEqual expected

    [<Test>]
    let ``Closed empty objects fail instead of gaining a synthetic JSON field`` () =
        let empty =
            jsonObject [ "type", jsonString "object" ; "additionalProperties", jsonBool false ]

        let pathItem = standardPathItem (responseWithSchema (reference "Empty")) [] []
        let source = document "3.0.3" [ "Empty", empty ] "/things" pathItem

        match OpenApiClientGenerator.parseAndPlan config source with
        | Ok _ -> failwith "Planning unexpectedly invented a field for a closed empty object"
        | Error diagnostics ->
            diagnostics
            |> List.exists (fun diagnostic -> diagnostic.Code = OpenApiGenerationDiagnosticCode.UnsupportedSchema)
            |> shouldEqual true

    [<Test>]
    let ``Required document strings reject explicit JSON null`` () =
        let source =
            """{"openapi":null,"info":{"title":"API","version":"1"},"paths":{},"components":{"schemas":{}}}"""

        match OpenApiClientGenerator.parseAndPlan config source with
        | Ok _ -> failwith "Planning unexpectedly treated a null OpenAPI version as absent"
        | Error diagnostics ->
            diagnostics
            |> List.exists (fun diagnostic ->
                diagnostic.Code = OpenApiGenerationDiagnosticCode.InvalidDocument
                && diagnostic.Location = "#/openapi"
            )
            |> shouldEqual true

    [<Test>]
    let ``Optional structural properties reject explicit JSON null`` () =
        let source =
            """{"openapi":"3.0.3","info":{"title":"API","version":"1"},"servers":null,"paths":{},"components":{"schemas":{}}}"""

        match OpenApiClientGenerator.parseAndPlan config source with
        | Ok _ -> failwith "Planning unexpectedly treated a null servers array as absent"
        | Error diagnostics ->
            diagnostics
            |> List.exists (fun diagnostic ->
                diagnostic.Code = OpenApiGenerationDiagnosticCode.InvalidDocument
                && diagnostic.Location = "#/servers"
            )
            |> shouldEqual true

    [<Test>]
    let ``Unsupported composition keywords on named objects are not bypassed by references`` () =
        let schema =
            jsonObject
                [
                    "type", jsonString "object"
                    "properties", jsonObject [ "id", schemaForScalar GeneratedScalar.Int64 false ]
                    "oneOf", jsonArray [ jsonObject [ "type", jsonString "object" ] ]
                ]

        let pathItem = standardPathItem (responseWithSchema (reference "Thing")) [] []
        let source = document "3.0.3" [ "Thing", schema ] "/things" pathItem

        match OpenApiClientGenerator.parseAndPlan config source with
        | Ok _ -> failwith "Planning unexpectedly ignored oneOf on a referenced object component"
        | Error diagnostics ->
            diagnostics
            |> List.exists (fun diagnostic -> diagnostic.Code = OpenApiGenerationDiagnosticCode.UnsupportedSchema)
            |> shouldEqual true

    [<Test>]
    let ``AllOf deduplication cannot hide unsupported keywords on a discarded property schema`` () =
        let propertySchema includeUnsupportedKeyword =
            jsonObject
                [
                    "type", jsonString "string"
                    if includeUnsupportedKeyword then
                        "oneOf", jsonArray [ jsonObject [ "type", jsonString "string" ] ]
                ]

        let branch includeUnsupportedKeyword =
            jsonObject
                [
                    "type", jsonString "object"
                    "properties", jsonObject [ "value", propertySchema includeUnsupportedKeyword ]
                ]

        let child = jsonObject [ "allOf", jsonArray [ branch false ; branch true ] ]
        let pathItem = standardPathItem (responseWithSchema (reference "Child")) [] []
        let source = document "3.0.3" [ "Child", child ] "/things" pathItem

        match OpenApiClientGenerator.parseAndPlan config source with
        | Ok _ -> failwith "Planning unexpectedly hid oneOf on a discarded allOf property"
        | Error diagnostics ->
            diagnostics
            |> List.exists (fun diagnostic ->
                diagnostic.Code = OpenApiGenerationDiagnosticCode.UnsupportedSchema
                && diagnostic.Location.Contains ("/allOf/1/properties/value", StringComparison.Ordinal)
            )
            |> shouldEqual true

    [<Test>]
    let ``Anonymous-type cache hits still validate every nested schema occurrence`` () =
        let schema includeUnsupportedKeyword =
            jsonObject
                [
                    "type", jsonString "object"
                    "properties",
                    jsonObject
                        [
                            "value",
                            jsonObject
                                [
                                    "type", jsonString "string"
                                    if includeUnsupportedKeyword then
                                        "oneOf", jsonArray [ jsonObject [ "type", jsonString "string" ] ]
                                ]
                        ]
                ]

        let pathItem =
            jsonObject
                [
                    "get",
                    jsonObject
                        [
                            "operationId", jsonString "getThing"
                            "responses",
                            jsonObject
                                [
                                    "200", responseWithSchema (schema false)
                                    "201", responseWithSchema (schema true)
                                ]
                        ]
                ]

        let source = document "3.0.3" [] "/things" pathItem

        match OpenApiClientGenerator.parseAndPlan config source with
        | Ok _ -> failwith "Planning unexpectedly hid oneOf on a cached anonymous type"
        | Error diagnostics ->
            diagnostics
            |> List.exists (fun diagnostic ->
                diagnostic.Code = OpenApiGenerationDiagnosticCode.UnsupportedSchema
                && diagnostic.Location.Contains ("/responses/201", StringComparison.Ordinal)
                && diagnostic.Location.EndsWith ("/properties/value", StringComparison.Ordinal)
            )
            |> shouldEqual true

    [<Test>]
    let ``Sanitised component and field identifiers remain unique`` () =
        let collidingSchema =
            jsonObject
                [
                    "type", jsonString "object"
                    "properties",
                    jsonObject
                        [
                            "foo-bar", schemaForScalar GeneratedScalar.String false
                            "foo_bar", schemaForScalar GeneratedScalar.String false
                        ]
                ]

        let otherSchema =
            jsonObject [ "type", jsonString "object" ; "properties", jsonObject [] ]

        let pathParameter =
            parameter "id" "path" true (schemaForScalar GeneratedScalar.Int64 false)

        let pathItem =
            standardPathItem (responseWithSchema (reference "foo-bar")) [ pathParameter ] []

        let actual =
            document "3.0.3" [ "foo-bar", collidingSchema ; "foo_bar", otherSchema ] "/things/{id}" pathItem
            |> plan

        let typeNames = actual.Types |> List.map _.FSharpName
        typeNames |> Set.ofList |> Set.count |> shouldEqual typeNames.Length

        let fieldNames =
            actual.Types
            |> List.find (fun definition -> definition.SourceName = "foo-bar")
            |> _.Fields
            |> List.map _.FSharpName

        fieldNames |> Set.ofList |> Set.count |> shouldEqual fieldNames.Length

    [<Test>]
    let ``AllOf object composition has the union of its fields`` () =
        let baseSchema =
            jsonObject
                [
                    "type", jsonString "object"
                    "required", jsonArray [ jsonString "id" ]
                    "properties", jsonObject [ "id", schemaForScalar GeneratedScalar.Int64 false ]
                ]

        let extension =
            jsonObject
                [
                    "type", jsonString "object"
                    "properties", jsonObject [ "display-name", schemaForScalar GeneratedScalar.String false ]
                ]

        let childSchema = jsonObject [ "allOf", jsonArray [ reference "Base" ; extension ] ]

        let pathParameter =
            parameter "id" "path" true (schemaForScalar GeneratedScalar.Int64 false)

        let pathItem =
            standardPathItem (responseWithSchema (reference "Child")) [ pathParameter ] []

        let actual =
            document "3.0.3" [ "Base", baseSchema ; "Child", childSchema ] "/things/{id}" pathItem
            |> plan

        let child =
            actual.Types |> List.find (fun definition -> definition.SourceName = "Child")

        child.Fields
        |> List.map _.JsonName
        |> Set.ofList
        |> shouldEqual (Set [ "id" ; "display-name" ])

        child.Fields
        |> List.find (fun field -> field.JsonName = "id")
        |> _.Required
        |> shouldEqual true

    [<Test>]
    let ``Equivalent inline success schemas share one planned type despite annotations and ordering`` () =
        let first =
            jsonObject
                [
                    "type", jsonString "object"
                    "description", jsonString "first spelling"
                    "required", jsonArray [ jsonString "id" ; jsonString "name" ]
                    "properties",
                    jsonObject
                        [
                            "id", schemaForScalar GeneratedScalar.Int64 false
                            "name", schemaForScalar GeneratedScalar.String false
                        ]
                ]

        let second =
            jsonObject
                [
                    "description", jsonString "same wire shape, different annotation"
                    "properties",
                    jsonObject
                        [
                            "name", schemaForScalar GeneratedScalar.String false
                            "id", schemaForScalar GeneratedScalar.Int64 false
                        ]
                    "required", jsonArray [ jsonString "name" ; jsonString "id" ]
                    "type", jsonString "object"
                ]

        let pathItem =
            jsonObject
                [
                    "get",
                    jsonObject
                        [
                            "operationId", jsonString "getThing"
                            "responses",
                            jsonObject [ "200", responseWithSchema first ; "201", responseWithSchema second ]
                        ]
                ]

        let actual = document "3.0.3" [] "/things" pathItem |> plan
        actual.Types |> List.length |> shouldEqual 1

        actual.Operations
        |> List.exactlyOne
        |> _.ReturnType
        |> shouldEqual (OpenApiPlannedType.Named (actual.Types |> List.exactlyOne |> _.FSharpName))

    [<Test>]
    let ``Equivalent inline allOf schemas share one type across branch permutations`` () =
        let baseSchema =
            jsonObject
                [
                    "type", jsonString "object"
                    "required", jsonArray [ jsonString "id" ]
                    "properties", jsonObject [ "id", schemaForScalar GeneratedScalar.Int64 false ]
                ]

        let extension =
            jsonObject
                [
                    "type", jsonString "object"
                    "properties", jsonObject [ "name", schemaForScalar GeneratedScalar.String false ]
                ]

        let composed (description : string) (branches : JsonNode list) =
            jsonObject
                [
                    "description", jsonString description
                    "allOf", jsonArray (branches |> List.map (fun branch -> branch.DeepClone ()))
                ]

        let pathItem =
            jsonObject
                [
                    "get",
                    jsonObject
                        [
                            "operationId", jsonString "getThing"
                            "responses",
                            jsonObject
                                [
                                    "200", responseWithSchema (composed "first" [ baseSchema ; extension ])
                                    "201", responseWithSchema (composed "second" [ extension ; baseSchema ])
                                ]
                        ]
                ]

        let actual = document "3.0.3" [] "/things" pathItem |> plan
        let definition = actual.Types |> List.exactlyOne

        definition.Fields
        |> List.map _.JsonName
        |> Set.ofList
        |> shouldEqual (Set [ "id" ; "name" ])

        actual.Operations
        |> List.exactlyOne
        |> _.ReturnType
        |> shouldEqual (OpenApiPlannedType.Named definition.FSharpName)

    [<Test>]
    let ``Direct and single-branch allOf object schemas share their flattened record type`` () =
        let direct =
            jsonObject
                [
                    "type", jsonString "object"
                    "required", jsonArray [ jsonString "id" ]
                    "properties", jsonObject [ "id", schemaForScalar GeneratedScalar.Int64 false ]
                ]

        let composed =
            jsonObject
                [
                    "description", jsonString "different syntax, same record"
                    "allOf", jsonArray [ direct.DeepClone () ]
                ]

        let pathItem =
            jsonObject
                [
                    "get",
                    jsonObject
                        [
                            "operationId", jsonString "getThing"
                            "responses",
                            jsonObject [ "200", responseWithSchema direct ; "201", responseWithSchema composed ]
                        ]
                ]

        let actual = document "3.0.3" [] "/things" pathItem |> plan
        let definition = actual.Types |> List.exactlyOne
        definition.Fields |> List.map _.JsonName |> shouldEqual [ "id" ]

        actual.Operations
        |> List.exactlyOne
        |> _.ReturnType
        |> shouldEqual (OpenApiPlannedType.Named definition.FSharpName)

    [<Test>]
    let ``Anonymous object deduplication keeps incompatible field types distinct`` () =
        let schema scalar =
            jsonObject
                [
                    "type", jsonString "object"
                    "properties", jsonObject [ "value", schemaForScalar scalar false ]
                ]

        let pathItem =
            jsonObject
                [
                    "get",
                    jsonObject
                        [
                            "operationId", jsonString "getThing"
                            "responses",
                            jsonObject
                                [
                                    "200", responseWithSchema (schema GeneratedScalar.String)
                                    "201", responseWithSchema (schema GeneratedScalar.Int64)
                                ]
                        ]
                ]

        let source = document "3.0.3" [] "/things" pathItem

        match OpenApiClientGenerator.parseAndPlan config source with
        | Ok _ -> failwith "Planning unexpectedly merged incompatible anonymous record fields"
        | Error diagnostics ->
            diagnostics
            |> List.exists (fun diagnostic ->
                diagnostic.Code = OpenApiGenerationDiagnosticCode.AmbiguousSuccessResponse
                && diagnostic.Location.EndsWith ("/responses/201", StringComparison.Ordinal)
            )
            |> shouldEqual true

    [<Test>]
    let ``Media selection exhaustively follows supported priority and ignores insertion order`` () =
        let objectSchema =
            jsonObject
                [
                    "type", jsonString "object"
                    "properties", jsonObject [ "id", schemaForScalar GeneratedScalar.Int64 false ]
                ]

        let stringSchema = schemaForScalar GeneratedScalar.String false

        let media =
            [
                "application/json", objectSchema
                "application/problem+json", objectSchema
                "text/plain", stringSchema
                "application/octet-stream", stringSchema
            ]

        for selectedIndex in 0 .. media.Length - 1 do
            let selectedName = fst media.[selectedIndex]
            let available = media |> List.skip selectedIndex

            let makePlan (entries : (string * JsonNode) list) =
                let response =
                    jsonObject
                        [
                            "description", jsonString "success"
                            "content",
                            jsonObject (
                                entries
                                |> List.map (fun (name, schema) -> name, jsonObject [ "schema", schema.DeepClone () ])
                            )
                        ]

                let pathItem = standardPathItem response [] []
                document "3.0.3" [] "/things" pathItem |> plan

            let forward = makePlan available
            let reversed = makePlan (List.rev available)
            reversed |> shouldEqual forward

            let operation = forward.Operations |> List.exactlyOne
            operation.Accept |> shouldEqual (Some selectedName)

            match selectedIndex with
            | 0
            | 1 ->
                let definition = forward.Types |> List.exactlyOne

                operation.ReturnType
                |> shouldEqual (OpenApiPlannedType.Named definition.FSharpName)
            | 2 ->
                operation.ReturnType
                |> shouldEqual (OpenApiPlannedType.Primitive OpenApiPrimitive.String)

                forward.Types |> shouldEqual []
            | 3 ->
                operation.ReturnType |> shouldEqual OpenApiPlannedType.Stream
                forward.Types |> shouldEqual []
            | _ -> failwith "Unreachable media case"

    [<Test>]
    let ``Incompatible successful response media produce a diagnostic at the mutated status`` () =
        let response mediaType =
            jsonObject
                [
                    "description", jsonString "success"
                    "content",
                    jsonObject
                        [
                            mediaType, jsonObject [ "schema", schemaForScalar GeneratedScalar.String false ]
                        ]
                ]

        let pathItem =
            jsonObject
                [
                    "get",
                    jsonObject
                        [
                            "operationId", jsonString "getThing"
                            "responses",
                            jsonObject [ "200", response "text/plain" ; "201", response "application/octet-stream" ]
                        ]
                ]

        let source = document "3.0.3" [] "/things" pathItem

        match OpenApiClientGenerator.parseAndPlan config source with
        | Ok _ -> failwith "Planning unexpectedly accepted incompatible successful responses"
        | Error diagnostics ->
            diagnostics
            |> List.exists (fun diagnostic ->
                diagnostic.Code = OpenApiGenerationDiagnosticCode.AmbiguousSuccessResponse
                && diagnostic.Location.EndsWith ("/responses/201", StringComparison.Ordinal)
            )
            |> shouldEqual true

    [<Test>]
    let ``Default responses constrain unspecified successful statuses unless 2XX covers them`` () =
        let response mediaType =
            jsonObject
                [
                    "description", jsonString "response"
                    "content",
                    jsonObject
                        [
                            mediaType, jsonObject [ "schema", schemaForScalar GeneratedScalar.String false ]
                        ]
                ]

        let makeSource responses =
            let pathItem =
                jsonObject
                    [
                        "get", jsonObject [ "operationId", jsonString "getThing" ; "responses", jsonObject responses ]
                    ]

            document "3.0.3" [] "/things" pathItem

        let incompatible =
            makeSource
                [
                    "200", response "text/plain"
                    "default", response "application/octet-stream"
                ]

        match OpenApiClientGenerator.parseAndPlan config incompatible with
        | Ok _ -> failwith "Planning unexpectedly ignored default for unspecified 2xx statuses"
        | Error diagnostics ->
            diagnostics
            |> List.exists (fun diagnostic ->
                diagnostic.Code = OpenApiGenerationDiagnosticCode.AmbiguousSuccessResponse
                && diagnostic.Location.EndsWith ("/responses/default", StringComparison.Ordinal)
            )
            |> shouldEqual true

        let rangeCovered =
            makeSource
                [
                    "2XX", response "text/plain"
                    "default", response "application/octet-stream"
                ]
            |> plan

        rangeCovered.Operations
        |> List.exactlyOne
        |> _.ReturnType
        |> shouldEqual (OpenApiPlannedType.Primitive OpenApiPrimitive.String)

        let defaultOnly = makeSource [ "default", response "text/plain" ] |> plan

        defaultOnly.Operations
        |> List.exactlyOne
        |> _.Accept
        |> shouldEqual (Some "text/plain")

    [<Test>]
    let ``AllOf rejects fields made impossible by another branch's additionalProperties constraint`` () =
        let closed =
            jsonObject
                [
                    "type", jsonString "object"
                    "additionalProperties", jsonBool false
                    "properties", jsonObject [ "id", schemaForScalar GeneratedScalar.Int64 false ]
                ]

        let extension =
            jsonObject
                [
                    "type", jsonString "object"
                    "properties", jsonObject [ "display-name", schemaForScalar GeneratedScalar.String false ]
                ]

        let child = jsonObject [ "allOf", jsonArray [ closed ; extension ] ]
        let pathItem = standardPathItem (responseWithSchema (reference "Child")) [] []
        let source = document "3.0.3" [ "Child", child ] "/things" pathItem

        match OpenApiClientGenerator.parseAndPlan config source with
        | Ok _ -> failwith "Planning unexpectedly weakened an allOf additionalProperties constraint"
        | Error diagnostics ->
            diagnostics
            |> List.exists (fun diagnostic -> diagnostic.Code = OpenApiGenerationDiagnosticCode.UnsupportedSchema)
            |> shouldEqual true

    [<Test>]
    let ``A single path-parameter mismatch mutation always produces a diagnostic`` () =
        for missingParameter in [ false ; true ] do
            let path, parameters =
                if missingParameter then
                    "/things/{id}", []
                else
                    "/things", [ parameter "id" "path" true (schemaForScalar GeneratedScalar.Int64 false) ]

            let pathItem = standardPathItem (noContentResponse ()) parameters []
            let source = document "3.0.3" [] path pathItem

            match OpenApiClientGenerator.parseAndPlan config source with
            | Ok _ -> failwith "Planning unexpectedly accepted a path/parameter mismatch"
            | Error diagnostics ->
                diagnostics
                |> List.exists (fun diagnostic ->
                    diagnostic.Code = OpenApiGenerationDiagnosticCode.UnsupportedParameter
                )
                |> shouldEqual true

    [<Test>]
    let ``OpenAPI 3.1 is rejected rather than silently interpreted as 3.0`` () =
        let pathItem = standardPathItem (noContentResponse ()) [] []
        let source = document "3.1.0" [] "/things" pathItem

        match OpenApiClientGenerator.parseAndPlan config source with
        | Ok _ -> failwith "Planning unexpectedly accepted OpenAPI 3.1"
        | Error diagnostics ->
            diagnostics
            |> List.exists (fun diagnostic -> diagnostic.Code = OpenApiGenerationDiagnosticCode.UnsupportedVersion)
            |> shouldEqual true

    [<Test>]
    let ``Paths beginning with multiple slashes are rejected before URI semantics can change them`` () =
        let pathItem = standardPathItem (noContentResponse ()) [] []
        let source = document "3.0.3" [] "//things" pathItem

        match OpenApiClientGenerator.parseAndPlan config source with
        | Ok _ -> failwith "Planning unexpectedly allowed the HTTP shell to reinterpret a network-path reference"
        | Error diagnostics ->
            diagnostics
            |> List.exists (fun diagnostic ->
                diagnostic.Code = OpenApiGenerationDiagnosticCode.InvalidDocument
                && diagnostic.Location.Contains ("~1~1things", StringComparison.Ordinal)
            )
            |> shouldEqual true
