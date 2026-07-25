namespace WoofWare.Myriad.Plugins.Test

open System
open System.Text.Json.Nodes
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.Myriad.Plugins

/// The OpenAPI planner's treatment of `security` and `components.securitySchemes`: which credentials
/// each operation sends, and which documents it refuses rather than silently authenticating nothing.
[<TestFixture>]
module TestOpenApi3Security =

    let private jsonString (value : string) : JsonNode = JsonValue.Create value

    let private jsonObject (properties : (string * JsonNode) list) : JsonNode =
        let result = JsonObject ()

        for name, value in properties do
            result.Add (name, value)

        result :> JsonNode

    let private jsonArray (values : JsonNode list) : JsonNode =
        JsonArray (values |> List.toArray) :> JsonNode

    let private apiKeyHeaderScheme (headerName : string) : JsonNode =
        jsonObject
            [
                "type", jsonString "apiKey"
                "name", jsonString headerName
                "in", jsonString "header"
            ]

    let private apiKeyElsewhereScheme (location : string) : JsonNode =
        jsonObject
            [
                "type", jsonString "apiKey"
                "name", jsonString "key"
                "in", jsonString location
            ]

    let private httpScheme (scheme : string) : JsonNode =
        jsonObject [ "type", jsonString "http" ; "scheme", jsonString scheme ]

    let private oauth2Scheme () : JsonNode =
        jsonObject
            [
                "type", jsonString "oauth2"
                "flows",
                jsonObject
                    [
                        "clientCredentials",
                        jsonObject
                            [
                                "tokenUrl", jsonString "https://example.test/token"
                                "scopes", jsonObject [ "read", jsonString "Read things" ]
                            ]
                    ]
            ]

    let private openIdConnectScheme () : JsonNode =
        jsonObject
            [
                "type", jsonString "openIdConnect"
                "openIdConnectUrl", jsonString "https://example.test/.well-known/openid-configuration"
            ]

    /// One alternative of a `security` array: all of these schemes together.
    let private requirement (schemes : string list) : JsonNode =
        jsonObject (schemes |> List.map (fun scheme -> scheme, jsonArray []))

    /// A document whose operations are named by their path, so a test can look up an operation's
    /// plan by name. `operations` gives each operation's `security` array, if it declares one.
    let private securityDocument
        (schemes : (string * JsonNode) list)
        (rootSecurity : JsonNode list option)
        (operations : (string * JsonNode list option) list)
        : string
        =
        let pathItem (name : string) (security : JsonNode list option) =
            jsonObject
                [
                    "get",
                    jsonObject
                        [
                            "operationId", jsonString name
                            "responses", jsonObject [ "200", jsonObject [ "description", jsonString "success" ] ]
                            match security with
                            | None -> ()
                            | Some security -> "security", jsonArray security
                        ]
                ]

        jsonObject
            [
                "openapi", jsonString "3.0.3"
                "info", jsonObject [ "title", jsonString "Generated API" ; "version", jsonString "1.0.0" ]
                "servers", jsonArray [ jsonObject [ "url", jsonString "/api/v1" ] ]
                "paths",
                jsonObject (
                    operations
                    |> List.map (fun (name, security) -> $"/%s{name}", pathItem name security)
                )
                "components", jsonObject [ "securitySchemes", jsonObject schemes ]
                match rootSecurity with
                | None -> ()
                | Some security -> "security", jsonArray security
            ]
        |> _.ToJsonString()

    let private config = Map [ "CLASSNAME", "GeneratedClient" ]

    let private plan (parameters : Map<string, string>) (source : string) : OpenApiClientPlan =
        match OpenApiClientGenerator.parseAndPlan parameters source with
        | Ok value -> value
        | Error diagnostics ->
            diagnostics
            |> List.map (fun diagnostic -> $"%s{diagnostic.Location}: %s{diagnostic.Message}")
            |> String.concat Environment.NewLine
            |> failwith

    let private diagnostics (parameters : Map<string, string>) (source : string) : OpenApiGenerationDiagnostic list =
        match OpenApiClientGenerator.parseAndPlan parameters source with
        | Ok _ -> failwith "Planning unexpectedly succeeded"
        | Error diagnostics -> diagnostics

    /// The operation whose path was `/name`, by the F# name the planner gave it.
    let private operation (plan : OpenApiClientPlan) (name : string) : OpenApiPlannedOperation =
        plan.Operations
        |> List.filter (fun operation -> operation.Path = $"/%s{name}")
        |> List.exactlyOne

    [<Test>]
    let ``A root security requirement is applied to every operation`` () =
        let source =
            securityDocument
                [ "bearerAuth", httpScheme "bearer" ]
                (Some [ requirement [ "bearerAuth" ] ])
                [ "first", None ; "second", None ]

        let plan = plan config source

        for name in [ "first" ; "second" ] do
            (operation plan name).Security |> shouldEqual [ "bearerAuth" ]

        let credential = plan.Credentials.["bearerAuth"]
        credential.HeaderName |> shouldEqual "Authorization"
        credential.Kind |> shouldEqual (OpenApiSecuritySchemeKind.Http "bearer")
        credential.FSharpName |> shouldEqual "BearerAuth"

    [<Test>]
    let ``An operation's security replaces the root's, and an empty one demands no credentials`` () =
        let source =
            securityDocument
                [
                    "bearerAuth", httpScheme "bearer"
                    "apiKeyAuth", apiKeyHeaderScheme "X-API-Key"
                ]
                (Some [ requirement [ "bearerAuth" ] ])
                [
                    "inherited", None
                    "overridden", Some [ requirement [ "apiKeyAuth" ] ]
                    "public", Some []
                ]

        let plan = plan config source

        (operation plan "inherited").Security |> shouldEqual [ "bearerAuth" ]
        (operation plan "overridden").Security |> shouldEqual [ "apiKeyAuth" ]
        (operation plan "public").Security |> shouldEqual []

        plan.Credentials.["apiKeyAuth"].HeaderName |> shouldEqual "X-API-Key"

        plan.Credentials.["apiKeyAuth"].Kind
        |> shouldEqual OpenApiSecuritySchemeKind.ApiKey

    [<Test>]
    let ``A requirement naming several schemes sends all of their credentials`` () =
        let source =
            securityDocument
                [
                    "bearerAuth", httpScheme "bearer"
                    "apiKeyAuth", apiKeyHeaderScheme "X-API-Key"
                ]
                None
                [ "both", Some [ requirement [ "apiKeyAuth" ; "bearerAuth" ] ] ]

        let plan = plan config source

        (operation plan "both").Security |> shouldEqual [ "apiKeyAuth" ; "bearerAuth" ]

        plan.Credentials |> Map.count |> shouldEqual 2

    [<Test>]
    let ``Only the schemes some operation uses become credentials the caller must supply`` () =
        let source =
            securityDocument
                [
                    "used", httpScheme "bearer"
                    "unused", apiKeyHeaderScheme "X-Unused"
                    "alsoUnrepresentable", apiKeyElsewhereScheme "query"
                ]
                None
                [ "thing", Some [ requirement [ "used" ] ] ]

        let plan = plan config source

        plan.Credentials |> Map.toList |> List.map fst |> shouldEqual [ "used" ]

    [<Test>]
    let ``OAuth2 and OpenID Connect credentials are carried, but no flow is performed`` () =
        let source =
            securityDocument
                [ "oauth", oauth2Scheme () ; "oidc", openIdConnectScheme () ]
                None
                [
                    "oauthThing", Some [ requirement [ "oauth" ] ]
                    "oidcThing", Some [ requirement [ "oidc" ] ]
                ]

        let plan = plan config source

        plan.Credentials.["oauth"].Kind |> shouldEqual OpenApiSecuritySchemeKind.OAuth2

        plan.Credentials.["oauth"].HeaderName |> shouldEqual "Authorization"

        plan.Credentials.["oidc"].Kind
        |> shouldEqual OpenApiSecuritySchemeKind.OpenIdConnect

    [<Test>]
    let ``The first representable alternative is the one applied`` () =
        let source =
            securityDocument
                [
                    "queryKey", apiKeyElsewhereScheme "query"
                    "bearerAuth", httpScheme "bearer"
                    "apiKeyAuth", apiKeyHeaderScheme "X-API-Key"
                ]
                None
                [
                    "thing",
                    Some
                        [
                            requirement [ "queryKey" ]
                            requirement [ "bearerAuth" ]
                            requirement [ "apiKeyAuth" ]
                        ]
                ]

        let plan = plan config source

        (operation plan "thing").Security |> shouldEqual [ "bearerAuth" ]

    [<Test>]
    let ``An operation whose alternatives include the empty requirement first sends nothing`` () =
        let source =
            securityDocument
                [ "bearerAuth", httpScheme "bearer" ]
                None
                [ "thing", Some [ requirement [] ; requirement [ "bearerAuth" ] ] ]

        let plan = plan config source

        (operation plan "thing").Security |> shouldEqual []
        plan.Credentials |> shouldEqual Map.empty

    [<Test>]
    let ``An unsatisfiable security requirement fails the build rather than authenticating nothing`` () =
        for location in [ "query" ; "cookie" ] do
            let source =
                securityDocument
                    [ "key", apiKeyElsewhereScheme location ]
                    None
                    [ "thing", Some [ requirement [ "key" ] ] ]

            diagnostics config source
            |> List.filter (fun diagnostic -> diagnostic.Code = OpenApiGenerationDiagnosticCode.UnsupportedSecurity)
            |> List.map _.Location
            |> shouldEqual [ "#/paths/~1thing/get/security" ]

    [<Test>]
    let ``A root security requirement we cannot satisfy fails every operation which inherits it`` () =
        let source =
            securityDocument
                [ "key", apiKeyElsewhereScheme "query" ]
                (Some [ requirement [ "key" ] ])
                [ "thing", None ; "public", Some [] ]

        let diagnostics = diagnostics config source

        diagnostics
        |> List.filter (fun diagnostic -> diagnostic.Code = OpenApiGenerationDiagnosticCode.UnsupportedSecurity)
        |> List.length
        |> shouldEqual 1

    [<Test>]
    let ``A requirement naming an undefined scheme is a dangling reference`` () =
        let source =
            securityDocument
                [ "bearerAuth", httpScheme "bearer" ]
                None
                [ "thing", Some [ requirement [ "nonexistent" ] ] ]

        diagnostics config source
        |> List.exists (fun diagnostic -> diagnostic.Code = OpenApiGenerationDiagnosticCode.UnresolvedReference)
        |> shouldEqual true

    [<Test>]
    let ``A security scheme of unknown type is rejected`` () =
        let source =
            securityDocument
                [ "weird", jsonObject [ "type", jsonString "magic" ] ]
                None
                [ "thing", Some [ requirement [ "weird" ] ] ]

        let diagnostics = diagnostics config source

        diagnostics
        |> List.exists (fun diagnostic -> diagnostic.Code = OpenApiGenerationDiagnosticCode.InvalidDocument)
        |> shouldEqual true

        diagnostics
        |> List.exists (fun diagnostic -> diagnostic.Code = OpenApiGenerationDiagnosticCode.UnsupportedSecurity)
        |> shouldEqual true

    [<Test>]
    let ``The SecuritySchemes parameter chooses between alternatives`` () =
        let source =
            securityDocument
                [
                    "bearerAuth", httpScheme "bearer"
                    "apiKeyAuth", apiKeyHeaderScheme "X-API-Key"
                ]
                None
                [
                    "thing", Some [ requirement [ "bearerAuth" ] ; requirement [ "apiKeyAuth" ] ]
                ]

        let restricted = plan (config |> Map.add "SECURITYSCHEMES" "apiKeyAuth") source

        (operation restricted "thing").Security |> shouldEqual [ "apiKeyAuth" ]

        // Without the restriction we'd have taken the document's first alternative.
        (operation (plan config source) "thing").Security
        |> shouldEqual [ "bearerAuth" ]

    [<Test>]
    let ``The SecuritySchemes parameter cannot silently exclude every alternative`` () =
        let source =
            securityDocument
                [ "bearerAuth", httpScheme "bearer" ]
                None
                [ "thing", Some [ requirement [ "bearerAuth" ] ] ]

        diagnostics (config |> Map.add "SECURITYSCHEMES" "apiKeyAuth") source
        |> List.exists (fun diagnostic -> diagnostic.Code = OpenApiGenerationDiagnosticCode.UnsupportedSecurity)
        |> shouldEqual true

    [<Test>]
    let ``A SecuritySchemes parameter naming an undefined scheme is rejected`` () =
        let source =
            securityDocument
                [ "bearerAuth", httpScheme "bearer" ]
                None
                [ "thing", Some [ requirement [ "bearerAuth" ] ] ]

        diagnostics (config |> Map.add "SECURITYSCHEMES" "bearerAuth, typo") source
        |> List.exists (fun diagnostic ->
            diagnostic.Code = OpenApiGenerationDiagnosticCode.InvalidDocument
            && diagnostic.Location.Contains ("SecuritySchemes", StringComparison.Ordinal)
        )
        |> shouldEqual true

    [<Test>]
    let ``Credential property names do not collide with operation names`` () =
        // The scheme and the operation sanitise to the same F# identifier, and both are members of
        // the same interface.
        let source =
            securityDocument [ "thing", httpScheme "bearer" ] None [ "thing", Some [ requirement [ "thing" ] ] ]

        let plan = plan config source

        (operation plan "thing").FSharpName |> shouldEqual "Thing"
        plan.Credentials.["thing"].FSharpName |> shouldEqual "Thing2"

    type private GeneratedSchemeKind =
        | Bearer
        | ApiKeyHeader
        | OAuth2
        | ApiKeyQuery
        | ApiKeyCookie

    /// A scheme as the generated document declares it, paired with whether the planner can carry it.
    type private GeneratedScheme =
        {
            Name : string
            Json : JsonNode
            Representable : bool
        }

    let private generatedScheme (index : int) (kind : GeneratedSchemeKind) : GeneratedScheme =
        let json, representable =
            match kind with
            | Bearer -> httpScheme "bearer", true
            | ApiKeyHeader -> apiKeyHeaderScheme $"X-Key-%i{index}", true
            | OAuth2 -> oauth2Scheme (), true
            | ApiKeyQuery -> apiKeyElsewhereScheme "query", false
            | ApiKeyCookie -> apiKeyElsewhereScheme "cookie", false

        {
            Name = $"scheme%i{index}"
            Json = json
            Representable = representable
        }

    /// A document's schemes, plus one operation's alternatives as indices into them.
    let private securityCase : Gen<GeneratedScheme list * int list list> =
        gen {
            let! schemeCount = Gen.choose (1, 4)

            let! kinds =
                Gen.listOfLength
                    schemeCount
                    (Gen.elements [ Bearer ; ApiKeyHeader ; OAuth2 ; ApiKeyQuery ; ApiKeyCookie ])

            let schemes = kinds |> List.mapi generatedScheme

            let alternative =
                gen {
                    let! size = Gen.choose (0, 2)
                    let! indices = Gen.listOfLength size (Gen.choose (0, schemeCount - 1))
                    // A requirement is a JSON object, so it cannot name the same scheme twice.
                    return List.distinct indices
                }

            let! alternativeCount = Gen.choose (0, 4)
            let! alternatives = Gen.listOfLength alternativeCount alternative
            return schemes, alternatives
        }

    [<Test>]
    let ``The applied requirement is always the document's first satisfiable alternative`` () =
        let property (schemes : GeneratedScheme list, alternatives : int list list) =
            let representable =
                schemes
                |> List.mapi (fun index scheme -> index, scheme.Representable)
                |> Map.ofList

            // The oracle: the first alternative all of whose schemes we can carry. An absent
            // alternative list is not a failure; it means "this operation needs no credentials".
            let expected =
                if List.isEmpty alternatives then
                    Some []
                else
                    alternatives
                    |> List.tryFind (fun alternative -> alternative |> List.forall (fun index -> representable.[index]))
                    |> Option.map (fun alternative ->
                        alternative
                        |> List.map (fun index -> schemes.[index].Name)
                        |> List.distinct
                        |> List.sort
                    )

            let source =
                securityDocument
                    (schemes |> List.map (fun scheme -> scheme.Name, scheme.Json))
                    None
                    [
                        "thing",
                        Some (
                            alternatives
                            |> List.map (fun alternative ->
                                alternative |> List.map (fun index -> schemes.[index].Name) |> requirement
                            )
                        )
                    ]

            match OpenApiClientGenerator.parseAndPlan config source, expected with
            | Ok plan, Some expected ->
                (operation plan "thing").Security = expected
                && (plan.Credentials |> Map.toList |> List.map fst) = List.distinct expected
            | Error diagnostics, None ->
                diagnostics
                |> List.exists (fun diagnostic -> diagnostic.Code = OpenApiGenerationDiagnosticCode.UnsupportedSecurity)
            | Ok plan, None ->
                let sent = (operation plan "thing").Security
                failwith $"Planning accepted an unsatisfiable requirement, sending %+A{sent}"
            | Error diagnostics, Some expected ->
                failwith
                    $"Planning rejected a satisfiable requirement %+A{expected}: %+A{diagnostics |> List.map _.Message}"

        Check.QuickThrowOnFailure (Prop.forAll (Arb.fromGen securityCase) property)
