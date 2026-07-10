namespace WoofWare.Myriad.Plugins.Test

open System
open System.Collections.Generic
open System.IO
open System.Net
open System.Net.Http
open System.Numerics
open System.Text.Json.Nodes
open FsUnitTyped
open NUnit.Framework
open OpenApiPetstore

[<TestFixture>]
module TestOpenApi3Client =

    let private response (status : HttpStatusCode) (body : string option) =
        let result = new HttpResponseMessage (status)

        match body with
        | None -> ()
        | Some body -> result.Content <- new StringContent (body)

        result

    [<Test>]
    let ``Generated OpenAPI client composes server paths, media, bodies, and recursive DTOs`` () =
        task {
            let mutable calls = 0
            let mutable counterReads = 0

            let handler (message : HttpRequestMessage) =
                async {
                    calls <- calls + 1

                    match message.Method.Method, message.RequestUri.ToString () with
                    | "GET", "https://api.example.test/v1/public/pets/42" ->
                        message.Headers.Accept
                        |> Seq.exactlyOne
                        |> _.MediaType
                        |> shouldEqual "application/json"

                        return
                            response
                                HttpStatusCode.OK
                                (Some
                                    """{"id":42,"name":"Ada","parent":{"id":1,"name":"Root"},"metadata":{"source":"fixture"},"discarded":null}""")
                    | "POST", "https://api.example.test/v1/public/pets" ->
                        message.Headers.Accept
                        |> Seq.exactlyOne
                        |> _.MediaType
                        |> shouldEqual "application/json"

                        message.Content.Headers.ContentType.MediaType |> shouldEqual "application/json"

                        let! requestBody = message.Content.ReadAsStringAsync () |> Async.AwaitTask
                        let requestBody = JsonNode.Parse requestBody
                        requestBody.["name"].GetValue<string> () |> shouldEqual "Ada"
                        requestBody.["tag"].GetValue<string> () |> shouldEqual "cat"

                        return response HttpStatusCode.Created (Some """{"id":43,"name":"Ada","tag":"cat"}""")
                    | "DELETE", "https://api.example.test/v1/public/pets/43" ->
                        return response HttpStatusCode.NoContent None
                    | "GET", "https://api.example.test/v1/public/status" ->
                        message.Headers.Accept
                        |> Seq.exactlyOne
                        |> _.MediaType
                        |> shouldEqual "text/plain"

                        return response HttpStatusCode.OK (Some "healthy")
                    | "GET", "https://api.example.test/v1/public/download" ->
                        message.Headers.Accept
                        |> Seq.exactlyOne
                        |> _.MediaType
                        |> shouldEqual "application/octet-stream"

                        return response HttpStatusCode.OK (Some "binary payload")
                    | "POST", "https://api.example.test/v1/public/echo" ->
                        message.Headers.Accept
                        |> Seq.exactlyOne
                        |> _.MediaType
                        |> shouldEqual "text/plain"

                        message.Content.Headers.ContentType.MediaType |> shouldEqual "text/plain"
                        let! requestBody = message.Content.ReadAsStringAsync () |> Async.AwaitTask
                        requestBody |> shouldEqual "hello"
                        return response HttpStatusCode.OK (Some requestBody)
                    | "POST", "https://api.example.test/v1/public/anything" ->
                        message.Headers.Accept
                        |> Seq.exactlyOne
                        |> _.MediaType
                        |> shouldEqual "application/json"

                        message.Content.Headers.ContentType.MediaType |> shouldEqual "application/json"
                        let! requestBody = message.Content.ReadAsStringAsync () |> Async.AwaitTask
                        JsonNode.Parse requestBody |> ignore
                        return response HttpStatusCode.OK (Some requestBody)
                    | "GET", "https://api.example.test/v1/public/counter" ->
                        message.Headers.Accept
                        |> Seq.exactlyOne
                        |> _.MediaType
                        |> shouldEqual "application/json"

                        counterReads <- counterReads + 1

                        return response HttpStatusCode.OK (Some (if counterReads = 1 then "1e30" else "1.0"))
                    | "POST", "https://api.example.test/v1/public/counter" ->
                        message.Headers.Accept
                        |> Seq.exactlyOne
                        |> _.MediaType
                        |> shouldEqual "application/json"

                        message.Content.Headers.ContentType.MediaType |> shouldEqual "application/json"
                        let! requestBody = message.Content.ReadAsStringAsync () |> Async.AwaitTask
                        requestBody |> shouldEqual "1000000000000000000000000000000"
                        return response HttpStatusCode.OK (Some requestBody)
                    | method, uri -> return failwith $"Unexpected generated request: %s{method} %s{uri}"
                }

            use httpClient = HttpClientMock.makeNoUri handler
            let client = OpenApiPetstore.make httpClient

            let! fetched = client.GetPet 42L
            fetched.Id |> shouldEqual 42L
            fetched.Name |> shouldEqual "Ada"
            fetched.Parent |> Option.map _.Name |> shouldEqual (Some "Root")

            fetched.AdditionalProperties.["metadata"]
            |> Option.get
            |> fun metadata -> metadata.["source"].GetValue<string> ()
            |> shouldEqual "fixture"

            fetched.AdditionalProperties.["discarded"] |> shouldEqual None

            let roundTripped = Pet.toJsonNode fetched

            roundTripped.["metadata"].["source"].GetValue<string> ()
            |> shouldEqual "fixture"

            roundTripped.AsObject().ContainsKey "discarded" |> shouldEqual true
            isNull roundTripped.["discarded"] |> shouldEqual true

            let newPet =
                {
                    AdditionalProperties = Dictionary<string, JsonNode option> ()
                    Name = "Ada"
                    Tag = Some "cat"
                }

            let! created = client.CreatePet newPet
            created.Id |> shouldEqual 43L
            created.Tag |> shouldEqual (Some "cat")

            do! client.DeletePet 43L
            let! status = client.GetStatus ()
            status |> shouldEqual "healthy"

            use! download = client.Download ()
            use reader = new StreamReader (download)
            let! downloaded = reader.ReadToEndAsync ()
            downloaded |> shouldEqual "binary payload"

            let! echoed = client.Echo "hello"
            echoed |> shouldEqual "hello"

            let! anything = client.EchoAnything None
            anything |> shouldEqual None

            let anythingBody = JsonNode.Parse """{"nested":[1,null,"value"]}"""
            let! echoedAnything = client.EchoAnything (Some anythingBody)

            JsonNode.DeepEquals (echoedAnything |> Option.get, anythingBody)
            |> shouldEqual true

            isNull anythingBody.Parent |> shouldEqual true

            let! counter = client.GetCounter ()
            counter |> shouldEqual (BigInteger.Pow (10I, 30))

            let! zeroFractionCounter = client.GetCounter ()
            zeroFractionCounter |> shouldEqual 1I

            let! echoedCounter = client.EchoCounter counter
            echoedCounter |> shouldEqual counter
            calls |> shouldEqual 11
        }

    [<Test>]
    let ``Generated extension-data codecs round-trip every JSON value kind repeatedly`` () =
        let extensionCases : (string * JsonNode option) list =
            [
                "null", None
                "scalar", Some (JsonValue.Create 17 :> JsonNode)
                "object", Some (JsonNode.Parse """{"nested":"yes"}""")
                "array", Some (JsonNode.Parse """[1,null,"x"]""")
            ]

        for caseName, extensionValue in extensionCases do
            for hasParent in [ false ; true ] do
                let extensions = Dictionary<string, JsonNode option> ()
                extensions.Add (caseName, extensionValue |> Option.map (fun value -> value.DeepClone ()))

                let parent =
                    if hasParent then
                        Some
                            {
                                AdditionalProperties = Dictionary<string, JsonNode option> ()
                                Id = 1L
                                Name = "Parent"
                                Parent = None
                                Tag = None
                            }
                    else
                        None

                let pet =
                    {
                        AdditionalProperties = extensions
                        Id = 2L
                        Name = "Child"
                        Parent = parent
                        Tag = Some "tag"
                    }

                let first = Pet.toJsonNode pet
                let repeatedFromInput = Pet.toJsonNode pet
                JsonNode.DeepEquals (first, repeatedFromInput) |> shouldEqual true

                let parsed = Pet.jsonParse first
                let second = Pet.toJsonNode parsed
                let third = Pet.toJsonNode parsed
                JsonNode.DeepEquals (first, second) |> shouldEqual true
                JsonNode.DeepEquals (second, third) |> shouldEqual true
