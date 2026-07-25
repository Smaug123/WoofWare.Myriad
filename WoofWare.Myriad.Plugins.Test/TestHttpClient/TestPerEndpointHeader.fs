namespace WoofWare.Myriad.Plugins.Test

open System
open System.Net
open System.Net.Http
open System.Threading
open NUnit.Framework
open FsUnitTyped
open PureGym

/// Headers whose values come from an interface property, but which are stamped only onto the
/// endpoints that ask for them. This is how per-operation OpenAPI security requirements are
/// expressed: two endpoints of the same API may need different credentials, or none.
[<TestFixture>]
module TestPerEndpointHeader =

    /// Echoes the request's headers back, one per line, so the test can assert on them.
    let private echoHeaders (message : HttpRequestMessage) : HttpResponseMessage Async =
        async {
            let headers =
                [
                    for h in message.Headers do
                        yield $"%s{h.Key}: %s{Seq.exactlyOne h.Value}"
                ]
                |> List.sort
                |> String.concat "\n"

            let resp = new HttpResponseMessage (HttpStatusCode.OK)
            resp.Content <- new StringContent (headers)
            return resp
        }

    let private makeApi (client : HttpClient) (bearerReads : int ref) (apiKeyReads : int ref) =
        let bearerToken () =
            Interlocked.Increment bearerReads |> ignore
            "token-value"

        let apiKey () =
            Interlocked.Increment apiKeyReads |> ignore
            42

        ApiWithPerEndpointHeaders.make bearerToken apiKey (fun () -> "everywhere") client

    [<Test>]
    let ``An endpoint gets only the credentials it asks for`` () =
        use client = HttpClientMock.make (Uri "https://example.com") echoHeaders
        let bearerReads = ref 0
        let apiKeyReads = ref 0
        let api = makeApi client bearerReads apiKeyReads

        api.Authorized("param").Result.Split "\n"
        |> shouldEqual [| "Authorization: token-value" ; "X-Everywhere: everywhere" |]

        bearerReads.Value |> shouldEqual 1
        apiKeyReads.Value |> shouldEqual 0

    [<Test>]
    let ``An endpoint requiring several credentials gets all of them`` () =
        use client = HttpClientMock.make (Uri "https://example.com") echoHeaders
        let bearerReads = ref 0
        let apiKeyReads = ref 0
        let api = makeApi client bearerReads apiKeyReads

        api.Both("param").Result.Split "\n"
        |> shouldEqual
            [|
                "Authorization: token-value"
                "X-Api-Key: 42"
                "X-Everywhere: everywhere"
            |]

        bearerReads.Value |> shouldEqual 1
        apiKeyReads.Value |> shouldEqual 1

    [<Test>]
    let ``An endpoint requiring no credentials is not sent any`` () =
        use client = HttpClientMock.make (Uri "https://example.com") echoHeaders
        let bearerReads = ref 0
        let apiKeyReads = ref 0
        let api = makeApi client bearerReads apiKeyReads

        api.Anonymous("param").Result.Split "\n"
        |> shouldEqual [| "X-Everywhere: everywhere" |]

        // The credentials aren't merely omitted from the request: they're never even asked for,
        // so a caller who has no credentials at all can still call this endpoint.
        bearerReads.Value |> shouldEqual 0
        apiKeyReads.Value |> shouldEqual 0

    [<Test>]
    let ``Credentials are re-read on every request`` () =
        use client = HttpClientMock.make (Uri "https://example.com") echoHeaders
        let bearerReads = ref 0
        let apiKeyReads = ref 0
        let api = makeApi client bearerReads apiKeyReads

        for _ in 1..3 do
            api.Authorized("param").Result |> ignore<string>

        bearerReads.Value |> shouldEqual 3

    /// The generated `make` takes one argument per property and an HttpClient of its own; a property
    /// named `Client` claims the name the HttpClient would otherwise have had, so the generator has
    /// to rename its own argument. If it didn't, the generated source wouldn't compile at all.
    [<Test>]
    let ``A property named Client does not collide with the generated HttpClient argument`` () =
        use client = HttpClientMock.make (Uri "https://example.com") echoHeaders
        let api = ApiWithClientProperty.make (fun () -> "clash") client

        api.Get().Result.Split "\n" |> shouldEqual [| "X-Client: clash" |]
