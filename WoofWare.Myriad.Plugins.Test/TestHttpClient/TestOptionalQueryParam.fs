namespace WoofWare.Myriad.Plugins.Test

open System
open System.Net
open System.Net.Http
open NUnit.Framework
open PureGym
open FsUnitTyped

[<TestFixture>]
module TestOptionalQueryParam =

    let private makeClient (expectedUri : string) : HttpClientMock =
        let proc (message : HttpRequestMessage) : HttpResponseMessage Async =
            async {
                message.Method |> shouldEqual HttpMethod.Get
                message.RequestUri.AbsoluteUri |> shouldEqual expectedUri

                let resp = new HttpResponseMessage (HttpStatusCode.OK)
                resp.Content <- new StringContent ("response")
                return resp
            }

        HttpClientMock.make (Uri "https://example.com") proc

    [<Test>]
    let ``All params present`` () =
        use client = makeClient "https://example.com/endpoint?page=3&limit=10&search=hello"
        let api = ApiWithOptionalQuery.make client

        api.GetWithMixedQuery(Some 3, 10, Some "hello").Result |> shouldEqual "response"

    [<Test>]
    let ``First param missing`` () =
        use client = makeClient "https://example.com/endpoint?limit=10&search=hello"
        let api = ApiWithOptionalQuery.make client

        api.GetWithMixedQuery(None, 10, Some "hello").Result |> shouldEqual "response"

    [<Test>]
    let ``Last param missing`` () =
        use client = makeClient "https://example.com/endpoint?page=3&limit=10"
        let api = ApiWithOptionalQuery.make client

        api.GetWithMixedQuery(Some 3, 10, None).Result |> shouldEqual "response"

    [<Test>]
    let ``Optional params are escaped`` () =
        use client = makeClient "https://example.com/endpoint?limit=10&search=a%20b"
        let api = ApiWithOptionalQuery.make client

        api.GetWithMixedQuery(None, 10, Some "a b").Result |> shouldEqual "response"

    [<Test>]
    let ``Sole optional param present`` () =
        use client = makeClient "https://example.com/endpoint?since=2024-01-15"
        let api = ApiWithOptionalQuery.make client

        api.GetWithAllOptionalQuery(Some (DateOnly (2024, 1, 15))).Result
        |> shouldEqual "response"

    [<Test>]
    let ``Sole optional param missing leaves the URL bare`` () =
        use client = makeClient "https://example.com/endpoint"
        let api = ApiWithOptionalQuery.make client

        api.GetWithAllOptionalQuery(None).Result |> shouldEqual "response"

    [<Test>]
    let ``Optional list param present contributes one pair per element`` () =
        use client = makeClient "https://example.com/endpoint?tag=a&tag=b&limit=10"
        let api = ApiWithOptionalQuery.make client

        api.GetWithOptionalListQuery(Some [ "a" ; "b" ], 10).Result
        |> shouldEqual "response"

    [<Test>]
    let ``Optional list param empty contributes nothing`` () =
        use client = makeClient "https://example.com/endpoint?limit=10"
        let api = ApiWithOptionalQuery.make client

        api.GetWithOptionalListQuery(Some [], 10).Result |> shouldEqual "response"

    [<Test>]
    let ``Optional list param missing contributes nothing`` () =
        use client = makeClient "https://example.com/endpoint?limit=10"
        let api = ApiWithOptionalQuery.make client

        api.GetWithOptionalListQuery(None, 10).Result |> shouldEqual "response"

    [<Test>]
    let ``Optional array param present contributes one pair per element`` () =
        use client = makeClient "https://example.com/endpoint?id=1&id=2&limit=10"
        let api = ApiWithOptionalQuery.make client

        api.GetWithOptionalArrayQuery(Some [| 1 ; 2 |], 10).Result
        |> shouldEqual "response"

    [<Test>]
    let ``Optional array param missing contributes nothing`` () =
        use client = makeClient "https://example.com/endpoint?limit=10"
        let api = ApiWithOptionalQuery.make client

        api.GetWithOptionalArrayQuery(None, 10).Result |> shouldEqual "response"
