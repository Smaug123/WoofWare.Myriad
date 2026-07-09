namespace WoofWare.Myriad.Plugins.Test

open System
open System.Net
open System.Net.Http
open NUnit.Framework
open PureGym
open FsUnitTyped

[<TestFixture>]
module TestListQueryParam =

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
    let ``Each list element becomes its own key=value pair`` () =
        use client = makeClient "https://example.com/endpoint?tag=a&tag=b&limit=10"
        let api = ApiWithListQuery.make client

        api.GetWithListQuery([ "a" ; "b" ], 10).Result |> shouldEqual "response"

    [<Test>]
    let ``An empty list contributes nothing`` () =
        use client = makeClient "https://example.com/endpoint?limit=10"
        let api = ApiWithListQuery.make client

        api.GetWithListQuery([], 10).Result |> shouldEqual "response"

    [<Test>]
    let ``A sole empty list leaves the URL bare`` () =
        use client = makeClient "https://example.com/endpoint"
        let api = ApiWithListQuery.make client

        api.GetWithSoleListQuery([]).Result |> shouldEqual "response"

    [<Test>]
    let ``List elements are escaped individually`` () =
        use client = makeClient "https://example.com/endpoint?tag=a%20b&tag=c%26d&limit=10"
        let api = ApiWithListQuery.make client

        api.GetWithListQuery([ "a b" ; "c&d" ], 10).Result |> shouldEqual "response"

    [<Test>]
    let ``Array-typed query parameters behave like lists`` () =
        use client = makeClient "https://example.com/endpoint?id=1&id=2&id=3"
        let api = ApiWithListQuery.make client

        api.GetWithArrayQuery([| 1 ; 2 ; 3 |]).Result |> shouldEqual "response"
