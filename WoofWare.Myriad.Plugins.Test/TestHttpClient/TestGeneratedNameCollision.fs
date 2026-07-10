namespace WoofWare.Myriad.Plugins.Test

open System
open System.Net
open System.Net.Http
open NUnit.Framework
open PureGym
open FsUnitTyped

/// The generator introduces local bindings (e.g. to hold the serialised query string) into the
/// scope in which the URI, body and headers are constructed. Those names must not capture user
/// parameters which happen to share them.
[<TestFixture>]
module TestGeneratedNameCollision =

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
    let ``A path parameter named queryString is not shadowed`` () =
        use client = makeClient "https://example.com/endpoint/hello?limit=10"
        let api = ApiShadowingGeneratedNames.make client

        api.GetWithPathNamedQueryString("hello", 10).Result |> shouldEqual "response"

    [<Test>]
    let ``A query parameter named queryString is not shadowed`` () =
        use client = makeClient "https://example.com/endpoint?queryString=hello&limit=10"
        let api = ApiShadowingGeneratedNames.make client

        api.GetWithQueryNamedQueryString("hello", 10).Result |> shouldEqual "response"
