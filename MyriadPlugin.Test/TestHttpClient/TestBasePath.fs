namespace MyriadPlugin.Test

open System.Net
open System.Net.Http
open NUnit.Framework
open PureGym
open FsUnitTyped

[<TestFixture>]
module TestBasePath =
    [<Test>]
    let ``Base path is respected`` () =
        let proc (message : HttpRequestMessage) : HttpResponseMessage Async =
            async {
                message.Method |> shouldEqual HttpMethod.Get
                message.Content.ReadAsStringAsync().Result |> shouldEqual "param"
                let content = new StringContent (message.RequestUri.ToString ())
                let resp = new HttpResponseMessage (HttpStatusCode.OK)
                resp.Content <- content
                return resp
            }

        use client = HttpClientMock.makeNoUri proc
        let api = PureGymApi.make client

        let observedUri = api.GetPathParam("param").Result
        observedUri |> shouldEqual "https://whatnot.com/endpoint/param"
