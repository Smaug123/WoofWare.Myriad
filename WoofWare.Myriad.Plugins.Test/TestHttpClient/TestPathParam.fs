namespace WoofWare.Myriad.Plugins.Test

open System
open System.Net
open System.Net.Http
open NUnit.Framework
open FsUnitTyped
open PureGym

[<TestFixture>]
module TestPathParam =

    [<Test>]
    let ``Path params are escaped`` () =
        let proc (message : HttpRequestMessage) : HttpResponseMessage Async =
            async {
                message.Method |> shouldEqual HttpMethod.Get

                let expectedUriPrefix = "https://example.com/endpoint/"

                let actualUri = message.RequestUri.ToString ()

                if not (actualUri.StartsWith (expectedUriPrefix, StringComparison.Ordinal)) then
                    failwith $"wrong prefix on %s{actualUri}"

                let content = new StringContent (actualUri.Substring expectedUriPrefix.Length)
                let resp = new HttpResponseMessage (HttpStatusCode.OK)
                resp.Content <- content
                return resp
            }

        use client = HttpClientMock.make (Uri "https://example.com") proc
        let api = PureGymApi.make client

        api.GetPathParam("hello/world?(hi)").Result
        |> shouldEqual "hello%2fworld%3f(hi)"
