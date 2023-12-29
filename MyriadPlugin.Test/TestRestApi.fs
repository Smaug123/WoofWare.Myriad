namespace MyriadPlugin.Test

open System
open System.Net
open System.Net.Http
open NUnit.Framework
open PureGym
open FsUnitTyped

[<TestFixture>]
module TestRestApi =
    // several of these, to check behaviour around treatment of initial slashes
    let baseUris = [ "https://example.com" ; "https://example.com/foo" ] |> List.map Uri

    let gymCases =
        PureGymDtos.gymCases |> List.allPairs baseUris |> List.map TestCaseData

    [<TestCaseSource(nameof gymCases)>]
    let ``Test GetGym`` (baseUri : Uri, (json : string, expected : Gym)) =
        let requestedGym = 3

        let proc (message : HttpRequestMessage) : HttpResponseMessage Async =
            async {
                message.Method |> shouldEqual HttpMethod.Get

                message.RequestUri.ToString ()
                |> shouldEqual $"https://example.com/v1/gyms/%i{requestedGym}"

                let content = new StringContent (json)
                let resp = new HttpResponseMessage (HttpStatusCode.OK)
                resp.Content <- content
                return resp
            }

        use client = HttpClientMock.make baseUri proc
        let api = PureGymApi.make client

        api.GetGym(requestedGym).Result |> shouldEqual expected
