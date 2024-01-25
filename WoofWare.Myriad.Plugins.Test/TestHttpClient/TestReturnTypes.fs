namespace WoofWare.Myriad.Plugins.Test

open System
open System.IO
open System.Net
open FsUnitTyped
open System.Net.Http
open PureGym
open NUnit.Framework

[<TestFixture>]
module TestReturnTypes =

    [<Test>]
    let ``String return`` () =
        let proc (message : HttpRequestMessage) : HttpResponseMessage Async =
            async {
                message.Method |> shouldEqual HttpMethod.Get
                let content = new StringContent ("this is not a JSON string")
                let resp = new HttpResponseMessage (HttpStatusCode.OK)
                resp.Content <- content
                return resp
            }

        use client = HttpClientMock.make (Uri "https://example.com") proc
        let api = PureGymApi.make client

        api.GetPathParam("hi").Result |> shouldEqual "this is not a JSON string"

    [<TestCase "GetStream">]
    [<TestCase "GetStream'">]
    [<TestCase "GetStream''">]
    let ``Stream return`` (case : string) =
        let result = [| 1uy ; 2uy ; 3uy ; 4uy |]

        let proc (message : HttpRequestMessage) : HttpResponseMessage Async =
            async {
                message.Method |> shouldEqual HttpMethod.Get
                let result = new MemoryStream (result)
                let content = new StreamContent (result)
                let resp = new HttpResponseMessage (HttpStatusCode.OK)
                resp.Content <- content
                return resp
            }

        use client = HttpClientMock.make (Uri "https://example.com") proc
        let api = PureGymApi.make client

        use stream =
            match case with
            | "GetStream" -> api.GetStream().Result
            | "GetStream'" -> api.GetStream'().Result
            | "GetStream''" -> api.GetStream''().Result
            | _ -> failwith $"unrecognised case: %s{case}"

        let buf = Array.zeroCreate 10
        let written = stream.ReadAtLeast (buf.AsSpan (), 10, false)
        Array.take written buf |> shouldEqual result

    [<TestCase "GetResponseMessage">]
    [<TestCase "GetResponseMessage'">]
    [<TestCase "GetResponseMessage''">]
    [<TestCase "GetResponseMessage'''">]
    let ``HttpResponseMessage return`` (case : string) =
        let mutable responseMessage = None

        let proc (message : HttpRequestMessage) : HttpResponseMessage Async =
            async {
                message.Method |> shouldEqual HttpMethod.Get
                let content = new StringContent ("a response!")
                let resp = new HttpResponseMessage (HttpStatusCode.OK)
                resp.Content <- content
                responseMessage <- Some resp
                return resp
            }

        use client = HttpClientMock.make (Uri "https://example.com") proc
        let api = PureGymApi.make client

        let message =
            match case with
            | "GetResponseMessage" -> api.GetResponseMessage().Result
            | "GetResponseMessage'" -> api.GetResponseMessage'().Result
            | "GetResponseMessage''" -> api.GetResponseMessage''().Result
            | "GetResponseMessage'''" -> api.GetResponseMessage'''().Result
            | _ -> failwith $"unrecognised case: %s{case}"

        Object.ReferenceEquals (message, Option.get responseMessage) |> shouldEqual true

    [<TestCase "Task<Response>">]
    [<TestCase "Task<RestEase.Response>">]
    [<TestCase "RestEase.Response Task">]
    [<TestCase "RestEase.Response Task">]
    let ``Response return`` (case : string) =
        for json, memberDto in PureGymDtos.memberActivityDtoCases do
            let mutable responseMessage = None

            let proc (message : HttpRequestMessage) : HttpResponseMessage Async =
                async {
                    message.Method |> shouldEqual HttpMethod.Get
                    let content = new StringContent (json)
                    let resp = new HttpResponseMessage (HttpStatusCode.OK)
                    resp.Content <- content
                    responseMessage <- Some resp
                    return resp
                }

            use client = HttpClientMock.make (Uri "https://example.com") proc
            let api = PureGymApi.make client

            let response =
                match case with
                | "Task<Response>" -> api.GetResponse().Result
                | "Task<RestEase.Response>" -> api.GetResponse'().Result
                | "Response Task" -> api.GetResponse''().Result
                | "RestEase.Response Task" -> api.GetResponse'''().Result
                | _ -> failwith $"unrecognised case: %s{case}"

            response.ResponseMessage |> shouldEqual (Option.get responseMessage)
            response.StringContent |> shouldEqual json
            response.GetContent () |> shouldEqual memberDto
