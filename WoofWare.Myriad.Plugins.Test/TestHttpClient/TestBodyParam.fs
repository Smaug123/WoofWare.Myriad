namespace WoofWare.Myriad.Plugins.Test

open System
open System.IO
open System.Net
open System.Net.Http
open System.Text.Json.Nodes
open NUnit.Framework
open PureGym
open FsUnitTyped

[<TestFixture>]
module TestBodyParam =

    [<Test>]
    let ``Body param of string`` () =
        let proc (message : HttpRequestMessage) : HttpResponseMessage Async =
            async {
                message.Method |> shouldEqual HttpMethod.Post
                let! content = message.Content.ReadAsStringAsync () |> Async.AwaitTask
                let content = new StringContent (content)
                let resp = new HttpResponseMessage (HttpStatusCode.OK)
                resp.Content <- content
                return resp
            }

        use client = HttpClientMock.make (Uri "https://example.com") proc
        let api = PureGymApi.make client

        let observedUri = api.CreateUserString("username?not!url%encoded").Result
        observedUri |> shouldEqual "username?not!url%encoded"

    [<Test>]
    let ``Body param of stream`` () =
        let proc (message : HttpRequestMessage) : HttpResponseMessage Async =
            async {
                message.Method |> shouldEqual HttpMethod.Post
                let! content = message.Content.ReadAsStreamAsync () |> Async.AwaitTask
                let content = new StreamContent (content)
                let resp = new HttpResponseMessage (HttpStatusCode.OK)
                resp.Content <- content
                return resp
            }

        let contents = [| 1uy ; 2uy ; 3uy ; 4uy |]

        use client = HttpClientMock.make (Uri "https://example.com") proc
        let api = PureGymApi.make client

        use stream = new MemoryStream (contents)
        let observedContent = api.CreateUserStream(stream).Result
        let buf = Array.zeroCreate 10
        let written = observedContent.ReadAtLeast (buf.AsSpan (), 5, false)
        buf |> Array.take written |> shouldEqual contents

    [<Test>]
    let ``Body param of HttpContent`` () =
        let mutable observedContent = None

        let proc (message : HttpRequestMessage) : HttpResponseMessage Async =
            async {
                message.Method |> shouldEqual HttpMethod.Post
                let resp = new HttpResponseMessage (HttpStatusCode.OK)
                observedContent <- Some message.Content
                resp.Content <- new StringContent ("oh hi")
                return resp
            }

        use client = HttpClientMock.make (Uri "https://example.com") proc
        let api = PureGymApi.make client

        use content = new StringContent ("hello!")

        api.CreateUserHttpContent(content).Result |> shouldEqual "oh hi"
        Object.ReferenceEquals (Option.get observedContent, content) |> shouldEqual true

    [<TestCase "ByteArr">]
    [<TestCase "ByteArr'">]
    [<TestCase "ByteArr''">]
    let ``Body param of byte arr`` (case : string) =
        let proc (message : HttpRequestMessage) : HttpResponseMessage Async =
            async {
                message.Method |> shouldEqual HttpMethod.Post
                let! content = message.Content.ReadAsStreamAsync () |> Async.AwaitTask
                let content = new StreamContent (content)
                let resp = new HttpResponseMessage (HttpStatusCode.OK)
                resp.Content <- content
                return resp
            }

        use client = HttpClientMock.make (Uri "https://example.com") proc
        let api = PureGymApi.make client

        let contents = [| 1uy ; 2uy ; 3uy ; 4uy |]

        let observedContent =
            match case with
            | "ByteArr" -> api.CreateUserByteArr(contents).Result
            | "ByteArr'" -> api.CreateUserByteArr'(contents).Result
            | "ByteArr''" -> api.CreateUserByteArr''(contents).Result
            | _ -> failwith $"Unrecognised case: %s{case}"

        let buf = Array.zeroCreate 10
        let written = observedContent.ReadAtLeast (buf.AsSpan (), 5, false)
        buf |> Array.take written |> shouldEqual contents
