namespace WoofWare.Myriad.Plugins.Test

open System
open System.IO
open System.Net
open System.Net.Http
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

    [<Test>]
    let ``Body param of serialised thing`` () =
        let proc (message : HttpRequestMessage) : HttpResponseMessage Async =
            async {
                message.Method |> shouldEqual HttpMethod.Post
                let! content = message.Content.ReadAsStringAsync () |> Async.AwaitTask
                let content = new StringContent ("Done! " + content)
                let resp = new HttpResponseMessage (HttpStatusCode.OK)
                resp.Content <- content
                return resp
            }

        use client = HttpClientMock.make (Uri "https://example.com") proc
        let api = PureGymApi.make client

        let expected =
            {
                Id = 3
                CompoundMemberId = "compound!"
                FirstName = "Patrick"
                LastName = "Stevens"
                HomeGymId = 100
                HomeGymName = "Big Boy Gym"
                EmailAddress = "woof@ware"
                GymAccessPin = "l3tm31n"
                // To the reader: what's the significance of this date?
                // answer rot13: ghevatpbzchgnovyvglragfpurvqhatfceboyrzcncre
                DateOfBirth = DateOnly (1936, 05, 28)
                MobileNumber = "+44-GHOST-BUSTERS"
                Postcode = "W1A 111"
                MembershipName = "mario"
                MembershipLevel = 4
                SuspendedReason = 1090
                MemberStatus = -3
            }

        let result = api.CreateUserSerialisedBody(expected).Result

        result.StartsWith ("Done! ", StringComparison.Ordinal) |> shouldEqual true
        let result = result.[6..]

        result
        |> System.Text.Json.Nodes.JsonNode.Parse
        |> PureGym.Member.jsonParse
        |> shouldEqual expected

    [<Test>]
    let ``Body param of primitive: int`` () =
        let proc (message : HttpRequestMessage) : HttpResponseMessage Async =
            async {
                message.Method |> shouldEqual HttpMethod.Post
                let! content = message.Content.ReadAsStringAsync () |> Async.AwaitTask
                let content = new StringContent ("Done! " + content)
                let resp = new HttpResponseMessage (HttpStatusCode.OK)
                resp.Content <- content
                return resp
            }

        use client = HttpClientMock.make (Uri "https://example.com") proc
        let api = PureGymApi.make client

        let result = api.CreateUserSerialisedIntBody(3).Result

        result |> shouldEqual "Done! 3"

    [<Test>]
    let ``Body param of primitive: Uri`` () =
        let proc (message : HttpRequestMessage) : HttpResponseMessage Async =
            async {
                message.Method |> shouldEqual HttpMethod.Post
                let! content = message.Content.ReadAsStringAsync () |> Async.AwaitTask
                let content = new StringContent ("Done! " + content)
                let resp = new HttpResponseMessage (HttpStatusCode.OK)
                resp.Content <- content
                return resp
            }

        use client = HttpClientMock.make (Uri "https://example.com") proc
        let api = PureGymApi.make client

        let result = api.CreateUserSerialisedUrlBody(Uri "https://mything.com/blah").Result

        result |> shouldEqual "Done! \"https://mything.com/blah\""
