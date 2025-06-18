namespace WoofWare.Myriad.Plugins.Test

open System
open System.Net
open System.Net.Http
open System.Text.Json.Nodes
open System.Threading
open NUnit.Framework
open FsUnitTyped
open PureGym
open WoofWare.Expect

[<TestFixture>]
module TestVariableHeader =

    [<Test>]
    let ``Headers are set`` () : unit =
        let proc (message : HttpRequestMessage) : HttpResponseMessage Async =
            async {
                message.Method |> shouldEqual HttpMethod.Get

                message.RequestUri.ToString ()
                |> shouldEqual "https://example.com/endpoint/param"

                let headers =
                    [
                        for h in message.Headers do
                            yield $"%s{h.Key}: %s{Seq.exactlyOne h.Value}"
                    ]
                    |> String.concat "\n"

                let content = new StringContent (headers)
                let resp = new HttpResponseMessage (HttpStatusCode.OK)
                resp.Content <- content
                return resp
            }

        use client = HttpClientMock.make (Uri "https://example.com") proc

        let someHeaderCount = ref 10

        let someHeader () =
            (Interlocked.Increment someHeaderCount : int).ToString ()

        let someOtherHeaderCount = ref -100

        let someOtherHeader () =
            Interlocked.Increment someOtherHeaderCount

        let api = ApiWithHeaders.make someHeader someOtherHeader client

        someHeaderCount.Value |> shouldEqual 10
        someOtherHeaderCount.Value |> shouldEqual -100

        expect {
            snapshotJson
                @"[
  ""Authorization: -99"",
  ""Header-Name: Header-Value"",
  ""Something-Else: val"",
  ""X-Foo: 11""
]"

            return api.GetPathParam("param").Result.Split "\n" |> Array.sort
        }

        someHeaderCount.Value |> shouldEqual 11
        someOtherHeaderCount.Value |> shouldEqual -99

    [<Test>]
    let ``Headers get re-evaluated every time`` () : unit =
        let proc (message : HttpRequestMessage) : HttpResponseMessage Async =
            async {
                message.Method |> shouldEqual HttpMethod.Get

                message.RequestUri.ToString ()
                |> shouldEqual "https://example.com/endpoint/param"

                let headers =
                    [
                        for h in message.Headers do
                            yield $"%s{h.Key}: %s{Seq.exactlyOne h.Value}"
                    ]
                    |> String.concat "\n"

                let content = new StringContent (headers)
                let resp = new HttpResponseMessage (HttpStatusCode.OK)
                resp.Content <- content
                return resp
            }

        use client = HttpClientMock.make (Uri "https://example.com") proc

        let someHeaderCount = ref 10

        let someHeader () =
            (Interlocked.Increment someHeaderCount : int).ToString ()

        let someOtherHeaderCount = ref -100

        let someOtherHeader () =
            Interlocked.Increment someOtherHeaderCount

        let api = ApiWithHeaders.make someHeader someOtherHeader client

        someHeaderCount.Value |> shouldEqual 10
        someOtherHeaderCount.Value |> shouldEqual -100

        expect {
            snapshotJson
                @"[
  ""Authorization: -99"",
  ""Header-Name: Header-Value"",
  ""Something-Else: val"",
  ""X-Foo: 11""
]"

            return api.GetPathParam("param").Result.Split "\n" |> Array.sort
        }

        expect {
            snapshotJson
                @"[
  ""Authorization: -98"",
  ""Header-Name: Header-Value"",
  ""Something-Else: val"",
  ""X-Foo: 12""
]"

            return api.GetPathParam("param").Result.Split "\n" |> Array.sort
        }

        someHeaderCount.Value |> shouldEqual 12
        someOtherHeaderCount.Value |> shouldEqual -98

    let pureGymMember =
        {
            Id = 3
            CompoundMemberId = "compound"
            FirstName = "Patrick"
            LastName = "Stevens"
            HomeGymId = 1223
            HomeGymName = "Arnie's Temple o' Gainz"
            EmailAddress = "patrick@home"
            GymAccessPin = "1234"
            DateOfBirth = DateOnly (1992, 03, 04)
            MobileNumber = "number"
            Postcode = "postcode"
            MembershipName = "member"
            MembershipLevel = 9999
            SuspendedReason = -1
            MemberStatus = 100
        }

    [<Test>]
    let ``Content-Type header is automatically inserted`` () =
        let proc (message : HttpRequestMessage) : HttpResponseMessage Async =
            async {
                message.Method |> shouldEqual HttpMethod.Post

                message.RequestUri.ToString ()
                |> shouldEqual "https://example.com/endpoint/param"

                let headers =
                    [
                        for h in message.Content.Headers do
                            yield $"%s{h.Key}: %s{Seq.exactlyOne h.Value}"
                    ]
                    |> String.concat "\n"

                let! ct = Async.CancellationToken
                let! content = message.Content.ReadAsStringAsync ct |> Async.AwaitTask
                content |> JsonNode.Parse |> Member.jsonParse |> shouldEqual pureGymMember

                let content = new StringContent (headers)
                let resp = new HttpResponseMessage (HttpStatusCode.OK)
                resp.Content <- content
                return resp
            }

        use client = HttpClientMock.make (Uri "https://example.com") proc

        let api = ClientWithJsonBody.make client
        let result = api.GetPathParam ("param", pureGymMember) |> _.Result

        expect {
            snapshot @"Content-Type: application/json; charset=utf-8"
            return result
        }

    [<Test>]
    let ``Content-Type header is respected if overridden`` () =
        let proc (message : HttpRequestMessage) : HttpResponseMessage Async =
            async {
                message.Method |> shouldEqual HttpMethod.Post

                message.RequestUri.ToString ()
                |> shouldEqual "https://example.com/endpoint/param"

                let headers =
                    [
                        for h in message.Content.Headers do
                            yield $"%s{h.Key}: %s{Seq.exactlyOne h.Value}"
                    ]
                    |> String.concat "\n"

                let! ct = Async.CancellationToken
                let! content = message.Content.ReadAsStringAsync ct |> Async.AwaitTask
                content |> JsonNode.Parse |> Member.jsonParse |> shouldEqual pureGymMember

                let content = new StringContent (headers)
                let resp = new HttpResponseMessage (HttpStatusCode.OK)
                resp.Content <- content
                return resp
            }

        use client = HttpClientMock.make (Uri "https://example.com") proc

        let api = ClientWithJsonBodyOverridden.make client
        let result = api.GetPathParam ("param", pureGymMember) |> _.Result

        expect {
            snapshot @"Content-Type: application/text; charset=utf-8"
            return result
        }

    [<Test>]
    let ``Content-Type header is the default for strings`` () =
        let proc (message : HttpRequestMessage) : HttpResponseMessage Async =
            async {
                message.Method |> shouldEqual HttpMethod.Post

                message.RequestUri.ToString ()
                |> shouldEqual "https://example.com/endpoint/param"

                let headers =
                    [
                        for h in message.Content.Headers do
                            yield $"%s{h.Key}: %s{Seq.exactlyOne h.Value}"
                    ]
                    |> String.concat "\n"

                let! ct = Async.CancellationToken
                let! content = message.Content.ReadAsStringAsync ct |> Async.AwaitTask
                content |> shouldEqual "hello!"

                let content = new StringContent (headers)
                let resp = new HttpResponseMessage (HttpStatusCode.OK)
                resp.Content <- content
                return resp
            }

        use client = HttpClientMock.make (Uri "https://example.com") proc

        let api = ClientWithStringBody.make client
        let result = api.GetPathParam ("param", "hello!") |> _.Result

        expect {
            snapshot @"Content-Type: application/text; charset=utf-8"
            return result
        }
