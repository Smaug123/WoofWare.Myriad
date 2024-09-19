namespace WoofWare.Myriad.Plugins.Test

open System
open System.Net
open System.Net.Http
open System.Threading
open NUnit.Framework
open FsUnitTyped
open PureGym

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

        api.GetPathParam("param").Result.Split "\n"
        |> Array.sort
        |> shouldEqual
            [|
                "Authorization: -99"
                "Header-Name: Header-Value"
                "Something-Else: val"
                "X-Foo: 11"
            |]

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

        api.GetPathParam("param").Result.Split "\n"
        |> Array.sort
        |> shouldEqual
            [|
                "Authorization: -99"
                "Header-Name: Header-Value"
                "Something-Else: val"
                "X-Foo: 11"
            |]

        api.GetPathParam("param").Result.Split "\n"
        |> Array.sort
        |> shouldEqual
            [|
                "Authorization: -98"
                "Header-Name: Header-Value"
                "Something-Else: val"
                "X-Foo: 12"
            |]

        someHeaderCount.Value |> shouldEqual 12
        someOtherHeaderCount.Value |> shouldEqual -98
