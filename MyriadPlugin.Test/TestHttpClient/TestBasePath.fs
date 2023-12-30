namespace MyriadPlugin.Test

open System
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
                let content = new StringContent (message.RequestUri.ToString ())
                let resp = new HttpResponseMessage (HttpStatusCode.OK)
                resp.Content <- content
                return resp
            }

        use client = HttpClientMock.makeNoUri proc
        let api = PureGymApi.make client

        let observedUri = api.GetPathParam("param").Result
        observedUri |> shouldEqual "https://whatnot.com/endpoint/param"

    [<Test>]
    let ``Without a base path but with BaseAddress, request goes through`` () =
        let proc (message : HttpRequestMessage) : HttpResponseMessage Async =
            async {
                message.Method |> shouldEqual HttpMethod.Get
                let content = new StringContent (message.RequestUri.ToString ())
                let resp = new HttpResponseMessage (HttpStatusCode.OK)
                resp.Content <- content
                return resp
            }

        use client = HttpClientMock.make (System.Uri "https://baseaddress.com") proc
        let api = ApiWithoutBasePath.make client

        let observedUri = api.GetPathParam("param").Result
        observedUri |> shouldEqual "https://baseaddress.com/endpoint/param"

    [<Test>]
    let ``Without a base path, request throws`` () =
        let proc (message : HttpRequestMessage) : HttpResponseMessage Async =
            async {
                message.Method |> shouldEqual HttpMethod.Get
                let content = new StringContent (message.RequestUri.ToString ())
                let resp = new HttpResponseMessage (HttpStatusCode.OK)
                resp.Content <- content
                return resp
            }

        use client = HttpClientMock.makeNoUri proc
        let api = ApiWithoutBasePath.make client

        let observedExc =
            async {
                let! result = api.GetPathParam ("param") |> Async.AwaitTask |> Async.Catch

                match result with
                | Choice1Of2 _ -> return failwith "test failure"
                | Choice2Of2 exc -> return exc
            }
            |> Async.RunSynchronously

        let observedExc =
            match observedExc with
            | :? AggregateException as exc ->
                match exc.InnerException with
                | :? ArgumentNullException as exc -> exc
                | _ -> failwith "test failure"
            | _ -> failwith "test failure"

        observedExc.Message
        |> shouldEqual
            "No base path was supplied on the type, and no BaseAddress was on the HttpClient. (Parameter 'BaseAddress')"
