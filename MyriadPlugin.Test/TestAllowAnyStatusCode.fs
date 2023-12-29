namespace MyriadPlugin.Test

open System
open System.Net
open System.Net.Http
open NUnit.Framework
open FsUnitTyped
open PureGym

[<TestFixture>]
module TestAllowAnyStatusCode =

    [<Test>]
    let ``Without AllowAnyStatusCode we throw`` () =
        let proc (message : HttpRequestMessage) : HttpResponseMessage Async =
            async {
                message.Method |> shouldEqual HttpMethod.Get
                let content = new StringContent ("nothing was here :(")
                let resp = new HttpResponseMessage (HttpStatusCode.NotFound)
                resp.Content <- content
                return resp
            }

        use client = HttpClientMock.make (Uri "https://example.com") proc
        let api = PureGymApi.make client

        let exc =
            async {
                let! message = Async.AwaitTask (api.GetWithoutAnyReturnCode ()) |> Async.Catch

                match message with
                | Choice1Of2 _ -> return failwith "test failure"
                | Choice2Of2 exc -> return exc
            }
            |> Async.RunSynchronously

        let exc =
            match exc with
            | :? AggregateException as exc -> exc
            | exc -> failwith $"Test failure: expected AggregateException, got %+A{exc}"

        match exc.InnerException with
        | :? HttpRequestException as exc -> exc.Message.Contains "404 (Not Found)" |> shouldEqual true
        | e -> failwith $"Test failure: %+A{e}"

    [<Test>]
    let ``With AllowAnyStatusCode we do not throw`` () =
        let proc (message : HttpRequestMessage) : HttpResponseMessage Async =
            async {
                message.Method |> shouldEqual HttpMethod.Get
                let content = new StringContent ("nothing was here :(")
                let resp = new HttpResponseMessage (HttpStatusCode.NotFound)
                resp.Content <- content
                return resp
            }

        use client = HttpClientMock.make (Uri "https://example.com") proc
        let api = PureGymApi.make client

        let message = api.GetWithoutAnyReturnCode().Result
        message.StatusCode |> shouldEqual HttpStatusCode.NotFound
        message.Content.ReadAsStringAsync().Result |> shouldEqual "nothing was here :("
