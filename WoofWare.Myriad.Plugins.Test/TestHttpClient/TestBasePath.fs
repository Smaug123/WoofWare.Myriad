namespace WoofWare.Myriad.Plugins.Test

open System
open System.Net
open System.Net.Http
open NUnit.Framework
open PureGym
open FsUnitTyped

[<TestFixture>]
module TestBasePath =
    let replyWithUrl (message : HttpRequestMessage) : HttpResponseMessage Async =
        async {
            message.Method |> shouldEqual HttpMethod.Get
            let content = new StringContent (message.RequestUri.ToString ())
            let resp = new HttpResponseMessage (HttpStatusCode.OK)
            resp.Content <- content
            return resp
        }

    [<Test>]
    let ``Base address is respected`` () =
        use client = HttpClientMock.makeNoUri replyWithUrl
        let api = PureGymApi.make client

        let observedUri = api.GetPathParam("param").Result
        observedUri |> shouldEqual "https://whatnot.com/endpoint/param"

    [<Test>]
    let ``Without a base address attr but with BaseAddress on client, request goes through`` () =
        use client = HttpClientMock.make (Uri "https://baseaddress.com") replyWithUrl
        let api = ApiWithoutBaseAddress.make client

        let observedUri = api.GetPathParam("param").Result
        observedUri |> shouldEqual "https://baseaddress.com/endpoint/param"

    [<Test>]
    let ``Base address on client takes precedence`` () =
        use client = HttpClientMock.make (Uri "https://baseaddress.com") replyWithUrl
        let api = PureGymApi.make client

        let observedUri = api.GetPathParam("param").Result
        observedUri |> shouldEqual "https://baseaddress.com/endpoint/param"

    [<Test>]
    let ``Without a base address attr or BaseAddress on client, request throws`` () =
        use client = HttpClientMock.makeNoUri replyWithUrl
        let api = ApiWithoutBaseAddress.make client

        let observedExc =
            async {
                let! result = api.GetPathParam "param" |> Async.AwaitTask |> Async.Catch

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
            "No base address was supplied on the type, and no BaseAddress was on the HttpClient. (Parameter 'BaseAddress')"

    [<Test>]
    let ``Relative base path, no base address, relative attribute`` () : unit =
        do
            use client = HttpClientMock.makeNoUri replyWithUrl
            let api = ApiWithBasePath.make client

            let exc =
                Assert.Throws<AggregateException> (fun () -> api.GetPathParam("hi").Result |> ignore<string>)

            exc.InnerException.Message
            |> shouldEqual
                "No base address was supplied on the type, and no BaseAddress was on the HttpClient. (Parameter 'BaseAddress')"

        use client = HttpClientMock.make (Uri "https://whatnot.com/thing") replyWithUrl
        let api = ApiWithBasePath.make client
        let result = api.GetPathParam("hi").Result
        result |> shouldEqual "https://whatnot.com/thing/foo/endpoint/hi"

    [<Test>]
    let ``Relative base path, base address, relative attribute`` () : unit =
        use client = HttpClientMock.makeNoUri replyWithUrl
        let api = ApiWithBasePathAndAddress.make client
        let result = api.GetPathParam("hi").Result
        result |> shouldEqual "https://whatnot.com/thing/foo/endpoint/hi"

    [<Test>]
    let ``Absolute base path, no base address, relative attribute`` () : unit =
        do
            use client = HttpClientMock.makeNoUri replyWithUrl
            let api = ApiWithAbsoluteBasePath.make client

            let exc =
                Assert.Throws<AggregateException> (fun () -> api.GetPathParam("hi").Result |> ignore<string>)

            exc.InnerException.Message
            |> shouldEqual
                "No base address was supplied on the type, and no BaseAddress was on the HttpClient. (Parameter 'BaseAddress')"

        use client = HttpClientMock.make (Uri "https://whatnot.com/thing") replyWithUrl
        let api = ApiWithAbsoluteBasePath.make client
        let result = api.GetPathParam("hi").Result
        result |> shouldEqual "https://whatnot.com/foo/endpoint/hi"

    [<Test>]
    let ``Absolute base path, base address, relative attribute`` () : unit =
        use client = HttpClientMock.makeNoUri replyWithUrl
        let api = ApiWithAbsoluteBasePathAndAddress.make client
        let result = api.GetPathParam("hi").Result
        result |> shouldEqual "https://whatnot.com/foo/endpoint/hi"

    [<Test>]
    let ``Relative base path, no base address, absolute attribute`` () : unit =
        do
            use client = HttpClientMock.makeNoUri replyWithUrl
            let api = ApiWithBasePathAndAbsoluteEndpoint.make client

            let exc =
                Assert.Throws<AggregateException> (fun () -> api.GetPathParam("hi").Result |> ignore<string>)

            exc.InnerException.Message
            |> shouldEqual
                "No base address was supplied on the type, and no BaseAddress was on the HttpClient. (Parameter 'BaseAddress')"

        use client = HttpClientMock.make (Uri "https://whatnot.com/thing") replyWithUrl
        let api = ApiWithBasePathAndAbsoluteEndpoint.make client
        let result = api.GetPathParam("hi").Result
        result |> shouldEqual "https://whatnot.com/endpoint/hi"

    [<Test>]
    let ``Relative base path, base address, absolute attribute`` () : unit =
        use client = HttpClientMock.makeNoUri replyWithUrl
        let api = ApiWithBasePathAndAddressAndAbsoluteEndpoint.make client
        let result = api.GetPathParam("hi").Result
        result |> shouldEqual "https://whatnot.com/endpoint/hi"

    [<Test>]
    let ``Absolute base path, no base address, absolute attribute`` () : unit =
        do
            use client = HttpClientMock.makeNoUri replyWithUrl
            let api = ApiWithAbsoluteBasePathAndAbsoluteEndpoint.make client

            let exc =
                Assert.Throws<AggregateException> (fun () -> api.GetPathParam("hi").Result |> ignore<string>)

            exc.InnerException.Message
            |> shouldEqual
                "No base address was supplied on the type, and no BaseAddress was on the HttpClient. (Parameter 'BaseAddress')"

        use client = HttpClientMock.make (Uri "https://whatnot.com/thing") replyWithUrl
        let api = ApiWithAbsoluteBasePathAndAbsoluteEndpoint.make client
        let result = api.GetPathParam("hi").Result
        result |> shouldEqual "https://whatnot.com/endpoint/hi"

    [<Test>]
    let ``Absolute base path, base address, absolute attribute`` () : unit =
        use client = HttpClientMock.makeNoUri replyWithUrl
        let api = ApiWithAbsoluteBasePathAndAddressAndAbsoluteEndpoint.make client
        let result = api.GetPathParam("hi").Result
        result |> shouldEqual "https://whatnot.com/endpoint/hi"
