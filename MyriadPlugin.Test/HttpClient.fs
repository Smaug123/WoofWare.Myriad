namespace MyriadPlugin.Test

open System.Net.Http

/// Simple implementation of an HttpClient.
type HttpClientMock (result : HttpRequestMessage -> Async<HttpResponseMessage>) =
    inherit HttpClient ()

    override this.SendAsync (message, ct) =
        Async.StartAsTask (result message, cancellationToken = ct)

[<RequireQualifiedAccess>]
module HttpClientMock =
    let makeNoUri (handler : HttpRequestMessage -> Async<HttpResponseMessage>) =
        let result = new HttpClientMock (handler)
        result

    let make (baseUrl : System.Uri) (handler : HttpRequestMessage -> Async<HttpResponseMessage>) =
        let result = makeNoUri handler
        result.BaseAddress <- baseUrl
        result
