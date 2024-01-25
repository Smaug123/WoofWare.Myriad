namespace WoofWare.Myriad.Plugins.Test

open System
open System.Net
open System.Net.Http
open NUnit.Framework
open FsUnitTyped
open ConsumePlugin

[<TestFixture>]
module TestVaultClient =

    [<Test>]
    let ``URI example`` () =
        let proc (message : HttpRequestMessage) : HttpResponseMessage Async =
            async {
                message.Method |> shouldEqual HttpMethod.Get

                let requestUri = message.RequestUri.ToString ()

                match requestUri with
                | "https://my-vault.com/v1/auth/jwt/login" ->
                    let content =
                        new StringContent ("""{"someUri": "https://patrick@en.wikipedia.org/wiki/foo"}""")

                    let resp = new HttpResponseMessage (HttpStatusCode.OK)
                    resp.Content <- content
                    return resp
                | "https://my-vault.com/v1/mount/path" ->
                    let content =
                        new StringContent ("""{"someUri": "https://patrick@en.wikipedia.org/wiki/foo"}""")

                    let resp = new HttpResponseMessage (HttpStatusCode.OK)
                    resp.Content <- content
                    return resp
                | _ -> return failwith $"bad URI: %s{requestUri}"
            }

        use client = HttpClientMock.make (Uri "https://my-vault.com") proc
        let api = VaultClient.make client

        let vaultResponse = api.GetJwt("role", "jwt").Result
        let value = api.GetSecret(vaultResponse, "path", "mount").Result

        value.Data
        |> Seq.toList
        |> List.map (fun (KeyValue (k, v)) -> k, v)
        |> shouldEqual [ "hi", "value1" ; "bye", "value2" ]
