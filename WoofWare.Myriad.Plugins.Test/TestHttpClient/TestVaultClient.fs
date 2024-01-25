namespace WoofWare.Myriad.Plugins.Test

open System
open System.Net
open System.Net.Http
open NUnit.Framework
open FsUnitTyped
open ConsumePlugin

[<TestFixture>]
module TestVaultClient =

    let exampleVaultKeyResponseString =
        """{
    "request_id": "e2470000-0000-0000-0000-000000001f47",
    "lease_id": "",
    "renewable": false,
    "lease_duration": 0,
    "data": {
      "key1_1": "value1_1",
      "key1_2": "value1_2"
    },
    "data2": {
      "key2_1": "value2_1",
      "key2_2": "value2_2"
    },
    "data3": {
      "key3_1": "value3_1",
      "key3_2": "value3_2"
    },
    "data4": {
      "key4_1": "value4_1",
      "key4_2": "value4_2"
    },
    "data5": {
      "https://example.com/data5/1": "value5_1",
      "https://example.com/data5/2": "value5_2"
    },
    "data6": {
      "https://example.com/data6/1": "value6_1",
      "https://example.com/data6/2": "value6_2"
    },
    "data7": {
      "key7_1": 71,
      "key7_2": 72
    },
    "data8": {
      "key8_1": "https://example.com/data8/1",
      "key8_2": "https://example.com/data8/2"
    }
}"""

    let exampleVaultJwtResponseString =
        """{
    "request_id": "80000000-0000-0000-0000-00000000000d",
    "lease_id": "",
    "renewable": false,
    "lease_duration": 0,
    "data": null,
    "wrap_info": null,
    "warnings": null,
    "auth": {
      "client_token": "redacted_client_token",
      "accessor": "redacted_accessor",
      "policies": [
        "policy1",
        "default"
      ],
      "identity_policies": [
        "identity-policy",
        "default-2"
      ],
      "token_policies": [
        "token-policy",
        "default-3"
      ],
      "metadata": {
        "role": "some-role"
      },
      "lease_duration": 43200,
      "renewable": true,
      "entity_id": "20000000-0000-0000-0000-000000000007",
      "token_type": "service",
      "orphan": true,
      "mfa_requirement": null,
      "num_uses": 0
    }
}"""

    [<Test>]
    let ``URI example`` () =
        let proc (message : HttpRequestMessage) : HttpResponseMessage Async =
            async {
                message.Method |> shouldEqual HttpMethod.Get

                let requestUri = message.RequestUri.ToString ()

                match requestUri with
                | "https://my-vault.com/v1/auth/jwt/login" ->
                    let content = new StringContent (exampleVaultJwtResponseString)

                    let resp = new HttpResponseMessage (HttpStatusCode.OK)
                    resp.Content <- content
                    return resp
                | "https://my-vault.com/v1/mount/path" ->
                    let content = new StringContent (exampleVaultKeyResponseString)

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
        |> shouldEqual [ "key1_1", "value1_1" ; "key1_2", "value1_2" ]

        value.Data2
        |> Seq.toList
        |> List.map (fun (KeyValue (k, v)) -> k, v)
        |> shouldEqual [ "key2_1", "value2_1" ; "key2_2", "value2_2" ]

        value.Data3
        |> Seq.toList
        |> List.map (fun (KeyValue (k, v)) -> k, v)
        |> shouldEqual [ "key3_1", "value3_1" ; "key3_2", "value3_2" ]

        value.Data4
        |> Seq.toList
        |> List.map (fun (KeyValue (k, v)) -> k, v)
        |> shouldEqual [ "key4_1", "value4_1" ; "key4_2", "value4_2" ]

        value.Data5
        |> Seq.toList
        |> List.map (fun (KeyValue (k, v)) -> (k : Uri).ToString (), v)
        |> shouldEqual
            [
                "https://example.com/data5/1", "value5_1"
                "https://example.com/data5/2", "value5_2"
            ]

        value.Data6
        |> Seq.toList
        |> List.map (fun (KeyValue (k, v)) -> (k : Uri).ToString (), v)
        |> shouldEqual
            [
                "https://example.com/data6/1", "value6_1"
                "https://example.com/data6/2", "value6_2"
            ]

        value.Data7
        |> Seq.toList
        |> List.map (fun (KeyValue (k, v)) -> k, v)
        |> shouldEqual [ "key7_1", 71 ; "key7_2", 72 ]

        value.Data8
        |> Seq.toList
        |> List.map (fun (KeyValue (k, v)) -> k, (v : Uri).ToString ())
        |> shouldEqual
            [
                "key8_1", "https://example.com/data8/1"
                "key8_2", "https://example.com/data8/2"
            ]
