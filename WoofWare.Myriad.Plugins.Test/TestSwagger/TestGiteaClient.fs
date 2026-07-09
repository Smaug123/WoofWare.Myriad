namespace WoofWare.Myriad.Plugins.Test

open System
open System.Net
open System.Net.Http
open NUnit.Framework
open FsUnitTyped

[<TestFixture>]
module TestGiteaClient =

    [<Test>]
    let ``Content-Type comes from consumes and Accept from produces`` () =
        // The Gitea spec says renderMarkdownRaw consumes text/plain and produces text/html.
        let proc (message : HttpRequestMessage) : HttpResponseMessage Async =
            async {
                message.Method |> shouldEqual HttpMethod.Post

                message.Content.Headers.ContentType.MediaType |> shouldEqual "text/plain"

                message.Headers.Accept
                |> Seq.map (fun a -> a.MediaType)
                |> List.ofSeq
                |> shouldEqual [ "text/html" ]

                let resp = new HttpResponseMessage (HttpStatusCode.OK)
                resp.Content <- new StringContent ("<p>hi</p>")
                return resp
            }

        use client = HttpClientMock.make (Uri "https://example.com/") proc
        let api = Gitea.Gitea.make client

        api.RenderMarkdownRaw("# hi").Result |> shouldEqual "<p>hi</p>"

    [<Test>]
    let ``Accept header is sent for JSON endpoints`` () =
        let proc (message : HttpRequestMessage) : HttpResponseMessage Async =
            async {
                message.Method |> shouldEqual HttpMethod.Get

                message.Headers.Accept
                |> Seq.map (fun a -> a.MediaType)
                |> List.ofSeq
                |> shouldEqual [ "application/json" ]

                let resp = new HttpResponseMessage (HttpStatusCode.OK)
                resp.Content <- new StringContent ("""{"id": 3, "name": "hi"}""")
                return resp
            }

        use client = HttpClientMock.make (Uri "https://example.com/") proc
        let api = Gitea.Gitea.make client

        let team = api.OrgGetTeam(3L).Result
        team.Name |> shouldEqual (Some "hi")
