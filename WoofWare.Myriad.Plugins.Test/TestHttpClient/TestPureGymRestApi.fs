namespace WoofWare.Myriad.Plugins.Test

open System
open System.Net
open System.Net.Http
open NUnit.Framework
open PureGym
open FsUnitTyped

[<TestFixture>]
module TestPureGymRestApi =
    // several of these, to check behaviour around treatment of initial slashes
    let baseUris =
        [
            // Everything is relative to the root:
            "https://example.com"
            // Everything is also relative to the root, because `foo` is not a subdir:
            "https://example.com/foo"
            // Everything is relative to `foo`, because `foo` is a subdir
            "https://example.com/foo/"
        ]
        |> List.map Uri

    let gymsCases =
        PureGymDtos.gymCases
        |> List.collect (fun (json, gym) -> [ $"[%s{json}]", [ gym ] ; $"[%s{json}, %s{json}]", [ gym ; gym ] ])
        |> List.allPairs baseUris
        |> List.map TestCaseData

    [<TestCaseSource(nameof gymsCases)>]
    let ``Test GetGyms`` (baseUri : Uri, (json : string, expected : Gym list)) =
        let proc (message : HttpRequestMessage) : HttpResponseMessage Async =
            async {
                message.Method |> shouldEqual HttpMethod.Get

                // URI is relative in the attribute on the IPureGymApi member,
                // so this never gets redirected
                let expectedUri =
                    match baseUri.ToString () with
                    | "https://example.com/" -> "https://example.com/v1/gyms/"
                    | "https://example.com/foo" -> "https://example.com/v1/gyms/"
                    | "https://example.com/foo/" -> "https://example.com/foo/v1/gyms/"
                    | s -> failwith $"Unrecognised base URI: %s{s}"

                message.RequestUri.ToString () |> shouldEqual expectedUri

                let content = new StringContent (json)
                let resp = new HttpResponseMessage (HttpStatusCode.OK)
                resp.Content <- content
                return resp
            }

        use client = HttpClientMock.make baseUri proc
        let api = PureGymApi.make client

        api.GetGyms().Result |> shouldEqual expected

    let gymAttendanceCases =
        PureGymDtos.gymAttendanceCases
        |> List.allPairs baseUris
        |> List.map TestCaseData

    [<TestCaseSource(nameof gymAttendanceCases)>]
    let ``Test GetGymAttendance`` (baseUri : Uri, (json : string, expected : GymAttendance)) =
        let requestedGym = 3

        let proc (message : HttpRequestMessage) : HttpResponseMessage Async =
            async {
                message.Method |> shouldEqual HttpMethod.Get

                // URI is relative in the attribute on the IPureGymApi member,
                // so this never gets redirected
                let expectedUri =
                    match baseUri.ToString () with
                    | "https://example.com/" -> $"https://example.com/v1/gyms/%i{requestedGym}/attendance"
                    | "https://example.com/foo" -> $"https://example.com/v1/gyms/%i{requestedGym}/attendance"
                    | "https://example.com/foo/" -> $"https://example.com/foo/v1/gyms/%i{requestedGym}/attendance"
                    | s -> failwith $"Unrecognised base URI: %s{s}"

                message.RequestUri.ToString () |> shouldEqual expectedUri

                let content = new StringContent (json)
                let resp = new HttpResponseMessage (HttpStatusCode.OK)
                resp.Content <- content
                return resp
            }

        use client = HttpClientMock.make baseUri proc
        let api = PureGymApi.make client

        api.GetGymAttendance(requestedGym).Result |> shouldEqual expected

    let memberCases =
        PureGymDtos.memberCases |> List.allPairs baseUris |> List.map TestCaseData

    [<TestCaseSource(nameof memberCases)>]
    let ``Test GetMember`` (baseUri : Uri, (json : string, expected : Member)) =
        let proc (message : HttpRequestMessage) : HttpResponseMessage Async =
            async {
                message.Method |> shouldEqual HttpMethod.Get

                // URI is relative in the attribute on the IPureGymApi member,
                // so this never gets redirected
                let expectedUri =
                    match baseUri.ToString () with
                    | "https://example.com/" -> "https://example.com/v1/member"
                    | "https://example.com/foo" -> "https://example.com/v1/member"
                    | "https://example.com/foo/" -> "https://example.com/foo/v1/member"
                    | s -> failwith $"Unrecognised base URI: %s{s}"

                message.RequestUri.ToString () |> shouldEqual expectedUri

                let content = new StringContent (json)
                let resp = new HttpResponseMessage (HttpStatusCode.OK)
                resp.Content <- content
                return resp
            }

        use client = HttpClientMock.make baseUri proc
        let api = PureGymApi.make client

        api.GetMember().Result |> shouldEqual expected

    let gymCases =
        PureGymDtos.gymCases |> List.allPairs baseUris |> List.map TestCaseData

    [<TestCaseSource(nameof gymCases)>]
    let ``Test GetGym`` (baseUri : Uri, (json : string, expected : Gym)) =
        let requestedGym = 3

        let proc (message : HttpRequestMessage) : HttpResponseMessage Async =
            async {
                message.Method |> shouldEqual HttpMethod.Get

                // URI is relative in the attribute on the IPureGymApi member,
                // so this never gets redirected
                let expectedUri =
                    match baseUri.ToString () with
                    | "https://example.com/" -> $"https://example.com/v1/gyms/%i{requestedGym}"
                    | "https://example.com/foo" -> $"https://example.com/v1/gyms/%i{requestedGym}"
                    | "https://example.com/foo/" -> $"https://example.com/foo/v1/gyms/%i{requestedGym}"
                    | s -> failwith $"Unrecognised base URI: %s{s}"

                message.RequestUri.ToString () |> shouldEqual expectedUri

                let content = new StringContent (json)
                let resp = new HttpResponseMessage (HttpStatusCode.OK)
                resp.Content <- content
                return resp
            }

        use client = HttpClientMock.make baseUri proc
        let api = PureGymApi.make client

        api.GetGym(requestedGym).Result |> shouldEqual expected

    let memberActivityCases =
        PureGymDtos.memberActivityDtoCases
        |> List.allPairs baseUris
        |> List.map TestCaseData

    [<TestCaseSource(nameof memberActivityCases)>]
    let ``Test GetMemberActivity`` (baseUri : Uri, (json : string, expected : MemberActivityDto)) =
        let proc (message : HttpRequestMessage) : HttpResponseMessage Async =
            async {
                message.Method |> shouldEqual HttpMethod.Get

                // URI is relative in the attribute on the IPureGymApi member,
                // so this never gets redirected
                let expectedUri =
                    match baseUri.ToString () with
                    | "https://example.com/" -> "https://example.com/v1/member/activity"
                    | "https://example.com/foo" -> "https://example.com/v1/member/activity"
                    | "https://example.com/foo/" -> "https://example.com/foo/v1/member/activity"
                    | s -> failwith $"Unrecognised base URI: %s{s}"

                message.RequestUri.ToString () |> shouldEqual expectedUri

                let content = new StringContent (json)
                let resp = new HttpResponseMessage (HttpStatusCode.OK)
                resp.Content <- content
                return resp
            }

        use client = HttpClientMock.make baseUri proc
        let api = PureGymApi.make client

        api.GetMemberActivity().Result |> shouldEqual expected

    let dates =
        [
            for month = 1 to 3 do
                // span the number 12, to catch muddling up month and day
                for day = 11 to 13 do
                    yield DateOnly (2023, month, day)
        ]

    let sessionsCases =
        PureGymDtos.sessionsCases
        |> List.allPairs dates
        |> List.allPairs dates
        |> List.allPairs baseUris
        |> List.map TestCaseData

    let inline dateOnlyToString (d : DateOnly) : string =
        let month = if d.Month < 10 then $"0%i{d.Month}" else $"%i{d.Month}"
        let day = if d.Day < 10 then $"0%i{d.Day}" else $"%i{d.Day}"
        $"{d.Year}-{month}-{day}"

    [<TestCaseSource(nameof sessionsCases)>]
    let ``Test GetSessions``
        (baseUri : Uri, (startDate : DateOnly, (endDate : DateOnly, (json : string, expected : Sessions))))
        =
        let proc (message : HttpRequestMessage) : HttpResponseMessage Async =
            async {
                message.Method |> shouldEqual HttpMethod.Get

                // This one is specified as being absolute, in its attribute on the IPureGymApi type
                let expectedUri =
                    let fromDate = dateOnlyToString startDate
                    let toDate = dateOnlyToString endDate
                    $"https://example.com/v2/gymSessions/member?fromDate=%s{fromDate}&toDate=%s{toDate}"

                message.RequestUri.ToString () |> shouldEqual expectedUri

                let content = new StringContent (json)
                let resp = new HttpResponseMessage (HttpStatusCode.OK)
                resp.Content <- content
                return resp
            }

        use client = HttpClientMock.make baseUri proc
        let api = PureGymApi.make client

        api.GetSessions(startDate, endDate).Result |> shouldEqual expected

    [<Test>]
    let ``URI example`` () =
        let proc (message : HttpRequestMessage) : HttpResponseMessage Async =
            async {
                message.Method |> shouldEqual HttpMethod.Get

                message.RequestUri.ToString () |> shouldEqual "https://whatnot.com/some/url"

                let content =
                    new StringContent ("""{"someUri": "https://patrick@en.wikipedia.org/wiki/foo"}""")

                let resp = new HttpResponseMessage (HttpStatusCode.OK)
                resp.Content <- content
                return resp
            }

        use client = HttpClientMock.makeNoUri proc
        let api = PureGymApi.make client

        let uri = api.GetUrl().Result.SomeUri
        uri.ToString () |> shouldEqual "https://patrick@en.wikipedia.org/wiki/foo"
        uri.UserInfo |> shouldEqual "patrick"
        uri.Host |> shouldEqual "en.wikipedia.org"

    [<TestCase false>]
    [<TestCase true>]
    let ``Map<string, string> option example`` (isSome : bool) =
        let proc (message : HttpRequestMessage) : HttpResponseMessage Async =
            async {
                message.Method |> shouldEqual HttpMethod.Get

                message.RequestUri.ToString () |> shouldEqual "https://whatnot.com/some/url"
                let! content = message.Content.ReadAsStringAsync () |> Async.AwaitTask

                if isSome then
                    content |> shouldEqual """{"hi":"bye"}"""
                else
                    content |> shouldEqual "null"

                let content = new StringContent (content)

                let resp = new HttpResponseMessage (HttpStatusCode.OK)
                resp.Content <- content
                return resp
            }

        use client = HttpClientMock.makeNoUri proc
        let api = PureGymApi.make client

        let expected =
            if isSome then
                [ "hi", "bye" ] |> Map.ofList |> Some
            else
                None

        let actual = api.PostStringToString(expected).Result
        actual |> shouldEqual expected
