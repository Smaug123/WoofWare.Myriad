namespace MyriadPlugin.Test

open System
open System.Net
open System.Net.Http
open NUnit.Framework
open PureGym
open FsUnitTyped

[<TestFixture>]
module TestRestApi =
    // several of these, to check behaviour around treatment of initial slashes
    let baseUris = [ "https://example.com" ; "https://example.com/foo" ] |> List.map Uri

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

                message.RequestUri.ToString () |> shouldEqual $"https://example.com/v1/gyms/"

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

                message.RequestUri.ToString ()
                |> shouldEqual $"https://example.com/v1/gyms/%i{requestedGym}/attendance"

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

                message.RequestUri.ToString () |> shouldEqual "https://example.com/v1/member"

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

                message.RequestUri.ToString ()
                |> shouldEqual $"https://example.com/v1/gyms/%i{requestedGym}"

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

                message.RequestUri.ToString ()
                |> shouldEqual "https://example.com/v1/member/activity"

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
        (
            baseUri : Uri,
            (startDate : DateOnly, (endDate : DateOnly, (json : string, expected : Sessions)))
        )
        =
        let proc (message : HttpRequestMessage) : HttpResponseMessage Async =
            async {
                message.Method |> shouldEqual HttpMethod.Get

                message.RequestUri.ToString ()
                |> shouldEqual
                    $"https://example.com/v2/gymSessions/member?fromDate={dateOnlyToString startDate}&toDate={dateOnlyToString endDate}"

                let content = new StringContent (json)
                let resp = new HttpResponseMessage (HttpStatusCode.OK)
                resp.Content <- content
                return resp
            }

        use client = HttpClientMock.make baseUri proc
        let api = PureGymApi.make client

        api.GetSessions(startDate, endDate).Result |> shouldEqual expected
