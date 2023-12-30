namespace MyriadPlugin.Test

open System
open System.Text.Json.Nodes
open NUnit.Framework
open FsUnitTyped
open PureGym

[<TestFixture>]
module TestPureGymJson =

    let gymOpeningHoursCases = PureGymDtos.gymOpeningHoursCases |> List.map TestCaseData

    [<TestCaseSource(nameof gymOpeningHoursCases)>]
    let ``GymOpeningHours JSON parse`` (json : string, expected : GymOpeningHours) =
        JsonNode.Parse json |> GymOpeningHours.jsonParse |> shouldEqual expected

    let gymAccessOptionsCases =
        PureGymDtos.gymAccessOptionsCases |> List.map TestCaseData

    [<TestCaseSource(nameof gymAccessOptionsCases)>]
    let ``GymAccessOptions JSON parse`` (json : string, expected : GymAccessOptions) =
        JsonNode.Parse json |> GymAccessOptions.jsonParse |> shouldEqual expected

    let gymLocationCases = PureGymDtos.gymLocationCases |> List.map TestCaseData

    [<TestCaseSource(nameof gymLocationCases)>]
    let ``GymLocation JSON parse`` (json : string, expected : GymLocation) =
        JsonNode.Parse json |> GymLocation.jsonParse |> shouldEqual expected

    let gymAddressCases = PureGymDtos.gymAddressCases |> List.map TestCaseData

    [<TestCaseSource(nameof gymAddressCases)>]
    let ``GymAddress JSON parse`` (json : string, expected : GymAddress) =
        JsonNode.Parse (json, Nullable (JsonNodeOptions (PropertyNameCaseInsensitive = true)))
        |> GymAddress.jsonParse
        |> shouldEqual expected

    let gymCases = PureGymDtos.gymCases |> List.map TestCaseData

    [<TestCaseSource(nameof gymCases)>]
    let ``Gym JSON parse`` (json : string, expected : Gym) =
        JsonNode.Parse json |> Gym.jsonParse |> shouldEqual expected

    let memberCases = PureGymDtos.memberCases |> List.map TestCaseData

    [<TestCaseSource(nameof memberCases)>]
    let ``Member JSON parse`` (json : string, expected : Member) =
        json |> JsonNode.Parse |> Member.jsonParse |> shouldEqual expected

    let gymAttendanceCases = PureGymDtos.gymAttendanceCases |> List.map TestCaseData

    [<TestCaseSource(nameof gymAttendanceCases)>]
    let ``GymAttendance JSON parse`` (json : string, expected : GymAttendance) =
        json |> JsonNode.Parse |> GymAttendance.jsonParse |> shouldEqual expected

    let memberActivityDtoCases =
        PureGymDtos.memberActivityDtoCases |> List.map TestCaseData

    [<TestCaseSource(nameof memberActivityDtoCases)>]
    let ``MemberActivityDto JSON parse`` (json : string, expected : MemberActivityDto) =
        json |> JsonNode.Parse |> MemberActivityDto.jsonParse |> shouldEqual expected

    let sessionsCases = PureGymDtos.sessionsCases |> List.map TestCaseData

    [<TestCaseSource(nameof sessionsCases)>]
    let ``Sessions JSON parse`` (json : string, expected : Sessions) =
        json
        |> fun o -> JsonNode.Parse (o, Nullable (JsonNodeOptions (PropertyNameCaseInsensitive = true)))
        |> Sessions.jsonParse
        |> shouldEqual expected
