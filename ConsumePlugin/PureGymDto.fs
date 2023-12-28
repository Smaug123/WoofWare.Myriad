// Copied from https://gitea.patrickstevens.co.uk/patrick/puregym-unofficial-dotnet/src/commit/2741c5e36cf0bdb203b12b78a8062e25af9d89c7/PureGym/Api.fs

namespace PureGym

open System
open System.Text.Json.Serialization

[<WoofWare.Myriad.Plugins.JsonParse>]
type GymOpeningHours =
    {
        IsAlwaysOpen : bool
        OpeningHours : string list
    }

[<WoofWare.Myriad.Plugins.JsonParse>]
type GymAccessOptions =
    {
        PinAccess : bool
        QrCodeAccess : bool
    }

[<WoofWare.Myriad.Plugins.JsonParse>]
type GymLocation =
    {
        [<JsonNumberHandling(JsonNumberHandling.AllowReadingFromString)>]
        Longitude : float
        [<JsonNumberHandling(JsonNumberHandling.AllowReadingFromString)>]
        Latitude : float
    }

[<WoofWare.Myriad.Plugins.JsonParse>]
type GymAddress =
    {
        [<JsonRequired>]
        AddressLine1 : string
        AddressLine2 : string option
        AddressLine3 : string option
        [<JsonRequired>]
        Town : string
        County : string option
        [<JsonRequired>]
        Postcode : string
    }

[<WoofWare.Myriad.Plugins.JsonParse>]
type Gym =
    {
        [<JsonRequired>]
        Name : string
        [<JsonRequired>]
        Id : int
        [<JsonRequired>]
        Status : int
        [<JsonRequired>]
        Address : GymAddress
        [<JsonRequired>]
        PhoneNumber : string
        [<JsonRequired>]
        EmailAddress : string
        [<JsonRequired>]
        GymOpeningHours : GymOpeningHours
        [<JsonRequired>]
        AccessOptions : GymAccessOptions
        [<JsonRequired>]
        Location : GymLocation
        [<JsonRequired>]
        TimeZone : string
        ReopenDate : string
    }

[<WoofWare.Myriad.Plugins.JsonParse>]
type Member =
    {
        Id : int
        CompoundMemberId : string
        FirstName : string
        LastName : string
        HomeGymId : int
        HomeGymName : string
        EmailAddress : string
        GymAccessPin : string
        [<JsonPropertyName "dateofBirth">]
        DateOfBirth : DateOnly
        MobileNumber : string
        [<JsonPropertyName "postCode">]
        Postcode : string
        MembershipName : string
        MembershipLevel : int
        SuspendedReason : int
        MemberStatus : int
    }

[<WoofWare.Myriad.Plugins.JsonParse>]
type GymAttendance =
    {
        [<JsonRequired>]
        Description : string
        [<JsonRequired>]
        TotalPeopleInGym : int
        [<JsonRequired>]
        TotalPeopleInClasses : int
        TotalPeopleSuffix : string option
        [<JsonRequired>]
        IsApproximate : bool
        AttendanceTime : DateTime
        LastRefreshed : DateTime
        LastRefreshedPeopleInClasses : DateTime
        MaximumCapacity : int
    }

[<WoofWare.Myriad.Plugins.JsonParse>]
type MemberActivityDto =
    {
        [<JsonRequired>]
        TotalDuration : int
        [<JsonRequired>]
        AverageDuration : int
        [<JsonRequired>]
        TotalVisits : int
        [<JsonRequired>]
        TotalClasses : int
        [<JsonRequired>]
        IsEstimated : bool
        [<JsonRequired>]
        LastRefreshed : DateTime
    }

[<WoofWare.Myriad.Plugins.JsonParse>]
type SessionsAggregate =
    {
        Activities : int
        Visits : int
        Duration : int
    }

[<WoofWare.Myriad.Plugins.JsonParse>]
type VisitGym =
    {
        Id : int
        Name : string
        Status : string
    }

[<WoofWare.Myriad.Plugins.JsonParse>]
type Visit =
    {
        IsDurationEstimated : bool
        StartTime : DateTime
        Duration : int
        Gym : VisitGym
    }

[<WoofWare.Myriad.Plugins.JsonParse>]
type SessionsSummary =
    {
        Total : SessionsAggregate
        ThisWeek : SessionsAggregate
    }

[<WoofWare.Myriad.Plugins.JsonParse>]
type Sessions =
    {
        Summary : SessionsSummary
        Visits : Visit list
    }