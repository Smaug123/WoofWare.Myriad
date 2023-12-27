// Copied from https://gitea.patrickstevens.co.uk/patrick/puregym-unofficial-dotnet/src/commit/2741c5e36cf0bdb203b12b78a8062e25af9d89c7/PureGym/Api.fs

namespace PureGym

open System
open System.Text.Json.Serialization

/// Describes the opening hours of a given gym.
[<MyriadPlugin.JsonParse>]
type GymOpeningHours =
    {
        /// If this is true, there should be no OpeningHours (but nothing enforces that).
        IsAlwaysOpen : bool
        /// This is a pretty unstructured list, which is in general not really parseable: it's human-readable only.
        OpeningHours : string list
    }

    /// Human-readable representation
    override this.ToString () =
        if this.IsAlwaysOpen then
            "always open"
        else
            this.OpeningHours |> String.concat ", "

/// How a human can authenticate with a gym when they physically try to enter it
[<MyriadPlugin.JsonParse>]
type GymAccessOptions =
    {
        /// This gym has PIN entry pads
        PinAccess : bool
        /// This gym has a QR code scanner. QR codes can be generated with the PureGym app.
        QrCodeAccess : bool
    }

/// Where a gym is on the Earth
[<MyriadPlugin.JsonParse>]
type GymLocation =
    {
        /// Measured in degrees
        [<JsonNumberHandling(JsonNumberHandling.AllowReadingFromString)>]
        Longitude : float
        /// Measured in degrees
        [<JsonNumberHandling(JsonNumberHandling.AllowReadingFromString)>]
        Latitude : float
    }

/// The postal address of a gym
[<MyriadPlugin.JsonParse>]
type GymAddress =
    {
        /// E.g. "Canterbury Court"
        [<JsonRequired>]
        AddressLine1 : string
        /// E.g. "Units 4, 4A, 5 And 5A"
        AddressLine2 : string option
        /// E.g. "Kennington Park"
        AddressLine3 : string option
        /// E.g. "LONDON"
        [<JsonRequired>]
        Town : string
        County : string option
        /// E.g. "SW9 6DE"
        [<JsonRequired>]
        Postcode : string
    }

    /// Human-readable statement of the address
    override this.ToString () =
        [
            yield Some this.AddressLine1
            yield this.AddressLine2
            yield this.AddressLine3
            match this.County with
            | None -> yield Some $"%s{this.Town} %s{this.Postcode}"
            | Some county ->
                yield Some this.Town
                yield Some $"%s{county} %s{this.Postcode}"
        ]
        |> Seq.choose id
        |> String.concat "\n"

/// Metadata about a physical gym
[<MyriadPlugin.JsonParse>]
type Gym =
    {
        // The following fields are returned but are always null
        // ReasonsToJoin : string
        // VirtualTourUrl : Uri
        // PersonalTrainersUrl : Uri
        // WebViewUrl : Uri
        // FloorPlanUrl : Uri
        // StaffMembers : string

        /// The name of this gym, e.g. "London Oval"
        [<JsonRequired>]
        Name : string
        /// This gym's ID in the PureGym system, e.g. 19
        [<JsonRequired>]
        Id : int
        /// I don't know what this status is. Please tell me if you know!
        [<JsonRequired>]
        Status : int
        /// Postal address of this gym
        [<JsonRequired>]
        Address : GymAddress
        /// Phone number of this gym, e.g. "+44 1234 567890"
        [<JsonRequired>]
        PhoneNumber : string
        /// Contact email address for this gym's staff
        [<JsonRequired>]
        EmailAddress : string
        /// When this gym is open
        [<JsonRequired>]
        GymOpeningHours : GymOpeningHours
        /// How a human can physically authenticate when they physically enter this gym
        [<JsonRequired>]
        AccessOptions : GymAccessOptions
        /// Where this gym is physically located
        [<JsonRequired>]
        Location : GymLocation
        /// The IANA time zone this gym observes, e.g. "Europe/London"
        [<JsonRequired>]
        TimeZone : string
        /// This is a date-time in the format yyyy-MM-ddTHH:mm:ss+01 Europe/London
        ReopenDate : string
    }

    /// Human-readable representation of the most important information about this gym
    override this.ToString () =
        $"""%s{this.Name} (%i{this.Id})
{this.Address}
%s{this.EmailAddress}   %s{this.PhoneNumber}
Opening hours: %s{string<GymOpeningHours> this.GymOpeningHours}
%s{string<GymAccessOptions> this.AccessOptions}
"""

/// A human member of PureGym
[<MyriadPlugin.JsonParse>]
type Member =
    {
        /// This member's ID. This is a fairly large number.
        Id : int
        /// No idea what this is - please tell me if you know!
        CompoundMemberId : string
        /// First name, e.g. "Patrick"
        FirstName : string
        /// Last name, e.g. "Stevens"
        LastName : string
        /// ID of the gym designated as this user's home gym. This is also the "Id" field of the appropriate Gym object.
        HomeGymId : int
        /// The name of the gym designated as this user's home gym. This is also the "Name" field of the appropriate
        /// Gym object.
        HomeGymName : string
        /// This user's email address
        EmailAddress : string
        /// This user's gym access pin, probably 8 digits
        GymAccessPin : string
        /// This user's recorded date of birth
        [<JsonPropertyName "dateofBirth">]
        DateOfBirth : DateOnly
        /// This user's phone number, human-readable
        MobileNumber : string
        /// This user's registered home postcode
        [<JsonPropertyName "postCode">]
        Postcode : string
        /// E.g. "Corporate"
        MembershipName : string
        MembershipLevel : int
        SuspendedReason : int
        MemberStatus : int
    }

/// Statistics for how many people are currently at a gym
[<MyriadPlugin.JsonParse>]
type GymAttendance =
    {
        /// This appears always to be just equal to TotalPeopleInGym, but a string.
        [<JsonRequired>]
        Description : string
        /// How many people are in the gym as of this statistics snapshot
        [<JsonRequired>]
        TotalPeopleInGym : int
        /// How many people are in classes at the gym as of this statistics snapshot
        [<JsonRequired>]
        TotalPeopleInClasses : int
        /// E.g. " or fewer"
        TotalPeopleSuffix : string option
        [<JsonRequired>]
        IsApproximate : bool
        /// When the query was received (I think)
        AttendanceTime : DateTime
        /// When the "total people in gym" snapshot was taken that is reported here
        LastRefreshed : DateTime
        /// When the "number of people in classes" snapshot was taken that is reported here
        LastRefreshedPeopleInClasses : DateTime
        /// Maximum capacity of the gym, or 0 if no listed capacity
        MaximumCapacity : int
    }

/// The visit statistics for a particular human to a particular gym.
/// The semantics of this class are basically unknown.
type MemberActivityThisMonth =
    {
        /// How many minutes, including classes, have been logged so far this month
        TotalDurationMinutes : int
        /// How long, in minutes, each visit has been on average this month
        AverageDurationMinutes : int
        /// How many visits have been made this month, excluding classes
        TotalVisits : int
        /// How many classes have been attended this month
        TotalClasses : int
        /// Whether this block of statistics is estimated rather than exact
        IsEstimated : bool
        /// When this data was constructed
        LastRefreshed : DateTime
    }

/// Don't use this type. It's public because System.Text.Json can't do private types.
[<MyriadPlugin.JsonParse>]
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

    member this.ToMemberActivity () =
        {
            TotalDurationMinutes = this.TotalDuration
            AverageDurationMinutes = this.AverageDuration
            TotalVisits = this.TotalVisits
            TotalClasses = this.TotalClasses
            IsEstimated = this.IsEstimated
            LastRefreshed = this.LastRefreshed
        }

[<MyriadPlugin.JsonParse>]
type SessionsAggregate =
    {
        /// Number of gym "activities" within some query-defined time period; presumably this is like classes?
        /// It's always 0 for me.
        Activities : int
        /// Number of visits to the gym within some query-defined time period.
        Visits : int
        /// In minutes: total time spent in gym during the query-defined time period.
        Duration : int
    }

/// The DTO for gym info returned from the Sessions endpoint.
[<MyriadPlugin.JsonParse>]
type VisitGym =
    {
        // Omitting Location, GymAccess, ContactInfo, TimeZone because these were all null for me
        /// The PureGym ID of this gym, e.g. 19
        Id : int
        /// E.g. "London Oval", the canonical name of this gym
        Name : string
        /// For some reason this always seems to be "Blocked"
        Status : string
    }

/// Summary of a single visit to a gym.
[<MyriadPlugin.JsonParse>]
type Visit =
    {
        // Omitted Name because it always was null for me
        /// Whether the Duration field is estimated.
        IsDurationEstimated : bool
        /// When the visit began.
        StartTime : DateTime
        /// In minutes.
        Duration : int
        /// Which gym was visited
        Gym : VisitGym
    }

    /// Human-readable non-round-trip representation.
    override this.ToString () =
        let startTime = this.StartTime.ToString "yyyy-MM-dd HH:mm"
        $"%s{this.Gym.Name}: %s{startTime} (%i{this.Duration} minutes)"

/// Aggregate statistics for gym visits across a time period.
[<MyriadPlugin.JsonParse>]
type SessionsSummary =
    {
        /// Aggregate stats for gym visits within the query-dependent time period.
        Total : SessionsAggregate
        /// Aggregate stats for gym visits "this week", whatever that means to PureGym.
        ThisWeek : SessionsAggregate
    }

    /// Human-readable non-round-trip representation.
    override this.ToString () =
        $"%i{this.Total.Visits} visits, totalling %i{this.Total.Duration} minutes"

[<MyriadPlugin.JsonParse>]
type Sessions =
    {
        Summary : SessionsSummary
        Visits : Visit list
    }

    /// Human-readable non-round-trip representation.
    override this.ToString () =
        let summary = string<SessionsSummary> this.Summary
        let visits = this.Visits |> Seq.map string<Visit> |> String.concat "\n"

        $"%s{summary}\n%s{visits}"
