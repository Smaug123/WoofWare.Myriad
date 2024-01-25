namespace WoofWare.Myriad.Plugins.Test

open PureGym
open System

[<RequireQualifiedAccess>]
module PureGymDtos =

    let gymOpeningHoursCases =
        [
            """{"openingHours": [], "isAlwaysOpen": false}""",
            {
                GymOpeningHours.OpeningHours = []
                IsAlwaysOpen = false
            }
            """{"openingHours": ["something"], "isAlwaysOpen": false}""",
            {
                GymOpeningHours.OpeningHours = [ "something" ]
                IsAlwaysOpen = false
            }
        ]

    let gymAccessOptionsCases =
        List.allPairs [ true ; false ] [ true ; false ]
        |> List.map (fun (a, b) ->
            let s = sprintf """{"pinAccess": %b, "qrCodeAccess": %b}""" a b

            s,
            {
                GymAccessOptions.PinAccess = a
                QrCodeAccess = b
            }
        )

    let gymAddressCases =
        [
            """{"addressLine1": "", "postCode": "hi", "town": ""}""",
            {
                GymAddress.AddressLine1 = ""
                AddressLine2 = None
                AddressLine3 = None
                County = None
                Postcode = "hi"
                Town = ""
            }
            """{"addressLine1": "", "addressLine2": null, "postCode": "hi", "town": ""}""",
            {
                GymAddress.AddressLine1 = ""
                AddressLine2 = None
                AddressLine3 = None
                County = None
                Postcode = "hi"
                Town = ""
            }
        ]

    let gymLocationCases =
        [
            """{"latitude": 1.0, "longitude": 3.0}""",
            {
                GymLocation.Latitude = 1.0
                Longitude = 3.0
            }
        ]

    let gymCases =
        let ovalJson =
            """{"name":"London Oval","id":19,"status":2,"address":{"addressLine1":"Canterbury Court","addressLine2":"Units 4, 4A, 5 And 5A","addressLine3":"Kennington Park","town":"LONDON","county":null,"postcode":"SW9 6DE"},"phoneNumber":"+44 3444770005","emailAddress":"info.londonoval@puregym.com","staffMembers":null,"gymOpeningHours":{"isAlwaysOpen":true,"openingHours":[]},"reasonsToJoin":null,"accessOptions":{"pinAccess":true,"qrCodeAccess":true},"virtualTourUrl":null,"personalTrainersUrl":null,"webViewUrl":null,"floorPlanUrl":null,"location":{"longitude":"-0.110252","latitude":"51.480401"},"timeZone":"Europe/London","reopenDate":"2021-04-12T00:00:00+01 Europe/London"}"""

        let oval =
            {
                Gym.Name = "London Oval"
                Id = 19
                Status = 2
                Address =
                    {
                        AddressLine1 = "Canterbury Court"
                        AddressLine2 = Some "Units 4, 4A, 5 And 5A"
                        AddressLine3 = Some "Kennington Park"
                        Town = "LONDON"
                        County = None
                        Postcode = "SW9 6DE"
                    }
                PhoneNumber = "+44 3444770005"
                EmailAddress = "info.londonoval@puregym.com"
                GymOpeningHours =
                    {
                        IsAlwaysOpen = true
                        OpeningHours = []
                    }
                AccessOptions =
                    {
                        PinAccess = true
                        QrCodeAccess = true
                    }
                Location =
                    {
                        Longitude = -0.110252
                        Latitude = 51.480401
                    }
                TimeZone = "Europe/London"
                ReopenDate = "2021-04-12T00:00:00+01 Europe/London"
            }

        [ ovalJson, oval ]

    let memberCases =
        let me =
            {
                Id = 1234567
                CompoundMemberId = "12A123456"
                FirstName = "Patrick"
                LastName = "Stevens"
                HomeGymId = 19
                HomeGymName = "London Oval"
                EmailAddress = "someone@somewhere"
                GymAccessPin = "00000000"
                DateOfBirth = DateOnly (1994, 01, 02)
                MobileNumber = "+44 1234567"
                Postcode = "W1A 1AA"
                MembershipName = "Corporate"
                MembershipLevel = 12
                SuspendedReason = 0
                MemberStatus = 2
            }

        let meJson =
            """{
  "id": 1234567,
  "compoundMemberId": "12A123456",
  "firstName": "Patrick",
  "lastName": "Stevens",
  "homeGymId": 19,
  "homeGymName": "London Oval",
  "emailAddress": "someone@somewhere",
  "gymAccessPin": "00000000",
  "dateofBirth": "1994-01-02",
  "mobileNumber": "+44 1234567",
  "postCode": "W1A 1AA",
  "membershipName": "Corporate",
  "membershipLevel": 12,
  "suspendedReason": 0,
  "memberStatus": 2
}"""

        [ meJson, me ]

    let gymAttendanceCases =
        let json =
            """{
  "description": "65",
  "totalPeopleInGym": 65,
  "totalPeopleInClasses": 2,
  "totalPeopleSuffix": null,
  "isApproximate": false,
  "attendanceTime": "2023-12-27T18:54:09.5101697",
  "lastRefreshed": "2023-12-27T18:54:09.5101697Z",
  "lastRefreshedPeopleInClasses": "2023-12-27T18:50:26.0782286Z",
  "maximumCapacity": 0
}"""

        let expected =
            {
                Description = "65"
                TotalPeopleInGym = 65
                TotalPeopleInClasses = 2
                TotalPeopleSuffix = None
                IsApproximate = false
                AttendanceTime =
                    DateTime (2023, 12, 27, 18, 54, 09, 510, 169, DateTimeKind.Utc)
                    + TimeSpan.FromTicks 7L
                LastRefreshed =
                    DateTime (2023, 12, 27, 18, 54, 09, 510, 169, DateTimeKind.Utc)
                    + TimeSpan.FromTicks 7L
                LastRefreshedPeopleInClasses =
                    DateTime (2023, 12, 27, 18, 50, 26, 078, 228, DateTimeKind.Utc)
                    + TimeSpan.FromTicks 6L
                MaximumCapacity = 0
            }

        [ json, expected ]

    let memberActivityDtoCases =
        let json =
            """{"totalDuration":2217,"averageDuration":48,"totalVisits":46,"totalClasses":0,"isEstimated":false,"lastRefreshed":"2023-12-27T19:00:56.0309892Z"}"""

        let value =
            {
                TotalDuration = 2217
                AverageDuration = 48
                TotalVisits = 46
                TotalClasses = 0
                IsEstimated = false
                LastRefreshed =
                    DateTime (2023, 12, 27, 19, 00, 56, 030, 989, DateTimeKind.Utc)
                    + TimeSpan.FromTicks 2L
            }

        [ json, value ]

    let sessionsCases =
        let json =
            """{
    "Summary":{"Total":{"Activities":0,"Visits":10,"Duration":445},"ThisWeek":{"Activities":0,"Visits":0,"Duration":0}},
    "Visits":[
        {"IsDurationEstimated":false,"Gym":{"Id":19,"Name":"London Oval","Status":"Blocked","Location":null,"GymAccess":null,"ContactInfo":null,"TimeZone":null},"StartTime":"2023-12-21T10:12:00","Duration":50,"Name":null},
        {"IsDurationEstimated":false,"Gym":{"Id":19,"Name":"London Oval","Status":"Blocked","Location":null,"GymAccess":null,"ContactInfo":null,"TimeZone":null},"StartTime":"2023-12-20T12:05:00","Duration":80,"Name":null},
        {"IsDurationEstimated":false,"Gym":{"Id":19,"Name":"London Oval","Status":"Blocked","Location":null,"GymAccess":null,"ContactInfo":null,"TimeZone":null},"StartTime":"2023-12-17T19:37:00","Duration":46,"Name":null},
        {"IsDurationEstimated":false,"Gym":{"Id":19,"Name":"London Oval","Status":"Blocked","Location":null,"GymAccess":null,"ContactInfo":null,"TimeZone":null},"StartTime":"2023-12-16T12:19:00","Duration":37,"Name":null},
        {"IsDurationEstimated":false,"Gym":{"Id":19,"Name":"London Oval","Status":"Blocked","Location":null,"GymAccess":null,"ContactInfo":null,"TimeZone":null},"StartTime":"2023-12-15T11:14:00","Duration":47,"Name":null},
        {"IsDurationEstimated":false,"Gym":{"Id":19,"Name":"London Oval","Status":"Blocked","Location":null,"GymAccess":null,"ContactInfo":null,"TimeZone":null},"StartTime":"2023-12-13T10:30:00","Duration":36,"Name":null},
        {"IsDurationEstimated":false,"Gym":{"Id":19,"Name":"London Oval","Status":"Blocked","Location":null,"GymAccess":null,"ContactInfo":null,"TimeZone":null},"StartTime":"2023-12-10T16:18:00","Duration":32,"Name":null},
        {"IsDurationEstimated":false,"Gym":{"Id":19,"Name":"London Oval","Status":"Blocked","Location":null,"GymAccess":null,"ContactInfo":null,"TimeZone":null},"StartTime":"2023-12-05T22:36:00","Duration":40,"Name":null},
        {"IsDurationEstimated":false,"Gym":{"Id":19,"Name":"London Oval","Status":"Blocked","Location":null,"GymAccess":null,"ContactInfo":null,"TimeZone":null},"StartTime":"2023-12-03T17:59:00","Duration":48,"Name":null},
        {"IsDurationEstimated":false,"Gym":{"Id":19,"Name":"London Oval","Status":"Blocked","Location":null,"GymAccess":null,"ContactInfo":null,"TimeZone":null},"StartTime":"2023-12-01T21:41:00","Duration":29,"Name":null}],
        "Activities":[]}
"""

        let singleVisit startTime duration =
            {
                IsDurationEstimated = false
                Gym =
                    {
                        Id = 19
                        Name = "London Oval"
                        Status = "Blocked"
                    }
                StartTime = startTime
                Duration = duration
            }

        let expected =
            {
                Summary =
                    {
                        Total =
                            {
                                Activities = 0
                                Visits = 10
                                Duration = 445
                            }
                        ThisWeek =
                            {
                                Activities = 0
                                Visits = 0
                                Duration = 0
                            }
                    }
                Visits =
                    [
                        singleVisit (DateTime (2023, 12, 21, 10, 12, 00)) 50
                        singleVisit (DateTime (2023, 12, 20, 12, 05, 00)) 80
                        singleVisit (DateTime (2023, 12, 17, 19, 37, 00)) 46
                        singleVisit (DateTime (2023, 12, 16, 12, 19, 00)) 37
                        singleVisit (DateTime (2023, 12, 15, 11, 14, 00)) 47
                        singleVisit (DateTime (2023, 12, 13, 10, 30, 00)) 36
                        singleVisit (DateTime (2023, 12, 10, 16, 18, 00)) 32
                        singleVisit (DateTime (2023, 12, 05, 22, 36, 00)) 40
                        singleVisit (DateTime (2023, 12, 03, 17, 59, 00)) 48
                        singleVisit (DateTime (2023, 12, 01, 21, 41, 00)) 29
                    ]
            }

        [ json, expected ]
