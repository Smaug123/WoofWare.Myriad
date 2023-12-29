namespace PureGym

open System
open System.Threading
open System.Threading.Tasks
open System.IO
open System.Net
open System.Net.Http
open RestEase

[<WoofWare.Myriad.Plugins.HttpClient>]
type IPureGymApi =
    [<Get "v1/gyms/">]
    abstract GetGyms : ?ct : CancellationToken -> Task<Gym list>

    [<Get "v1/gyms/{gym_id}/attendance">]
    abstract GetGymAttendance : [<Path "gym_id">] gymId : int * ?ct : CancellationToken -> Task<GymAttendance>

    [<RestEase.GetAttribute "v1/member">]
    abstract GetMember : ?ct : CancellationToken -> Task<Member>

    [<RestEase.Get "v1/gyms/{gym_id}">]
    abstract GetGym : [<Path "gym_id">] gymId : int * ?ct : CancellationToken -> Task<Gym>

    [<GetAttribute "v1/member/activity">]
    abstract GetMemberActivity : ?ct : CancellationToken -> Task<MemberActivityDto>

    // We'll use this one to check handling of absolute URIs too
    [<Get "/v2/gymSessions/member">]
    abstract GetSessions :
        [<Query>] fromDate : DateOnly * [<Query>] toDate : DateOnly * ?ct : CancellationToken -> Task<Sessions>

    [<Get "endpoint/{param}">]
    abstract GetPathParam : [<Path "param">] parameter : string * ?ct : CancellationToken -> Task<string>

    [<Get "endpoint">]
    abstract GetStream : ?ct : CancellationToken -> Task<System.IO.Stream>

    [<Get "endpoint">]
    abstract GetStream' : ?ct : CancellationToken -> Task<IO.Stream>

    [<Get "endpoint">]
    abstract GetStream'' : ?ct : CancellationToken -> Task<Stream>

    [<Get "endpoint">]
    abstract GetResponseMessage : ?ct : CancellationToken -> Task<System.Net.Http.HttpResponseMessage>

    [<Get "endpoint">]
    abstract GetResponseMessage' : ?ct : CancellationToken -> Task<Net.Http.HttpResponseMessage>

    [<Get "endpoint">]
    abstract GetResponseMessage'' : ?ct : CancellationToken -> Task<Http.HttpResponseMessage>

    [<Get "endpoint">]
    abstract GetResponseMessage''' : ?ct : CancellationToken -> Task<HttpResponseMessage>

    [<Get "endpoint">]
    [<AllowAnyStatusCode>]
    abstract GetWithAnyReturnCode : ?ct : CancellationToken -> Task<HttpResponseMessage>

    [<Get "endpoint">]
    abstract GetWithoutAnyReturnCode : ?ct : CancellationToken -> Task<HttpResponseMessage>
