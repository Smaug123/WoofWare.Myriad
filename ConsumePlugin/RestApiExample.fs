namespace PureGym

open System
open System.Threading
open System.Threading.Tasks
open RestEase

[<WoofWare.Myriad.Plugins.HttpClient>]
type IPureGymApi =
    [<Get "v1/gyms/">]
    abstract GetGyms : ?ct : CancellationToken -> Task<Gym list>

    [<Get "v1/gyms/{gym_id}/attendance">]
    abstract GetGymAttendance : [<Path "gym_id">] gymId : int * ?ct : CancellationToken -> Task<GymAttendance>

    [<Get "v1/member">]
    abstract GetMember : ?ct : CancellationToken -> Task<Member>

    [<Get "v1/gyms/{gym_id}">]
    abstract GetGym : [<Path "gym_id">] gymId : int * ?ct : CancellationToken -> Task<Gym>

    [<Get "v1/member/activity">]
    abstract GetMemberActivity : ?ct : CancellationToken -> Task<MemberActivityDto>

    // We'll use this one to check handling of absolute URIs too
    [<Get "/v2/gymSessions/member">]
    abstract GetSessions :
        [<Query>] fromDate : DateOnly * [<Query>] toDate : DateOnly * ?ct : CancellationToken -> Task<Sessions>

    // An example from RestEase's own docs
    [<Post "users/new">]
    abstract CreateUserString : [<Body>] user: string * ?ct : CancellationToken -> Task<string>

    [<Post "users/new">]
    abstract CreateUserStream : [<Body>] user: System.IO.Stream * ?ct : CancellationToken -> Task<string>

    [<Post "users/new">]
    abstract CreateUserByteArr : [<Body>] user: byte[] * ?ct : CancellationToken -> Task<string>

    [<Post "users/new">]
    abstract CreateUserByteArr' : [<Body>] user: array<byte> * ?ct : CancellationToken -> Task<string>

    [<Post "users/new">]
    abstract CreateUserByteArr'' : [<Body>] user: byte array * ?ct : CancellationToken -> Task<string>

    [<Post "users/new">]
    abstract CreateUserHttpContent : [<Body>] user: System.Net.Http.HttpContent * ?ct : CancellationToken -> Task<string>
