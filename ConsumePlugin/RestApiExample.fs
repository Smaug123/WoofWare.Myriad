namespace PureGym

open System
open System.Threading
open System.Threading.Tasks
open System.IO
open System.Net
open System.Net.Http
open RestEase

[<WoofWare.Myriad.Plugins.HttpClient>]
[<BaseAddress "https://whatnot.com">]
type IPureGymApi =
    [<Get("v1/gyms/")>]
    abstract GetGyms : ?ct : CancellationToken -> Task<Gym list>

    [<Get "v1/gyms/{gym_id}/attendance">]
    abstract GetGymAttendance : [<Path "gym_id">] gymId : int * ?ct : CancellationToken -> Task<GymAttendance>

    [<Get "v1/gyms/{gym_id}/attendance">]
    abstract GetGymAttendance' : [<Path("gym_id")>] gymId : int * ?ct : CancellationToken -> Task<GymAttendance>

    [<RestEase.GetAttribute "v1/member">]
    abstract GetMember : ?ct : CancellationToken -> Member Task

    [<RestEase.Get "v1/gyms/{gym}">]
    abstract GetGym : [<Path>] gym : int * ?ct : CancellationToken -> Task<Gym>

    [<GetAttribute "v1/member/activity">]
    abstract GetMemberActivity : ?ct : CancellationToken -> Task<MemberActivityDto>

    [<Get "some/url">]
    abstract GetUrl : ?ct : CancellationToken -> Task<UriThing>

    [<Post "some/url">]
    abstract PostStringToString :
        [<Body>] foo : Map<string, string> option * ?ct : CancellationToken -> Task<Map<string, string> option>

    // We'll use this one to check handling of absolute URIs too
    [<Get "/v2/gymSessions/member">]
    abstract GetSessions :
        [<Query>] fromDate : DateOnly * [<Query>] toDate : DateOnly * ?ct : CancellationToken -> Task<Sessions>

    [<Get "/v2/gymSessions/member?foo=1">]
    abstract GetSessionsWithQuery :
        [<Query>] fromDate : DateOnly * [<Query>] toDate : DateOnly * ?ct : CancellationToken -> Task<Sessions>

    // An example from RestEase's own docs
    [<Post "users/new">]
    abstract CreateUserString : [<Body>] user : string * ?ct : CancellationToken -> Task<string>

    [<Post "users/new">]
    abstract CreateUserStream : [<Body>] user : System.IO.Stream * ?ct : CancellationToken -> Task<Stream>

    [<Post "users/new">]
    abstract CreateUserByteArr : [<Body>] user : byte[] * ?ct : CancellationToken -> Task<Stream>

    [<Post "users/new">]
    abstract CreateUserByteArr' : [<Body>] user : array<byte> * ?ct : CancellationToken -> Task<Stream>

    [<Post "users/new">]
    abstract CreateUserByteArr'' : [<Body>] user : byte array * ?ct : CancellationToken -> Task<Stream>

    [<Post "users/new">]
    abstract CreateUserSerialisedBody : [<Body>] user : PureGym.Member * ?ct : CancellationToken -> Task<string>

    [<Post "users/new">]
    abstract CreateUserSerialisedUrlBody : [<Body>] user : Uri * ?ct : CancellationToken -> Task<string>

    [<Post "users/new">]
    abstract CreateUserSerialisedIntBody : [<Body>] user : int * ?ct : CancellationToken -> Task<string>

    [<Post "users/new">]
    abstract CreateUserHttpContent :
        [<Body>] user : System.Net.Http.HttpContent * ?ct : CancellationToken -> Task<string>

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
    abstract GetResponse : ?ct : CancellationToken -> Task<Response<MemberActivityDto>>

    [<Get "endpoint">]
    abstract GetResponse' : ?ct : CancellationToken -> Task<RestEase.Response<MemberActivityDto>>

    [<Get "endpoint">]
    abstract GetResponse'' : ?ct : CancellationToken -> Task<MemberActivityDto Response>

    [<Get "endpoint">]
    abstract GetResponse''' : ?ct : CancellationToken -> Task<MemberActivityDto RestEase.Response>

    [<Get "endpoint">]
    [<AllowAnyStatusCode>]
    abstract GetWithAnyReturnCode : ?ct : CancellationToken -> Task<HttpResponseMessage>

    [<Get "endpoint">]
    abstract GetWithoutAnyReturnCode : ?ct : CancellationToken -> Task<HttpResponseMessage>

[<WoofWare.Myriad.Plugins.HttpClient>]
type internal IApiWithoutBaseAddress =
    [<Get "endpoint/{param}">]
    abstract GetPathParam : [<Path "param">] parameter : string * ?ct : CancellationToken -> Task<string>

[<WoofWare.Myriad.Plugins.HttpClient>]
[<BasePath "foo">]
type IApiWithBasePath =
    // Example where we use the bundled attributes rather than RestEase's
    [<WoofWare.Myriad.Plugins.RestEase.Get "endpoint/{param}">]
    abstract GetPathParam : [<Path "param">] parameter : string * ?cancellationToken : CancellationToken -> Task<string>

[<WoofWare.Myriad.Plugins.HttpClient>]
[<BaseAddress "https://whatnot.com/thing">]
[<BasePath "foo">]
type IApiWithBasePathAndAddress =
    [<Get "endpoint/{param}">]
    abstract GetPathParam : [<Path "param">] parameter : string * ?ct : CancellationToken -> Task<string>

[<WoofWare.Myriad.Plugins.HttpClient>]
[<BasePath "/foo">]
type IApiWithAbsoluteBasePath =
    // Example where we use the bundled attributes rather than RestEase's
    [<WoofWare.Myriad.Plugins.RestEase.Get "endpoint/{param}">]
    abstract GetPathParam : [<Path "param">] parameter : string * ?cancellationToken : CancellationToken -> Task<string>

[<WoofWare.Myriad.Plugins.HttpClient>]
[<BaseAddress "https://whatnot.com/thing">]
[<BasePath "/foo">]
type IApiWithAbsoluteBasePathAndAddress =
    [<Get "endpoint/{param}">]
    abstract GetPathParam : [<Path "param">] parameter : string * ?ct : CancellationToken -> Task<string>

[<WoofWare.Myriad.Plugins.HttpClient>]
[<BasePath "foo">]
type IApiWithBasePathAndAbsoluteEndpoint =
    // Example where we use the bundled attributes rather than RestEase's
    [<WoofWare.Myriad.Plugins.RestEase.Get "/endpoint/{param}">]
    abstract GetPathParam : [<Path "param">] parameter : string * ?cancellationToken : CancellationToken -> Task<string>

[<WoofWare.Myriad.Plugins.HttpClient>]
[<BaseAddress "https://whatnot.com/thing">]
[<BasePath "foo">]
type IApiWithBasePathAndAddressAndAbsoluteEndpoint =
    [<Get "/endpoint/{param}">]
    abstract GetPathParam : [<Path "param">] parameter : string * ?ct : CancellationToken -> Task<string>

[<WoofWare.Myriad.Plugins.HttpClient>]
[<BasePath "/foo">]
type IApiWithAbsoluteBasePathAndAbsoluteEndpoint =
    // Example where we use the bundled attributes rather than RestEase's
    [<WoofWare.Myriad.Plugins.RestEase.Get "/endpoint/{param}">]
    abstract GetPathParam : [<Path "param">] parameter : string * ?cancellationToken : CancellationToken -> Task<string>

[<WoofWare.Myriad.Plugins.HttpClient>]
[<BaseAddress "https://whatnot.com/thing">]
[<BasePath "/foo">]
type IApiWithAbsoluteBasePathAndAddressAndAbsoluteEndpoint =
    [<Get "/endpoint/{param}">]
    abstract GetPathParam : [<Path "param">] parameter : string * ?ct : CancellationToken -> Task<string>

[<WoofWare.Myriad.Plugins.HttpClient>]
[<Header("Header-Name", "Header-Value")>]
type IApiWithHeaders =
    [<Header "X-Foo">]
    abstract SomeHeader : string

    [<Header "Authorization">]
    abstract SomeOtherHeader : int

    [<Get "endpoint/{param}">]
    [<Header("Something-Else", "val")>]
    abstract GetPathParam : [<Path "param">] parameter : string * ?ct : CancellationToken -> Task<string>

[<WoofWare.Myriad.Plugins.HttpClient>]
[<WoofWare.Myriad.Plugins.RestEase.Header("Header-Name", "Header-Value")>]
type IApiWithHeaders2 =
    [<WoofWare.Myriad.Plugins.RestEase.Header "X-Foo">]
    abstract SomeHeader : string

    [<WoofWare.Myriad.Plugins.RestEase.Header "Authorization">]
    abstract SomeOtherHeader : int

    [<Get "endpoint/{param}">]
    abstract GetPathParam :
        [<WoofWare.Myriad.Plugins.RestEase.Path "param">] parameter : string * ?ct : CancellationToken -> Task<string>

[<WoofWare.Myriad.Plugins.HttpClient>]
type IClientWithJsonBody =
    // As a POST request of a JSON-serialised body, we automatically set Content-Type: application/json.
    [<Post "endpoint/{param}">]
    abstract GetPathParam :
        [<RestEase.Path "param">] parameter : string *
        [<WoofWare.Myriad.Plugins.RestEase.Body>] mem : PureGym.Member *
        ?ct : CancellationToken ->
            Task<string>

[<WoofWare.Myriad.Plugins.HttpClient>]
type IClientWithJsonBodyOverridden =
    // As a POST request of a JSON-serialised body, we *would* automatically set Content-Type: application/json,
    // but this method has overridden it.
    [<Post "endpoint/{param}">]
    [<Header("Content-Type", "application/ecmascript")>]
    abstract GetPathParam :
        [<RestEase.Path "param">] parameter : string *
        [<WoofWare.Myriad.Plugins.RestEase.Body>] mem : PureGym.Member *
        ?ct : CancellationToken ->
            Task<string>

[<WoofWare.Myriad.Plugins.HttpClient>]
type IClientWithStringBody =
    // As a POST request of a bare string body, we don't override the Content-Type.
    [<Post "endpoint/{param}">]
    abstract GetPathParam :
        [<RestEase.Path "param">] parameter : string *
        [<WoofWare.Myriad.Plugins.RestEase.Body>] mem : string *
        ?ct : CancellationToken ->
            Task<string>
