//------------------------------------------------------------------------------
//        This code was generated by myriad.
//        Changes to this file will be lost when the code is regenerated.
//------------------------------------------------------------------------------


namespace PureGym

open System
open System.Threading
open System.Threading.Tasks
open System.IO
open System.Net
open System.Net.Http
open RestEase

/// Module for constructing a REST client.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module PureGymApi =
    /// Create a REST client.
    let make (client : System.Net.Http.HttpClient) : IPureGymApi =
        { new IPureGymApi with
            member _.GetGyms (ct : CancellationToken option) =
                async {
                    let! ct = Async.CancellationToken

                    let uri =
                        System.Uri (client.BaseAddress, System.Uri ("v1/gyms/", System.UriKind.Relative))

                    let httpMessage =
                        new System.Net.Http.HttpRequestMessage (
                            Method = System.Net.Http.HttpMethod.Get,
                            RequestUri = uri
                        )

                    let! response = client.SendAsync (httpMessage, ct) |> Async.AwaitTask
                    let response = response.EnsureSuccessStatusCode ()
                    let! stream = response.Content.ReadAsStreamAsync ct |> Async.AwaitTask

                    let! node =
                        System.Text.Json.Nodes.JsonNode.ParseAsync (stream, cancellationToken = ct)
                        |> Async.AwaitTask

                    return node.AsArray () |> Seq.map (fun elt -> Gym.jsonParse elt) |> List.ofSeq
                }
                |> (fun a -> Async.StartAsTask (a, ?cancellationToken = ct))

            member _.GetGymAttendance (gymId : int, ct : CancellationToken option) =
                async {
                    let! ct = Async.CancellationToken

                    let uri =
                        System.Uri (
                            client.BaseAddress,
                            System.Uri (
                                "v1/gyms/{gym_id}/attendance"
                                    .Replace ("{gym_id}", gymId.ToString () |> System.Web.HttpUtility.UrlEncode),
                                System.UriKind.Relative
                            )
                        )

                    let httpMessage =
                        new System.Net.Http.HttpRequestMessage (
                            Method = System.Net.Http.HttpMethod.Get,
                            RequestUri = uri
                        )

                    let! response = client.SendAsync (httpMessage, ct) |> Async.AwaitTask
                    let response = response.EnsureSuccessStatusCode ()
                    let! stream = response.Content.ReadAsStreamAsync ct |> Async.AwaitTask

                    let! node =
                        System.Text.Json.Nodes.JsonNode.ParseAsync (stream, cancellationToken = ct)
                        |> Async.AwaitTask

                    return GymAttendance.jsonParse node
                }
                |> (fun a -> Async.StartAsTask (a, ?cancellationToken = ct))

            member _.GetMember (ct : CancellationToken option) =
                async {
                    let! ct = Async.CancellationToken

                    let uri =
                        System.Uri (client.BaseAddress, System.Uri ("v1/member", System.UriKind.Relative))

                    let httpMessage =
                        new System.Net.Http.HttpRequestMessage (
                            Method = System.Net.Http.HttpMethod.Get,
                            RequestUri = uri
                        )

                    let! response = client.SendAsync (httpMessage, ct) |> Async.AwaitTask
                    let response = response.EnsureSuccessStatusCode ()
                    let! stream = response.Content.ReadAsStreamAsync ct |> Async.AwaitTask

                    let! node =
                        System.Text.Json.Nodes.JsonNode.ParseAsync (stream, cancellationToken = ct)
                        |> Async.AwaitTask

                    return Member.jsonParse node
                }
                |> (fun a -> Async.StartAsTask (a, ?cancellationToken = ct))

            member _.GetGym (gymId : int, ct : CancellationToken option) =
                async {
                    let! ct = Async.CancellationToken

                    let uri =
                        System.Uri (
                            client.BaseAddress,
                            System.Uri (
                                "v1/gyms/{gym_id}"
                                    .Replace ("{gym_id}", gymId.ToString () |> System.Web.HttpUtility.UrlEncode),
                                System.UriKind.Relative
                            )
                        )

                    let httpMessage =
                        new System.Net.Http.HttpRequestMessage (
                            Method = System.Net.Http.HttpMethod.Get,
                            RequestUri = uri
                        )

                    let! response = client.SendAsync (httpMessage, ct) |> Async.AwaitTask
                    let response = response.EnsureSuccessStatusCode ()
                    let! stream = response.Content.ReadAsStreamAsync ct |> Async.AwaitTask

                    let! node =
                        System.Text.Json.Nodes.JsonNode.ParseAsync (stream, cancellationToken = ct)
                        |> Async.AwaitTask

                    return Gym.jsonParse node
                }
                |> (fun a -> Async.StartAsTask (a, ?cancellationToken = ct))

            member _.GetMemberActivity (ct : CancellationToken option) =
                async {
                    let! ct = Async.CancellationToken

                    let uri =
                        System.Uri (client.BaseAddress, System.Uri ("v1/member/activity", System.UriKind.Relative))

                    let httpMessage =
                        new System.Net.Http.HttpRequestMessage (
                            Method = System.Net.Http.HttpMethod.Get,
                            RequestUri = uri
                        )

                    let! response = client.SendAsync (httpMessage, ct) |> Async.AwaitTask
                    let response = response.EnsureSuccessStatusCode ()
                    let! stream = response.Content.ReadAsStreamAsync ct |> Async.AwaitTask

                    let! node =
                        System.Text.Json.Nodes.JsonNode.ParseAsync (stream, cancellationToken = ct)
                        |> Async.AwaitTask

                    return MemberActivityDto.jsonParse node
                }
                |> (fun a -> Async.StartAsTask (a, ?cancellationToken = ct))

            member _.GetSessions (fromDate : DateOnly, toDate : DateOnly, ct : CancellationToken option) =
                async {
                    let! ct = Async.CancellationToken

                    let uri =
                        System.Uri (
                            client.BaseAddress,
                            System.Uri (
                                ("/v2/gymSessions/member"
                                 + "?fromDate="
                                 + ((fromDate.ToString "yyyy-MM-dd") |> System.Web.HttpUtility.UrlEncode)
                                 + "&toDate="
                                 + ((toDate.ToString "yyyy-MM-dd") |> System.Web.HttpUtility.UrlEncode)),
                                System.UriKind.Relative
                            )
                        )

                    let httpMessage =
                        new System.Net.Http.HttpRequestMessage (
                            Method = System.Net.Http.HttpMethod.Get,
                            RequestUri = uri
                        )

                    let! response = client.SendAsync (httpMessage, ct) |> Async.AwaitTask
                    let response = response.EnsureSuccessStatusCode ()
                    let! stream = response.Content.ReadAsStreamAsync ct |> Async.AwaitTask

                    let! node =
                        System.Text.Json.Nodes.JsonNode.ParseAsync (stream, cancellationToken = ct)
                        |> Async.AwaitTask

                    return Sessions.jsonParse node
                }
                |> (fun a -> Async.StartAsTask (a, ?cancellationToken = ct))

            member _.GetPathParam (parameter : string, ct : CancellationToken option) =
                async {
                    let! ct = Async.CancellationToken

                    let uri =
                        System.Uri (
                            client.BaseAddress,
                            System.Uri (
                                "endpoint/{param}"
                                    .Replace ("{param}", parameter.ToString () |> System.Web.HttpUtility.UrlEncode),
                                System.UriKind.Relative
                            )
                        )

                    let httpMessage =
                        new System.Net.Http.HttpRequestMessage (
                            Method = System.Net.Http.HttpMethod.Get,
                            RequestUri = uri
                        )

                    let! response = client.SendAsync (httpMessage, ct) |> Async.AwaitTask
                    let response = response.EnsureSuccessStatusCode ()
                    let! node = response.Content.ReadAsStringAsync ct |> Async.AwaitTask
                    return node
                }
                |> (fun a -> Async.StartAsTask (a, ?cancellationToken = ct))

            member _.GetStream (ct : CancellationToken option) =
                async {
                    let! ct = Async.CancellationToken

                    let uri =
                        System.Uri (client.BaseAddress, System.Uri ("endpoint", System.UriKind.Relative))

                    let httpMessage =
                        new System.Net.Http.HttpRequestMessage (
                            Method = System.Net.Http.HttpMethod.Get,
                            RequestUri = uri
                        )

                    let! response = client.SendAsync (httpMessage, ct) |> Async.AwaitTask
                    let response = response.EnsureSuccessStatusCode ()
                    let! node = response.Content.ReadAsStreamAsync ct |> Async.AwaitTask
                    return node
                }
                |> (fun a -> Async.StartAsTask (a, ?cancellationToken = ct))

            member _.GetStream' (ct : CancellationToken option) =
                async {
                    let! ct = Async.CancellationToken

                    let uri =
                        System.Uri (client.BaseAddress, System.Uri ("endpoint", System.UriKind.Relative))

                    let httpMessage =
                        new System.Net.Http.HttpRequestMessage (
                            Method = System.Net.Http.HttpMethod.Get,
                            RequestUri = uri
                        )

                    let! response = client.SendAsync (httpMessage, ct) |> Async.AwaitTask
                    let response = response.EnsureSuccessStatusCode ()
                    let! node = response.Content.ReadAsStreamAsync ct |> Async.AwaitTask
                    return node
                }
                |> (fun a -> Async.StartAsTask (a, ?cancellationToken = ct))

            member _.GetStream'' (ct : CancellationToken option) =
                async {
                    let! ct = Async.CancellationToken

                    let uri =
                        System.Uri (client.BaseAddress, System.Uri ("endpoint", System.UriKind.Relative))

                    let httpMessage =
                        new System.Net.Http.HttpRequestMessage (
                            Method = System.Net.Http.HttpMethod.Get,
                            RequestUri = uri
                        )

                    let! response = client.SendAsync (httpMessage, ct) |> Async.AwaitTask
                    let response = response.EnsureSuccessStatusCode ()
                    let! node = response.Content.ReadAsStreamAsync ct |> Async.AwaitTask
                    return node
                }
                |> (fun a -> Async.StartAsTask (a, ?cancellationToken = ct))

            member _.GetResponseMessage (ct : CancellationToken option) =
                async {
                    let! ct = Async.CancellationToken

                    let uri =
                        System.Uri (client.BaseAddress, System.Uri ("endpoint", System.UriKind.Relative))

                    let httpMessage =
                        new System.Net.Http.HttpRequestMessage (
                            Method = System.Net.Http.HttpMethod.Get,
                            RequestUri = uri
                        )

                    let! response = client.SendAsync (httpMessage, ct) |> Async.AwaitTask
                    let response = response.EnsureSuccessStatusCode ()
                    let! stream = response.Content.ReadAsStreamAsync ct |> Async.AwaitTask

                    let! node =
                        System.Text.Json.Nodes.JsonNode.ParseAsync (stream, cancellationToken = ct)
                        |> Async.AwaitTask

                    return System.Net.Http.HttpResponseMessage.jsonParse node
                }
                |> (fun a -> Async.StartAsTask (a, ?cancellationToken = ct))

            member _.GetResponseMessage' (ct : CancellationToken option) =
                async {
                    let! ct = Async.CancellationToken

                    let uri =
                        System.Uri (client.BaseAddress, System.Uri ("endpoint", System.UriKind.Relative))

                    let httpMessage =
                        new System.Net.Http.HttpRequestMessage (
                            Method = System.Net.Http.HttpMethod.Get,
                            RequestUri = uri
                        )

                    let! response = client.SendAsync (httpMessage, ct) |> Async.AwaitTask
                    let response = response.EnsureSuccessStatusCode ()
                    let! stream = response.Content.ReadAsStreamAsync ct |> Async.AwaitTask

                    let! node =
                        System.Text.Json.Nodes.JsonNode.ParseAsync (stream, cancellationToken = ct)
                        |> Async.AwaitTask

                    return Net.Http.HttpResponseMessage.jsonParse node
                }
                |> (fun a -> Async.StartAsTask (a, ?cancellationToken = ct))

            member _.GetResponseMessage'' (ct : CancellationToken option) =
                async {
                    let! ct = Async.CancellationToken

                    let uri =
                        System.Uri (client.BaseAddress, System.Uri ("endpoint", System.UriKind.Relative))

                    let httpMessage =
                        new System.Net.Http.HttpRequestMessage (
                            Method = System.Net.Http.HttpMethod.Get,
                            RequestUri = uri
                        )

                    let! response = client.SendAsync (httpMessage, ct) |> Async.AwaitTask
                    let response = response.EnsureSuccessStatusCode ()
                    let! stream = response.Content.ReadAsStreamAsync ct |> Async.AwaitTask

                    let! node =
                        System.Text.Json.Nodes.JsonNode.ParseAsync (stream, cancellationToken = ct)
                        |> Async.AwaitTask

                    return Http.HttpResponseMessage.jsonParse node
                }
                |> (fun a -> Async.StartAsTask (a, ?cancellationToken = ct))

            member _.GetResponseMessage''' (ct : CancellationToken option) =
                async {
                    let! ct = Async.CancellationToken

                    let uri =
                        System.Uri (client.BaseAddress, System.Uri ("endpoint", System.UriKind.Relative))

                    let httpMessage =
                        new System.Net.Http.HttpRequestMessage (
                            Method = System.Net.Http.HttpMethod.Get,
                            RequestUri = uri
                        )

                    let! response = client.SendAsync (httpMessage, ct) |> Async.AwaitTask
                    let response = response.EnsureSuccessStatusCode ()
                    let! stream = response.Content.ReadAsStreamAsync ct |> Async.AwaitTask

                    let! node =
                        System.Text.Json.Nodes.JsonNode.ParseAsync (stream, cancellationToken = ct)
                        |> Async.AwaitTask

                    return HttpResponseMessage.jsonParse node
                }
                |> (fun a -> Async.StartAsTask (a, ?cancellationToken = ct))
        }
