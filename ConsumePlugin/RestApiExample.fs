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

(*
    [<Get "v1/member">]
    abstract GetMember : unit -> Task<Member>

    [<Get "v1/gyms/{gym_id}">]
    abstract GetGym : [<Path "gym_id">] gymId : int -> Task<Gym>

    [<Get "v1/member/activity">]
    abstract GetMemberActivity : unit -> Task<MemberActivityDto>

    [<Get "v2/gymSessions/member">]
    abstract GetSessions : [<Query>] fromDate : DateTime -> [<Query>] toDate : DateTime -> Task<Sessions>
    *)


module Foo =
    let make (client : System.Net.Http.HttpClient) =
        { new IPureGymApi with
            member _.GetGyms (ct : CancellationToken option) =
                async {
                    let! ct = Async.CancellationToken

                    let! response =
                        client.GetAsync (client.BaseAddress.ToString () + "v1/gyms/", ct)
                        |> Async.AwaitTask

                    let response = response.EnsureSuccessStatusCode ()
                    let! stream = response.Content.ReadAsStreamAsync ct |> Async.AwaitTask

                    let! node =
                        System.Text.Json.Nodes.JsonNode.ParseAsync (stream, cancellationToken = ct)
                        |> Async.AwaitTask

                    return
                        node.AsArray ()
                        |> Seq.map (fun elt -> elt.AsValue () |> Gym.jsonParse)
                        |> List.ofSeq
                }
                |> fun a -> Async.StartAsTask (a, ?cancellationToken = ct)

            member _.GetGymAttendance (gym_id : int, ct : CancellationToken option) =
                async {
                    let! ct = Async.CancellationToken

                    let! response =
                        client.GetAsync (client.BaseAddress.ToString () + $"v1/gyms/{gym_id}/attendance", ct)
                        |> Async.AwaitTask

                    let response = response.EnsureSuccessStatusCode ()
                    let! stream = response.Content.ReadAsStreamAsync ct |> Async.AwaitTask

                    let! node =
                        System.Text.Json.Nodes.JsonNode.ParseAsync (stream, cancellationToken = ct)
                        |> Async.AwaitTask

                    return GymAttendance.jsonParse node
                }
                |> fun a -> Async.StartAsTask (a, ?cancellationToken = ct)
        }
