//------------------------------------------------------------------------------
//        This code was generated by myriad.
//        Changes to this file will be lost when the code is regenerated.
//------------------------------------------------------------------------------



namespace ConsumePlugin

/// Module containing JSON parsing methods for the JwtVaultAuthResponse type
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module JwtVaultAuthResponse =
    /// Parse from a JSON node.
    let jsonParse (node : System.Text.Json.Nodes.JsonNode) : JwtVaultAuthResponse =
        let NumUses =
            (match node.["num_uses"] with
             | null ->
                 raise (
                     System.Collections.Generic.KeyNotFoundException (
                         sprintf "Required key '%s' not found on JSON object" ("num_uses")
                     )
                 )
             | v -> v)
                .AsValue()
                .GetValue<int> ()

        let Orphan =
            (match node.["orphan"] with
             | null ->
                 raise (
                     System.Collections.Generic.KeyNotFoundException (
                         sprintf "Required key '%s' not found on JSON object" ("orphan")
                     )
                 )
             | v -> v)
                .AsValue()
                .GetValue<bool> ()

        let EntityId =
            (match node.["entity_id"] with
             | null ->
                 raise (
                     System.Collections.Generic.KeyNotFoundException (
                         sprintf "Required key '%s' not found on JSON object" ("entity_id")
                     )
                 )
             | v -> v)
                .AsValue()
                .GetValue<string> ()

        let TokenType =
            (match node.["token_type"] with
             | null ->
                 raise (
                     System.Collections.Generic.KeyNotFoundException (
                         sprintf "Required key '%s' not found on JSON object" ("token_type")
                     )
                 )
             | v -> v)
                .AsValue()
                .GetValue<string> ()

        let Renewable =
            (match node.["renewable"] with
             | null ->
                 raise (
                     System.Collections.Generic.KeyNotFoundException (
                         sprintf "Required key '%s' not found on JSON object" ("renewable")
                     )
                 )
             | v -> v)
                .AsValue()
                .GetValue<bool> ()

        let LeaseDuration =
            (match node.["lease_duration"] with
             | null ->
                 raise (
                     System.Collections.Generic.KeyNotFoundException (
                         sprintf "Required key '%s' not found on JSON object" ("lease_duration")
                     )
                 )
             | v -> v)
                .AsValue()
                .GetValue<int> ()

        let IdentityPolicies =
            (match node.["identity_policies"] with
             | null ->
                 raise (
                     System.Collections.Generic.KeyNotFoundException (
                         sprintf "Required key '%s' not found on JSON object" ("identity_policies")
                     )
                 )
             | v -> v)
                .AsArray ()
            |> Seq.map (fun elt -> elt.AsValue().GetValue<string> ())
            |> List.ofSeq

        let TokenPolicies =
            (match node.["token_policies"] with
             | null ->
                 raise (
                     System.Collections.Generic.KeyNotFoundException (
                         sprintf "Required key '%s' not found on JSON object" ("token_policies")
                     )
                 )
             | v -> v)
                .AsArray ()
            |> Seq.map (fun elt -> elt.AsValue().GetValue<string> ())
            |> List.ofSeq

        let Policies =
            (match node.["policies"] with
             | null ->
                 raise (
                     System.Collections.Generic.KeyNotFoundException (
                         sprintf "Required key '%s' not found on JSON object" ("policies")
                     )
                 )
             | v -> v)
                .AsArray ()
            |> Seq.map (fun elt -> elt.AsValue().GetValue<string> ())
            |> List.ofSeq

        let Accessor =
            (match node.["accessor"] with
             | null ->
                 raise (
                     System.Collections.Generic.KeyNotFoundException (
                         sprintf "Required key '%s' not found on JSON object" ("accessor")
                     )
                 )
             | v -> v)
                .AsValue()
                .GetValue<string> ()

        let ClientToken =
            (match node.["client_token"] with
             | null ->
                 raise (
                     System.Collections.Generic.KeyNotFoundException (
                         sprintf "Required key '%s' not found on JSON object" ("client_token")
                     )
                 )
             | v -> v)
                .AsValue()
                .GetValue<string> ()

        {
            ClientToken = ClientToken
            Accessor = Accessor
            Policies = Policies
            TokenPolicies = TokenPolicies
            IdentityPolicies = IdentityPolicies
            LeaseDuration = LeaseDuration
            Renewable = Renewable
            TokenType = TokenType
            EntityId = EntityId
            Orphan = Orphan
            NumUses = NumUses
        }
namespace ConsumePlugin

/// Module containing JSON parsing methods for the JwtVaultResponse type
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module JwtVaultResponse =
    /// Parse from a JSON node.
    let jsonParse (node : System.Text.Json.Nodes.JsonNode) : JwtVaultResponse =
        let Auth =
            JwtVaultAuthResponse.jsonParse (
                match node.["auth"] with
                | null ->
                    raise (
                        System.Collections.Generic.KeyNotFoundException (
                            sprintf "Required key '%s' not found on JSON object" ("auth")
                        )
                    )
                | v -> v
            )

        let LeaseDuration =
            (match node.["lease_duration"] with
             | null ->
                 raise (
                     System.Collections.Generic.KeyNotFoundException (
                         sprintf "Required key '%s' not found on JSON object" ("lease_duration")
                     )
                 )
             | v -> v)
                .AsValue()
                .GetValue<int> ()

        let Renewable =
            (match node.["renewable"] with
             | null ->
                 raise (
                     System.Collections.Generic.KeyNotFoundException (
                         sprintf "Required key '%s' not found on JSON object" ("renewable")
                     )
                 )
             | v -> v)
                .AsValue()
                .GetValue<bool> ()

        let LeaseId =
            (match node.["lease_id"] with
             | null ->
                 raise (
                     System.Collections.Generic.KeyNotFoundException (
                         sprintf "Required key '%s' not found on JSON object" ("lease_id")
                     )
                 )
             | v -> v)
                .AsValue()
                .GetValue<string> ()

        let RequestId =
            (match node.["request_id"] with
             | null ->
                 raise (
                     System.Collections.Generic.KeyNotFoundException (
                         sprintf "Required key '%s' not found on JSON object" ("request_id")
                     )
                 )
             | v -> v)
                .AsValue()
                .GetValue<string> ()

        {
            RequestId = RequestId
            LeaseId = LeaseId
            Renewable = Renewable
            LeaseDuration = LeaseDuration
            Auth = Auth
        }
namespace ConsumePlugin

/// Module containing JSON parsing methods for the JwtSecretResponse type
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module JwtSecretResponse =
    /// Parse from a JSON node.
    let jsonParse (node : System.Text.Json.Nodes.JsonNode) : JwtSecretResponse =
        let Data8 =
            (match node.["data8"] with
             | null ->
                 raise (
                     System.Collections.Generic.KeyNotFoundException (
                         sprintf "Required key '%s' not found on JSON object" ("data8")
                     )
                 )
             | v -> v)
                .AsObject ()
            |> Seq.map (fun kvp ->
                let key = (kvp.Key)
                let value = (kvp.Value).AsValue().GetValue<string> () |> System.Uri
                key, value
            )
            |> Seq.map System.Collections.Generic.KeyValuePair
            |> System.Collections.Generic.Dictionary

        let Data7 =
            (match node.["data7"] with
             | null ->
                 raise (
                     System.Collections.Generic.KeyNotFoundException (
                         sprintf "Required key '%s' not found on JSON object" ("data7")
                     )
                 )
             | v -> v)
                .AsObject ()
            |> Seq.map (fun kvp ->
                let key = (kvp.Key)
                let value = (kvp.Value).AsValue().GetValue<int> ()
                key, value
            )
            |> Map.ofSeq

        let Data6 =
            (match node.["data6"] with
             | null ->
                 raise (
                     System.Collections.Generic.KeyNotFoundException (
                         sprintf "Required key '%s' not found on JSON object" ("data6")
                     )
                 )
             | v -> v)
                .AsObject ()
            |> Seq.map (fun kvp ->
                let key = (kvp.Key) |> System.Uri
                let value = (kvp.Value).AsValue().GetValue<string> ()
                key, value
            )
            |> dict

        let Data5 =
            (match node.["data5"] with
             | null ->
                 raise (
                     System.Collections.Generic.KeyNotFoundException (
                         sprintf "Required key '%s' not found on JSON object" ("data5")
                     )
                 )
             | v -> v)
                .AsObject ()
            |> Seq.map (fun kvp ->
                let key = (kvp.Key) |> System.Uri
                let value = (kvp.Value).AsValue().GetValue<string> ()
                key, value
            )
            |> readOnlyDict

        let Data4 =
            (match node.["data4"] with
             | null ->
                 raise (
                     System.Collections.Generic.KeyNotFoundException (
                         sprintf "Required key '%s' not found on JSON object" ("data4")
                     )
                 )
             | v -> v)
                .AsObject ()
            |> Seq.map (fun kvp ->
                let key = (kvp.Key)
                let value = (kvp.Value).AsValue().GetValue<string> ()
                key, value
            )
            |> Map.ofSeq

        let Data3 =
            (match node.["data3"] with
             | null ->
                 raise (
                     System.Collections.Generic.KeyNotFoundException (
                         sprintf "Required key '%s' not found on JSON object" ("data3")
                     )
                 )
             | v -> v)
                .AsObject ()
            |> Seq.map (fun kvp ->
                let key = (kvp.Key)
                let value = (kvp.Value).AsValue().GetValue<string> ()
                key, value
            )
            |> Seq.map System.Collections.Generic.KeyValuePair
            |> System.Collections.Generic.Dictionary

        let Data2 =
            (match node.["data2"] with
             | null ->
                 raise (
                     System.Collections.Generic.KeyNotFoundException (
                         sprintf "Required key '%s' not found on JSON object" ("data2")
                     )
                 )
             | v -> v)
                .AsObject ()
            |> Seq.map (fun kvp ->
                let key = (kvp.Key)
                let value = (kvp.Value).AsValue().GetValue<string> ()
                key, value
            )
            |> dict

        let Data =
            (match node.["data"] with
             | null ->
                 raise (
                     System.Collections.Generic.KeyNotFoundException (
                         sprintf "Required key '%s' not found on JSON object" ("data")
                     )
                 )
             | v -> v)
                .AsObject ()
            |> Seq.map (fun kvp ->
                let key = (kvp.Key)
                let value = (kvp.Value).AsValue().GetValue<string> ()
                key, value
            )
            |> readOnlyDict

        let LeaseDuration =
            (match node.["lease_duration"] with
             | null ->
                 raise (
                     System.Collections.Generic.KeyNotFoundException (
                         sprintf "Required key '%s' not found on JSON object" ("lease_duration")
                     )
                 )
             | v -> v)
                .AsValue()
                .GetValue<int> ()

        let Renewable =
            (match node.["renewable"] with
             | null ->
                 raise (
                     System.Collections.Generic.KeyNotFoundException (
                         sprintf "Required key '%s' not found on JSON object" ("renewable")
                     )
                 )
             | v -> v)
                .AsValue()
                .GetValue<bool> ()

        let LeaseId =
            (match node.["lease_id"] with
             | null ->
                 raise (
                     System.Collections.Generic.KeyNotFoundException (
                         sprintf "Required key '%s' not found on JSON object" ("lease_id")
                     )
                 )
             | v -> v)
                .AsValue()
                .GetValue<string> ()

        let RequestId =
            (match node.["request_id"] with
             | null ->
                 raise (
                     System.Collections.Generic.KeyNotFoundException (
                         sprintf "Required key '%s' not found on JSON object" ("request_id")
                     )
                 )
             | v -> v)
                .AsValue()
                .GetValue<string> ()

        {
            RequestId = RequestId
            LeaseId = LeaseId
            Renewable = Renewable
            LeaseDuration = LeaseDuration
            Data = Data
            Data2 = Data2
            Data3 = Data3
            Data4 = Data4
            Data5 = Data5
            Data6 = Data6
            Data7 = Data7
            Data8 = Data8
        }

namespace ConsumePlugin

open System
open System.Collections.Generic
open System.Text.Json.Serialization
open System.Threading
open System.Threading.Tasks
open RestEase

/// Module for constructing a REST client.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module VaultClient =
    /// Create a REST client.
    let make (client : System.Net.Http.HttpClient) : IVaultClient =
        { new IVaultClient with
            member _.GetSecret
                (
                    jwt : JwtVaultResponse,
                    path : string,
                    mountPoint : string,
                    ct : CancellationToken option
                )
                =
                async {
                    let! ct = Async.CancellationToken

                    let uri =
                        System.Uri (
                            (match client.BaseAddress with
                             | null ->
                                 raise (
                                     System.ArgumentNullException (
                                         nameof (client.BaseAddress),
                                         "No base address was supplied on the type, and no BaseAddress was on the HttpClient."
                                     )
                                 )
                             | v -> v),
                            System.Uri (
                                "v1/{mountPoint}/{path}"
                                    .Replace("{path}", path.ToString () |> System.Web.HttpUtility.UrlEncode)
                                    .Replace (
                                        "{mountPoint}",
                                        mountPoint.ToString () |> System.Web.HttpUtility.UrlEncode
                                    ),
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
                    let! responseStream = response.Content.ReadAsStreamAsync ct |> Async.AwaitTask

                    let! jsonNode =
                        System.Text.Json.Nodes.JsonNode.ParseAsync (responseStream, cancellationToken = ct)
                        |> Async.AwaitTask

                    return JwtSecretResponse.jsonParse jsonNode
                }
                |> (fun a -> Async.StartAsTask (a, ?cancellationToken = ct))

            member _.GetJwt (role : string, jwt : string, ct : CancellationToken option) =
                async {
                    let! ct = Async.CancellationToken

                    let uri =
                        System.Uri (
                            (match client.BaseAddress with
                             | null ->
                                 raise (
                                     System.ArgumentNullException (
                                         nameof (client.BaseAddress),
                                         "No base address was supplied on the type, and no BaseAddress was on the HttpClient."
                                     )
                                 )
                             | v -> v),
                            System.Uri ("v1/auth/jwt/login", System.UriKind.Relative)
                        )

                    let httpMessage =
                        new System.Net.Http.HttpRequestMessage (
                            Method = System.Net.Http.HttpMethod.Get,
                            RequestUri = uri
                        )

                    let! response = client.SendAsync (httpMessage, ct) |> Async.AwaitTask
                    let response = response.EnsureSuccessStatusCode ()
                    let! responseStream = response.Content.ReadAsStreamAsync ct |> Async.AwaitTask

                    let! jsonNode =
                        System.Text.Json.Nodes.JsonNode.ParseAsync (responseStream, cancellationToken = ct)
                        |> Async.AwaitTask

                    return JwtVaultResponse.jsonParse jsonNode
                }
                |> (fun a -> Async.StartAsTask (a, ?cancellationToken = ct))
        }
