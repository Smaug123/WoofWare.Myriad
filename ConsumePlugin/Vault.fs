namespace ConsumePlugin

open System.Collections.Generic
open System.Text.Json.Serialization
open System.Threading
open System.Threading.Tasks
open RestEase

[<WoofWare.Myriad.Plugins.JsonParse>]
type JwtVaultAuthResponse =
    {
        [<JsonPropertyName "client_token">]
        ClientToken : string
        Accessor : string
        Policies : string list
        [<JsonPropertyName "token_policies">]
        TokenPolicies : string list
        [<JsonPropertyName "identity_policies">]
        IdentityPolicies : string list
        [<JsonPropertyName "lease_duration">]
        LeaseDuration : int
        Renewable : bool
        TokenType : string
        [<JsonPropertyName "entity_id">]
        EntityId : string
        Orphan : bool
        [<JsonPropertyName "num_uses">]
        NumUses : int
    }

[<WoofWare.Myriad.Plugins.JsonParse>]
type JwtVaultResponse =
    {
        [<JsonPropertyName "request_id">]
        RequestId : string
        [<JsonPropertyName "lease_id">]
        LeaseId : string
        Renewable : bool
        [<JsonPropertyName "lease_duration">]
        LeaseDuration : int
        Auth : JwtVaultAuthResponse
    }

[<WoofWare.Myriad.Plugins.JsonParse>]
type JwtSecretResponse =
    {
        [<JsonPropertyName "request_id">]
        RequestId : string
        [<JsonPropertyName "lease_id">]
        LeaseId : string
        Renewable : bool
        [<JsonPropertyName "lease_duration">]
        LeaseDuration : int
        Data : IReadOnlyDictionary<string, string>
    }

[<WoofWare.Myriad.Plugins.HttpClient>]
type IVaultClient =
    [<Get "v1/{mountPoint}/{path}">]
    abstract GetSecret :
        jwt : JwtVaultResponse *
        [<Path "path">] path : string *
        [<Path "mountPoint">] mountPoint : string *
        ?ct : CancellationToken ->
            Task<JwtSecretResponse>

    [<Get "v1/auth/jwt/login">]
    abstract GetJwt : role : string * jwt : string * ?ct : CancellationToken -> Task<JwtVaultResponse>
