namespace ConsumePlugin

open System
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
        [<JsonPropertyName "token_type">]
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
        // These ones aren't actually part of the Vault response, but are here for tests
        Data2 : IDictionary<string, string>
        Data3 : Dictionary<string, string>
        Data4 : Map<string, string>
        Data5 : IReadOnlyDictionary<System.Uri, string>
        Data6 : IDictionary<Uri, string>
        Data7 : Map<string, int>
        Data8 : Dictionary<string, Uri>
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

[<WoofWare.Myriad.Plugins.HttpClient false>]
type IVaultClientNonExtensionMethod =
    [<Get "v1/{mountPoint}/{path}">]
    abstract GetSecret :
        jwt : JwtVaultResponse *
        [<Path "path">] path : string *
        [<Path "mountPoint">] mountPoint : string *
        ?ct : CancellationToken ->
            Task<JwtSecretResponse>

    [<Get "v1/auth/jwt/login">]
    abstract GetJwt : role : string * jwt : string * ?ct : CancellationToken -> Task<JwtVaultResponse>

[<WoofWare.Myriad.Plugins.HttpClient(true)>]
type IVaultClientExtensionMethod =
    [<Get "v1/{mountPoint}/{path}">]
    abstract GetSecret :
        jwt : JwtVaultResponse *
        [<Path "path">] path : string *
        [<Path "mountPoint">] mountPoint : string *
        ?ct : CancellationToken ->
            Task<JwtSecretResponse>

    [<Get "v1/auth/jwt/login">]
    abstract GetJwt : role : string * jwt : string * ?ct : CancellationToken -> Task<JwtVaultResponse>

[<RequireQualifiedAccess>]
module VaultClientExtensionMethod =
    let thisClashes = 99
