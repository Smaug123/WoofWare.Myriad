namespace WoofWare.Myriad.Plugins

open System

/// An HTTP method. This is System.Net.Http.HttpMethod, but
/// a proper discriminated union.
type HttpMethod =
    /// HTTP Get
    | Get
    /// HTTP Post
    | Post
    /// HTTP Delete
    | Delete
    /// HTTP Patch
    | Patch
    /// HTTP Options
    | Options
    /// HTTP Head
    | Head
    /// HTTP Put
    | Put
    /// HTTP Trace
    | Trace

    /// Convert to the standard library's enum type.
    member this.ToDotNet () : System.Net.Http.HttpMethod =
        match this with
        | HttpMethod.Get -> System.Net.Http.HttpMethod.Get
        | HttpMethod.Post -> System.Net.Http.HttpMethod.Post
        | HttpMethod.Delete -> System.Net.Http.HttpMethod.Delete
        | HttpMethod.Patch -> System.Net.Http.HttpMethod.Patch
        | HttpMethod.Options -> System.Net.Http.HttpMethod.Options
        | HttpMethod.Head -> System.Net.Http.HttpMethod.Head
        | HttpMethod.Put -> System.Net.Http.HttpMethod.Put
        | HttpMethod.Trace -> System.Net.Http.HttpMethod.Trace

    /// Human-readable string representation.
    override this.ToString () : string =
        match this with
        | HttpMethod.Get -> "Get"
        | HttpMethod.Post -> "Post"
        | HttpMethod.Delete -> "Delete"
        | HttpMethod.Patch -> "Patch"
        | HttpMethod.Options -> "Options"
        | HttpMethod.Head -> "Head"
        | HttpMethod.Put -> "Put"
        | HttpMethod.Trace -> "Trace"

    /// Throws on invalid inputs.
    static member Parse (s : string) : HttpMethod =
        if String.Equals (s, "get", StringComparison.OrdinalIgnoreCase) then
            HttpMethod.Get
        elif String.Equals (s, "post", StringComparison.OrdinalIgnoreCase) then
            HttpMethod.Post
        elif String.Equals (s, "patch", StringComparison.OrdinalIgnoreCase) then
            HttpMethod.Patch
        elif String.Equals (s, "delete", StringComparison.OrdinalIgnoreCase) then
            HttpMethod.Delete
        elif String.Equals (s, "head", StringComparison.OrdinalIgnoreCase) then
            HttpMethod.Head
        elif String.Equals (s, "options", StringComparison.OrdinalIgnoreCase) then
            HttpMethod.Options
        elif String.Equals (s, "put", StringComparison.OrdinalIgnoreCase) then
            HttpMethod.Put
        else
            failwith $"Unrecognised method: %s{s}"
