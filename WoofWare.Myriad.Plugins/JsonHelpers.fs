namespace WoofWare.Myriad.Plugins

open System.Text.Json.Nodes

[<AutoOpen>]
module internal JsonHelpers =
    let inline asString (n : JsonNode) (key : string) : string =
        match n.[key] with
        | null -> failwith $"Expected node to have a key '%s{key}', but it did not: %s{n.ToJsonString ()}"
        | s -> s.GetValue<string> ()

    [<RequiresExplicitTypeArguments>]
    let inline asOpt<'ret> (n : JsonNode) (key : string) : 'ret option =
        match n.[key] with
        | null -> None
        | s -> s.GetValue<'ret> () |> Some

    let inline asObj (n : JsonNode) (key : string) : JsonObject =
        match n.[key] with
        | null -> failwith $"Expected node to have a key '%s{key}', but it did not: %s{n.ToJsonString ()}"
        | o -> o.AsObject ()

    let inline asObjOpt (n : JsonNode) (key : string) : JsonObject option =
        match n.[key] with
        | null -> None
        | o -> o.AsObject () |> Some

    let inline asArr (n : JsonNode) (key : string) : JsonArray =
        match n.[key] with
        | null -> failwith $"Expected node to have a key '%s{key}', but it did not: %s{n.ToJsonString ()}"
        | o -> o.AsArray ()

    let inline asArrOpt (n : JsonNode) (key : string) : JsonArray option =
        match n.[key] with
        | null -> None
        | o -> o.AsArray () |> Some

    [<RequiresExplicitTypeArguments>]
    let inline asArr'<'v> (n : JsonNode) (key : string) : 'v list =
        match n.[key] with
        | null -> failwith $"Expected node to have a key '%s{key}', but it did not: %s{n.ToJsonString ()}"
        | o -> o.AsArray () |> Seq.map (fun v -> v.GetValue<'v> ()) |> Seq.toList

    [<RequiresExplicitTypeArguments>]
    let inline asArrOpt'<'v> (n : JsonNode) (key : string) : 'v list option =
        match n.[key] with
        | null -> None
        | o -> o.AsArray () |> Seq.map (fun v -> v.GetValue<'v> ()) |> Seq.toList |> Some
