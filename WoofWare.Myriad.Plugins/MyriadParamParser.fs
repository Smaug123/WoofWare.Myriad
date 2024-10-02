namespace WoofWare.Myriad.Plugins

open System.Collections.Generic

type internal DesiredGenerator =
    | InterfaceMock of isInternal : bool option
    | JsonParse of extensionMethod : bool option
    | JsonSerialize of extensionMethod : bool option
    | HttpClient of extensionMethod : bool option

    static member Parse (s : string) =
        match s with
        | "GenerateMock" -> DesiredGenerator.InterfaceMock None
        | "GenerateMock(true)" -> DesiredGenerator.InterfaceMock (Some true)
        | "GenerateMock(false)" -> DesiredGenerator.InterfaceMock (Some false)
        | "JsonParse" -> DesiredGenerator.JsonParse None
        | "JsonParse(true)" -> DesiredGenerator.JsonParse (Some true)
        | "JsonParse(false)" -> DesiredGenerator.JsonParse (Some false)
        | "JsonSerialize" -> DesiredGenerator.JsonSerialize None
        | "JsonSerialize(true)" -> DesiredGenerator.JsonSerialize (Some true)
        | "JsonSerialize(false)" -> DesiredGenerator.JsonSerialize (Some false)
        | "HttpClient" -> DesiredGenerator.HttpClient None
        | "HttpClient(true)" -> DesiredGenerator.HttpClient (Some true)
        | "HttpClient(false)" -> DesiredGenerator.HttpClient (Some false)
        | _ -> failwith $"Failed to parse as a generator specification: %s{s}"

[<RequireQualifiedAccess>]
module internal MyriadParamParser =
    (*
      An apparent bug in Myriad's argument parsing means that this:

        <MyriadParams>
            <Foo>bar</Foo>
            <Baz>quux</Baz>
        </MyriadParams>

      leads to this:

        Foo = "bar;Baz=quux"

      I'm not going to put effort into fixing Myriad, though, because I want
      to build something much more powerful instead.
    *)

    /// Call this with `context.AdditionalParameters`.
    let render (pars : IDictionary<string, string>) : Map<string, string> =
        match pars.Count with
        | 0 -> Map.empty
        | 1 ->
            let (KeyValue (key, value)) = pars |> Seq.exactlyOne

            match value.Split ';' |> Seq.toList with
            | [] -> failwith "LOGIC ERROR"
            | value :: rest ->
                rest
                |> Seq.map (fun v ->
                    let split = v.Split '='
                    split.[0], String.concat "=" split.[1..]
                )
                |> Seq.append (Seq.singleton (key, value))
                |> Map.ofSeq
        | _ ->
            // assume the Myriad bug is fixed!
            pars |> Seq.map (fun (KeyValue (k, v)) -> k, v) |> Map.ofSeq
