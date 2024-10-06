namespace WoofWare.Myriad.Plugins

open System.Collections.Generic

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
