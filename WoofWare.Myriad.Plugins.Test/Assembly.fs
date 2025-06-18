namespace WoofWare.Myriad.Plugins.Test

open System
open System.IO
open System.Reflection

[<RequireQualifiedAccess>]
module Assembly =

    let getEmbeddedResource (assembly : Assembly) (name : string) : string =
        let names = assembly.GetManifestResourceNames ()

        let names =
            names |> Seq.filter (fun s -> s.EndsWith (name, StringComparison.Ordinal))

        use s =
            names
            |> Seq.exactlyOne
            |> assembly.GetManifestResourceStream
            |> fun s -> new StreamReader (s)

        s.ReadToEnd ()
