namespace WoofWare.Myriad.Plugins

open System
open System.Text
open Fantomas.FCS.Syntax
open Fantomas.FCS.Text.Range

[<RequireQualifiedAccess>]
module internal Ident =
    let inline create (s : string) = Ident (s, range0)

    let lowerFirstLetter (x : Ident) : Ident =
        let result = StringBuilder x.idText.Length
        result.Append (Char.ToLowerInvariant x.idText.[0]) |> ignore
        result.Append x.idText.[1..] |> ignore
        create ((result : StringBuilder).ToString ())
