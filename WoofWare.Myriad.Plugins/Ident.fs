namespace WoofWare.Myriad.Plugins

open System
open System.Text
open Fantomas.FCS.Syntax
open Myriad.Core

[<RequireQualifiedAccess>]
module internal Ident =
    let lowerFirstLetter (x : Ident) : Ident =
        let result = StringBuilder x.idText.Length
        result.Append (Char.ToLowerInvariant x.idText.[0]) |> ignore
        result.Append x.idText.[1..] |> ignore
        Ident.Create ((result : StringBuilder).ToString ())
