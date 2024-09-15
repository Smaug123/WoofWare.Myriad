namespace WoofWare.Myriad.Plugins

open System
open System.Text
open System.Text.RegularExpressions
open Fantomas.FCS.Syntax
open Fantomas.FCS.Text.Range

[<RequireQualifiedAccess>]
module internal Ident =
    let inline create (s : string) = Ident (s, range0)

    /// Fantomas bug, perhaps? "type" is not rendered as ``type``, although the ASTs are identical
    /// apart from the ranges?
    /// Awful hack: here is a function that does this sort of thing.
    let createSanitisedParamName (s : string) =
        match s with
        | "type" -> create "type'"
        | "private" -> create "private'"
        | _ ->

        let result = StringBuilder ()

        for i = 0 to s.Length - 1 do
            if Char.IsLetter s.[i] then
                result.Append s.[i] |> ignore<StringBuilder>
            elif Char.IsNumber s.[i] then
                if result.Length > 0 then
                    result.Append s.[i] |> ignore<StringBuilder>
            elif s.[i] = '_' || s.[i] = '-' then
                result.Append '_' |> ignore<StringBuilder>
            else
                failwith $"could not convert to ident: %s{s}"

        create (result.ToString ())

    let private alnum = Regex @"^[a-zA-Z][a-zA-Z0-9]*$"

    let createSanitisedTypeName (s : string) =
        let result = StringBuilder ()
        let mutable capitalize = true

        for i = 0 to s.Length - 1 do
            if Char.IsLetter s.[i] then
                if capitalize then
                    result.Append (Char.ToUpperInvariant s.[i]) |> ignore<StringBuilder>
                    capitalize <- false
                else
                    result.Append s.[i] |> ignore<StringBuilder>
            elif Char.IsNumber s.[i] then
                if result.Length > 0 then
                    result.Append s.[i] |> ignore<StringBuilder>
            elif s.[i] = '_' then
                capitalize <- true

        if result.Length = 0 then
            failwith $"String %s{s} was not suitable as a type identifier"

        Ident (result.ToString (), range0)

    let lowerFirstLetter (x : Ident) : Ident =
        let result = StringBuilder x.idText.Length
        result.Append (Char.ToLowerInvariant x.idText.[0]) |> ignore
        result.Append x.idText.[1..] |> ignore
        create ((result : StringBuilder).ToString ())
