namespace WoofWare.Myriad.Plugins

open Fantomas.FCS.Syntax

[<RequireQualifiedAccess>]
module internal SynArgPats =
    let createNamed (caseNames : string list) : SynArgPats =
        match caseNames.Length with
        | 0 -> SynArgPats.Pats []
        | 1 -> [ SynPat.named caseNames.[0] ] |> SynArgPats.Pats
        | _ ->
            caseNames
            |> List.map SynPat.named
            |> SynPat.tuple
            |> List.singleton
            |> SynArgPats.Pats

    let create (pats : SynPat list) : SynArgPats =
        match pats.Length with
        | 0 -> SynArgPats.Pats []
        | 1 -> [ pats.[0] ] |> SynArgPats.Pats
        | _ -> pats |> SynPat.tuple |> List.singleton |> SynArgPats.Pats
