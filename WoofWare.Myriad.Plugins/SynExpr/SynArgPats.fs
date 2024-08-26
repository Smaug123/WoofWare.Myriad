namespace WoofWare.Myriad.Plugins

open Fantomas.FCS.Syntax
open Fantomas.FCS.Text.Range

[<RequireQualifiedAccess>]
module internal SynArgPats =
    let createNamed (caseNames : string list) : SynArgPats =
        match caseNames.Length with
        | 0 -> SynArgPats.Pats []
        | 1 ->
            SynPat.Named (SynIdent.SynIdent (Ident.create caseNames.[0], None), false, None, range0)
            |> List.singleton
            |> SynArgPats.Pats
        | len ->
            caseNames
            |> List.map (fun name -> SynPat.Named (SynIdent.SynIdent (Ident.create name, None), false, None, range0))
            |> fun t -> SynPat.Tuple (false, t, List.replicate (len - 1) range0, range0)
            |> fun t -> SynPat.Paren (t, range0)
            |> List.singleton
            |> SynArgPats.Pats

    let create (pats : SynPat list) : SynArgPats =
        match pats.Length with
        | 0 -> SynArgPats.Pats []
        | 1 -> [ pats.[0] ] |> SynArgPats.Pats
        | len ->
            SynPat.Paren (SynPat.Tuple (false, pats, List.replicate (len - 1) range0, range0), range0)
            |> List.singleton
            |> SynArgPats.Pats
