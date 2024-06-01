namespace WoofWare.Myriad.Plugins

open Fantomas.FCS.Syntax

[<RequireQualifiedAccess>]
module internal SynArgPats =
    let create (caseNames : Ident list) : SynArgPats =
        match caseNames.Length with
        | 0 -> SynArgPats.Pats []
        | 1 -> [ SynPat.named caseNames.[0].idText ] |> SynArgPats.Pats
        | _ ->
            caseNames
            |> List.map (fun i -> SynPat.named i.idText)
            |> SynPat.tuple
            |> List.singleton
            |> SynArgPats.Pats
