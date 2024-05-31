namespace WoofWare.Myriad.Plugins

open Fantomas.FCS.Syntax

[<RequireQualifiedAccess>]
module internal SynArgPats =
    let create (caseNames : Ident list) : SynArgPats =
        if caseNames.IsEmpty then
            SynArgPats.Pats []
        else

        caseNames
        |> List.map (fun i -> SynPat.named i.idText)
        |> SynPat.tuple
        |> List.singleton
        |> SynArgPats.Pats
