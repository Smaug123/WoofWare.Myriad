namespace WoofWare.Myriad.Plugins

open Fantomas.FCS.Syntax
open Fantomas.FCS.Text.Range

[<RequireQualifiedAccess>]
module internal SynArgPats =
    let create (caseNames : Ident list) : SynArgPats =
        if caseNames.IsEmpty then
            SynArgPats.Pats []
        else

        caseNames
        |> List.map (fun ident -> SynPat.Named (SynIdent.SynIdent (ident, None), false, None, range0))
        |> fun ps -> SynPat.Tuple (false, ps, List.replicate (ps.Length - 1) range0, range0)
        |> fun p -> SynPat.Paren (p, range0)
        |> List.singleton
        |> SynArgPats.Pats
