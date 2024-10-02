namespace WoofWare.Myriad.Plugins

open Fantomas.FCS.Syntax
open Fantomas.FCS.Text.Range

[<RequireQualifiedAccess>]
module internal SynSimplePats =

    let create (pats : SynSimplePat list) : SynSimplePats =
        match pats with
        | [] -> SynSimplePats.SimplePats ([], [], range0)
        | pats -> SynSimplePats.SimplePats (pats, List.replicate (pats.Length - 1) range0, range0)
