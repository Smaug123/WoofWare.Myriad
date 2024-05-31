namespace WoofWare.Myriad.Plugins

open Fantomas.FCS.Syntax
open Fantomas.FCS.Text.Range

[<RequireQualifiedAccess>]
module internal SynPat =

    let annotateType (ty : SynType) (pat : SynPat) =
        SynPat.Paren (SynPat.Typed (pat, ty, range0), range0)

    let named (s : string) : SynPat =
        SynPat.Named (SynIdent.SynIdent (Ident (s, range0), None), false, None, range0)
