namespace WoofWare.Myriad.Plugins

open Fantomas.FCS.Syntax

[<RequireQualifiedAccess>]
module internal SynIdent =
    let inline createI (i : Ident) : SynIdent = SynIdent.SynIdent (i, None)

    let inline createS (i : string) : SynIdent =
        SynIdent.SynIdent (Ident.create i, None)
