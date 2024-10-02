namespace WoofWare.Myriad.Plugins

open Fantomas.FCS.Syntax
open Fantomas.FCS.Text.Range

[<RequireQualifiedAccess>]
module internal SynSimplePat =

    let createId (id : Ident) : SynSimplePat =
        SynSimplePat.Id (id, None, false, false, false, range0)
