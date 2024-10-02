namespace WoofWare.Myriad.Plugins

open Fantomas.FCS.Syntax

[<RequireQualifiedAccess>]
module internal SynValInfo =
    let empty = SynValInfo.SynValInfo ([], SynArgInfo.empty)
