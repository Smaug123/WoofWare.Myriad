namespace WoofWare.Myriad.Plugins

open Fantomas.FCS.Syntax

[<RequireQualifiedAccess>]
module internal SynArgInfo =
    let empty = SynArgInfo.SynArgInfo ([], false, None)
