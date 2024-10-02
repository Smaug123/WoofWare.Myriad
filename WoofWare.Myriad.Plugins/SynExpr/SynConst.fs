namespace WoofWare.Myriad.Plugins

open Fantomas.FCS.Syntax
open Fantomas.FCS.Text.Range

[<AutoOpen>]
module internal SynConstExt =
    type SynConst with
        static member Create (s : string) : SynConst =
            SynConst.String (s, SynStringKind.Regular, range0)
