namespace WoofWare.Myriad.Plugins

open Fantomas.FCS.Syntax
open Fantomas.FCS.Text.Range

[<RequireQualifiedAccess>]
module SynAttributes =
    let ofAttrs (attrs : SynAttribute list) : SynAttributes =
        attrs
        |> List.map (fun a ->
            {
                Attributes = [ a ]
                Range = range0
            }
        )
