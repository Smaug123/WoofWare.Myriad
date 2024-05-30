namespace WoofWare.Myriad.Plugins

open Fantomas.FCS.Syntax

[<RequireQualifiedAccess>]
module internal SynType =
    let rec stripOptionalParen (ty : SynType) : SynType =
        match ty with
        | SynType.Paren (ty, _) -> stripOptionalParen ty
        | ty -> ty
