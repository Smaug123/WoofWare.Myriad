namespace WoofWare.Myriad.Plugins

open Fantomas.FCS.SyntaxTrivia

[<RequireQualifiedAccess>]
module internal SynExprLetOrUseTrivia =
    let empty : SynExprLetOrUseTrivia =
        {
            InKeyword = None
        }
