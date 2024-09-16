namespace WoofWare.Myriad.Plugins

open System

[<AutoOpen>]
module internal Text =
    let (|StartsWith|_|) (prefix : string) (s : string) : string option =
        if s.StartsWith (prefix, StringComparison.Ordinal) then
            Some (s.Substring prefix.Length)
        else
            None
