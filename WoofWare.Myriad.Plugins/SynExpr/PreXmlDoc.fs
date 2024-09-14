namespace WoofWare.Myriad.Plugins

open Fantomas.FCS.Xml
open Fantomas.FCS.Text.Range

[<RequireQualifiedAccess>]
module internal PreXmlDoc =
    let create (s : string) : PreXmlDoc =
        let s = s.Split "\n"

        for i = 0 to s.Length - 1 do
            s.[i] <- " " + s.[i]

        PreXmlDoc.Create (s, range0)

    let create' (s : string seq) : PreXmlDoc =
        PreXmlDoc.Create (Array.ofSeq s, range0)
