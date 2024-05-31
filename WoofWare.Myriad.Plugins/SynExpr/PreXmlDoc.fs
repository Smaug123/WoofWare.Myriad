namespace WoofWare.Myriad.Plugins

open Fantomas.FCS.Xml
open Fantomas.FCS.Text.Range

[<RequireQualifiedAccess>]
module internal PreXmlDoc =
    let create (s : string) : PreXmlDoc =
        PreXmlDoc.Create ([| " " + s |], range0)
