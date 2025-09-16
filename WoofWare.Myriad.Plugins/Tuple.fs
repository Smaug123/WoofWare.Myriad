namespace WoofWare.Myriad.Plugins

[<RequireQualifiedAccess>]
module internal Tuple =
    let withLeft left right = left, right
    let withRight right left = left, right
