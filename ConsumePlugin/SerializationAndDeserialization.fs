namespace ConsumePlugin

open System.Text.Json.Serialization

[<WoofWare.Myriad.Plugins.JsonParse true>]
[<WoofWare.Myriad.Plugins.JsonSerialize true>]
type InnerTypeWithBoth =
    {
        [<JsonPropertyName("it's-a-me")>]
        Thing : string
    }

[<WoofWare.Myriad.Plugins.JsonParse true>]
[<WoofWare.Myriad.Plugins.JsonSerialize true>]
type JsonRecordTypeWithBoth =
    {
        A : int
        B : string
        C : int list
        D : InnerTypeWithBoth
        E : string array
        F : int[]
    }
