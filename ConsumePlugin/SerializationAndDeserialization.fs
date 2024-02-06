namespace ConsumePlugin

open System
open System.Collections.Generic
open System.Text.Json.Serialization

[<WoofWare.Myriad.Plugins.JsonParse true>]
[<WoofWare.Myriad.Plugins.JsonSerialize true>]
type InnerTypeWithBoth =
    {
        [<JsonPropertyName("it's-a-me")>]
        Thing : Guid
        Map : Map<string, Uri>
        ReadOnlyDict : IReadOnlyDictionary<string, char list>
        Dict : IDictionary<Uri, bool>
        ConcreteDict : Dictionary<string, InnerTypeWithBoth>
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
