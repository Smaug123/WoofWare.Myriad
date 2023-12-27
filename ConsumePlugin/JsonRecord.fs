namespace ConsumePlugin

open System.Text.Json.Serialization

module Literals =
    [<Literal>]
    let something = "something"

[<MyriadPlugin.JsonParse>]
type InnerType =
    {
        [<JsonPropertyName(Literals.something)>]
        Thing : string
    }

/// My whatnot
[<MyriadPlugin.JsonParse>]
type JsonRecordType =
    {
        /// A thing!
        A : int
        /// Another thing!
        [<JsonPropertyName "another-thing">]
        B : string
        [<System.Text.Json.Serialization.JsonPropertyName "hi">]
        C : int list
        D : InnerType
        E : string array
        F : int[]
    }
