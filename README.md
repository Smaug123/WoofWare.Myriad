# fsharp-arguments

Some helpers in [Myriad](https://github.com/MoiraeSoftware/myriad/) which might be useful for someone writing an argument parser.

## `RemoveOptions`

Takes a record like this:

```fsharp
type Foo =
    {
        A : int option
        B : string
        C : float list
    }
```

and stamps out a record like this:

```fsharp
[<RequireQualifiedAccess>]
module Foo =
    type Short =
        {
            A : int
            B : string
            C : float list
        }
```

(This is a proof of concept. It would be better to somehow disambiguate the module name.)

## `JsonParse`

Takes records like this:

```fsharp
[<MyriadPlugin.JsonParse>]
type InnerType =
    {
        [<JsonPropertyName "something">]
        Thing : string
    }

/// My whatnot
[<MyriadPlugin.JsonParse>]
type JsonRecordType =
    {
        /// A thing!
        A : int
        /// Another thing!
        B : string
        [<System.Text.Json.Serialization.JsonPropertyName "hi">]
        C : int list
        D : InnerType
    }

```

and stamps out parsing methods like this:

```fsharp

/// Module containing JSON parsing methods for the InnerType type
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module InnerType =
    /// Parse from a JSON node.
    let jsonParse (node: System.Text.Json.Nodes.JsonNode) : InnerType =
        let Thing = node.["something"].AsValue().GetValue<string>()
        { Thing = Thing }
namespace UsePlugin

/// Module containing JSON parsing methods for the JsonRecordType type
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module JsonRecordType =
    /// Parse from a JSON node.
    let jsonParse (node: System.Text.Json.Nodes.JsonNode) : JsonRecordType =
        let D = InnerType.jsonParse node.["d"]

        let C =
            node.["hi"].AsArray() |> Seq.map (fun elt -> elt.GetValue<int>()) |> List.ofSeq

        let B = node.["b"].AsValue().GetValue<string>()
        let A = node.["a"].AsValue().GetValue<int>()
        { A = A; B = B; C = C; D = D }
```
