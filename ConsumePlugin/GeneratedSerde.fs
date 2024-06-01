//------------------------------------------------------------------------------
//        This code was generated by myriad.
//        Changes to this file will be lost when the code is regenerated.
//------------------------------------------------------------------------------


namespace ConsumePlugin

open System
open System.Collections.Generic
open System.Text.Json.Serialization

/// Module containing JSON serializing extension members for the InnerTypeWithBoth type
[<AutoOpen>]
module InnerTypeWithBothJsonSerializeExtension =
    /// Extension methods for JSON parsing
    type InnerTypeWithBoth with

        /// Serialize to a JSON node
        static member toJsonNode (input : InnerTypeWithBoth) : System.Text.Json.Nodes.JsonNode =
            let node = System.Text.Json.Nodes.JsonObject ()

            do
                node.Add (("it's-a-me"), System.Text.Json.Nodes.JsonValue.Create<Guid> input.Thing)

                node.Add (
                    "map",
                    (fun field ->
                        let ret = System.Text.Json.Nodes.JsonObject ()

                        for (KeyValue (key, value)) in field do
                            ret.Add (key.ToString (), System.Text.Json.Nodes.JsonValue.Create<Uri> value)

                        ret
                    )
                        input.Map
                )

                node.Add (
                    "readOnlyDict",
                    (fun field ->
                        let ret = System.Text.Json.Nodes.JsonObject ()

                        for (KeyValue (key, value)) in field do
                            ret.Add (
                                key.ToString (),
                                (fun field ->
                                    let arr = System.Text.Json.Nodes.JsonArray ()

                                    for mem in field do
                                        arr.Add (System.Text.Json.Nodes.JsonValue.Create<char> mem)

                                    arr
                                )
                                    value
                            )

                        ret
                    )
                        input.ReadOnlyDict
                )

                node.Add (
                    "dict",
                    (fun field ->
                        let ret = System.Text.Json.Nodes.JsonObject ()

                        for (KeyValue (key, value)) in field do
                            ret.Add (key.ToString (), System.Text.Json.Nodes.JsonValue.Create<bool> value)

                        ret
                    )
                        input.Dict
                )

                node.Add (
                    "concreteDict",
                    (fun field ->
                        let ret = System.Text.Json.Nodes.JsonObject ()

                        for (KeyValue (key, value)) in field do
                            ret.Add (key.ToString (), InnerTypeWithBoth.toJsonNode value)

                        ret
                    )
                        input.ConcreteDict
                )

            node :> _
namespace ConsumePlugin

open System
open System.Collections.Generic
open System.Text.Json.Serialization

/// Module containing JSON serializing extension members for the JsonRecordTypeWithBoth type
[<AutoOpen>]
module JsonRecordTypeWithBothJsonSerializeExtension =
    /// Extension methods for JSON parsing
    type JsonRecordTypeWithBoth with

        /// Serialize to a JSON node
        static member toJsonNode (input : JsonRecordTypeWithBoth) : System.Text.Json.Nodes.JsonNode =
            let node = System.Text.Json.Nodes.JsonObject ()

            do
                node.Add ("a", System.Text.Json.Nodes.JsonValue.Create<int> input.A)
                node.Add ("b", System.Text.Json.Nodes.JsonValue.Create<string> input.B)

                node.Add (
                    "c",
                    (fun field ->
                        let arr = System.Text.Json.Nodes.JsonArray ()

                        for mem in field do
                            arr.Add (System.Text.Json.Nodes.JsonValue.Create<int> mem)

                        arr
                    )
                        input.C
                )

                node.Add ("d", InnerTypeWithBoth.toJsonNode input.D)

                node.Add (
                    "e",
                    (fun field ->
                        let arr = System.Text.Json.Nodes.JsonArray ()

                        for mem in field do
                            arr.Add (System.Text.Json.Nodes.JsonValue.Create<string> mem)

                        arr
                    )
                        input.E
                )

                node.Add (
                    "f",
                    (fun field ->
                        let arr = System.Text.Json.Nodes.JsonArray ()

                        for mem in field do
                            arr.Add (System.Text.Json.Nodes.JsonValue.Create<int> mem)

                        arr
                    )
                        input.F
                )

            node :> _
namespace ConsumePlugin

open System
open System.Collections.Generic
open System.Text.Json.Serialization

/// Module containing JSON serializing extension members for the FirstDu type
[<AutoOpen>]
module FirstDuJsonSerializeExtension =
    /// Extension methods for JSON parsing
    type FirstDu with

        /// Serialize to a JSON node
        static member toJsonNode (input : FirstDu) : System.Text.Json.Nodes.JsonNode =
            let node = System.Text.Json.Nodes.JsonObject ()

            match input with
            | FirstDu.EmptyCase -> node.Add ("type", System.Text.Json.Nodes.JsonValue.Create "emptyCase")
            | FirstDu.Case1 arg0 ->
                node.Add ("type", System.Text.Json.Nodes.JsonValue.Create "case1")
                let dataNode = System.Text.Json.Nodes.JsonObject ()
                dataNode.Add ("data", System.Text.Json.Nodes.JsonValue.Create<string> arg0)
                node.Add ("data", dataNode)
            | FirstDu.Case2 (arg0, arg1) ->
                node.Add ("type", System.Text.Json.Nodes.JsonValue.Create "case2")
                let dataNode = System.Text.Json.Nodes.JsonObject ()
                dataNode.Add ("record", JsonRecordTypeWithBoth.toJsonNode arg0)
                dataNode.Add ("i", System.Text.Json.Nodes.JsonValue.Create<int> arg1)
                node.Add ("data", dataNode)

            node :> _

namespace ConsumePlugin

/// Module containing JSON parsing extension members for the InnerTypeWithBoth type
[<AutoOpen>]
module InnerTypeWithBothJsonParseExtension =
    /// Extension methods for JSON parsing
    type InnerTypeWithBoth with

        /// Parse from a JSON node.
        static member jsonParse (node : System.Text.Json.Nodes.JsonNode) : InnerTypeWithBoth =
            let arg_4 =
                (match node.["concreteDict"] with
                 | null ->
                     raise (
                         System.Collections.Generic.KeyNotFoundException (
                             sprintf "Required key '%s' not found on JSON object" ("concreteDict")
                         )
                     )
                 | v -> v)
                    .AsObject ()
                |> Seq.map (fun kvp ->
                    let key = (kvp.Key)
                    let value = InnerTypeWithBoth.jsonParse (kvp.Value)
                    key, value
                )
                |> Seq.map System.Collections.Generic.KeyValuePair
                |> System.Collections.Generic.Dictionary

            let arg_3 =
                (match node.["dict"] with
                 | null ->
                     raise (
                         System.Collections.Generic.KeyNotFoundException (
                             sprintf "Required key '%s' not found on JSON object" ("dict")
                         )
                     )
                 | v -> v)
                    .AsObject ()
                |> Seq.map (fun kvp ->
                    let key = (kvp.Key) |> System.Uri
                    let value = (kvp.Value).AsValue().GetValue<bool> ()
                    key, value
                )
                |> dict

            let arg_2 =
                (match node.["readOnlyDict"] with
                 | null ->
                     raise (
                         System.Collections.Generic.KeyNotFoundException (
                             sprintf "Required key '%s' not found on JSON object" ("readOnlyDict")
                         )
                     )
                 | v -> v)
                    .AsObject ()
                |> Seq.map (fun kvp ->
                    let key = (kvp.Key)

                    let value =
                        (kvp.Value).AsArray ()
                        |> Seq.map (fun elt -> elt.AsValue().GetValue<System.Char> ())
                        |> List.ofSeq

                    key, value
                )
                |> readOnlyDict

            let arg_1 =
                (match node.["map"] with
                 | null ->
                     raise (
                         System.Collections.Generic.KeyNotFoundException (
                             sprintf "Required key '%s' not found on JSON object" ("map")
                         )
                     )
                 | v -> v)
                    .AsObject ()
                |> Seq.map (fun kvp ->
                    let key = (kvp.Key)
                    let value = (kvp.Value).AsValue().GetValue<string> () |> System.Uri
                    key, value
                )
                |> Map.ofSeq

            let arg_0 =
                (match node.[("it's-a-me")] with
                 | null ->
                     raise (
                         System.Collections.Generic.KeyNotFoundException (
                             sprintf "Required key '%s' not found on JSON object" (("it's-a-me"))
                         )
                     )
                 | v -> v)
                    .AsValue()
                    .GetValue<string> ()
                |> System.Guid.Parse

            {
                Thing = arg_0
                Map = arg_1
                ReadOnlyDict = arg_2
                Dict = arg_3
                ConcreteDict = arg_4
            }
namespace ConsumePlugin

/// Module containing JSON parsing extension members for the JsonRecordTypeWithBoth type
[<AutoOpen>]
module JsonRecordTypeWithBothJsonParseExtension =
    /// Extension methods for JSON parsing
    type JsonRecordTypeWithBoth with

        /// Parse from a JSON node.
        static member jsonParse (node : System.Text.Json.Nodes.JsonNode) : JsonRecordTypeWithBoth =
            let arg_5 =
                (match node.["f"] with
                 | null ->
                     raise (
                         System.Collections.Generic.KeyNotFoundException (
                             sprintf "Required key '%s' not found on JSON object" ("f")
                         )
                     )
                 | v -> v)
                    .AsArray ()
                |> Seq.map (fun elt -> elt.AsValue().GetValue<int> ())
                |> Array.ofSeq

            let arg_4 =
                (match node.["e"] with
                 | null ->
                     raise (
                         System.Collections.Generic.KeyNotFoundException (
                             sprintf "Required key '%s' not found on JSON object" ("e")
                         )
                     )
                 | v -> v)
                    .AsArray ()
                |> Seq.map (fun elt -> elt.AsValue().GetValue<string> ())
                |> Array.ofSeq

            let arg_3 =
                InnerTypeWithBoth.jsonParse (
                    match node.["d"] with
                    | null ->
                        raise (
                            System.Collections.Generic.KeyNotFoundException (
                                sprintf "Required key '%s' not found on JSON object" ("d")
                            )
                        )
                    | v -> v
                )

            let arg_2 =
                (match node.["c"] with
                 | null ->
                     raise (
                         System.Collections.Generic.KeyNotFoundException (
                             sprintf "Required key '%s' not found on JSON object" ("c")
                         )
                     )
                 | v -> v)
                    .AsArray ()
                |> Seq.map (fun elt -> elt.AsValue().GetValue<int> ())
                |> List.ofSeq

            let arg_1 =
                (match node.["b"] with
                 | null ->
                     raise (
                         System.Collections.Generic.KeyNotFoundException (
                             sprintf "Required key '%s' not found on JSON object" ("b")
                         )
                     )
                 | v -> v)
                    .AsValue()
                    .GetValue<string> ()

            let arg_0 =
                (match node.["a"] with
                 | null ->
                     raise (
                         System.Collections.Generic.KeyNotFoundException (
                             sprintf "Required key '%s' not found on JSON object" ("a")
                         )
                     )
                 | v -> v)
                    .AsValue()
                    .GetValue<int> ()

            {
                A = arg_0
                B = arg_1
                C = arg_2
                D = arg_3
                E = arg_4
                F = arg_5
            }
namespace ConsumePlugin

/// Module containing JSON parsing extension members for the FirstDu type
[<AutoOpen>]
module FirstDuJsonParseExtension =
    /// Extension methods for JSON parsing
    type FirstDu with

        /// Parse from a JSON node.
        static member jsonParse (node : System.Text.Json.Nodes.JsonNode) : FirstDu =
            let ty =
                (match node.["type"] with
                 | null ->
                     raise (
                         System.Collections.Generic.KeyNotFoundException (
                             sprintf "Required key '%s' not found on JSON object" ("type")
                         )
                     )
                 | v -> v)
                |> (fun v -> v.GetValue<string> ())

            match ty with
            | "emptyCase" -> FirstDu.EmptyCase
            | "case1" ->
                let node =
                    (match node.["data"] with
                     | null ->
                         raise (
                             System.Collections.Generic.KeyNotFoundException (
                                 sprintf "Required key '%s' not found on JSON object" ("data")
                             )
                         )
                     | v -> v)

                FirstDu.Case1 (
                    (match node.["data"] with
                     | null ->
                         raise (
                             System.Collections.Generic.KeyNotFoundException (
                                 sprintf "Required key '%s' not found on JSON object" ("data")
                             )
                         )
                     | v -> v)
                        .AsValue()
                        .GetValue<string> ()
                )
            | "case2" ->
                let node =
                    (match node.["data"] with
                     | null ->
                         raise (
                             System.Collections.Generic.KeyNotFoundException (
                                 sprintf "Required key '%s' not found on JSON object" ("data")
                             )
                         )
                     | v -> v)

                FirstDu.Case2 (
                    JsonRecordTypeWithBoth.jsonParse (
                        match node.["record"] with
                        | null ->
                            raise (
                                System.Collections.Generic.KeyNotFoundException (
                                    sprintf "Required key '%s' not found on JSON object" ("record")
                                )
                            )
                        | v -> v
                    ),
                    (match node.["i"] with
                     | null ->
                         raise (
                             System.Collections.Generic.KeyNotFoundException (
                                 sprintf "Required key '%s' not found on JSON object" ("i")
                             )
                         )
                     | v -> v)
                        .AsValue()
                        .GetValue<int> ()
                )
            | v -> failwith ("Unrecognised 'type' field value: " + v)
