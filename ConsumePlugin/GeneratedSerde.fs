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
                node.Add (("it's-a-me"), (input.Thing |> System.Text.Json.Nodes.JsonValue.Create<Guid>))

                node.Add (
                    "map",
                    (input.Map
                     |> (fun field ->
                         let ret = System.Text.Json.Nodes.JsonObject ()

                         for (KeyValue (key, value)) in field do
                             ret.Add (key.ToString (), System.Text.Json.Nodes.JsonValue.Create<Uri> value)

                         ret
                     ))
                )

                node.Add (
                    "readOnlyDict",
                    (input.ReadOnlyDict
                     |> (fun field ->
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
                     ))
                )

                node.Add (
                    "dict",
                    (input.Dict
                     |> (fun field ->
                         let ret = System.Text.Json.Nodes.JsonObject ()

                         for (KeyValue (key, value)) in field do
                             ret.Add (key.ToString (), System.Text.Json.Nodes.JsonValue.Create<bool> value)

                         ret
                     ))
                )

                node.Add (
                    "concreteDict",
                    (input.ConcreteDict
                     |> (fun field ->
                         let ret = System.Text.Json.Nodes.JsonObject ()

                         for (KeyValue (key, value)) in field do
                             ret.Add (key.ToString (), InnerTypeWithBoth.toJsonNode value)

                         ret
                     ))
                )

            node :> _
namespace ConsumePlugin

open System
open System.Collections.Generic
open System.Text.Json.Serialization

/// Module containing JSON serializing extension members for the SomeEnum type
[<AutoOpen>]
module SomeEnumJsonSerializeExtension =
    /// Extension methods for JSON parsing
    type SomeEnum with

        /// Serialize to a JSON node
        static member toJsonNode (input : SomeEnum) : System.Text.Json.Nodes.JsonNode =
            match input with
            | SomeEnum.Blah -> System.Text.Json.Nodes.JsonValue.Create 1
            | SomeEnum.Thing -> System.Text.Json.Nodes.JsonValue.Create 0
            | v -> failwith (sprintf "Unrecognised value for enum: %O" v)
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
                node.Add ("a", (input.A |> System.Text.Json.Nodes.JsonValue.Create<int>))
                node.Add ("b", (input.B |> System.Text.Json.Nodes.JsonValue.Create<string>))

                node.Add (
                    "c",
                    (input.C
                     |> (fun field ->
                         let arr = System.Text.Json.Nodes.JsonArray ()

                         for mem in field do
                             arr.Add (System.Text.Json.Nodes.JsonValue.Create<int> mem)

                         arr
                     ))
                )

                node.Add ("d", (input.D |> InnerTypeWithBoth.toJsonNode))

                node.Add (
                    "e",
                    (input.E
                     |> (fun field ->
                         let arr = System.Text.Json.Nodes.JsonArray ()

                         for mem in field do
                             arr.Add (System.Text.Json.Nodes.JsonValue.Create<string> mem)

                         arr
                     ))
                )

                node.Add (
                    "arr",
                    (input.Arr
                     |> (fun field ->
                         let arr = System.Text.Json.Nodes.JsonArray ()

                         for mem in field do
                             arr.Add (System.Text.Json.Nodes.JsonValue.Create<int> mem)

                         arr
                     ))
                )

                node.Add ("byte", (input.Byte |> System.Text.Json.Nodes.JsonValue.Create<byte<measure>>))
                node.Add ("sbyte", (input.Sbyte |> System.Text.Json.Nodes.JsonValue.Create<sbyte<measure>>))
                node.Add ("i", (input.I |> System.Text.Json.Nodes.JsonValue.Create<int<measure>>))
                node.Add ("i32", (input.I32 |> System.Text.Json.Nodes.JsonValue.Create<int32<measure>>))
                node.Add ("i64", (input.I64 |> System.Text.Json.Nodes.JsonValue.Create<int64<measure>>))
                node.Add ("u", (input.U |> System.Text.Json.Nodes.JsonValue.Create<uint<measure>>))
                node.Add ("u32", (input.U32 |> System.Text.Json.Nodes.JsonValue.Create<uint32<measure>>))
                node.Add ("u64", (input.U64 |> System.Text.Json.Nodes.JsonValue.Create<uint64<measure>>))
                node.Add ("f", (input.F |> System.Text.Json.Nodes.JsonValue.Create<float<measure>>))
                node.Add ("f32", (input.F32 |> System.Text.Json.Nodes.JsonValue.Create<float32<measure>>))
                node.Add ("single", (input.Single |> System.Text.Json.Nodes.JsonValue.Create<single<measure>>))

                node.Add (
                    "intMeasureOption",
                    (input.IntMeasureOption
                     |> (fun field ->
                         match field with
                         | None -> null :> System.Text.Json.Nodes.JsonNode
                         | Some field ->
                             (System.Text.Json.Nodes.JsonValue.Create<int<measure>> field)
                             :> System.Text.Json.Nodes.JsonNode
                     ))
                )

                node.Add (
                    "intMeasureNullable",
                    (input.IntMeasureNullable
                     |> (fun field ->
                         if field.HasValue then
                             System.Text.Json.Nodes.JsonValue.Create<int<measure>> field.Value
                             :> System.Text.Json.Nodes.JsonNode
                         else
                             null :> System.Text.Json.Nodes.JsonNode
                     ))
                )

                node.Add ("enum", (input.Enum |> SomeEnum.toJsonNode))

                node.Add (
                    "timestamp",
                    (input.Timestamp
                     |> (fun field -> field.ToString "o" |> System.Text.Json.Nodes.JsonValue.Create<string>))
                )

                node.Add ("unit", (input.Unit |> (fun value -> System.Text.Json.Nodes.JsonObject ())))

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

open System
open System.Collections.Generic
open System.Text.Json.Serialization

/// Module containing JSON serializing extension members for the HeaderAndValue type
[<AutoOpen>]
module HeaderAndValueJsonSerializeExtension =
    /// Extension methods for JSON parsing
    type HeaderAndValue with

        /// Serialize to a JSON node
        static member toJsonNode (input : HeaderAndValue) : System.Text.Json.Nodes.JsonNode =
            let node = System.Text.Json.Nodes.JsonObject ()

            do
                node.Add ("header", (input.Header |> System.Text.Json.Nodes.JsonValue.Create<string>))
                node.Add ("value", (input.Value |> System.Text.Json.Nodes.JsonValue.Create<string>))

            node :> _
namespace ConsumePlugin

open System
open System.Collections.Generic
open System.Text.Json.Serialization

/// Module containing JSON serializing extension members for the Foo type
[<AutoOpen>]
module FooJsonSerializeExtension =
    /// Extension methods for JSON parsing
    type Foo with

        /// Serialize to a JSON node
        static member toJsonNode (input : Foo) : System.Text.Json.Nodes.JsonNode =
            let node = System.Text.Json.Nodes.JsonObject ()

            do
                node.Add (
                    "message",
                    (input.Message
                     |> (fun field ->
                         match field with
                         | None -> null :> System.Text.Json.Nodes.JsonNode
                         | Some field -> HeaderAndValue.toJsonNode field
                     ))
                )

            node :> _
namespace ConsumePlugin

open System
open System.Collections.Generic
open System.Text.Json.Serialization

/// Module containing JSON serializing extension members for the CollectRemaining type
[<AutoOpen>]
module CollectRemainingJsonSerializeExtension =
    /// Extension methods for JSON parsing
    type CollectRemaining with

        /// Serialize to a JSON node
        static member toJsonNode (input : CollectRemaining) : System.Text.Json.Nodes.JsonNode =
            let node = System.Text.Json.Nodes.JsonObject ()

            do
                node.Add (
                    "message",
                    (input.Message
                     |> (fun field ->
                         match field with
                         | None -> null :> System.Text.Json.Nodes.JsonNode
                         | Some field -> HeaderAndValue.toJsonNode field
                     ))
                )

                for KeyValue (key, value) in input.Rest do
                    node.Add (key, id value)

            node :> _
namespace ConsumePlugin

open System
open System.Collections.Generic
open System.Text.Json.Serialization

/// Module containing JSON serializing extension members for the OuterCollectRemaining type
[<AutoOpen>]
module OuterCollectRemainingJsonSerializeExtension =
    /// Extension methods for JSON parsing
    type OuterCollectRemaining with

        /// Serialize to a JSON node
        static member toJsonNode (input : OuterCollectRemaining) : System.Text.Json.Nodes.JsonNode =
            let node = System.Text.Json.Nodes.JsonObject ()

            do
                for KeyValue (key, value) in input.Others do
                    node.Add (key, System.Text.Json.Nodes.JsonValue.Create<int> value)

                node.Add ("remaining", (input.Remaining |> CollectRemaining.toJsonNode))

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
                    let value = (kvp.Value).AsValue().GetValue<System.Boolean> ()
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

/// Module containing JSON parsing extension members for the SomeEnum type
[<AutoOpen>]
module SomeEnumJsonParseExtension =
    /// Extension methods for JSON parsing
    type SomeEnum with

        /// Parse from a JSON node.
        static member jsonParse (node : System.Text.Json.Nodes.JsonNode) : SomeEnum =
            match node.GetValueKind () with
            | System.Text.Json.JsonValueKind.Number -> node.AsValue().GetValue<int> () |> enum<SomeEnum>
            | System.Text.Json.JsonValueKind.String ->
                match node.AsValue().GetValue<string>().ToLowerInvariant () with
                | "blah" -> SomeEnum.Blah
                | "thing" -> SomeEnum.Thing
                | v -> failwith ("Unrecognised value for enum: %i" + v)
            | _ -> failwith ("Unrecognised kind for enum of type: " + "SomeEnum")
namespace ConsumePlugin

/// Module containing JSON parsing extension members for the JsonRecordTypeWithBoth type
[<AutoOpen>]
module JsonRecordTypeWithBothJsonParseExtension =
    /// Extension methods for JSON parsing
    type JsonRecordTypeWithBoth with

        /// Parse from a JSON node.
        static member jsonParse (node : System.Text.Json.Nodes.JsonNode) : JsonRecordTypeWithBoth =
            let arg_21 = ()

            let arg_20 =
                (match node.["timestamp"] with
                 | null ->
                     raise (
                         System.Collections.Generic.KeyNotFoundException (
                             sprintf "Required key '%s' not found on JSON object" ("timestamp")
                         )
                     )
                 | v -> v)
                    .AsValue()
                    .GetValue<string> ()
                |> System.DateTimeOffset.Parse

            let arg_19 =
                SomeEnum.jsonParse (
                    match node.["enum"] with
                    | null ->
                        raise (
                            System.Collections.Generic.KeyNotFoundException (
                                sprintf "Required key '%s' not found on JSON object" ("enum")
                            )
                        )
                    | v -> v
                )

            let arg_18 =
                match node.["intMeasureNullable"] with
                | null -> System.Nullable ()
                | v ->
                    v.AsValue().GetValue<System.Int32> ()
                    |> LanguagePrimitives.Int32WithMeasure
                    |> System.Nullable

            let arg_17 =
                match node.["intMeasureOption"] with
                | null -> None
                | v ->
                    v.AsValue().GetValue<System.Int32> ()
                    |> LanguagePrimitives.Int32WithMeasure
                    |> Some

            let arg_16 =
                (match node.["single"] with
                 | null ->
                     raise (
                         System.Collections.Generic.KeyNotFoundException (
                             sprintf "Required key '%s' not found on JSON object" ("single")
                         )
                     )
                 | v -> v)
                    .AsValue()
                    .GetValue<System.Single> ()
                |> LanguagePrimitives.Float32WithMeasure

            let arg_15 =
                (match node.["f32"] with
                 | null ->
                     raise (
                         System.Collections.Generic.KeyNotFoundException (
                             sprintf "Required key '%s' not found on JSON object" ("f32")
                         )
                     )
                 | v -> v)
                    .AsValue()
                    .GetValue<System.Single> ()
                |> LanguagePrimitives.Float32WithMeasure

            let arg_14 =
                (match node.["f"] with
                 | null ->
                     raise (
                         System.Collections.Generic.KeyNotFoundException (
                             sprintf "Required key '%s' not found on JSON object" ("f")
                         )
                     )
                 | v -> v)
                    .AsValue()
                    .GetValue<System.Double> ()
                |> LanguagePrimitives.FloatWithMeasure

            let arg_13 =
                (match node.["u64"] with
                 | null ->
                     raise (
                         System.Collections.Generic.KeyNotFoundException (
                             sprintf "Required key '%s' not found on JSON object" ("u64")
                         )
                     )
                 | v -> v)
                    .AsValue()
                    .GetValue<System.UInt64> ()
                |> LanguagePrimitives.UInt64WithMeasure

            let arg_12 =
                (match node.["u32"] with
                 | null ->
                     raise (
                         System.Collections.Generic.KeyNotFoundException (
                             sprintf "Required key '%s' not found on JSON object" ("u32")
                         )
                     )
                 | v -> v)
                    .AsValue()
                    .GetValue<System.UInt32> ()
                |> LanguagePrimitives.UInt32WithMeasure

            let arg_11 =
                (match node.["u"] with
                 | null ->
                     raise (
                         System.Collections.Generic.KeyNotFoundException (
                             sprintf "Required key '%s' not found on JSON object" ("u")
                         )
                     )
                 | v -> v)
                    .AsValue()
                    .GetValue<System.UInt32> ()
                |> LanguagePrimitives.UInt32WithMeasure

            let arg_10 =
                (match node.["i64"] with
                 | null ->
                     raise (
                         System.Collections.Generic.KeyNotFoundException (
                             sprintf "Required key '%s' not found on JSON object" ("i64")
                         )
                     )
                 | v -> v)
                    .AsValue()
                    .GetValue<System.Int64> ()
                |> LanguagePrimitives.Int64WithMeasure

            let arg_9 =
                (match node.["i32"] with
                 | null ->
                     raise (
                         System.Collections.Generic.KeyNotFoundException (
                             sprintf "Required key '%s' not found on JSON object" ("i32")
                         )
                     )
                 | v -> v)
                    .AsValue()
                    .GetValue<System.Int32> ()
                |> LanguagePrimitives.Int32WithMeasure

            let arg_8 =
                (match node.["i"] with
                 | null ->
                     raise (
                         System.Collections.Generic.KeyNotFoundException (
                             sprintf "Required key '%s' not found on JSON object" ("i")
                         )
                     )
                 | v -> v)
                    .AsValue()
                    .GetValue<System.Int32> ()
                |> LanguagePrimitives.Int32WithMeasure

            let arg_7 =
                (match node.["sbyte"] with
                 | null ->
                     raise (
                         System.Collections.Generic.KeyNotFoundException (
                             sprintf "Required key '%s' not found on JSON object" ("sbyte")
                         )
                     )
                 | v -> v)
                    .AsValue()
                    .GetValue<System.SByte> ()
                |> LanguagePrimitives.SByteWithMeasure

            let arg_6 =
                (match node.["byte"] with
                 | null ->
                     raise (
                         System.Collections.Generic.KeyNotFoundException (
                             sprintf "Required key '%s' not found on JSON object" ("byte")
                         )
                     )
                 | v -> v)
                    .AsValue()
                    .GetValue<System.Byte> ()
                |> LanguagePrimitives.ByteWithMeasure

            let arg_5 =
                (match node.["arr"] with
                 | null ->
                     raise (
                         System.Collections.Generic.KeyNotFoundException (
                             sprintf "Required key '%s' not found on JSON object" ("arr")
                         )
                     )
                 | v -> v)
                    .AsArray ()
                |> Seq.map (fun elt -> elt.AsValue().GetValue<System.Int32> ())
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
                |> Seq.map (fun elt -> elt.AsValue().GetValue<System.String> ())
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
                |> Seq.map (fun elt -> elt.AsValue().GetValue<System.Int32> ())
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
                    .GetValue<System.String> ()

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
                    .GetValue<System.Int32> ()

            {
                A = arg_0
                B = arg_1
                C = arg_2
                D = arg_3
                E = arg_4
                Arr = arg_5
                Byte = arg_6
                Sbyte = arg_7
                I = arg_8
                I32 = arg_9
                I64 = arg_10
                U = arg_11
                U32 = arg_12
                U64 = arg_13
                F = arg_14
                F32 = arg_15
                Single = arg_16
                IntMeasureOption = arg_17
                IntMeasureNullable = arg_18
                Enum = arg_19
                Timestamp = arg_20
                Unit = arg_21
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
                        .GetValue<System.String> ()
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
                        .GetValue<System.Int32> ()
                )
            | v -> failwith ("Unrecognised 'type' field value: " + v)
namespace ConsumePlugin

/// Module containing JSON parsing extension members for the HeaderAndValue type
[<AutoOpen>]
module HeaderAndValueJsonParseExtension =
    /// Extension methods for JSON parsing
    type HeaderAndValue with

        /// Parse from a JSON node.
        static member jsonParse (node : System.Text.Json.Nodes.JsonNode) : HeaderAndValue =
            let arg_1 =
                (match node.["value"] with
                 | null ->
                     raise (
                         System.Collections.Generic.KeyNotFoundException (
                             sprintf "Required key '%s' not found on JSON object" ("value")
                         )
                     )
                 | v -> v)
                    .AsValue()
                    .GetValue<System.String> ()

            let arg_0 =
                (match node.["header"] with
                 | null ->
                     raise (
                         System.Collections.Generic.KeyNotFoundException (
                             sprintf "Required key '%s' not found on JSON object" ("header")
                         )
                     )
                 | v -> v)
                    .AsValue()
                    .GetValue<System.String> ()

            {
                Header = arg_0
                Value = arg_1
            }
namespace ConsumePlugin

/// Module containing JSON parsing extension members for the Foo type
[<AutoOpen>]
module FooJsonParseExtension =
    /// Extension methods for JSON parsing
    type Foo with

        /// Parse from a JSON node.
        static member jsonParse (node : System.Text.Json.Nodes.JsonNode) : Foo =
            let arg_0 =
                match node.["message"] with
                | null -> None
                | v -> HeaderAndValue.jsonParse v |> Some

            {
                Message = arg_0
            }
namespace ConsumePlugin

/// Module containing JSON parsing extension members for the CollectRemaining type
[<AutoOpen>]
module CollectRemainingJsonParseExtension =
    /// Extension methods for JSON parsing
    type CollectRemaining with

        /// Parse from a JSON node.
        static member jsonParse (node : System.Text.Json.Nodes.JsonNode) : CollectRemaining =
            let arg_1 =
                let result =
                    System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode> ()

                let node = node.AsObject ()

                for KeyValue (key, value) in node do
                    if key = "message" then () else result.Add (key, node.[key])

                result

            let arg_0 =
                match node.["message"] with
                | null -> None
                | v -> HeaderAndValue.jsonParse v |> Some

            {
                Message = arg_0
                Rest = arg_1
            }
namespace ConsumePlugin

/// Module containing JSON parsing extension members for the OuterCollectRemaining type
[<AutoOpen>]
module OuterCollectRemainingJsonParseExtension =
    /// Extension methods for JSON parsing
    type OuterCollectRemaining with

        /// Parse from a JSON node.
        static member jsonParse (node : System.Text.Json.Nodes.JsonNode) : OuterCollectRemaining =
            let arg_1 =
                CollectRemaining.jsonParse (
                    match node.["remaining"] with
                    | null ->
                        raise (
                            System.Collections.Generic.KeyNotFoundException (
                                sprintf "Required key '%s' not found on JSON object" ("remaining")
                            )
                        )
                    | v -> v
                )

            let arg_0 =
                let result = System.Collections.Generic.Dictionary<string, int> ()
                let node = node.AsObject ()

                for KeyValue (key, value) in node do
                    if key = "remaining" then
                        ()
                    else
                        result.Add (
                            key,
                            (match node.[key] with
                             | null ->
                                 raise (
                                     System.Collections.Generic.KeyNotFoundException (
                                         sprintf "Required key '%s' not found on JSON object" (key)
                                     )
                                 )
                             | v -> v)
                                .AsValue()
                                .GetValue<System.Int32> ()
                        )

                result

            {
                Others = arg_0
                Remaining = arg_1
            }
