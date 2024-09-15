namespace WoofWare.Myriad.Plugins.Test

open System
open System.Collections.Generic
open System.Text.Json.Nodes
open FsCheck.Random
open Microsoft.FSharp.Reflection
open NUnit.Framework
open FsCheck
open FsUnitTyped
open ConsumePlugin

[<TestFixture>]
module TestJsonSerde =

    let uriGen : Gen<Uri> =
        gen {
            let! suffix = Arb.generate<int>
            return Uri $"https://example.com/%i{suffix}"
        }

    let rec innerGen (count : int) : Gen<InnerTypeWithBoth> =
        gen {
            let! guid = Arb.generate<Guid>
            let! mapKeys = Gen.listOf Arb.generate<NonNull<string>>
            let mapKeys = mapKeys |> List.map _.Get |> List.distinct
            let! mapValues = Gen.listOfLength mapKeys.Length uriGen
            let map = List.zip mapKeys mapValues |> Map.ofList

            let! concreteDictKeys =
                if count > 0 then
                    Gen.listOf Arb.generate<NonNull<string>>
                else
                    Gen.constant []

            let concreteDictKeys =
                concreteDictKeys
                |> List.map _.Get
                |> List.distinct
                |> fun x -> List.take (min 3 x.Length) x

            let! concreteDictValues =
                if count > 0 then
                    Gen.listOfLength concreteDictKeys.Length (innerGen (count - 1))
                else
                    Gen.constant []

            let concreteDict =
                List.zip concreteDictKeys concreteDictValues
                |> List.map KeyValuePair
                |> Dictionary

            let! readOnlyDictKeys = Gen.listOf Arb.generate<NonNull<string>>
            let readOnlyDictKeys = readOnlyDictKeys |> List.map _.Get |> List.distinct
            let! readOnlyDictValues = Gen.listOfLength readOnlyDictKeys.Length (Gen.listOf Arb.generate<char>)
            let readOnlyDict = List.zip readOnlyDictKeys readOnlyDictValues |> readOnlyDict

            let! dictKeys = Gen.listOf uriGen
            let! dictValues = Gen.listOfLength dictKeys.Length Arb.generate<bool>
            let dict = List.zip dictKeys dictValues |> dict

            return
                {
                    Thing = guid
                    Map = map
                    ReadOnlyDict = readOnlyDict
                    Dict = dict
                    ConcreteDict = concreteDict
                }
        }

    let outerGen : Gen<JsonRecordTypeWithBoth> =
        gen {
            let! a = Arb.generate<int>
            let! b = Arb.generate<NonNull<string>>
            let! c = Gen.listOf Arb.generate<int>
            let! depth = Gen.choose (0, 2)
            let! d = innerGen depth
            let! e = Gen.arrayOf Arb.generate<NonNull<string>>
            let! arr = Gen.arrayOf Arb.generate<int>
            let! byte = Arb.generate
            let! sbyte = Arb.generate
            let! i = Arb.generate
            let! i32 = Arb.generate
            let! i64 = Arb.generate
            let! u = Arb.generate
            let! u32 = Arb.generate
            let! u64 = Arb.generate
            let! f = Arb.generate |> Gen.filter (fun s -> Double.IsFinite (s / 1.0<measure>))
            let! f32 = Arb.generate |> Gen.filter (fun s -> Single.IsFinite (s / 1.0f<measure>))
            let! single = Arb.generate |> Gen.filter (fun s -> Single.IsFinite (s / 1.0f<measure>))
            let! intMeasureOption = Arb.generate
            let! intMeasureNullable = Arb.generate
            let! someEnum = Gen.choose (0, 1)
            let! timestamp = Arb.generate

            return
                {
                    A = a
                    B = b.Get
                    C = c
                    D = d
                    E = e |> Array.map _.Get
                    Arr = arr
                    Byte = byte
                    Sbyte = sbyte
                    I = i
                    I32 = i32
                    I64 = i64
                    U = u
                    U32 = u32
                    U64 = u64
                    F = f
                    F32 = f32
                    Single = single
                    IntMeasureOption = intMeasureOption
                    IntMeasureNullable = intMeasureNullable
                    Enum = enum<SomeEnum> someEnum
                    Timestamp = timestamp
                    Unit = ()
                }
        }

    [<Test>]
    let ``It just works`` () =
        let property (o : JsonRecordTypeWithBoth) : bool =
            o
            |> JsonRecordTypeWithBoth.toJsonNode
            |> fun s -> s.ToJsonString ()
            |> JsonNode.Parse
            |> JsonRecordTypeWithBoth.jsonParse
            |> shouldEqual o

            true

        property |> Prop.forAll (Arb.fromGen outerGen) |> Check.QuickThrowOnFailure

    [<Test>]
    let ``Single example of big record`` () =
        let guid = Guid.Parse "dfe24db5-9f8d-447b-8463-4c0bcf1166d5"

        let data =
            {
                A = 3
                B = "hello!"
                C = [ 1 ; -9 ]
                D =
                    {
                        Thing = guid
                        Map = Map.ofList []
                        ReadOnlyDict = readOnlyDict []
                        Dict = dict []
                        ConcreteDict = Dictionary ()
                    }
                E = [| "I'm-a-string" |]
                Arr = [| -18883 ; 9100 |]
                Byte = 87uy<measure>
                Sbyte = 89y<measure>
                I = 199993345<measure>
                I32 = -485832<measure>
                I64 = -13458625689L<measure>
                U = 458582u<measure>
                U32 = 857362147u<measure>
                U64 = 1234567892123414596UL<measure>
                F = 8833345667.1<measure>
                F32 = 1000.98f<measure>
                Single = 0.334f<measure>
                IntMeasureOption = Some 981<measure>
                IntMeasureNullable = Nullable -883<measure>
                Enum = enum<SomeEnum> 1
                Timestamp = DateTimeOffset (2024, 07, 01, 17, 54, 00, TimeSpan.FromHours 1.0)
                Unit = ()
            }

        let expected =
            """{
    "a": 3,
    "b": "hello!",
    "c": [1, -9],
    "d": {
      "it\u0027s-a-me": "dfe24db5-9f8d-447b-8463-4c0bcf1166d5",
      "map": {},
      "readOnlyDict": {},
      "dict": {},
      "concreteDict": {}
    },
    "e": ["I\u0027m-a-string"],
    "arr": [-18883, 9100],
    "byte": 87,
    "sbyte": 89,
    "i": 199993345,
    "i32": -485832,
    "i64": -13458625689,
    "u": 458582,
    "u32": 857362147,
    "u64": 1234567892123414596,
    "f": 8833345667.1,
    "f32": 1000.98,
    "single": 0.334,
    "intMeasureOption": 981,
    "intMeasureNullable": -883,
    "enum": 1,
    "timestamp": "2024-07-01T17:54:00.0000000\u002B01:00",
    "unit": {}
}
"""
            |> fun s -> s.ToCharArray ()
            |> Array.filter (fun c -> not (Char.IsWhiteSpace c))
            |> fun s -> new String (s)

        JsonRecordTypeWithBoth.toJsonNode(data).ToJsonString () |> shouldEqual expected
        JsonRecordTypeWithBoth.jsonParse (JsonNode.Parse expected) |> shouldEqual data

    [<Test>]
    let ``Guids are treated just like strings`` () =
        let guidStr = "b1e7496e-6e79-4158-8579-a01de355d3b2"
        let guid = Guid.Parse guidStr

        let node =
            {
                Thing = guid
                Map = Map.empty
                ReadOnlyDict = readOnlyDict []
                Dict = dict []
                ConcreteDict = Dictionary ()
            }
            |> InnerTypeWithBoth.toJsonNode

        node.ToJsonString ()
        |> shouldEqual (
            sprintf """{"it\u0027s-a-me":"%s","map":{},"readOnlyDict":{},"dict":{},"concreteDict":{}}""" guidStr
        )

    type Generators =
        static member TestCase () =
            { new Arbitrary<InnerTypeWithBoth>() with
                override x.Generator = innerGen 5
            }

    let sanitiseInner (r : InnerTypeWithBoth) : InnerTypeWithBoth =
        {
            Thing = r.Thing
            Map = r.Map
            ReadOnlyDict = r.ReadOnlyDict
            Dict = r.Dict
            ConcreteDict = r.ConcreteDict
        }

    let sanitiseRec (r : JsonRecordTypeWithBoth) : JsonRecordTypeWithBoth =
        { r with
            B = if isNull r.B then "<null>" else r.B
            C =
                if Object.ReferenceEquals (r.C, (null : obj)) then
                    []
                else
                    r.C
            D = sanitiseInner r.D
            E = if isNull r.E then [||] else r.E
            Arr =
                if Object.ReferenceEquals (r.Arr, (null : obj)) then
                    [||]
                else
                    r.Arr
        }

    let duGen =
        gen {
            let! case = Gen.choose (0, 2)

            match case with
            | 0 -> return FirstDu.EmptyCase
            | 1 ->
                let! s = Arb.generate<NonNull<string>>
                return FirstDu.Case1 s.Get
            | 2 ->
                let! i = Arb.generate<int>
                let! record = outerGen
                return FirstDu.Case2 (record, i)
            | _ -> return failwith $"unexpected: %i{case}"
        }

    [<Test>]
    let ``Discriminated union works`` () =
        let property (du : FirstDu) : unit =
            du
            |> FirstDu.toJsonNode
            |> fun s -> s.ToJsonString ()
            |> JsonNode.Parse
            |> FirstDu.jsonParse
            |> shouldEqual du

        property |> Prop.forAll (Arb.fromGen duGen) |> Check.QuickThrowOnFailure

    [<Test>]
    let ``DU generator covers all cases`` () =
        let rand = Random ()
        let cases = FSharpType.GetUnionCases typeof<FirstDu>
        let counts = Array.zeroCreate<int> cases.Length

        let decompose = FSharpValue.PreComputeUnionTagReader typeof<FirstDu>

        let mutable i = 0

        while i < 10_000 && Array.exists (fun i -> i = 0) counts do
            let du = Gen.eval 10 (StdGen.StdGen (rand.Next (), rand.Next ())) duGen
            let tag = decompose du
            counts.[tag] <- counts.[tag] + 1
            i <- i + 1

        for i in counts do
            i |> shouldBeGreaterThan 0

    let dict<'a, 'b when 'a : equality> (xs : ('a * 'b) seq) : Dictionary<'a, 'b> =
        let result = Dictionary ()

        for k, v in xs do
            result.Add (k, v)

        result

    let inline makeJsonArr< ^t, ^u when ^u : (static member op_Implicit : ^t -> JsonNode) and ^u :> JsonNode>
        (arr : ^t seq)
        : JsonNode
        =
        let result = JsonArray ()

        for a in arr do
            result.Add a

        result :> JsonNode

    let normalise (d : Dictionary<'a, 'b>) : ('a * 'b) list =
        d |> Seq.map (fun (KeyValue (a, b)) -> a, b) |> Seq.toList |> List.sortBy fst

    [<Test>]
    let ``Can collect extension data`` () =
        let str =
            """{
    "message": { "header": "hi", "value": "bye" },
    "something": 3,
    "arr": ["egg", "toast"],
    "str": "whatnot"
}"""
            |> JsonNode.Parse

        let expected =
            {
                Rest =
                    [
                        "something", JsonNode.op_Implicit 3
                        "arr", makeJsonArr [| "egg" ; "toast" |]
                        "str", JsonNode.op_Implicit "whatnot"
                    ]
                    |> dict
                Message =
                    Some
                        {
                            Header = "hi"
                            Value = "bye"
                        }
            }

        let actual = CollectRemaining.jsonParse str

        actual.Message |> shouldEqual expected.Message

        normalise actual.Rest
        |> List.map (fun (k, v) -> k, v.ToJsonString ())
        |> shouldEqual (normalise expected.Rest |> List.map (fun (k, v) -> k, v.ToJsonString ()))

    [<Test>]
    let ``Can write out extension data`` () =
        let expected =
            """{"message":{"header":"hi","value":"bye"},"something":3,"arr":["egg","toast"],"str":"whatnot"}"""

        let toWrite =
            {
                Rest =
                    [
                        "something", JsonNode.op_Implicit 3
                        "arr", makeJsonArr [| "egg" ; "toast" |]
                        "str", JsonNode.op_Implicit "whatnot"
                    ]
                    |> dict
                Message =
                    Some
                        {
                            Header = "hi"
                            Value = "bye"
                        }
            }

        let actual = CollectRemaining.toJsonNode toWrite |> fun s -> s.ToJsonString ()

        actual |> shouldEqual expected

    [<Test>]
    let ``Can collect extension data, nested`` () =
        let str =
            """{
  "thing": 99,
  "baz": -123,
  "remaining": {
    "message": { "header": "hi", "value": "bye" },
    "something": 3,
    "arr": ["egg", "toast"],
    "str": "whatnot"
  }
}"""
            |> JsonNode.Parse

        let expected : OuterCollectRemaining =
            {
                Remaining =
                    {
                        Message =
                            Some
                                {
                                    Header = "hi"
                                    Value = "bye"
                                }
                        Rest =
                            [
                                "something", JsonNode.op_Implicit 3
                                "arr", makeJsonArr [| "egg" ; "toast" |]
                                "str", JsonNode.op_Implicit "whatnot"
                            ]
                            |> dict
                    }
                Others = [ "thing", 99 ; "baz", -123 ] |> dict
            }

        let actual = OuterCollectRemaining.jsonParse str

        normalise actual.Others |> shouldEqual (normalise expected.Others)

        let actual = actual.Remaining
        let expected = expected.Remaining

        actual.Message |> shouldEqual expected.Message

        normalise actual.Rest
        |> List.map (fun (k, v) -> k, v.ToJsonString ())
        |> shouldEqual (normalise expected.Rest |> List.map (fun (k, v) -> k, v.ToJsonString ()))

    [<Test>]
    let ``Can write out extension data, nested`` () =
        let expected =
            """{"thing":99,"baz":-123,"remaining":{"message":{"header":"hi","value":"bye"},"something":3,"arr":["egg","toast"],"str":"whatnot"}}"""

        let toWrite : OuterCollectRemaining =
            {
                Others = [ "thing", 99 ; "baz", -123 ] |> dict
                Remaining =
                    {
                        Rest =
                            [
                                "something", JsonNode.op_Implicit 3
                                "arr", makeJsonArr [| "egg" ; "toast" |]
                                "str", JsonNode.op_Implicit "whatnot"
                            ]
                            |> dict
                        Message =
                            Some
                                {
                                    Header = "hi"
                                    Value = "bye"
                                }
                    }
            }

        let actual = OuterCollectRemaining.toJsonNode toWrite |> fun s -> s.ToJsonString ()

        actual |> shouldEqual expected
