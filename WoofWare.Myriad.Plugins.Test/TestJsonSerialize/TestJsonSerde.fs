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
