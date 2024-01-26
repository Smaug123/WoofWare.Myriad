namespace WoofWare.Myriad.Plugins.Test

open System
open System.Collections.Generic
open System.Text.Json.Nodes
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
            let! s = Arb.generate<NonNull<string>>
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
                    Thing = s.Get
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
            let! f = Gen.arrayOf Arb.generate<int>

            return
                {
                    A = a
                    B = b.Get
                    C = c
                    D = d
                    E = e |> Array.map _.Get
                    F = f
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
