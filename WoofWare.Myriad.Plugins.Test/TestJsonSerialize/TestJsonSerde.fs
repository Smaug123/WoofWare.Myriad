namespace WoofWare.Myriad.Plugins.Test

open System.Text.Json.Nodes
open NUnit.Framework
open FsCheck
open FsUnitTyped
open ConsumePlugin

[<TestFixture>]
module TestJsonSerde =

    let innerGen : Gen<InnerTypeWithBoth> =
        gen {
            let! s = Arb.generate<NonNull<string>>

            return
                {
                    Thing = s.Get
                }
        }

    let outerGen : Gen<JsonRecordTypeWithBoth> =
        gen {
            let! a = Arb.generate<int>
            let! b = Arb.generate<NonNull<string>>
            let! c = Gen.listOf Arb.generate<int>
            let! d = innerGen
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
            |> JsonRecordTypeWithBoth.jsonParse
            |> shouldEqual o

            o
            |> JsonRecordTypeWithBoth.toJsonNode
            |> fun s -> s.ToJsonString ()
            |> JsonNode.Parse
            |> JsonRecordTypeWithBoth.jsonParse
            |> shouldEqual o

            true

        property |> Prop.forAll (Arb.fromGen outerGen) |> Check.QuickThrowOnFailure
