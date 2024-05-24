namespace WoofWare.Myriad.Plugins.Test

open System
open System.Numerics
open System.Text.Json.Nodes
open ConsumePlugin
open NUnit.Framework
open FsUnitTyped

[<TestFixture>]
module TestExtensionMethod =

    [<Test>]
    let ``Parse via extension method`` () =
        let json =
            """{
    "alpha": "hello!",
    "bravo": "https://example.com",
    "charlie": 0.3341,
    "delta": 110033.4,
    "echo": -0.000993,
    "foxtrot": -999999999999,
    "golf": -123456789101112,
    "hotel": 18446744073709551615,
    "india": 99884,
    "juliette": 12223334,
    "kilo": -2147483642,
    "lima": 4294967293,
    "mike": -32767,
    "november": 65533,
    "oscar": -125,
    "papa": 253,
    "quebec": 254,
    "tango": -3,
    "uniform": 1004443.300988393349583009,
    "victor": "x",
    "whiskey": 123456123456123456123456123456123456123456
}"""
            |> JsonNode.Parse

        let expected =
            {
                Alpha = "hello!"
                Bravo = Uri "https://example.com"
                Charlie = 0.3341
                Delta = 110033.4f
                Echo = -0.000993f
                Foxtrot = -999999999999.0
                Golf = -123456789101112L
                Hotel = 18446744073709551615UL
                India = 99884
                Juliette = 12223334u
                Kilo = -2147483642
                Lima = 4294967293u
                Mike = -32767s
                November = 65533us
                Oscar = -125y
                Papa = 253uy
                Quebec = 254uy
                Tango = -3y
                Uniform = 1004443.300988393349583009m
                Victor = 'x'
                Whiskey =
                    let mutable i = BigInteger 0

                    for _ = 0 to 6 do
                        i <- i * BigInteger 1000000 + BigInteger 123456

                    i
            }

        let actual = ToGetExtensionMethod.jsonParse json

        actual |> shouldEqual expected
