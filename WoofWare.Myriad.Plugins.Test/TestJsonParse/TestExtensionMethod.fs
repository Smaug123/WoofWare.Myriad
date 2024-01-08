namespace WoofWare.Myriad.Plugins.Test

open System
open System.Text.Json.Nodes
open ConsumePlugin
open NUnit.Framework
open FsUnitTyped

[<TestFixture>]
module TestExtensionMethod =

    [<Test>]
    let ``Parse via extension method`` () =
        let json =
            """{"tinker": "job", "tailor": 3, "soldier": "https://example.com", "sailor": 3.1}"""
            |> JsonNode.Parse

        let expected =
            {
                Tinker = "job"
                Tailor = 3
                Soldier = Uri "https://example.com"
                Sailor = 3.1
            }

        ToGetExtensionMethod.jsonParse json |> shouldEqual expected
