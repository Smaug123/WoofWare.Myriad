namespace WoofWare.Myriad.Plugins.Test

open System.Text.Json.Nodes
open ConsumePlugin
open NUnit.Framework
open FsUnitTyped

[<TestFixture>]
module TestJsonParse =
    [<Test>]
    let ``Single example`` () =
        let s =
            """
{
    "a": 3, "another-thing": "hello", "hi": [6, 1], "d": {"something": "oh hi"},
    "e": ["something", "else"], "f": []
}
"""

        let expected =
            {
                A = 3
                B = "hello"
                C = [ 6 ; 1 ]
                D =
                    {
                        Thing = "oh hi"
                    }
                E = [| "something" ; "else" |]
                F = [||]
            }

        let actual = s |> JsonNode.Parse |> JsonRecordType.jsonParse
        actual |> shouldEqual expected

    [<Test>]
    let ``Inner example`` () =
        let s =
            """{
    "something": "oh hi"
}"""

        let expected =
            {
                Thing = "oh hi"
            }

        let actual = s |> JsonNode.Parse |> InnerType.jsonParse
        actual |> shouldEqual expected
