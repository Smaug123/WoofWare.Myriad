namespace MyriadPlugin.Test

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
    "a": 3, "b": "hello", "hi": [6, 1], "d": {"something": "oh hi"}
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
            }

        let actual = s |> JsonNode.Parse |> JsonRecordType.jsonParse
        actual |> shouldEqual expected
