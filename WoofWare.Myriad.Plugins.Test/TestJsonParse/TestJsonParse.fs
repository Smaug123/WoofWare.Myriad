namespace WoofWare.Myriad.Plugins.Test

open System.Text.Json.Nodes
open ConsumePlugin
open NUnit.Framework
open FsUnitTyped

[<TestFixture>]
module TestJsonParse =
    let _canSeePastExtensionMethod = ToGetExtensionMethod.thisModuleWouldClash

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

    [<TestCase("thing", SomeEnum.Thing)>]
    [<TestCase("Thing", SomeEnum.Thing)>]
    [<TestCase("THING", SomeEnum.Thing)>]
    [<TestCase("blah", SomeEnum.Blah)>]
    [<TestCase("Blah", SomeEnum.Blah)>]
    [<TestCase("BLAH", SomeEnum.Blah)>]
    let ``Can deserialise enum`` (str : string, expected : SomeEnum) =
        sprintf "\"%s\"" str
        |> JsonNode.Parse
        |> SomeEnum.jsonParse
        |> shouldEqual expected
