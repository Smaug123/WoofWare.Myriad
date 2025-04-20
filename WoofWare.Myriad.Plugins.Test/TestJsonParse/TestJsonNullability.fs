namespace WoofWare.Myriad.Plugins.Test

open System.Text.Json.Nodes
open FsUnitTyped
open NUnit.Framework
open ConsumePlugin

[<TestFixture>]
module TestJsonNullability =

    [<Test>]
    let ``Can consume JsonParseNullness`` () =
        let options = JsonNodeOptions (PropertyNameCaseInsensitive = true)

        """{
    "b": null
}"""
        |> fun s -> JsonNode.Parse (s, options)
        |> ArrayOfInnerStruct.jsonParse
        |> shouldEqual
            {
                B = null
            }
