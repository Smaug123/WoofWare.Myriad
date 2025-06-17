namespace WoofWare.Myriad.Plugins.Test

open WoofWare.Myriad.Plugins.OpenApi3
open System.Text.Json.Nodes
open NUnit.Framework
open FsUnitTyped

[<TestFixture>]
module TestOpenApi3Parse =
    type Dummy = class end

    [<Test>]
    let foo () =
        let resource =
            Assembly.getEmbeddedResource typeof<Dummy>.Assembly "api-with-examples.json"
            |> JsonNode.Parse

        let actual = OpenApiSpec.Parse resource
        ()
