namespace WoofWare.Myriad.Plugins.Test

open NUnit.Framework
open FsUnitTyped
open ConsumePlugin

[<TestFixture>]
module TestArgParser =
    [<Test>]
    let ``Default values`` () =
        let getEnvVar (s : string) =
            s |> shouldEqual "CONSUMEPLUGIN_THINGS"
            "hi!"

        let args =
            [ "--foo" ; "3" ; "--bar=some string" ; "--baz" ; "--some-file=/path/to/file" ]

        let result = Args.parse' getEnvVar args

        result.OptionalThing |> shouldEqual (Choice2Of2 false)
        result.OptionalThingWithNoDefault |> shouldEqual None
        result.AnotherOptionalThing |> shouldEqual (Choice1Of2 3)
        result.YetAnotherOptionalThing |> shouldEqual (Choice2Of2 "hi!")
