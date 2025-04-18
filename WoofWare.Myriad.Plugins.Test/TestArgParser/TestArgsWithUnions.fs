namespace WoofWare.Myriad.Plugins.Test

open FsUnitTyped
open NUnit.Framework
open ConsumePlugin.ArgsWithUnions

[<TestFixture>]
module TestArgsWithUnions =

    let argsWithUnionsCases =
        [
            ["--token" ; "hello" ; "--foo" ; "3" ; "--bar=hi" ; "--baz"], { Auth = AuthOptions.Token { Token = "hello" } ; Basics = { Foo = 3 ; Bar = "hi" ; Baz = true ; Rest = [] } }
        ]
        |> List.map TestCaseData

    [<TestCaseSource (nameof argsWithUnionsCases)>]
    let ``foo`` (args : string list, expected : DoTheThing) : unit =
        args
        |> DoTheThing.parse' (fun _ -> failwith "didn't expect env var")
        |> shouldEqual expected

