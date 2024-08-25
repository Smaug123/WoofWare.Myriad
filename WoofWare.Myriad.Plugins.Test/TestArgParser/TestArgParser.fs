namespace WoofWare.Myriad.Plugins.Test

open System.Threading
open NUnit.Framework
open FsUnitTyped
open ConsumePlugin
open FsCheck

[<TestFixture>]
module TestArgParser =

    [<TestCase true>]
    [<TestCase false>]
    let ``Positionals get parsed: they don't have to be strings`` (sep : bool) =
        let getEnvVar (_ : string) = failwith "should not call"

        let property
            (fooSep : bool)
            (barSep : bool)
            (bazSep : bool)
            (pos0 : int list)
            (pos1 : int list)
            (pos2 : int list)
            (pos3 : int list)
            (pos4 : int list)
            =
            let args =
                [
                    yield! pos0 |> List.map string<int>
                    if fooSep then
                        yield "--foo=3"
                    else
                        yield "--foo"
                        yield "3"
                    yield! pos1 |> List.map string<int>
                    if barSep then
                        yield "--bar=4"
                    else
                        yield "--bar"
                        yield "4"
                    yield! pos2 |> List.map string<int>
                    if bazSep then
                        yield "--baz=true"
                    else
                        yield "--baz"
                        yield "true"
                    yield! pos3 |> List.map string<int>
                    if sep then
                        yield "--"
                    yield! pos4 |> List.map string<int>
                ]

            BasicWithIntPositionals.parse' getEnvVar args
            |> shouldEqual
                {
                    Foo = 3
                    Bar = "4"
                    Baz = true
                    Rest = pos0 @ pos1 @ pos2 @ pos3 @ pos4
                }

        Check.QuickThrowOnFailure property

    [<Test>]
    let ``Arg-like thing appearing before double dash`` () =
        let envCalls = ref 0

        let getEnvVar (_ : string) =
            Interlocked.Increment envCalls |> ignore<int>
            ""

        let args = [ "--foo=3" ; "--non-existent" ; "--bar=4" ; "--baz=true" ]

        let exc =
            Assert.Throws<exn> (fun () -> Basic.parse' getEnvVar args |> ignore<Basic>)

        envCalls.Value |> shouldEqual 0

        exc.Message |> shouldEqual "Unable to process supplied arg --non-existent"

    [<Test>]
    let ``Args appearing after double dash are positional`` () =
        let envCalls = ref 0

        let getEnvVar (_ : string) =
            Interlocked.Increment envCalls |> ignore<int>
            ""

        let args = [ "--" ; "--foo=3" ; "--bar=4" ; "--baz=true" ]

        let exc =
            Assert.Throws<exn> (fun () -> Basic.parse' getEnvVar args |> ignore<Basic>)

        exc.Message
        |> shouldEqual
            """Errors during parse!
Required argument '--foo' was missing
Required argument '--bar' was missing
Required argument '--baz' was missing"""

        envCalls.Value |> shouldEqual 0

    [<Test>]
    let ``Default values`` () =
        let getEnvVar (s : string) =
            s |> shouldEqual "CONSUMEPLUGIN_THINGS"
            "hi!"

        let args =
            [ "--foo" ; "3" ; "--bar=some string" ; "--baz" ; "--some-file=/path/to/file" ]

        let result = LoadsOfTypes.parse' getEnvVar args

        result.OptionalThing |> shouldEqual (Choice2Of2 false)
        result.OptionalThingWithNoDefault |> shouldEqual None
        result.AnotherOptionalThing |> shouldEqual (Choice1Of2 3)
        result.YetAnotherOptionalThing |> shouldEqual (Choice2Of2 "hi!")
