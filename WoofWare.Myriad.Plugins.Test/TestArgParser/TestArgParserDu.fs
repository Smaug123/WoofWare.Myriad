namespace WoofWare.Myriad.Plugins.Test

open NUnit.Framework
open FsUnitTyped
open ConsumePlugin

/// Discriminated-union argument schemas: exactly one case's arguments must be supplied, and the
/// parse result tells us which case it was.
[<TestFixture>]
module TestArgParserDu =

    let noEnv (_ : string) : string option = None

    [<Test>]
    let ``Supplying the first case's arguments selects it`` () =
        DuArgs.parse' noEnv [ "--foo=3" ]
        |> shouldEqual (
            FooCase
                {
                    Foo = 3
                }
        )

    [<Test>]
    let ``Supplying the second case's arguments selects it`` () =
        DuArgs.parse' noEnv [ "--bar=8" ; "--baz=9" ]
        |> shouldEqual (
            BarCase
                {
                    Bar = 8
                    Baz = 9
                }
        )

    [<Test>]
    let ``Case selection is case-insensitive like everything else`` () =
        DuArgs.parse' noEnv [ "--FOO=3" ]
        |> shouldEqual (
            FooCase
                {
                    Foo = 3
                }
        )

    [<Test>]
    let ``Arguments from two different cases are a conflict, not a parse`` () =
        let exc =
            Assert.Throws<exn> (fun () -> DuArgs.parse' noEnv [ "--foo=3" ; "--bar=8" ] |> ignore<DuArgs>)

        exc.Message
        |> shouldEqual
            """Errors during parse!
Arguments select more than one alternative: FooCase (via --foo=3), BarCase (via --bar=8)"""

    [<Test>]
    let ``A selected but incomplete case reports its own missing arguments`` () =
        let exc =
            Assert.Throws<exn> (fun () -> DuArgs.parse' noEnv [ "--bar=8" ] |> ignore<DuArgs>)

        exc.Message
        |> shouldEqual
            """Errors during parse!
Required argument '--baz' received no value"""

    [<Test>]
    let ``Supplying nothing selects nothing`` () =
        let exc = Assert.Throws<exn> (fun () -> DuArgs.parse' noEnv [] |> ignore<DuArgs>)

        exc.Message
        |> shouldEqual
            """Errors during parse!
No arguments were supplied to select one of: FooCase, BarCase"""

    [<Test>]
    let ``A union nested in a record: an argument selects its case`` () =
        WithModeArgs.parse' noEnv [ "--verbose=true" ; "--level=3" ]
        |> shouldEqual
            {
                Verbose = true
                Mode =
                    Manual
                        {
                            Level = 3
                        }
            }

    [<Test>]
    let ``A union nested in a record: the unique no-arguments case is the fallback`` () =
        WithModeArgs.parse' noEnv [ "--verbose=false" ]
        |> shouldEqual
            {
                Verbose = false
                Mode =
                    Auto
                        {
                            Quiet = None
                        }
            }

        WithModeArgs.parse' noEnv [ "--verbose=false" ; "--quiet=true" ]
        |> shouldEqual
            {
                Verbose = false
                Mode =
                    Auto
                        {
                            Quiet = Some true
                        }
            }

    [<Test>]
    let ``A union nested in a record: cross-case arguments are a conflict`` () =
        let exc =
            Assert.Throws<exn> (fun () ->
                WithModeArgs.parse' noEnv [ "--verbose=false" ; "--quiet=true" ; "--level=1" ]
                |> ignore<WithModeArgs>
            )

        exc.Message
        |> shouldEqual
            """Errors during parse!
Arguments select more than one alternative: Auto (via --quiet=true), Manual (via --level=1)"""
