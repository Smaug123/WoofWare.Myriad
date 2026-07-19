namespace WoofWare.Myriad.Plugins.Test

open NUnit.Framework
open FsUnitTyped
open FsCheck
open ConsumePlugin

/// Positional args inside the cases of a discriminated-union argument schema: each alternative
/// converts the positional stream at its own field's type, and case selection is purely
/// structural — it happens before any conversion, so a token's parseability never chooses a
/// case.
[<TestFixture>]
module TestArgParserDuCasePositional =

    let noEnv (_ : string) : string option = None

    [<Test>]
    let ``The named discriminator selects the case, whose sink converts the stream`` () =
        FooBarMode.parse' noEnv [ "--foo=1" ; "2" ; "3" ]
        |> shouldEqual (
            FooMode
                {
                    Foo = 1
                    Rest = [ 2 ; 3 ]
                }
        )

        FooBarMode.parse' noEnv [ "--bar=1" ; "x" ; "y" ]
        |> shouldEqual (
            BarMode
                {
                    Bar = 1
                    Rest = [ "x" ; "y" ]
                }
        )

    [<Test>]
    let ``Parseability at another case's element type does not influence selection`` () =
        // "2" and "3" would parse as int, but --bar selected BarMode, so they are strings.
        FooBarMode.parse' noEnv [ "--bar=1" ; "2" ; "--" ; "3" ]
        |> shouldEqual (
            BarMode
                {
                    Bar = 1
                    Rest = [ "2" ; "3" ]
                }
        )

    [<Test>]
    let ``Positional tokens alone select nothing, and conversions are not tried`` () =
        // "x" is not an int, but no int conversion is attempted: the only error is the
        // missing selection.
        let exc =
            Assert.Throws<exn> (fun () -> FooBarMode.parse' noEnv [ "x" ; "y" ] |> ignore<FooBarMode>)

        exc.Message
        |> shouldEqual
            """Errors during parse!
No arguments were supplied to select one of: FooMode, BarMode"""

    [<Test>]
    let ``A conversion failure in the selected case's sink does not fall back to the other case`` () =
        // "bad" is not an int; the parse fails with a conversion error rather than quietly
        // becoming BarMode (whose sink would have accepted any string).
        let exc =
            Assert.Throws<exn> (fun () -> FooBarMode.parse' noEnv [ "--foo=1" ; "bad" ] |> ignore<FooBarMode>)

        exc.Message.Contains "bad" |> shouldEqual true
        exc.Message.Contains "BarMode" |> shouldEqual false

    [<Test>]
    let ``The shared keyed form routes to whichever case the named arguments select`` () =
        FooBarMode.parse' noEnv [ "--foo=1" ; "--rest=2" ]
        |> shouldEqual (
            FooMode
                {
                    Foo = 1
                    Rest = [ 2 ]
                }
        )

        // Case-insensitively, and wherever it appears relative to the discriminator.
        FooBarMode.parse' noEnv [ "--REST=2" ; "--bar=3" ]
        |> shouldEqual (
            BarMode
                {
                    Bar = 3
                    Rest = [ "2" ]
                }
        )

    [<Test>]
    let ``The shared keyed form alone selects nothing`` () =
        let exc =
            Assert.Throws<exn> (fun () -> FooBarMode.parse' noEnv [ "--rest=2" ] |> ignore<FooBarMode>)

        exc.Message
        |> shouldEqual
            """Errors during parse!
No arguments were supplied to select one of: FooMode, BarMode"""

    [<Test>]
    let ``Cross-case named arguments are a conflict, and conversions are not tried`` () =
        let exc =
            Assert.Throws<exn> (fun () -> FooBarMode.parse' noEnv [ "--foo=1" ; "--bar=2" ; "x" ] |> ignore<FooBarMode>)

        exc.Message
        |> shouldEqual
            """Errors during parse!
Arguments select more than one alternative: FooMode (via --foo=1), BarMode (via --bar=2)"""

    [<Test>]
    let ``A sink in only one case: bare tokens structurally select that case`` () =
        // Only Pull can consume a positional token, so the token itself picks Pull; the
        // missing required argument is then reported in the ordinary vocabulary.
        let exc =
            Assert.Throws<exn> (fun () -> GitLike.parse' noEnv [ "main" ] |> ignore<GitLike>)

        exc.Message
        |> shouldEqual
            """Errors during parse!
Required argument '--source' received no value"""

        GitLike.parse' noEnv [ "--source=origin" ; "main" ; "dev" ]
        |> shouldEqual (
            Pull
                {
                    From = "origin"
                    Refs = [ "main" ; "dev" ]
                }
        )

    [<Test>]
    let ``A sink in only one case: the empty command line still falls back to the other case`` () =
        GitLike.parse' noEnv []
        |> shouldEqual (
            Status
                {
                    Verbose = None
                }
        )

    [<Test>]
    let ``An unrecognised flag-like token is still fatal`` () =
        let exc =
            Assert.Throws<exn> (fun () -> FooBarMode.parse' noEnv [ "--fooo=1" ; "2" ] |> ignore<FooBarMode>)

        exc.Message
        |> shouldEqual "Unable to process argument --fooo=1 as key --fooo and value 1"

    [<Test>]
    let ``Help text renders each case's positional args inside its group`` () =
        let exc =
            Assert.Throws<exn> (fun () -> FooBarMode.parse' noEnv [ "--help" ] |> ignore<FooBarMode>)

        exc.Message
        |> shouldEqual
            """Help text requested.
exactly one of the following sets of arguments:
FooMode:
  --foo  int32
  --rest  int32 (positional args) (can be repeated)
BarMode:
  --bar  int32
  --rest  string (positional args) (can be repeated)"""

    [<Test>]
    let ``Adding positional tokens never changes which case is selected`` () =
        // Insertion points are safe by construction: the named arguments use --key=value.
        let mutable nonemptyCount = 0

        let property (useFoo : bool) (pos0 : int list) (pos1 : int list) (sep : bool) : unit =
            if not (List.isEmpty (pos0 @ pos1)) then
                nonemptyCount <- nonemptyCount + 1

            let named = if useFoo then "--foo=1" else "--bar=1"

            let args =
                [
                    yield! pos0 |> List.map string<int>
                    yield named
                    if sep then
                        yield "--"
                    yield! pos1 |> List.map string<int>
                ]

            let expectedInts = pos0 @ pos1

            if useFoo then
                FooBarMode.parse' noEnv args
                |> shouldEqual (
                    FooMode
                        {
                            Foo = 1
                            Rest = expectedInts
                        }
                )
            else
                FooBarMode.parse' noEnv args
                |> shouldEqual (
                    BarMode
                        {
                            Bar = 1
                            Rest = expectedInts |> List.map string<int>
                        }
                )

        Check.QuickThrowOnFailure property

        // The property is vacuous over empty positional lists; make sure the generator
        // actually exercised nonempty ones.
        nonemptyCount |> shouldBeGreaterThan 30
