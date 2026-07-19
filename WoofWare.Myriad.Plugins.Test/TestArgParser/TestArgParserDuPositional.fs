namespace WoofWare.Myriad.Plugins.Test

open NUnit.Framework
open FsUnitTyped
open FsCheck
open ConsumePlugin

/// A discriminated-union argument schema beside a Reject-mode positional sink: named arguments
/// select the union case, and bare tokens are collected by the sink whichever case wins.
[<TestFixture>]
module TestArgParserDuPositional =

    let noEnv (_ : string) : string option = None

    [<Test>]
    let ``Positionals are collected while named arguments select the case`` () =
        ModeAndPositionals.parse' noEnv [ "1" ; "--level=3" ; "2" ; "3" ]
        |> shouldEqual
            {
                Mode =
                    Manual
                        {
                            Level = 3
                        }
                Rest = [ 1 ; 2 ; 3 ]
            }

    [<Test>]
    let ``Positionals after the separator are collected too`` () =
        ModeAndPositionals.parse' noEnv [ "--quiet=true" ; "1" ; "--" ; "2" ; "3" ]
        |> shouldEqual
            {
                Mode =
                    Auto
                        {
                            Quiet = Some true
                        }
                Rest = [ 1 ; 2 ; 3 ]
            }

    [<Test>]
    let ``Bare positional tokens do not stop the empty-satisfiable case being the fallback`` () =
        ModeAndPositionals.parse' noEnv [ "4" ; "5" ]
        |> shouldEqual
            {
                Mode =
                    Auto
                        {
                            Quiet = None
                        }
                Rest = [ 4 ; 5 ]
            }

    [<Test>]
    let ``The sink's own key routes values to the sink without touching selection`` () =
        ModeAndPositionals.parse' noEnv [ "--rest=1" ; "--level=2" ; "--rest" ; "3" ]
        |> shouldEqual
            {
                Mode =
                    Manual
                        {
                            Level = 2
                        }
                Rest = [ 1 ; 3 ]
            }

    [<Test>]
    let ``An unrecognised flag-like token is still fatal`` () =
        // Reject mode: a typo of a case-selecting argument must be reported, not collected.
        let exc =
            Assert.Throws<exn> (fun () ->
                ModeAndPositionals.parse' noEnv [ "--lvel=3" ; "2" ]
                |> ignore<ModeAndPositionals>
            )

        exc.Message
        |> shouldEqual "Unable to process argument --lvel=3 as key --lvel and value 3"

    [<Test>]
    let ``Bare positional tokens alone do not select a case`` () =
        // The tokens are strings, so no conversion can fail: the only error is the missing
        // selection. (Positional tokens are structurally neutral: they never choose a case.)
        let exc =
            Assert.Throws<exn> (fun () ->
                CommandAndPositionals.parse' noEnv [ "src" ; "docs" ]
                |> ignore<CommandAndPositionals>
            )

        exc.Message
        |> shouldEqual
            """Errors during parse!
No arguments were supplied to select one of: Fetch, Push"""

    [<Test>]
    let ``A literal Reject-mode sink behaves like the default one`` () =
        CommandAndPositionals.parse' noEnv [ "src" ; "--url=http://example.com" ; "docs" ]
        |> shouldEqual
            {
                Command =
                    Fetch
                        {
                            Url = "http://example.com"
                        }
                Paths = [ "src" ; "docs" ]
            }

    [<Test>]
    let ``A conversion failure in the sink does not change which case is selected`` () =
        // "nope" is not an int, but the selected case must still be Manual: the conversion
        // error is reported without falling back to another alternative.
        let exc =
            Assert.Throws<exn> (fun () ->
                ModeAndPositionals.parse' noEnv [ "--level=3" ; "nope" ]
                |> ignore<ModeAndPositionals>
            )

        exc.Message.Contains "nope" |> shouldEqual true

        exc.Message.Contains "Arguments select more than one alternative"
        |> shouldEqual false

        exc.Message.Contains "No arguments were supplied" |> shouldEqual false

    [<Test>]
    let ``Help text groups the union's alternatives and lists the sink last`` () =
        let exc =
            Assert.Throws<exn> (fun () -> ModeAndPositionals.parse' noEnv [ "--help" ] |> ignore<ModeAndPositionals>)

        exc.Message
        |> shouldEqual
            """Help text requested.
exactly one of the following sets of arguments:
Auto:
  --quiet  bool (optional)
Manual:
  --level  int32
--rest  int32 (positional args) (can be repeated)"""

    [<Test>]
    let ``Adding positional tokens never changes which case is selected`` () =
        // Insertion points are safe by construction: every named argument uses the --key=value
        // form, so no bare token can be consumed as a pending key's value.
        let mutable nonemptyCount = 0

        let property (useManual : bool) (pos0 : int list) (pos1 : int list) (pos2 : int list) (sep : bool) : unit =
            if not (List.isEmpty (pos0 @ pos1 @ pos2)) then
                nonemptyCount <- nonemptyCount + 1

            let named, expectedMode =
                if useManual then
                    [ "--level=3" ],
                    Manual
                        {
                            Level = 3
                        }
                else
                    [ "--quiet=false" ],
                    Auto
                        {
                            Quiet = Some false
                        }

            let args =
                [
                    yield! pos0 |> List.map string<int>
                    yield! named
                    yield! pos1 |> List.map string<int>
                    if sep then
                        yield "--"
                    yield! pos2 |> List.map string<int>
                ]

            ModeAndPositionals.parse' noEnv args
            |> shouldEqual
                {
                    Mode = expectedMode
                    Rest = pos0 @ pos1 @ pos2
                }

        Check.QuickThrowOnFailure property

        // The property is vacuous over empty positional lists; make sure the generator
        // actually exercised nonempty ones.
        nonemptyCount |> shouldBeGreaterThan 30
