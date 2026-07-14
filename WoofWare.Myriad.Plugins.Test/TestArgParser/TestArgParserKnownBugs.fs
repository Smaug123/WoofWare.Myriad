namespace WoofWare.Myriad.Plugins.Test

open System
open NUnit.Framework
open FsUnitTyped
open ConsumePlugin

/// These tests pin down current behaviour of the generated arg parser which we believe to be
/// buggy. Each test asserts what the parser *does today*, with a comment stating the desired
/// behaviour. They exist so that the planned rewrite of the arg parser flips them deliberately,
/// making every observable semantic change reviewable, rather than changing behaviour silently.
[<TestFixture>]
module TestArgParserKnownBugs =

    let noEnv (_ : string) : string option = None

    // DESIRED: a malformed value in `--key value` form should record the conversion error and
    // continue scanning, exactly as the `--key=value` form does. Instead, the scan stops dead:
    // `--bar` and `--baz` below are never processed, so they are spuriously reported missing.
    [<Test>]
    let ``BUG: a malformed space-separated value aborts the remainder of the scan`` () =
        let exc =
            Assert.Throws<exn> (fun () ->
                BasicNoPositionals.parse' noEnv [ "--foo" ; "bad" ; "--bar=present" ; "--baz=true" ]
                |> ignore<BasicNoPositionals>
            )

        exc.Message
        |> shouldEqual
            """Errors during parse!
The input string 'bad' was not in a correct format.
Required argument '--foo' received no value
Required argument '--bar' received no value
Required argument '--baz' received no value"""

    // Contrast case: the same malformed value in `--key=value` form *does* continue the scan.
    // This test is here to document the asymmetry with the test above; this behaviour (continue
    // scanning after a conversion error) is the desired one.
    [<Test>]
    let ``A malformed equals-form value does not abort the scan`` () =
        let exc =
            Assert.Throws<exn> (fun () ->
                BasicNoPositionals.parse' noEnv [ "--foo=bad" ; "--bar=present" ; "--baz=true" ]
                |> ignore<BasicNoPositionals>
            )

        exc.Message
        |> shouldEqual
            """Errors during parse!
The input string 'bad' was not in a correct format. (at arg --foo=bad)
Required argument '--foo' received no value"""

    // DESIRED: a duplicated flag must not consume the next token as its "value". Here the
    // duplicate `--baz` swallows `--foo=3`, so on top of the duplicate-arg error we get a
    // spurious "'--foo' received no value" error, and `--foo=3` is never parsed.
    [<Test>]
    let ``BUG: a duplicated flag consumes the following option as its value`` () =
        let exc =
            Assert.Throws<exn> (fun () ->
                BasicNoPositionals.parse' noEnv [ "--baz=true" ; "--baz" ; "--foo=3" ; "--bar=present" ]
                |> ignore<BasicNoPositionals>
            )

        exc.Message
        |> shouldEqual
            """Errors during parse!
Argument '--baz' was supplied multiple times: True and --foo=3
Required argument '--foo' received no value"""

    // DESIRED: a conversion failure in a list-typed (repeatable) argument should be recorded as a
    // parse error like scalar conversion failures are, not escape as a raw FormatException with
    // no indication of which argument was at fault.
    [<Test>]
    let ``BUG: a malformed list-element value throws a raw FormatException`` () =
        Assert.Throws<FormatException> (fun () ->
            BasicNoPositionals.parse' noEnv [ "--foo=1" ; "--bar=x" ; "--baz=true" ; "--rest" ; "notanint" ]
            |> ignore<BasicNoPositionals>
        )
        |> fun exc ->
            exc.Message
            |> shouldEqual "The input string 'notanint' was not in a correct format."

    // DESIRED: as above, but for a positional argument.
    [<Test>]
    let ``BUG: a malformed positional value throws a raw FormatException`` () =
        Assert.Throws<FormatException> (fun () ->
            BasicWithIntPositionals.parse' noEnv [ "--foo=1" ; "--bar=x" ; "--baz=true" ; "notanint" ]
            |> ignore<BasicWithIntPositionals>
        )
        |> fun exc ->
            exc.Message
            |> shouldEqual "The input string 'notanint' was not in a correct format."

    // DESIRED: as above, but for a positional argument appearing after the `--` separator.
    [<Test>]
    let ``BUG: a malformed positional value after the separator throws a raw FormatException`` () =
        Assert.Throws<FormatException> (fun () ->
            BasicWithIntPositionals.parse' noEnv [ "--foo=1" ; "--bar=x" ; "--baz=true" ; "--" ; "notanint" ]
            |> ignore<BasicWithIntPositionals>
        )
        |> fun exc ->
            exc.Message
            |> shouldEqual "The input string 'notanint' was not in a correct format."

    // DESIRED: a malformed environment-variable default should be reported through the parser's
    // error channel (naming the environment variable), not escape as a raw FormatException.
    [<Test>]
    let ``BUG: a malformed environment-variable default throws a raw FormatException`` () =
        Assert.Throws<FormatException> (fun () ->
            ContainsBoolEnvVar.parse' (fun _ -> Some "notabool") []
            |> ignore<ContainsBoolEnvVar>
        )
        |> fun exc ->
            exc.Message
            |> shouldEqual "String 'notabool' was not recognized as a valid Boolean."

    // DESIRED: once the parse is known to have failed, no further effects should run: the
    // environment should not be consulted for defaults. Today the env lookup runs anyway, so a
    // throwing `getEnvironmentVariable` masks the real parse error.
    [<Test>]
    let ``BUG: environment lookups run even after the parse has already failed`` () =
        let exc =
            Assert.Throws<exn> (fun () ->
                ContainsBoolEnvVar.parse' (fun _ -> failwith "env var was consulted") [ "--bool-var=notabool" ]
                |> ignore<ContainsBoolEnvVar>
            )

        exc.Message |> shouldEqual "env var was consulted"

    // DESIRED: help detection should use the same case-insensitive comparison as ordinary
    // argument matching. Today `--FOO=1` matches the field `Foo`, but `--HELP` is not help: it
    // falls through to ordinary (failed) key processing.
    [<Test>]
    let ``BUG: ordinary args match case-insensitively but help is case-sensitive`` () =
        // Case-insensitive ordinary match: this parses fine.
        BasicNoPositionals.parse' noEnv [ "--FOO=1" ; "--bar=x" ; "--baz=true" ]
        |> shouldEqual
            {
                Foo = 1
                Bar = "x"
                Baz = true
                Rest = []
            }

        // ...but --HELP does not produce the help text.
        let exc =
            Assert.Throws<exn> (fun () -> BasicNoPositionals.parse' noEnv [ "--HELP" ] |> ignore<BasicNoPositionals>)

        exc.Message
        |> shouldEqual
            """Errors during parse!
Trailing argument --HELP had no value. Use a double-dash to separate positional args from key-value args.
Required argument '--foo' received no value
Required argument '--bar' received no value
Required argument '--baz' received no value"""
