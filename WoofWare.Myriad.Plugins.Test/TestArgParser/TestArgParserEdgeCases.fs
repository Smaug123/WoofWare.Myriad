namespace WoofWare.Myriad.Plugins.Test

open NUnit.Framework
open FsUnitTyped
open ConsumePlugin

/// Edge cases of scanning and error reporting: malformed values, duplicate arguments, default
/// sources, and awkward token shapes.
[<TestFixture>]
module TestArgParserEdgeCases =

    let noEnv (_ : string) : string option = None

    [<Test>]
    let ``A malformed space-separated value does not abort the remainder of the scan`` () =
        let exc =
            Assert.Throws<exn> (fun () ->
                BasicNoPositionals.parse' noEnv [ "--foo" ; "bad" ; "--bar=present" ; "--baz=true" ]
                |> ignore<BasicNoPositionals>
            )

        exc.Message
        |> shouldEqual
            """Errors during parse!
The input string 'bad' was not in a correct format. (at arg --foo)
Required argument '--foo' received no value"""

    // The equals form behaves identically.
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

    [<Test>]
    let ``A duplicated flag does not consume the following option as its value`` () =
        let exc =
            Assert.Throws<exn> (fun () ->
                BasicNoPositionals.parse' noEnv [ "--baz=true" ; "--baz" ; "--foo=3" ; "--bar=present" ]
                |> ignore<BasicNoPositionals>
            )

        exc.Message
        |> shouldEqual
            """Errors during parse!
Flag '--baz' was supplied multiple times"""

    [<Test>]
    let ``A malformed list-element value is a parse error, not a raw FormatException`` () =
        let exc =
            Assert.Throws<exn> (fun () ->
                BasicNoPositionals.parse' noEnv [ "--foo=1" ; "--bar=x" ; "--baz=true" ; "--rest" ; "notanint" ]
                |> ignore<BasicNoPositionals>
            )

        exc.Message
        |> shouldEqual
            """Errors during parse!
The input string 'notanint' was not in a correct format. (at arg --rest)"""

    [<Test>]
    let ``A malformed positional value is a parse error, not a raw FormatException`` () =
        let exc =
            Assert.Throws<exn> (fun () ->
                BasicWithIntPositionals.parse' noEnv [ "--foo=1" ; "--bar=x" ; "--baz=true" ; "notanint" ]
                |> ignore<BasicWithIntPositionals>
            )

        exc.Message
        |> shouldEqual
            """Errors during parse!
The input string 'notanint' was not in a correct format. (at arg notanint)"""

    [<Test>]
    let ``A malformed positional value after the separator is a parse error, not a raw FormatException`` () =
        let exc =
            Assert.Throws<exn> (fun () ->
                BasicWithIntPositionals.parse' noEnv [ "--foo=1" ; "--bar=x" ; "--baz=true" ; "--" ; "notanint" ]
                |> ignore<BasicWithIntPositionals>
            )

        exc.Message
        |> shouldEqual
            """Errors during parse!
The input string 'notanint' was not in a correct format. (at arg notanint)"""

    [<Test>]
    let ``A malformed environment-variable default is a parse error naming the variable`` () =
        let exc =
            Assert.Throws<exn> (fun () ->
                ContainsBoolEnvVar.parse' (fun _ -> Some "notabool") []
                |> ignore<ContainsBoolEnvVar>
            )

        exc.Message
        |> shouldEqual
            """Errors during parse!
String 'notabool' was not recognized as a valid Boolean. (from environment variable CONSUMEPLUGIN_THINGS)"""

    // Defaults run only when the parse is otherwise clean, so a throwing
    // `getEnvironmentVariable` cannot mask the real parse error.
    [<Test>]
    let ``Environment lookups do not run after the parse has failed`` () =
        let exc =
            Assert.Throws<exn> (fun () ->
                ContainsBoolEnvVar.parse' (fun _ -> failwith "env var was consulted") [ "--bool-var=notabool" ]
                |> ignore<ContainsBoolEnvVar>
            )

        exc.Message
        |> shouldEqual
            """Errors during parse!
String 'notabool' was not recognized as a valid Boolean. (at arg --bool-var=notabool)"""

    [<Test>]
    let ``Help matches case-insensitively, like ordinary arguments`` () =
        BasicNoPositionals.parse' noEnv [ "--FOO=1" ; "--bar=x" ; "--baz=true" ]
        |> shouldEqual
            {
                Foo = 1
                Bar = "x"
                Baz = true
                Rest = []
            }

        let exc =
            Assert.Throws<exn> (fun () -> BasicNoPositionals.parse' noEnv [ "--HELP" ] |> ignore<BasicNoPositionals>)

        exc.Message
        |> shouldEqual
            """Help text requested.
--foo  int32
--bar  string
--baz  bool
--rest  int32 (can be repeated)"""

    [<Test>]
    let ``A non-positional list of booleans parses, in all three syntaxes`` () =
        NonPositionalBoolList.parse' noEnv [ "--flags" ; "true" ; "--flags=false" ; "--flags" ]
        |> shouldEqual
            {
                Flags = [ true ; false ; true ]
            }

    // The positional sink is addressable in keyed form under every one of its long forms, not
    // just the first (Rest here has both "rest" and "others").
    [<Test>]
    let ``Every long form of a positional sink reaches the sink in keyed syntax`` () =
        SameBaseNameArgs.parse' noEnv [ "--value=1" ; "--rest=a" ; "--others=b" ; "c" ]
        |> shouldEqual
            {
                Value = 1
                Rest = [ "a" ; "b" ; "c" ]
            }
