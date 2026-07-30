namespace WoofWare.Myriad.Plugins.Test

open System
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
            None

        let args = [ "--foo=3" ; "--non-existent" ; "--bar=4" ; "--baz=true" ]

        let exc =
            Assert.Throws<exn> (fun () -> Basic.parse' getEnvVar args |> ignore<Basic>)

        envCalls.Value |> shouldEqual 0

        exc.Message
        |> shouldEqual
            """Unable to process supplied arg --non-existent. Help text follows.
--foo  int32 : This is a foo!
--bar  string
--baz  bool
--rest  string (positional args) (can be repeated) : Here's where the rest of the args go"""

    [<Test>]
    let ``Can supply positional args with key`` () =
        let envCalls = ref 0

        let getEnvVar (_ : string) =
            Interlocked.Increment envCalls |> ignore<int>
            None

        let property (args : (int * bool) list) (afterDoubleDash : int list option) =
            let flatArgs =
                args
                |> List.collect (fun (value, sep) ->
                    if sep then
                        [ $"--rest=%i{value}" ]
                    else
                        [ "--rest" ; string<int> value ]
                )
                |> fun l -> l @ [ "--foo=3" ; "--bar=4" ; "--baz=true" ]

            let flatArgs, expected =
                match afterDoubleDash with
                | None -> flatArgs, List.map fst args
                | Some rest -> flatArgs @ [ "--" ] @ (List.map string<int> rest), List.map fst args @ rest

            BasicWithIntPositionals.parse' getEnvVar flatArgs
            |> shouldEqual
                {
                    Foo = 3
                    Bar = "4"
                    Baz = true
                    Rest = expected
                }

        Check.QuickThrowOnFailure property
        envCalls.Value |> shouldEqual 0

    [<Test>]
    let ``Consume multiple occurrences of required arg`` () =
        let envCalls = ref 0

        let getEnvVar (_ : string) =
            Interlocked.Increment envCalls |> ignore<int>
            None

        let args = [ "--foo=3" ; "--rest" ; "7" ; "--bar=4" ; "--baz=true" ; "--rest=8" ]

        let result = BasicNoPositionals.parse' getEnvVar args

        envCalls.Value |> shouldEqual 0

        result
        |> shouldEqual
            {
                Foo = 3
                Bar = "4"
                Baz = true
                Rest = [ 7 ; 8 ]
            }

    [<Test>]
    let ``Gracefully handle invalid multiple occurrences of required arg`` () =
        let envCalls = ref 0

        let getEnvVar (_ : string) =
            Interlocked.Increment envCalls |> ignore<int>
            None

        let args = [ "--foo=3" ; "--foo" ; "9" ; "--bar=4" ; "--baz=true" ; "--baz=false" ]

        let exc =
            Assert.Throws<exn> (fun () -> Basic.parse' getEnvVar args |> ignore<Basic>)

        envCalls.Value |> shouldEqual 0

        exc.Message
        |> shouldEqual
            """Errors during parse!
Argument '--foo' was supplied multiple times: 3 and 9
Argument '--baz' was supplied multiple times: True and false"""

    [<Test>]
    let ``Args appearing after double dash are positional`` () =
        let envCalls = ref 0

        let getEnvVar (_ : string) =
            Interlocked.Increment envCalls |> ignore<int>
            None

        let args = [ "--" ; "--foo=3" ; "--bar=4" ; "--baz=true" ]

        let exc =
            Assert.Throws<exn> (fun () -> Basic.parse' getEnvVar args |> ignore<Basic>)

        exc.Message
        |> shouldEqual
            """Errors during parse!
Required argument '--foo' received no value
Required argument '--bar' received no value
Required argument '--baz' received no value"""

        envCalls.Value |> shouldEqual 0

    [<Test>]
    let ``Help text`` () =
        let getEnvVar (s : string) =
            s |> shouldEqual "CONSUMEPLUGIN_THINGS"
            Some "hi!"

        let exc =
            Assert.Throws<exn> (fun () -> Basic.parse' getEnvVar [ "--help" ] |> ignore<Basic>)

        exc.Message
        |> shouldEqual
            """Help text requested.
--foo  int32 : This is a foo!
--bar  string
--baz  bool
--rest  string (positional args) (can be repeated) : Here's where the rest of the args go"""

    [<Test>]
    let ``Help text, with default values`` () =
        let envVars = ref 0

        let getEnvVar (_ : string) =
            Interlocked.Increment envVars |> ignore<int>
            None

        let exc =
            Assert.Throws<exn> (fun () -> LoadsOfTypes.parse' getEnvVar [ "--help" ] |> ignore<LoadsOfTypes>)

        exc.Message
        |> shouldEqual
            """Help text requested.
--foo  int32
--bar  string
--baz  bool
--some-file  FileInfo
--some-directory  DirectoryInfo
--some-list  DirectoryInfo (can be repeated)
--optional-thing-with-no-default  int32 (optional)
--optional-thing  bool (default value: True)
--another-optional-thing  int32 (default value: 3)
--yet-another-optional-thing  string (default value populated from env var CONSUMEPLUGIN_THINGS)
--positionals  int32 (positional args) (can be repeated)"""

        envVars.Value |> shouldEqual 0

    [<Test>]
    let ``Default values`` () =
        let getEnvVar (s : string) =
            s |> shouldEqual "CONSUMEPLUGIN_THINGS"
            Some "hi!"

        let args =
            [
                "--foo"
                "3"
                "--bar=some string"
                "--baz"
                "--some-file=/path/to/file"
                "--some-directory"
                "/a/dir"
                "--another-optional-thing"
                "3000"
            ]

        let result = LoadsOfTypes.parse' getEnvVar args

        result.OptionalThing |> shouldEqual (Choice2Of2 true)
        result.OptionalThingWithNoDefault |> shouldEqual None
        result.AnotherOptionalThing |> shouldEqual (Choice1Of2 3000)
        result.YetAnotherOptionalThing |> shouldEqual (Choice2Of2 "hi!")

    [<Test>]
    let ``ParseExact and help`` () =
        let count = ref 0

        let getEnvVar (_ : string) =
            Interlocked.Increment count |> ignore<int>
            None

        let exc =
            Assert.Throws<exn> (fun () -> DatesAndTimes.parse' getEnvVar [ "--help" ] |> ignore<DatesAndTimes>)

        exc.Message
        |> shouldEqual
            @"Help text requested.
--plain  TimeSpan
--invariant  TimeSpan
--exact  TimeSpan : An exact time please [Parse format (.NET): hh\:mm\:ss]
--invariant-exact  TimeSpan : [Parse format (.NET): hh\:mm\:ss]"

        count.Value |> shouldEqual 0

    [<Test>]
    let rec ``TimeSpans and their attributes`` () =
        let count = ref 0

        let getEnvVar (_ : string) =
            Interlocked.Increment count |> ignore<int>
            None

        let parsed =
            DatesAndTimes.parse'
                getEnvVar
                [
                    "--exact=11:34:00"
                    "--plain=1"
                    "--invariant=23:59"
                    "--invariant-exact=23:59:00"
                ]

        parsed.Plain |> shouldEqual (TimeSpan (1, 0, 0, 0))
        parsed.Invariant |> shouldEqual (TimeSpan (23, 59, 00))
        parsed.Exact |> shouldEqual (TimeSpan (11, 34, 00))
        parsed.InvariantExact |> shouldEqual (TimeSpan (23, 59, 00))

        let exc =
            Assert.Throws<exn> (fun () ->
                DatesAndTimes.parse'
                    getEnvVar
                    [
                        "--exact=11:34:00"
                        "--plain=1"
                        "--invariant=23:59"
                        "--invariant-exact=23:59"
                    ]
                |> ignore<DatesAndTimes>
            )

        exc.Message
        |> shouldEqual
            """Errors during parse!
Input string was not in a correct format. (at arg --invariant-exact=23:59)
Required argument '--invariant-exact' received no value"""

        let exc =
            Assert.Throws<exn> (fun () ->
                DatesAndTimes.parse'
                    getEnvVar
                    [
                        "--exact=11:34"
                        "--plain=1"
                        "--invariant=23:59"
                        "--invariant-exact=23:59:00"
                    ]
                |> ignore<DatesAndTimes>
            )

        exc.Message
        |> shouldEqual
            """Errors during parse!
Input string was not in a correct format. (at arg --exact=11:34)
Required argument '--exact' received no value"""

        count.Value |> shouldEqual 0

    [<Test>]
    let ``Can consume stacked record without positionals`` () =
        let getEnvVar (_ : string) = failwith "should not call"

        let parsed =
            ParentRecord.parse' getEnvVar [ "--and-another=true" ; "--thing1=9" ; "--thing2=a thing!" ]

        parsed
        |> shouldEqual
            {
                Child =
                    {
                        Thing1 = 9
                        Thing2 = "a thing!"
                    }
                AndAnother = true
            }

    [<Test>]
    let ``Every alias of a positional-args field is accepted at its own key`` () =
        let getEnvVar (_ : string) = failwith "should not call"

        AliasedPositionals.parse' getEnvVar [ "--count=1" ; "--rest=a" ; "--remainder" ; "b" ; "bare" ; "--" ; "c" ]
        |> shouldEqual
            {
                Count = 1
                Others = [ "a" ; "b" ; "bare" ; "c" ]
            }

    [<Test>]
    let ``Default functions on nested records resolve against the declaring record`` () =
        let getEnvVar (_ : string) = failwith "should not call"

        // The default function lives on ChildRecordWithDefault, not on the tagged root; the
        // generated code must call it there (this used to generate a call against the root,
        // which did not compile).
        ParentRecordChildDefault.parse' getEnvVar [ "--and-another=false" ]
        |> shouldEqual
            {
                Child =
                    {
                        FromFunction = Choice2Of2 97
                    }
                AndAnother = false
            }

        // A supplied value still wins over the default.
        ParentRecordChildDefault.parse' getEnvVar [ "--and-another=false" ; "--from-function=3" ]
        |> shouldEqual
            {
                Child =
                    {
                        FromFunction = Choice1Of2 3
                    }
                AndAnother = false
            }

    [<Test>]
    let ``Can consume stacked record, child has positionals`` () =
        let getEnvVar (_ : string) = failwith "should not call"

        let parsed =
            ParentRecordChildPos.parse'
                getEnvVar
                [
                    "--and-another=true"
                    "--thing1=9"
                    "--thing2=https://example.com"
                    "--thing2=http://example.com"
                ]

        parsed.AndAnother |> shouldEqual true
        parsed.Child.Thing1 |> shouldEqual 9

        parsed.Child.Thing2
        |> List.map (fun (x : Uri) -> x.ToString ())
        |> shouldEqual [ "https://example.com/" ; "http://example.com/" ]

    [<Test>]
    let ``Can consume stacked record, child has no positionals, parent has positionals`` () =
        let getEnvVar (_ : string) = failwith "should not call"

        let parsed =
            ParentRecordSelfPos.parse'
                getEnvVar
                [
                    "--and-another=true"
                    "--and-another=false"
                    "--and-another=true"
                    "--thing1=9"
                    "--thing2=some"
                ]

        parsed
        |> shouldEqual
            {
                Child =
                    {
                        Thing1 = 9
                        Thing2 = "some"
                    }
                AndAnother = [ true ; false ; true ]
            }

    [<Test>]
    let ``Help text for stacked records`` () =
        let getEnvVar (_ : string) = failwith "should not call"

        let exc =
            Assert.Throws<exn> (fun () ->
                ParentRecordSelfPos.parse' getEnvVar [ "--help" ] |> ignore<ParentRecordSelfPos>
            )

        exc.Message
        |> shouldEqual
            """Help text requested.
Child:
  --thing1  int32
  --thing2  string
--and-another  bool (positional args) (can be repeated)"""

    [<Test>]
    let ``Help text for a nested record is headed by the field's help text`` () =
        let getEnvVar (_ : string) = failwith "should not call"

        let exc =
            Assert.Throws<exn> (fun () ->
                ParentRecordWithGroupHelp.parse' getEnvVar [ "--help" ]
                |> ignore<ParentRecordWithGroupHelp>
            )

        exc.Message
        |> shouldEqual
            """Help text requested.
Child: Settings for the child thing
  --thing1  int32
  --thing2  string
--and-another  bool : Whether to and-another"""

    /// A nested type describes itself for every site which embeds it; a field which has something
    /// more specific to say overrides that, because one type may be embedded for several purposes.
    [<Test>]
    let ``A nested record's own help text heads the group, and the field's overrides it`` () =
        let getEnvVar (_ : string) = failwith "should not call"

        let exc =
            Assert.Throws<exn> (fun () ->
                ParentRecordWithTypeHelp.parse' getEnvVar [ "--help" ]
                |> ignore<ParentRecordWithTypeHelp>
            )

        exc.Message
        |> shouldEqual
            """Help text requested.
Primary: How to talk to the database
  --primary-host  string
  --primary-port  int32
Secondary: Where to fail over to
  --secondary-host  string
  --secondary-port  int32"""

    /// FCS hands the generator the *decoded* string, so a help text containing a backslash, a
    /// quote, or a control character must be re-escaped before it is reproduced in the generated
    /// file, or Fantomas (which escapes only quotes) would emit it wrong: `\t` would come out of
    /// the quotes as a literal tab, silently changing what the help text displays.
    [<Test>]
    let ``Help text needing escaping round-trips through the generated file`` () =
        let getEnvVar (_ : string) = failwith "should not call"

        let exc =
            Assert.Throws<exn> (fun () ->
                ParentRecordWithEscapedHelp.parse' getEnvVar [ "--help" ]
                |> ignore<ParentRecordWithEscapedHelp>
            )

        exc.Message
        |> shouldEqual
            "Help text requested.\nChild: Path is C:\\temp, quote is \" and tab is \t.\n  --thing1  int32\n  --thing2  string"

    /// A record field's name is reconstructed as a plain `Ident` when the generator builds the
    /// expression which constructs this type at runtime; a name which is not a plain identifier
    /// needs its backticks re-added there, exactly as its declaration needed them, or the
    /// generated file does not compile at all (so this test's mere presence in a green build
    /// partly stands for the property; the parse asserts the fields are actually populated).
    ///
    /// `` ``_`` `` and `` ``|A|_|`` `` specifically exercise the two shapes a general-purpose
    /// backtick-normaliser treats as already fine (the wildcard pattern and an active-pattern name
    /// are both meaningful bare tokens elsewhere in F#'s grammar), but which are not valid bare
    /// record labels.
    [<Test>]
    let ``A field name needing backticks survives re-emission in the constructed record`` () =
        let getEnvVar (_ : string) = failwith "should not call"

        AwkwardFieldName.parse' getEnvVar [ "--thing1=1" ; "--thing2=two" ; "--_=3" ; "--|-a|_|=4" ]
        |> shouldEqual
            {
                AwkwardFieldName.``back\tab`` =
                    {
                        Thing1 = 1
                        Thing2 = "two"
                    }
                AwkwardFieldName.``_`` = 3
                AwkwardFieldName.``|A|_|`` = 4
            }

    [<Test>]
    let ``Positionals are tagged with Choice`` () =
        let getEnvVar (_ : string) = failwith "should not call"

        ChoicePositionals.parse' getEnvVar [ "a" ; "b" ; "--" ; "--c" ; "--help" ]
        |> shouldEqual
            {
                Args = [ Choice1Of2 "a" ; Choice1Of2 "b" ; Choice2Of2 "--c" ; Choice2Of2 "--help" ]
            }

    let boolCases =
        [
            "1", true
            "0", false
            "true", true
            "false", false
            "TRUE", true
            "FALSE", false
        ]
        |> List.map TestCaseData

    [<TestCaseSource(nameof boolCases)>]
    let ``Bool env vars can be populated`` (envValue : string, boolValue : bool) =
        let getEnvVar (s : string) =
            s |> shouldEqual "CONSUMEPLUGIN_THINGS"
            Some envValue

        ContainsBoolEnvVar.parse' getEnvVar []
        |> shouldEqual
            {
                BoolVar = Choice2Of2 boolValue
            }

    [<Test>]
    let ``Bools can be treated with arity 0`` () =
        let getEnvVar (_ : string) = failwith "do not call"

        ContainsBoolEnvVar.parse' getEnvVar [ "--bool-var" ]
        |> shouldEqual
            {
                BoolVar = Choice1Of2 true
            }

    [<Test>]
    let ``Trailing double-dash does not discard pending arity-0 flag`` () =
        // Regression test: a bare `--` was being handled before the parser examined its pending state,
        // so the pending `--bool-var` flag was silently discarded and the env-var default won.
        // The `--bool-var` should be treated as an arity-0 flag exactly as if input had ended.
        ContainsBoolEnvVar.parse' (fun _ -> Some "false") [ "--bool-var" ; "--" ]
        |> shouldEqual
            {
                BoolVar = Choice1Of2 true
            }

    [<Test>]
    let ``A bare trailing double-dash is equivalent to end of input`` () =
        // A `--` with nothing after it introduces no positional args, so appending one to a
        // dash-free argument list must not change the parse outcome, whatever pending state we're in.
        let getEnvVar (_ : string) = Some "false"

        let outcome (args : string list) =
            try
                Ok (ContainsBoolEnvVar.parse' getEnvVar args)
            with e ->
                Error e.Message

        let tokenOf (i : int) =
            match ((i % 5) + 5) % 5 with
            | 0 -> "--bool-var"
            | 1 -> "true"
            | 2 -> "false"
            | 3 -> "--bool-var=true"
            | _ -> "--bool-var=false"

        let property (choices : int list) =
            // None of these tokens is itself "--", so appending a trailing "--" adds no positional args.
            let tokens = choices |> List.map tokenOf
            outcome tokens = outcome (tokens @ [ "--" ])

        Check.QuickThrowOnFailure property

    [<TestCaseSource(nameof boolCases)>]
    let ``Flag DUs can be parsed from env var`` (envValue : string, boolValue : bool) =
        let getEnvVar (s : string) =
            s |> shouldEqual "CONSUMEPLUGIN_THINGS"
            Some envValue

        let boolValue = if boolValue then DryRunMode.Dry else DryRunMode.Wet

        ContainsFlagEnvVar.parse' getEnvVar []
        |> shouldEqual
            {
                DryRun = Choice2Of2 boolValue
            }

    let dryRunData =
        [
            [ "--dry-run" ], DryRunMode.Dry
            [ "--dry-run" ; "true" ], DryRunMode.Dry
            [ "--dry-run=true" ], DryRunMode.Dry
            [ "--dry-run" ; "True" ], DryRunMode.Dry
            [ "--dry-run=True" ], DryRunMode.Dry
            [ "--dry-run" ; "false" ], DryRunMode.Wet
            [ "--dry-run=false" ], DryRunMode.Wet
            [ "--dry-run" ; "False" ], DryRunMode.Wet
            [ "--dry-run=False" ], DryRunMode.Wet
        ]
        |> List.map TestCaseData

    [<TestCaseSource(nameof dryRunData)>]
    let ``Flag DUs can be parsed`` (args : string list, expected : DryRunMode) =
        let getEnvVar (_ : string) = failwith "do not call"

        ContainsFlagEnvVar.parse' getEnvVar args
        |> shouldEqual
            {
                DryRun = Choice1Of2 expected
            }

    [<TestCaseSource(nameof dryRunData)>]
    let ``Flag DUs can be parsed, ArgumentDefaultFunction`` (args : string list, expected : DryRunMode) =
        let getEnvVar (_ : string) = failwith "do not call"

        ContainsFlagDefaultValue.parse' getEnvVar args
        |> shouldEqual
            {
                DryRun = Choice1Of2 expected
            }

    [<Test>]
    let ``Flag DUs can be given a default value`` () =
        let getEnvVar (_ : string) = failwith "do not call"

        ContainsFlagDefaultValue.parse' getEnvVar []
        |> shouldEqual
            {
                DryRun = Choice2Of2 DryRunMode.Wet
            }

    [<Test>]
    let ``Literal defaults are taken exactly when the argument is absent`` () =
        // The defining property of [<ArgumentDefaultValue>]: each field independently reports
        // Choice1Of2 of whatever the user typed, or Choice2Of2 of the attribute's literal when the
        // user typed nothing. Fields must not interfere with each other.
        let getEnvVar (_ : string) = failwith "do not call"

        let property
            (intVar : int option)
            (stringVar : NonNull<string> option)
            (boolVar : bool option)
            (charVar : char option)
            =
            // The `--key=value` spelling, so that a generated value which itself looks like a flag
            // (or is empty) is still unambiguously this argument's value.
            let args =
                [
                    match intVar with
                    | Some i -> $"--int-var=%i{i}"
                    | None -> ()
                    match stringVar with
                    | Some (NonNull s) -> $"--string-var=%s{s}"
                    | None -> ()
                    match boolVar with
                    | Some b -> $"--bool-var=%b{b}"
                    | None -> ()
                    match charVar with
                    | Some c -> $"--char-var=%c{c}"
                    | None -> ()
                ]

            let expected =
                {
                    IntVar =
                        match intVar with
                        | Some i -> Choice1Of2 i
                        | None -> Choice2Of2 3
                    StringVar =
                        match stringVar with
                        | Some (NonNull s) -> Choice1Of2 s
                        | None -> Choice2Of2 "hello world"
                    BoolVar =
                        match boolVar with
                        | Some b -> Choice1Of2 b
                        | None -> Choice2Of2 true
                    CharVar =
                        match charVar with
                        | Some c -> Choice1Of2 c
                        | None -> Choice2Of2 'q'
                }

            ContainsLiteralDefault.parse' getEnvVar args = expected

        Check.QuickThrowOnFailure property

    [<Test>]
    let ``Help text renders literal defaults`` () =
        let getEnvVar (_ : string) = failwith "do not call"

        let exc =
            Assert.Throws<exn> (fun () ->
                ContainsLiteralDefault.parse' getEnvVar [ "--help" ]
                |> ignore<ContainsLiteralDefault>
            )

        exc.Message
        |> shouldEqual
            """Help text requested.
--int-var  int32 (default value: 3)
--string-var  string (default value: hello world)
--bool-var  bool (default value: True)
--char-var  char (default value: q)"""

    /// The literal is rebuilt in the generated file rather than echoed as the user's source text,
    /// so a string whose spelling and value differ has to come out the other side intact. The
    /// expected values here are written out independently of the spellings in the record's
    /// attributes, which is the whole point of the test.
    [<Test>]
    let ``Awkward string defaults survive into the generated file`` () =
        let getEnvVar (_ : string) = failwith "do not call"

        let property
            (backslash : NonNull<string> option)
            (quotes : NonNull<string> option)
            (control : NonNull<string> option)
            (unicode : NonNull<string> option)
            =
            let args =
                [
                    match backslash with
                    | Some (NonNull s) -> $"--backslash=%s{s}"
                    | None -> ()
                    match quotes with
                    | Some (NonNull s) -> $"--quotes=%s{s}"
                    | None -> ()
                    match control with
                    | Some (NonNull s) -> $"--control=%s{s}"
                    | None -> ()
                    match unicode with
                    | Some (NonNull s) -> $"--unicode=%s{s}"
                    | None -> ()
                ]

            let expected =
                {
                    Backslash =
                        match backslash with
                        | Some (NonNull s) -> Choice1Of2 s
                        | None -> Choice2Of2 @"C:\temp"
                    Quotes =
                        match quotes with
                        | Some (NonNull s) -> Choice1Of2 s
                        | None -> Choice2Of2 "say \"hi\""
                    Control =
                        match control with
                        | Some (NonNull s) -> Choice1Of2 s
                        | None -> Choice2Of2 "tab\there\nnewline"
                    Unicode =
                        match unicode with
                        | Some (NonNull s) -> Choice1Of2 s
                        | None -> Choice2Of2 "caf\u00e9 \u2603"
                }

            ContainsAwkwardStringDefaults.parse' getEnvVar args = expected

        Check.QuickThrowOnFailure property

    [<Test>]
    let ``Help text renders awkward string defaults`` () =
        let getEnvVar (_ : string) = failwith "do not call"

        let exc =
            Assert.Throws<exn> (fun () ->
                ContainsAwkwardStringDefaults.parse' getEnvVar [ "--help" ]
                |> ignore<ContainsAwkwardStringDefaults>
            )

        let expected =
            [
                "Help text requested."
                @"--backslash  string (default value: C:\temp)"
                "--quotes  string (default value: say \"hi\")"
                "--control  string (default value: tab\there\nnewline)"
                "--unicode  string (default value: caf\u00e9 \u2603)"
            ]
            |> String.concat "\n"

        exc.Message |> shouldEqual expected

    [<Test>]
    let ``Help text for flag DU`` () =
        let getEnvVar (_ : string) = failwith "do not call"

        let exc =
            Assert.Throws<exn> (fun () ->
                ContainsFlagDefaultValue.parse' getEnvVar [ "--help" ]
                |> ignore<ContainsFlagDefaultValue>
            )

        exc.Message
        |> shouldEqual
            """Help text requested.
--dry-run  bool (default value: false)"""

    [<Test>]
    let ``Help text for flag DU, non default`` () =
        let getEnvVar (_ : string) = failwith "do not call"

        let exc =
            Assert.Throws<exn> (fun () -> WithFlagDu.parse' getEnvVar [ "--help" ] |> ignore<WithFlagDu>)

        exc.Message
        |> shouldEqual
            """Help text requested.
--dry-run  bool"""

    let longFormCases =
        let doTheThing =
            [
                [ "--do-something-else=foo" ]
                [ "--anotherarg=foo" ]
                [ "--do-something-else" ; "foo" ]
                [ "--anotherarg" ; "foo" ]
            ]

        let someFlag =
            [
                [ "--turn-it-on" ], true
                [ "--dont-turn-it-off" ], true
                [ "--turn-it-on=true" ], true
                [ "--dont-turn-it-off=true" ], true
                [ "--turn-it-on=false" ], false
                [ "--dont-turn-it-off=false" ], false
                [ "--turn-it-on" ; "true" ], true
                [ "--dont-turn-it-off" ; "true" ], true
                [ "--turn-it-on" ; "false" ], false
                [ "--dont-turn-it-off" ; "false" ], false
            ]

        List.allPairs doTheThing someFlag
        |> List.map (fun (doTheThing, (someFlag, someFlagResult)) ->
            let args = doTheThing @ someFlag

            let expected =
                {
                    DoTheThing = "foo"
                    SomeFlag = someFlagResult
                }

            args, expected
        )
        |> List.map TestCaseData

    [<TestCaseSource(nameof longFormCases)>]
    let ``Long-form args`` (args : string list, expected : ManyLongForms) =
        let getEnvVar (_ : string) = failwith "do not call"

        ManyLongForms.parse' getEnvVar args |> shouldEqual expected

    [<Test>]
    let ``Long-form args can't be referred to by their original name`` () =
        let getEnvVar (_ : string) = failwith "do not call"

        let exc =
            Assert.Throws<exn> (fun () ->
                ManyLongForms.parse' getEnvVar [ "--do-the-thing=foo" ] |> ignore<ManyLongForms>
            )

        exc.Message
        |> shouldEqual """Unable to process argument --do-the-thing=foo as key --do-the-thing and value foo"""

    [<Test>]
    let ``Long-form args help text`` () =
        let getEnvVar (_ : string) = failwith "do not call"

        let exc =
            Assert.Throws<exn> (fun () -> ManyLongForms.parse' getEnvVar [ "--help" ] |> ignore<ManyLongForms>)

        exc.Message
        |> shouldEqual
            """Help text requested.
--do-something-else / --anotherarg  string
--turn-it-on / --dont-turn-it-off  bool"""

    [<Test>]
    let ``Can collect *all* non-help args into positional args with includeFlagLike`` () =
        let getEnvVar (_ : string) = failwith "do not call"

        FlagsIntoPositionalArgs.parse' getEnvVar [ "--a" ; "foo" ; "--b=false" ; "--c" ; "hi" ; "--" ; "--help" ]
        |> shouldEqual
            {
                A = "foo"
                GrabEverything = [ "--b=false" ; "--c" ; "hi" ; "--help" ]
            }

        // Users might consider this eccentric!
        // But we're only a simple arg parser; we don't look around to see whether this is "almost"
        // a valid parse.
        FlagsIntoPositionalArgs.parse' getEnvVar [ "--a" ; "--b=false" ; "--c" ; "hi" ; "--" ; "--help" ]
        |> shouldEqual
            {
                A = "--b=false"
                GrabEverything = [ "--c" ; "hi" ; "--help" ]
            }

    [<Test>]
    let ``Trailing double-dash does not silently drop a pending unknown key`` () =
        // Regression test: previously `--unknown --` silently dropped `--unknown` because the `--`
        // was handled before the parser examined its pending `AwaitingValue` state. A pending key
        // with no value is now resolved exactly as if input had ended: it's not an arity-0 flag, so
        // this is the "trailing argument had no value" error rather than a silent success.
        let getEnvVar (_ : string) = failwith "do not call"

        let exc =
            Assert.Throws<exn> (fun () ->
                FlagsIntoPositionalArgs.parse' getEnvVar [ "--a=present" ; "--unknown" ; "--" ]
                |> ignore<FlagsIntoPositionalArgs>
            )

        exc.Message
        |> shouldEqual
            """Errors during parse!
Trailing argument --unknown had no value. Use a double-dash to separate positional args from key-value args."""

    [<Test>]
    let ``Can collect non-help args into positional args with Choice`` () =
        let getEnvVar (_ : string) = failwith "do not call"

        FlagsIntoPositionalArgsChoice.parse' getEnvVar [ "--a" ; "foo" ; "--b=false" ; "--c" ; "hi" ; "--" ; "--help" ]
        |> shouldEqual
            {
                A = "foo"
                GrabEverything =
                    [
                        Choice1Of2 "--b=false"
                        Choice1Of2 "--c"
                        Choice1Of2 "hi"
                        Choice2Of2 "--help"
                    ]
            }

    [<Test>]
    let ``Can collect non-help args into positional args, and we parse on the way`` () =
        let getEnvVar (_ : string) = failwith "do not call"

        FlagsIntoPositionalArgsInt.parse' getEnvVar [ "3" ; "--a" ; "foo" ; "5" ; "--" ; "98" ]
        |> shouldEqual
            {
                A = "foo"
                GrabEverything = [ 3 ; 5 ; 98 ]
            }

    [<Test>]
    let ``Can collect non-help args into positional args with Choice, and we parse on the way`` () =
        let getEnvVar (_ : string) = failwith "do not call"

        FlagsIntoPositionalArgsIntChoice.parse' getEnvVar [ "3" ; "--a" ; "foo" ; "5" ; "--" ; "98" ]
        |> shouldEqual
            {
                A = "foo"
                GrabEverything = [ Choice1Of2 3 ; Choice1Of2 5 ; Choice2Of2 98 ]
            }

    [<Test>]
    let ``Can refuse to collect non-help args with PositionalArgs false`` () =
        let getEnvVar (_ : string) = failwith "do not call"

        let exc =
            Assert.Throws<exn> (fun () ->
                FlagsIntoPositionalArgs'.parse'
                    getEnvVar
                    [ "--a" ; "foo" ; "--b=false" ; "--c" ; "hi" ; "--" ; "--help" ]
                |> ignore<FlagsIntoPositionalArgs'>
            )

        exc.Message
        |> shouldEqual """Unable to process argument --b=false as key --b and value false"""

        let exc =
            Assert.Throws<exn> (fun () ->
                FlagsIntoPositionalArgs'.parse' getEnvVar [ "--a" ; "--b=false" ; "--c=hi" ; "--" ; "--help" ]
                |> ignore<FlagsIntoPositionalArgs'>
            )

        // Again perhaps eccentric!
        // Again, we don't try to detect that the user has missed out the desired argument to `--a`.
        exc.Message
        |> shouldEqual """Unable to process argument --c=hi as key --c and value hi"""

    [<Test>]
    let ``Type-level help text appears in help output`` () =
        let getEnvVar (_ : string) = None

        let exc =
            Assert.Throws<exn> (fun () -> WithTypeHelp.parse' getEnvVar [ "--help" ] |> ignore<WithTypeHelp>)

        exc.Message
        |> shouldContainText
            "Parse command-line arguments for a basic configuration. This help text appears before the argument list."

        exc.Message
        |> shouldContainText "--config-file  string : The configuration file path"

        exc.Message |> shouldContainText "--verbose  bool : Enable verbose output"
        exc.Message |> shouldContainText "--port  int32"

    [<Test>]
    let ``Type-level help text appears before field help`` () =
        let getEnvVar (_ : string) = None

        let exc =
            Assert.Throws<exn> (fun () -> WithTypeHelp.parse' getEnvVar [ "--help" ] |> ignore<WithTypeHelp>)

        // Verify that the type help appears before the field help
        let typeHelpIndex =
            exc.Message.IndexOf "Parse command-line arguments for a basic configuration"

        let fieldHelpIndex = exc.Message.IndexOf "--config-file"

        typeHelpIndex |> shouldBeSmallerThan fieldHelpIndex

    [<Test>]
    let ``Multiline type-level help text works`` () =
        let getEnvVar (_ : string) = None

        let exc =
            Assert.Throws<exn> (fun () ->
                WithMultilineTypeHelp.parse' getEnvVar [ "--help" ]
                |> ignore<WithMultilineTypeHelp>
            )

        exc.Message |> shouldContainText "This is a multiline help text example."

        exc.Message
        |> shouldContainText "It spans multiple lines to test that multiline strings work correctly."

        exc.Message
        |> shouldContainText "You can use this to provide detailed documentation for your argument parser."

        exc.Message |> shouldContainText "--input-file  string : Input file to process"
        exc.Message |> shouldContainText "--output-dir  string : Output directory"
        exc.Message |> shouldContainText "--force  bool"

    [<Test>]
    let ``Type-level help text appears in error messages`` () =
        let getEnvVar (_ : string) = None

        let exc =
            Assert.Throws<exn> (fun () ->
                WithTypeHelp.parse' getEnvVar [ "--unknown-arg" ; "value" ]
                |> ignore<WithTypeHelp>
            )

        // Verify that the type help appears in error messages too
        exc.Message
        |> shouldContainText
            "Parse command-line arguments for a basic configuration. This help text appears before the argument list."

        exc.Message |> shouldContainText "--config-file"

    [<Test>]
    let ``Types without type-level help still work`` () =
        let getEnvVar (_ : string) = None

        let exc =
            Assert.Throws<exn> (fun () -> Basic.parse' getEnvVar [ "--help" ] |> ignore<Basic>)

        // Should not contain any type-level help, just the field help
        exc.Message |> shouldContainText "--foo  int32 : This is a foo!"
        exc.Message |> shouldContainText "--bar  string"
        // Make sure there's no extra blank line at the beginning
        exc.Message.StartsWith '\n' |> shouldEqual false

    /// An argument's spelling survives the generated file's round trip. A `SynConst.String` holds
    /// *decoded* text, so re-emitting it demands the escaping the author's own source supplied:
    /// without it `"back\\tab"` is emitted as `"back\tab"`, read back with a tab in it, and the
    /// argument answers to a name other than the one declared.
    [<Test>]
    let ``Long forms needing escaping survive re-emission`` () =
        let getEnvVar (_ : string) = failwith "should not call"

        AwkwardLongForms.parse' getEnvVar [ "--back\\tab=1" ; "--verbatim\\tab=2" ; "--café=3" ; "--paren\\tab=4" ]
        |> shouldEqual
            {
                Backslash = 1
                Verbatim = 2
                Unicode = 3
                Parenthesised = 4
            }

    /// The help text advertises the same spellings the scanner accepts.
    [<Test>]
    let ``Help text for long forms needing escaping`` () =
        let getEnvVar (_ : string) = failwith "should not call"

        let exc =
            Assert.Throws<exn> (fun () -> AwkwardLongForms.parse' getEnvVar [ "--help" ] |> ignore<AwkwardLongForms>)

        exc.Message
        |> shouldEqual
            """Help text requested.
--back\tab  int32
--verbatim\tab  int32
--café  int32
--paren\tab  int32"""
