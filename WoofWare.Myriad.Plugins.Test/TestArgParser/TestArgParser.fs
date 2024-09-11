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
            ""

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
            ""

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
            ""

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
            ""

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
            ""

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
            "hi!"

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
            ""

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
            "hi!"

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
            ""

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
            ""

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
--thing1  int32
--thing2  string
--and-another  bool (positional args) (can be repeated)"""

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

    [<TestCaseSource(nameof (boolCases))>]
    let ``Bool env vars can be populated`` (envValue : string, boolValue : bool) =
        let getEnvVar (s : string) =
            s |> shouldEqual "CONSUMEPLUGIN_THINGS"
            envValue

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

    [<TestCaseSource(nameof boolCases)>]
    let ``Flag DUs can be parsed from env var`` (envValue : string, boolValue : bool) =
        let getEnvVar (s : string) =
            s |> shouldEqual "CONSUMEPLUGIN_THINGS"
            envValue

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

        FlagsIntoPositionalArgs.parse' getEnvVar [ "--a" ; "--b=false" ; "--c=hi" ; "--" ; "--help" ]
        |> fun f -> f.GrabEverything
        |> shouldEqual [ "--a" ; "--b=false" ; "--c=hi" ; "--help" ]
