namespace ConsumePlugin

open System
open System.IO
open WoofWare.Myriad.Plugins

[<ArgParser>]
type BasicNoPositionals =
    {
        Foo : int
        Bar : string
        Baz : bool
        Rest : int list
    }

[<ArgParser>]
type Basic =
    {
        [<ArgumentHelpText "This is a foo!">]
        Foo : int
        Bar : string
        Baz : bool
        [<ArgumentHelpText "Here's where the rest of the args go">]
        [<PositionalArgs>]
        Rest : string list
    }

[<ArgParser>]
type BasicWithIntPositionals =
    {
        Foo : int
        Bar : string
        Baz : bool
        [<PositionalArgs>]
        Rest : int list
    }

[<ArgParser>]
type LoadsOfTypes =
    {
        Foo : int
        Bar : string
        Baz : bool
        SomeFile : FileInfo
        SomeDirectory : DirectoryInfo
        SomeList : DirectoryInfo list
        OptionalThingWithNoDefault : int option
        [<PositionalArgs>]
        Positionals : int list
        [<ArgumentDefaultFunction>]
        OptionalThing : Choice<bool, bool>
        [<ArgumentDefaultFunction>]
        AnotherOptionalThing : Choice<int, int>
        [<ArgumentDefaultEnvironmentVariable "CONSUMEPLUGIN_THINGS">]
        YetAnotherOptionalThing : Choice<string, string>
    }

    static member DefaultOptionalThing () = true

    static member DefaultAnotherOptionalThing () = 3

[<ArgParser>]
type LoadsOfTypesNoPositionals =
    {
        Foo : int
        Bar : string
        Baz : bool
        SomeFile : FileInfo
        SomeDirectory : DirectoryInfo
        SomeList : DirectoryInfo list
        OptionalThingWithNoDefault : int option
        [<ArgumentDefaultFunction>]
        OptionalThing : Choice<bool, bool>
        [<ArgumentDefaultFunction>]
        AnotherOptionalThing : Choice<int, int>
        [<ArgumentDefaultEnvironmentVariable "CONSUMEPLUGIN_THINGS">]
        YetAnotherOptionalThing : Choice<string, string>
    }

    static member DefaultOptionalThing () = false

    static member DefaultAnotherOptionalThing () = 3

[<ArgParser true>]
type DatesAndTimes =
    {
        Plain : TimeSpan
        [<InvariantCulture>]
        Invariant : TimeSpan
        [<ParseExact @"hh\:mm\:ss">]
        [<ArgumentHelpText "An exact time please">]
        Exact : TimeSpan
        [<InvariantCulture ; ParseExact @"hh\:mm\:ss">]
        InvariantExact : TimeSpan
    }

type ChildRecord =
    {
        Thing1 : int
        Thing2 : string
    }

[<ArgParser true>]
type ParentRecord =
    {
        Child : ChildRecord
        AndAnother : bool
    }

type ChildRecordWithPositional =
    {
        Thing1 : int
        [<PositionalArgs>]
        Thing2 : Uri list
    }

[<ArgParser true>]
type ParentRecordChildPos =
    {
        Child : ChildRecordWithPositional
        AndAnother : bool
    }

[<ArgParser true>]
type ParentRecordSelfPos =
    {
        Child : ChildRecord
        [<PositionalArgs>]
        AndAnother : bool list
    }

[<ArgParser true>]
type ChoicePositionals =
    {
        [<PositionalArgs>]
        Args : Choice<string, string> list
    }

[<ArgParser true>]
type ContainsBoolEnvVar =
    {
        [<ArgumentDefaultEnvironmentVariable "CONSUMEPLUGIN_THINGS">]
        BoolVar : Choice<bool, bool>
    }

[<RequireQualifiedAccess>]
module Consts =
    [<Literal>]
    let FALSE = false

    [<Literal>]
    let TRUE = true

type DryRunMode =
    | [<ArgumentFlag(Consts.FALSE)>] Wet
    | [<ArgumentFlag true>] Dry

[<ArgParser true>]
type WithFlagDu =
    {
        DryRun : DryRunMode
    }

[<ArgParser true>]
type ContainsFlagEnvVar =
    {
        // This phrasing is odd, but it's for a test. Nobody's really going to have `--dry-run`
        // controlled by an env var!
        [<ArgumentDefaultEnvironmentVariable "CONSUMEPLUGIN_THINGS">]
        DryRun : Choice<DryRunMode, DryRunMode>
    }

[<ArgParser true>]
type ContainsFlagDefaultValue =
    {
        [<ArgumentDefaultFunction>]
        DryRun : Choice<DryRunMode, DryRunMode>
    }

    static member DefaultDryRun () = DryRunMode.Wet

[<ArgParser true>]
type ManyLongForms =
    {
        [<ArgumentLongForm "do-something-else">]
        [<ArgumentLongForm "anotherarg">]
        DoTheThing : string

        [<ArgumentLongForm "turn-it-on">]
        [<ArgumentLongForm "dont-turn-it-off">]
        SomeFlag : bool
    }

[<RequireQualifiedAccess>]
type private IrrelevantDu =
    | Foo
    | Bar

[<ArgParser true>]
type FlagsIntoPositionalArgs =
    {
        A : string
        [<PositionalArgs true>]
        GrabEverything : string list
    }

[<ArgParser true>]
type FlagsIntoPositionalArgs' =
    {
        A : string
        [<PositionalArgs false>]
        DontGrabEverything : string list
    }
