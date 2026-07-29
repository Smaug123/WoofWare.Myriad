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

type ChildRecordWithDefault =
    {
        [<ArgumentDefaultFunction>]
        FromFunction : Choice<int, int>
    }

    /// The default-function convention resolves against the record which declares the field,
    /// not against the [<ArgParser>]-tagged root.
    static member DefaultFromFunction () = 97

[<ArgParser true>]
type ParentRecordChildDefault =
    {
        Child : ChildRecordWithDefault
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

/// `[<ArgumentDefaultValue>]` is shorthand for an `[<ArgumentDefaultFunction>]` whose function
/// returns a constant. The value must be written out as a literal: we reproduce it in the generated
/// file, where a name would not necessarily mean what it means here.
[<ArgParser true>]
type ContainsLiteralDefault =
    {
        [<ArgumentDefaultValue 3>]
        IntVar : Choice<int, int>
        [<ArgumentDefaultValue "hello world">]
        StringVar : Choice<string, string>
        [<ArgumentDefaultValue true>]
        BoolVar : Choice<bool, bool>
        // Chars are the one literal Fantomas will not round-trip through a spliced node, so we
        // rebuild the constant rather than passing the user's through.
        [<ArgumentDefaultValue 'q'>]
        CharVar : Choice<char, char>
    }

/// We rebuild the literal in the generated file rather than echoing the user's source text, so
/// escape-sensitive strings have to survive that round trip: a naively re-emitted `"C:\\temp"`
/// becomes `"C:\temp"`, whose `\t` is a tab.
[<ArgParser true>]
type ContainsAwkwardStringDefaults =
    {
        [<ArgumentDefaultValue "C:\\temp">]
        Backslash : Choice<string, string>
        [<ArgumentDefaultValue @"say ""hi""">]
        Quotes : Choice<string, string>
        [<ArgumentDefaultValue "tab\there\nnewline">]
        Control : Choice<string, string>
        [<ArgumentDefaultValue "caf\u00e9 \u2603">]
        Unicode : Choice<string, string>
    }

/// An argument's *spelling* goes through the same round trip as a default value, and needs the same
/// care: a `SynConst.String` holds decoded text, so re-emitting `"back\\tab"` naively gives
/// `"back\tab"`, whose `\t` is a tab, and the argument answers to a name other than the one
/// declared. A verbatim spelling decodes to the same text and must end up spelled the same way.
[<ArgParser true>]
type AwkwardLongForms =
    {
        [<ArgumentLongForm "back\\tab">]
        Backslash : int
        [<ArgumentLongForm @"verbatim\tab">]
        Verbatim : int
        [<ArgumentLongForm "café">]
        Unicode : int
        /// F#'s attribute syntax permits parentheses around the argument, and the name checks
        /// already see through them, so emission must too.
        [<ArgumentLongForm("paren\\tab")>]
        Parenthesised : int
    }

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

[<ArgParser true>]
type AliasedPositionals =
    {
        Count : int

        [<PositionalArgs>]
        [<ArgumentLongForm "rest">]
        [<ArgumentLongForm "remainder">]
        Others : string list
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
type FlagsIntoPositionalArgsChoice =
    {
        A : string
        [<PositionalArgs true>]
        GrabEverything : Choice<string, string> list
    }

[<ArgParser true>]
type FlagsIntoPositionalArgsInt =
    {
        A : string
        [<PositionalArgs true>]
        GrabEverything : int list
    }

[<ArgParser true>]
type FlagsIntoPositionalArgsIntChoice =
    {
        A : string
        [<PositionalArgs true>]
        GrabEverything : Choice<int, int> list
    }

[<ArgParser true>]
type FlagsIntoPositionalArgs' =
    {
        A : string
        [<PositionalArgs false>]
        DontGrabEverything : string list
    }

[<ArgParser>]
[<ArgumentHelpText "Parse command-line arguments for a basic configuration. This help text appears before the argument list.">]
type WithTypeHelp =
    {
        [<ArgumentHelpText "The configuration file path">]
        ConfigFile : string
        [<ArgumentHelpText "Enable verbose output">]
        Verbose : bool
        Port : int
    }

[<ArgParser>]
[<ArgumentHelpText "This is a multiline help text example.
It spans multiple lines to test that multiline strings work correctly.
You can use this to provide detailed documentation for your argument parser.">]
type WithMultilineTypeHelp =
    {
        [<ArgumentHelpText "Input file to process">]
        InputFile : string
        [<ArgumentHelpText "Output directory">]
        OutputDir : string
        Force : bool
    }

/// Regression test: the pre-rewrite generator produced uncompilable code for a non-positional
/// list of booleans (its accumulator was a ResizeArray but the flag machinery assumed an option).
[<ArgParser>]
type NonPositionalBoolList =
    {
        Flags : bool list
    }
