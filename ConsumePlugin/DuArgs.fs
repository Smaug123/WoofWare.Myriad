namespace ConsumePlugin

open WoofWare.Myriad.Plugins

type FooArgs =
    {
        [<ArgumentHelpText "The foo argument">]
        Foo : int
    }

type BarArgs =
    {
        Bar : int
        Baz : int
    }

/// The motivating example of discriminated-union argument parsing: the user supplies either
/// `--foo=3`, or both `--bar=8` and `--baz=9`, and the parse tells us which.
[<ArgParser>]
type DuArgs =
    | FooCase of FooArgs
    | BarCase of BarArgs

type AutoMode =
    {
        Quiet : bool option
    }

type ManualMode =
    {
        Level : int
    }

type Mode =
    | Auto of AutoMode
    | Manual of ManualMode

/// A union nested inside a record, with a case (Auto) which is satisfiable with no arguments:
/// an empty command line picks it.
[<ArgParser>]
type WithModeArgs =
    {
        Verbose : bool
        Mode : Mode
    }

type CompressArgs =
    {
        Level : int
    }

type EncryptArgs =
    {
        Recipient : string
    }

/// Every case demands an argument, so no command line satisfies this union by saying nothing --
/// which is what lets an absent group be told apart from a present one.
type Transform =
    | Compress of CompressArgs
    | Encrypt of EncryptArgs

/// A union of alternative argument sets which need not be chosen among at all.
[<ArgParser>]
type WithOptionalTransformArgs =
    {
        Verbose : bool
        Transform : Transform option
    }

type DefaultedArgs =
    {
        [<ArgumentDefaultFunction>]
        Retries : Choice<int, int>
    }

    /// The default-function convention resolves against the record which declares the field
    /// (this case's payload record), not against the [<ArgParser>]-tagged union.
    static member DefaultRetries () = 3

type PlainArgs =
    {
        Value : int
    }

/// A union case whose payload record carries a default function. The default makes the case
/// satisfiable with no arguments, but must not influence which case is selected.
[<ArgParser>]
type DuWithDefaultArgs =
    | Defaulted of DefaultedArgs
    | Plain of PlainArgs

/// A union-typed field's [<ArgumentHelpText>] describes the whole set of alternatives, so it
/// heads the group which the alternatives are then listed inside.
[<ArgParser>]
type WithModeHelpArgs =
    {
        Verbose : bool
        [<ArgumentHelpText "How loud to be">]
        Mode : Mode
    }

/// A union of alternative argument sets may describe itself, for the benefit of every field which
/// embeds it.
[<ArgumentHelpText "Which transport to use">]
type Transport =
    | Tcp of TcpArgs
    | Unix of UnixArgs

and TcpArgs =
    {
        TcpPort : int
    }

and UnixArgs =
    {
        SocketPath : string
    }

/// `Fallback` takes the union's own description; `Preferred` overrides it from the field.
[<ArgParser>]
type WithTransportArgs =
    {
        [<ArgumentPrefix "preferred">]
        [<ArgumentHelpText "Try this one first">]
        Preferred : Transport
        [<ArgumentPrefix "fallback">]
        Fallback : Transport
    }

[<ArgumentHelpText "Fetch a URL">]
type FetchWithHelpArgs =
    {
        Url : string
    }

[<ArgumentHelpText "Push args, not that you'd know it from the case header">]
type PushWithHelpArgs =
    {
        Remote : string
    }

/// A case's payload record may describe itself, for the benefit of that case; a case is not
/// reached through a field, so there is no field-level attribute to check first, but the case
/// itself is a more specific placement than its payload record and so overrides it, exactly as
/// a field overrides a nested record's own description.
[<ArgParser>]
type CommandWithHelp =
    | FetchCase of FetchWithHelpArgs
    | [<ArgumentHelpText "Push to a remote">] PushCase of PushWithHelpArgs

/// A union beside a positional sink (default, i.e. Reject-mode): named arguments select the
/// union case, and every bare token is routed to the sink whichever case wins. An unrecognised
/// `--key`-shaped token remains fatal. The sink converts, so selection must not depend on the
/// tokens' parseability as int.
[<ArgParser>]
type ModeAndPositionals =
    {
        Mode : Mode

        [<PositionalArgs>]
        Rest : int list
    }

type FetchArgs =
    {
        Url : string
    }

type PushArgs =
    {
        Remote : string
        Force : bool
    }

/// No case is satisfiable with no arguments, so bare positional tokens alone must fail with
/// "no case selected" rather than picking a fallback.
type Command =
    | Fetch of FetchArgs
    | Push of PushArgs

/// The literal [<PositionalArgs false>] spelling of a Reject-mode sink beside a union.
[<ArgParser>]
type CommandAndPositionals =
    {
        Command : Command

        [<PositionalArgs false>]
        Paths : string list
    }

type FooModeArgs =
    {
        Foo : int

        [<PositionalArgs>]
        Rest : int list
    }

type BarModeArgs =
    {
        Bar : int

        [<PositionalArgs>]
        Rest : string list
    }

/// The motivating example of per-case positional args: both cases collect the positional
/// stream (addressable under the same default `--rest` form, which is fine because the cases
/// are mutually exclusive), but convert it at different types. Selection happens before any
/// conversion, so whether a token parses as int never influences which case wins.
[<ArgParser>]
type FooBarMode =
    | FooMode of FooModeArgs
    | BarMode of BarModeArgs

type PullArgs =
    {
        [<ArgumentLongForm "source">]
        From : string

        [<PositionalArgs>]
        Refs : string list
    }

type StatusArgs =
    {
        Verbose : bool option
    }

/// Positional args in only one case: the other (all-optional) case is the empty command
/// line's fallback, and a bare token structurally selects Pull because only Pull can consume
/// it.
[<ArgParser>]
type GitLike =
    | Pull of PullArgs
    | Status of StatusArgs

/// A union with no data in any case is an argument *value*, spelled by case name and matched
/// case-insensitively (`--verbosity=quiet`, `--verbosity=Quiet`, `--verbosity=QUIET`), rather than
/// a set of alternative argument sets: with no arguments to tell its cases apart, no command line
/// could select among them.
type Verbosity =
    | Quiet
    | Normal
    | ExtremelyLoud

type Colour =
    | Red
    | Green

/// An enumerated value is an ordinary argument leaf, so it composes with every leaf modifier:
/// optionality, repetition, defaults and the positional stream.
[<ArgParser>]
type EnumArgs =
    {
        Verbosity : Verbosity

        [<ArgumentHelpText "Which colour to paint it">]
        Colour : Colour option

        Palette : Colour list

        [<ArgumentDefaultFunction>]
        Fallback : Choice<Verbosity, Verbosity>

        [<ArgumentDefaultEnvironmentVariable "CONSUMEPLUGIN_ENUM_COLOUR">]
        EnvColour : Choice<Colour, Colour>

        [<PositionalArgs>]
        Rest : Colour list
    }

    static member DefaultFallback () = Verbosity.Normal

/// Type and case names are arbitrary identifiers, so they may contain characters which are
/// meaningful inside an F# format string: `%E` and `%B` are both format specifiers. Such a name
/// must reach `sprintf` as an argument rather than being spliced into the format literal.
type ``Percent%Enum`` =
    | ``A%B``
    | Half

[<ArgParser>]
type PercentArgs =
    {
        Ratio : ``Percent%Enum``
    }

type BuildArgs =
    {
        Verbosity : Verbosity
    }

type CleanArgs =
    {
        Force : bool option
    }

/// An enumerated value inside a union case. Selection happens before any conversion, so the
/// *value* supplied to `--verbosity` never influences which case wins; only its presence does.
[<ArgParser>]
type EnumInUnion =
    | Build of BuildArgs
    | Clean of CleanArgs
