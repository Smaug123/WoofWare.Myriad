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
