namespace WoofWare.Myriad.Plugins.Test

open Fantomas.FCS.Syntax
open NUnit.Framework
open FsUnitTyped
open WoofWare.Whippet.Fantomas
open WoofWare.Myriad.Plugins

/// Generation-time rejection tests: sources which the ArgParser generator must refuse to
/// process, with comprehensible messages. These drive the full generator pipeline over
/// in-memory source, so rejection is asserted automatically (a rejected source could not take
/// part in an ordinary build).
///
/// The scanner in the generated parser matches argument names case-insensitively, so the
/// generator must reject name collisions under that same equality: a case-sensitive check
/// admits e.g. `foo` alongside `FOO`, and at parse time whichever is declared first silently
/// claims every spelling (for a union of alternative argument sets, silently selecting the
/// wrong alternative).
[<TestFixture>]
module TestArgParserRejection =

    /// Run the full generator pipeline over in-memory source.
    let private generateFromSource (source : string) : SynModuleOrNamespace list =
        ArgParserGenerator.generate (Ast.parse source)

    let private shouldRejectWith (message : string) (source : string) : unit =
        let exc =
            Assert.Throws<exn> (fun () -> generateFromSource source |> ignore<SynModuleOrNamespace list>)

        exc.Message |> shouldEqual message

    [<Test>]
    let ``Long forms which differ only by case are rejected`` () =
        """namespace TestMe

open WoofWare.Myriad.Plugins

[<ArgParser>]
type Args =
    {
        [<ArgumentLongForm "foo">]
        A : int
        [<ArgumentLongForm "FOO">]
        B : int
    }
"""
        |> shouldRejectWith
            "Conflicting argument names detected (names are matched case-insensitively):\nThe argument name '--foo' is claimed by: '--foo' (field 'A'); '--FOO' (field 'B')"

    [<Test>]
    let ``A long form colliding with a field-derived name only by case is rejected`` () =
        """namespace TestMe

open WoofWare.Myriad.Plugins

[<ArgParser>]
type Args =
    {
        Foo : int
        [<ArgumentLongForm "FOO">]
        B : int
    }
"""
        |> shouldRejectWith
            "Conflicting argument names detected (names are matched case-insensitively):\nThe argument name '--foo' is claimed by: '--foo' (field 'Foo'); '--FOO' (field 'B')"

    [<Test>]
    let ``Exact duplicate names are rejected`` () =
        """namespace TestMe

open WoofWare.Myriad.Plugins

[<ArgParser>]
type Args =
    {
        [<ArgumentLongForm "foo">]
        A : int
        [<ArgumentLongForm "foo">]
        B : int
    }
"""
        |> shouldRejectWith
            "Conflicting argument names detected (names are matched case-insensitively):\nThe argument name '--foo' is claimed by: '--foo' (field 'A'); '--foo' (field 'B')"

    [<Test>]
    let ``A field name colliding with another field's negated form is rejected`` () =
        """namespace TestMe

open WoofWare.Myriad.Plugins

[<ArgParser>]
type ConflictingFieldNames =
    {
        [<ArgumentNegateWithPrefix>]
        FooBar : bool
        NoFooBar : bool
    }
"""
        |> shouldRejectWith
            "Conflicting argument names detected (names are matched case-insensitively):\nThe argument name '--no-foo-bar' is claimed by: the --no- variant of field 'FooBar' (which has [<ArgumentNegateWithPrefix>]); '--no-foo-bar' (field 'NoFooBar')"

    [<Test>]
    let ``A long form colliding with a negated form is rejected`` () =
        """namespace TestMe

open WoofWare.Myriad.Plugins

[<ArgParser>]
type ConflictingLongForm =
    {
        [<ArgumentNegateWithPrefix>]
        Foo : bool
        [<ArgumentLongForm "no-foo">]
        Bar : bool
    }
"""
        |> shouldRejectWith
            "Conflicting argument names detected (names are matched case-insensitively):\nThe argument name '--no-foo' is claimed by: the --no- variant of field 'Foo' (which has [<ArgumentNegateWithPrefix>]); '--no-foo' (field 'Bar')"

    [<Test>]
    let ``A long form colliding with a negated form only by case is rejected`` () =
        """namespace TestMe

open WoofWare.Myriad.Plugins

[<ArgParser>]
type Args =
    {
        [<ArgumentNegateWithPrefix>]
        Foo : bool
        [<ArgumentLongForm "No-FOO">]
        Bar : bool
    }
"""
        |> shouldRejectWith
            "Conflicting argument names detected (names are matched case-insensitively):\nThe argument name '--no-foo' is claimed by: the --no- variant of field 'Foo' (which has [<ArgumentNegateWithPrefix>]); '--No-FOO' (field 'Bar')"

    [<Test>]
    let ``One of several long forms colliding with a negated form is rejected`` () =
        """namespace TestMe

open WoofWare.Myriad.Plugins

[<ArgParser>]
type ConflictingMultipleLongForms =
    {
        [<ArgumentLongForm "verbose">]
        [<ArgumentLongForm "v">]
        [<ArgumentNegateWithPrefix>]
        VerboseMode : bool

        [<ArgumentLongForm "no-verbose">]
        Quiet : bool
    }
"""
        |> shouldRejectWith
            "Conflicting argument names detected (names are matched case-insensitively):\nThe argument name '--no-verbose' is claimed by: the --no- variant of field 'VerboseMode' (which has [<ArgumentNegateWithPrefix>]); '--no-verbose' (field 'Quiet')"

    [<Test>]
    let ``A custom long form may collide with another custom long form's negation`` () =
        """namespace TestMe

open WoofWare.Myriad.Plugins

[<ArgParser>]
type ComplexConflict =
    {
        [<ArgumentLongForm "enable">]
        [<ArgumentNegateWithPrefix>]
        FeatureA : bool

        [<ArgumentLongForm "no-enable">]
        DisableAll : bool
    }
"""
        |> shouldRejectWith
            "Conflicting argument names detected (names are matched case-insensitively):\nThe argument name '--no-enable' is claimed by: the --no- variant of field 'FeatureA' (which has [<ArgumentNegateWithPrefix>]); '--no-enable' (field 'DisableAll')"

    [<Test>]
    let ``Names the scanner distinguishes are not collisions`` () =
        // "s" and "ſ" (long s) uppercase to the same string, but the scanner matches keys with
        // OrdinalIgnoreCase, which considers them distinct: this schema is unambiguous at parse
        // time, so generation must accept it. (A ToUpperInvariant-keyed check falsely rejects it.)
        let modules =
            generateFromSource
                """namespace TestMe

open WoofWare.Myriad.Plugins

[<ArgParser>]
type Args =
    {
        [<ArgumentLongForm "s">]
        A : int
        [<ArgumentLongForm "ſ">]
        B : int
    }
"""

        List.length modules |> shouldEqual 2

    [<Test>]
    let ``An empty long form is rejected`` () =
        """namespace TestMe

open WoofWare.Myriad.Plugins

[<ArgParser>]
type Args =
    {
        [<ArgumentLongForm "">]
        A : int
    }
"""
        |> shouldRejectWith
            "Invalid argument name for field 'A': an empty name's token would be '--', which is the positional separator."

    [<Test>]
    let ``A long form containing an equals sign is rejected`` () =
        // The scanner splits a --key=value token at its *first* '=', so such a name can never
        // match; a required argument under it would be permanently unsatisfiable.
        """namespace TestMe

open WoofWare.Myriad.Plugins

[<ArgParser>]
type Args =
    {
        [<ArgumentLongForm "foo=bar">]
        A : int
    }
"""
        |> shouldRejectWith
            "Invalid argument name 'foo=bar' for field 'A': a --key=value token splits at its first '=', so this argument could never be addressed."

    [<Test>]
    let ``Tagged type names may not claim the reserved runtime-module prefix`` () =
        // The generator emits one runtime module per namespace, named
        // ArgParserRuntime_<firstTaggedType>; a tagged type named ArgParserRuntime_Foo alongside
        // a tagged type Foo would therefore generate two modules with the same name, which does
        // not compile. The prefix is documented as reserved; enforce it where we can see it.
        """namespace TestMe

open WoofWare.Myriad.Plugins

[<ArgParser>]
type Foo =
    {
        A : int
    }

[<ArgParser>]
type ArgParserRuntime_Foo =
    {
        B : int
    }
"""
        |> shouldRejectWith
            "Type names beginning 'ArgParserRuntime_' are reserved: the ArgParser generator emits its runtime module under that prefix alongside the generated parsers. Rename the type 'ArgParserRuntime_Foo'."

    [<Test>]
    let ``Untagged types alongside a parser may not claim the runtime-module prefix either`` () =
        // The untagged record is visible to the generator (it arrives in the same recursive
        // group as the tagged type), and the emitted `module private ArgParserRuntime_Foo`
        // would collide with it at compile time.
        """namespace TestMe

open WoofWare.Myriad.Plugins

[<ArgParser>]
type Foo =
    {
        A : int
    }

type ArgParserRuntime_Foo =
    {
        B : int
    }
"""
        |> shouldRejectWith
            "Type names beginning 'ArgParserRuntime_' are reserved: the ArgParser generator emits its runtime module under that prefix alongside the generated parsers. Rename the type 'ArgParserRuntime_Foo'."

    [<Test>]
    let ``The reserved name help cannot be claimed, in any casing`` () =
        """namespace TestMe

open WoofWare.Myriad.Plugins

[<ArgParser>]
type ReservedHelpName =
    {
        [<ArgumentLongForm "HELP">]
        Foo : int
    }
"""
        |> shouldRejectWith "The argument name 'help' is reserved: --help always displays the help text."

    [<Test>]
    let ``Negation is only available on boolean-like fields`` () =
        let expectReject (source : string) =
            let exc =
                Assert.Throws<exn> (fun () -> generateFromSource source |> ignore<SynModuleOrNamespace list>)

            exc.Message.Contains "ArgumentNegateWithPrefix" |> shouldEqual true

        """namespace TestMe

open WoofWare.Myriad.Plugins

[<ArgParser>]
type InvalidAttributeOnNonBool =
    {
        [<ArgumentNegateWithPrefix>]
        NotABool : string
    }
"""
        |> expectReject

        """namespace TestMe

open WoofWare.Myriad.Plugins

[<ArgParser>]
type InvalidAttributeOnInt =
    {
        [<ArgumentNegateWithPrefix>]
        NotAFlag : int
    }
"""
        |> expectReject

    [<Test>]
    let ``Distinct negatable flags do not conflict`` () =
        // This must generate successfully.
        let modules =
            generateFromSource
                """namespace TestMe

open WoofWare.Myriad.Plugins

[<ArgParser>]
type NoConflict =
    {
        [<ArgumentNegateWithPrefix>]
        EnableFeature : bool

        [<ArgumentNegateWithPrefix>]
        VerboseMode : bool

        NormalField : string
    }
"""

        // One namespace for the embedded runtime module, one for the generated parser module.
        List.length modules |> shouldEqual 2

    // ------------------------------------------------------------------------------------------
    // Discriminated unions of alternative argument sets. Global name uniqueness (under the
    // scanner's case-insensitive equality) is the axiom which makes case selection sound: an
    // argument name shared between two cases would be routed to whichever case is declared
    // first, silently selecting it.

    [<Test>]
    let ``Argument names differing only by case collide across union cases`` () =
        // The empirical counterexample from review: with a case-sensitive check, `--FOO=3` parsed
        // successfully and constructed FooCase, and BarCase's argument was unreachable.
        """namespace TestMe

open WoofWare.Myriad.Plugins

type FooArgs =
    {
        Foo : int
    }

type BarArgs =
    {
        [<ArgumentLongForm "FOO">]
        Bar : int
    }

[<ArgParser>]
type DuArgs =
    | FooCase of FooArgs
    | BarCase of BarArgs
"""
        |> shouldRejectWith
            "Conflicting argument names detected (names are matched case-insensitively):\nThe argument name '--foo' is claimed by: '--foo' (field 'Foo'); '--FOO' (field 'Bar')"

    [<Test>]
    let ``Identical argument names collide across union cases`` () =
        """namespace TestMe

open WoofWare.Myriad.Plugins

type FooArgs =
    {
        Foo : int
    }

type BarArgs =
    {
        [<ArgumentLongForm "foo">]
        Bar : int
    }

[<ArgParser>]
type DuArgs =
    | FooCase of FooArgs
    | BarCase of BarArgs
"""
        |> shouldRejectWith
            "Conflicting argument names detected (names are matched case-insensitively):\nThe argument name '--foo' is claimed by: '--foo' (field 'Foo'); '--foo' (field 'Bar')"

    [<Test>]
    let ``Two union cases which are both satisfiable with no arguments are rejected`` () =
        // An empty command line could not choose between them.
        """namespace TestMe

open WoofWare.Myriad.Plugins

type AllOptionalA =
    {
        A : int option
    }

type AllOptionalB =
    {
        B : int option
    }

[<ArgParser>]
type AmbiguousEmptyCases =
    | CaseA of AllOptionalA
    | CaseB of AllOptionalB
"""
        |> shouldRejectWith
            "Cases CaseA, CaseB can all be satisfied without supplying any arguments, so an empty command line cannot choose between them. Make an argument in all but one of them mandatory."

    [<Test>]
    let ``Positional args are rejected inside a union case`` () =
        """namespace TestMe

open WoofWare.Myriad.Plugins

type SomePositionals =
    {
        [<PositionalArgs>]
        Rest : string list
    }

type NotPositional =
    {
        C : int
    }

type PositionalOrNot =
    | Pos of SomePositionals
    | NotPos of NotPositional

[<ArgParser>]
type PositionalInsideUnion =
    {
        Choice : PositionalOrNot
    }
"""
        |> shouldRejectWith
            "Positional args are not permitted inside cases of the [<ArgParser>] union PositionalOrNot: the parser could not tell which alternative a positional arg belongs to."

    [<Test>]
    let ``Positional args are rejected alongside a union`` () =
        // Conservative in v1: a Reject-mode sink beside a union is in principle sound (bare
        // tokens cannot influence case selection), but a Collect-mode sink silently swallows
        // typo'd case-selecting keys, so for now the combination is banned wholesale.
        """namespace TestMe

open WoofWare.Myriad.Plugins

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

[<ArgParser>]
type WithModeAndPositionals =
    {
        Mode : Mode

        [<PositionalArgs>]
        Rest : string list
    }
"""
        |> shouldRejectWith
            "Positional args cannot be combined with a discriminated-union arg: the parser could not tell which alternative an unrecognised arg belongs to."

    [<Test>]
    let ``A union case must carry a record defined alongside the union`` () =
        """namespace TestMe

open WoofWare.Myriad.Plugins

type FooArgs =
    {
        Foo : int
    }

[<ArgParser>]
type BadDu =
    | FooCase of int
    | BarCase of FooArgs
"""
        |> shouldRejectWith
            "Case FooCase of [<ArgParser>] union BadDu must have a payload which is a record defined alongside the union."

    [<Test>]
    let ``A union case must carry exactly one field`` () =
        """namespace TestMe

open WoofWare.Myriad.Plugins

type FooArgs =
    {
        Foo : int
    }

[<ArgParser>]
type BadDu =
    | FooCase of FooArgs * int
    | BarCase of FooArgs
"""
        |> shouldRejectWith
            "Case FooCase of [<ArgParser>] union BadDu must have exactly one field: a record holding that case's arguments."

    [<Test>]
    let ``Parenthesized type references are accepted wherever bare ones are`` () =
        // FCS represents `of (FooArgs)` as SynType.Paren; the by-name lookups for a case's
        // payload record, and for union- or record-typed fields, must see through it.
        let modules =
            generateFromSource
                """namespace TestMe

open WoofWare.Myriad.Plugins

type FooArgs =
    {
        Foo : int
    }

type BarArgs =
    {
        Bar : int
    }

[<ArgParser>]
type DuArgs =
    | FooCase of (FooArgs)
    | BarCase of BarArgs
"""

        List.length modules |> shouldEqual 2

        let modules =
            generateFromSource
                """namespace TestMe

open WoofWare.Myriad.Plugins

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

[<ArgParser>]
type WithModeArgs =
    {
        Verbose : bool
        Mode : (Mode)
    }
"""

        List.length modules |> shouldEqual 2

        let modules =
            generateFromSource
                """namespace TestMe

open WoofWare.Myriad.Plugins

type ChildRecord =
    {
        Thing : int
    }

[<ArgParser>]
type ParentRecord =
    {
        Child : (ChildRecord)
        AndAnother : bool
    }
"""

        List.length modules |> shouldEqual 2

    [<Test>]
    let ``The motivating union of alternative argument sets generates successfully`` () =
        let modules =
            generateFromSource
                """namespace TestMe

open WoofWare.Myriad.Plugins

type FooArgs =
    {
        Foo : int
    }

type BarArgs =
    {
        Bar : int
        Baz : int
    }

[<ArgParser>]
type DuArgs =
    | FooCase of FooArgs
    | BarCase of BarArgs
"""

        // One namespace for the embedded runtime module, one for the generated parser module.
        List.length modules |> shouldEqual 2
