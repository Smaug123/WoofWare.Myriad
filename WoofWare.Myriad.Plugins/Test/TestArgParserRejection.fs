namespace WoofWare.Myriad.Plugins.Test

open Fantomas.FCS.Syntax
open NUnit.Framework
open FsUnitTyped
open FsCheck
open FsCheck.FSharp
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

    /// The three default-supplying attributes are interchangeable as far as the checks below are
    /// concerned, so their rejection messages are shared; assert against these rather than
    /// restating them per attribute.
    let private defaultAttrOnNonChoice (field : string) : string =
        $"Field '%s{field}' has a default-value attribute ([<ArgumentDefaultFunction>], [<ArgumentDefaultValue>], or [<ArgumentDefaultEnvironmentVariable>]), but its type is not Choice<'a, 'a>. Defaults are surfaced through Choice<'a, 'a> so a successful parse can report whether a value was user-supplied (Choice1Of2) or defaulted (Choice2Of2); a bare field cannot express this. Change the field's type to Choice<'a, 'a>, or remove the attribute."

    let private contextSensitiveDefault (field : string) (constant : string) : string =
        $"Field '%s{field}' has an [<ArgumentDefaultValue>] whose value uses the context-sensitive constant %s{constant}. Its value depends on where it is written, and we reproduce it in the generated file rather than evaluating it at your attribute, so it would not mean there what it means in your source; we also emit it in more than one place, so it need not even be consistent within the generated file. Use [<ArgumentDefaultFunction>] instead: that function is evaluated in your own file."

    let private namedDefault (field : string) (name : string) : string =
        $"Field '%s{field}' has an [<ArgumentDefaultValue>] whose value names something (%s{name}) rather than writing out a constant. We reproduce the value in the generated file rather than evaluating it at your attribute, and that file hoists every `open` in your source above the parser, so the name need not resolve to the same binding there as here. Write the constant out literally, or use [<ArgumentDefaultFunction>]: that function is evaluated in your own file."

    let private defaultAttrOnPositional (field : string) : string =
        $"Field '%s{field}' is positional, so it cannot carry a default-value attribute ([<ArgumentDefaultFunction>], [<ArgumentDefaultValue>], or [<ArgumentDefaultEnvironmentVariable>]): positional args are collected, not defaulted."

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

    /// A union one of whose cases holds a positional sink, wrapped in a record; the sink's
    /// attribute is rendered from the given text.
    let private positionalInsideUnionSource (positionalAttr : string) : string =
        sprintf
            """namespace TestMe

open WoofWare.Myriad.Plugins

type SomePositionals =
    {
        A : int

        [<%s>]
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
            positionalAttr

    [<Test>]
    let ``Reject-mode positional args are permitted inside a union case`` () =
        for attr in [ "PositionalArgs" ; "PositionalArgs false" ] do
            // One namespace for the embedded runtime module, one for the generated parser module.
            generateFromSource (positionalInsideUnionSource attr)
            |> List.length
            |> shouldEqual 2

    [<Test>]
    let ``Collect-mode positional args are rejected inside a union case`` () =
        positionalInsideUnionSource "PositionalArgs true"
        |> shouldRejectWith
            "Positional args which collect unrecognised flag-like tokens ([<PositionalArgs true>]) cannot be combined with a discriminated-union arg: a mistyped case-selecting argument would be collected as a positional arg instead of being reported."

    [<Test>]
    let ``A sink inside a union case cannot coexist with a sink beside the union`` () =
        // Some complete alternative would contain two positional sinks, and argv holds a
        // single positional stream.
        """namespace TestMe

open WoofWare.Myriad.Plugins

type SomePositionals =
    {
        A : int

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

        [<PositionalArgs>]
        Extra : string list
    }
"""
        |> shouldRejectWith "Multiple entries tried to claim positional args! Choice and Extra"

    [<Test>]
    let ``Two positional fields in one record are rejected`` () =
        """namespace TestMe

open WoofWare.Myriad.Plugins

[<ArgParser>]
type TwoSinks =
    {
        [<PositionalArgs>]
        First : string list

        [<PositionalArgs>]
        Second : string list
    }
"""
        |> shouldRejectWith "Multiple entries tried to claim positional args! First and Second"

    [<Test>]
    let ``Sinks in mutually exclusive cases may share their forms`` () =
        // Both cases' sinks are addressable as --rest; a keyed --rest token has the same
        // meaning whichever case wins, so this must generate successfully.
        let modules =
            generateFromSource
                """namespace TestMe

open WoofWare.Myriad.Plugins

type FooArgs =
    {
        Foo : int

        [<PositionalArgs>]
        Rest : int list
    }

type BarArgs =
    {
        Bar : int

        [<PositionalArgs>]
        Rest : string list
    }

[<ArgParser>]
type DuArgs =
    | FooCase of FooArgs
    | BarCase of BarArgs
"""

        // One namespace for the embedded runtime module, one for the generated parser module.
        List.length modules |> shouldEqual 2

    [<Test>]
    let ``Sinks in mutually exclusive cases may have different forms`` () =
        // The cases' sinks are addressable as --rest and --others respectively; at most one
        // case wins, so neither keyed form is ambiguous and this must generate successfully.
        let modules =
            generateFromSource
                """namespace TestMe

open WoofWare.Myriad.Plugins

type FooArgs =
    {
        Foo : int

        [<PositionalArgs>]
        Rest : int list
    }

type BarArgs =
    {
        Bar : int

        [<PositionalArgs>]
        Others : string list
    }

[<ArgParser>]
type DuArgs =
    | FooCase of FooArgs
    | BarCase of BarArgs
"""

        // One namespace for the embedded runtime module, one for the generated parser module.
        List.length modules |> shouldEqual 2

    [<Test>]
    let ``A sink's form still may not collide with a named argument across cases`` () =
        """namespace TestMe

open WoofWare.Myriad.Plugins

type FooArgs =
    {
        Foo : int

        [<PositionalArgs>]
        [<ArgumentLongForm "target">]
        Rest : int list
    }

type BarArgs =
    {
        Target : string
    }

[<ArgParser>]
type DuArgs =
    | FooCase of FooArgs
    | BarCase of BarArgs
"""
        |> shouldRejectWith
            "Conflicting argument names detected (names are matched case-insensitively):\nThe argument name '--target' is claimed by: '--target' (field 'Target'); '--target' (the positional args, field 'Rest')"

    /// A record holding a union-typed field and a positional sink, with the sink's
    /// [<PositionalArgs>] attribute rendered from the given text.
    let private modeAndPositionalsSource (positionalAttr : string) : string =
        sprintf
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

        [<%s>]
        Rest : string list
    }
"""
            positionalAttr

    [<Test>]
    let ``A Reject-mode positional sink is permitted alongside a union`` () =
        // Sound because bare tokens cannot influence case selection, and in Reject mode an
        // unrecognised `--key`-shaped token is still fatal rather than swallowed. Both the
        // default form and the explicit literal must be accepted.
        for attr in [ "PositionalArgs" ; "PositionalArgs false" ] do
            // One namespace for the embedded runtime module, one for the generated parser module.
            generateFromSource (modeAndPositionalsSource attr)
            |> List.length
            |> shouldEqual 2

    [<Test>]
    let ``A Collect-mode positional sink is rejected alongside a union`` () =
        // A Collect-mode sink treats an unrecognised `--key` as a positional arg, so a typo of a
        // case-selecting argument would be silently collected instead of reported — and with a
        // union in play, that can silently change which alternative is chosen.
        modeAndPositionalsSource "PositionalArgs true"
        |> shouldRejectWith
            "Positional args which collect unrecognised flag-like tokens ([<PositionalArgs true>]) cannot be combined with a discriminated-union arg: a mistyped case-selecting argument would be collected as a positional arg instead of being reported."

    [<Test>]
    let ``A positional sink whose flag-like setting cannot be proved Reject is rejected alongside a union`` () =
        // The attribute argument is a [<Literal>] constant, which the untyped AST does not
        // resolve: the generator cannot prove it is `false`, so it must be conservative.
        """namespace TestMe

[<AutoOpen>]
module Constants =
    [<Literal>]
    let GrabEverything = false

namespace TestMe

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

        [<PositionalArgs(GrabEverything)>]
        Rest : string list
    }
"""
        |> shouldRejectWith
            "Positional args combined with a discriminated-union arg must provably reject unrecognised flag-like tokens: use [<PositionalArgs>] or a literal [<PositionalArgs false>]."

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

    // ------------------------------------------------------------------------------------------
    // Recursive schemas. An argument schema must be a finite tree: a record or union which
    // refers to itself, even indirectly, would expand forever. Without an explicit check the
    // generator recurses until the process dies with a stack overflow instead of producing a
    // comprehensible error.

    [<Test>]
    let ``A record which contains itself is rejected`` () =
        """namespace TestMe

open WoofWare.Myriad.Plugins

[<ArgParser>]
type SelfRef =
    {
        Value : int
        Nested : SelfRef
    }
"""
        |> shouldRejectWith
            "The [<ArgParser>] schema is recursive: SelfRef -> SelfRef. Argument records and unions may not contain themselves, even indirectly."

    [<Test>]
    let ``Mutually recursive records are rejected`` () =
        """namespace TestMe

open WoofWare.Myriad.Plugins

type Inner =
    {
        A : int
        Outer : OuterRef
    }

[<ArgParser>]
type OuterRef =
    {
        B : int
        Inner : Inner
    }
"""
        |> shouldRejectWith
            "The [<ArgParser>] schema is recursive: OuterRef -> Inner -> OuterRef. Argument records and unions may not contain themselves, even indirectly."

    [<Test>]
    let ``A union whose case payload contains the union itself is rejected`` () =
        // The review counterexample: descending into the payload record re-enters the union,
        // and the Myriad subprocess used to die with a stack overflow (exit 134).
        """namespace TestMe

open WoofWare.Myriad.Plugins

type LoopArgs =
    {
        Foo : int
        Again : LoopDu
    }

[<ArgParser>]
type LoopDu =
    | Loop of LoopArgs
"""
        |> shouldRejectWith
            "The [<ArgParser>] schema is recursive: LoopDu -> LoopArgs -> LoopDu. Argument records and unions may not contain themselves, even indirectly."

    [<Test>]
    let ``A cycle which does not pass through the tagged type is rejected`` () =
        """namespace TestMe

open WoofWare.Myriad.Plugins

type ModePayload =
    {
        Level : int
        Fallback : Mode
    }

type Mode =
    | Auto of ModePayload

[<ArgParser>]
type TopArgs =
    {
        Verbose : bool
        Mode : Mode
    }
"""
        |> shouldRejectWith
            "The [<ArgParser>] schema is recursive: TopArgs -> Mode -> ModePayload -> Mode. Argument records and unions may not contain themselves, even indirectly."

    // ------------------------------------------------------------------------------------------
    // Qualified type references. A type defined alongside the tagged type is referred to by its
    // bare name; resolving ambient references by *last segment* instead of by the complete
    // reference would let a local type capture a qualified reference to a foreign type (e.g.
    // `System.Uri` alongside a local type named `Uri`), silently generating code which does not
    // compile.

    let private renderOrFail (modules : SynModuleOrNamespace list) : string =
        match Ast.render modules with
        | Some rendered -> rendered
        | None -> failwith "expected the generated modules to render"

    [<Test>]
    let ``A qualified reference is not captured by a local union with the same last segment`` () =
        // `Address : System.Uri` names the BCL type. If the structural union `Uri` captured it,
        // the generated parser would construct `Uri.CaseA ...` where a `System.Uri` is required.
        let rendered =
            generateFromSource
                """namespace TestMe

open WoofWare.Myriad.Plugins

type CaseARecord =
    {
        Alpha : int
    }

type Uri =
    | CaseA of CaseARecord

[<ArgParser>]
type TopLevel =
    {
        Address : System.Uri
        Count : int
    }
"""
            |> renderOrFail

        rendered.Contains "CaseA" |> shouldEqual false
        rendered.Contains "System.Uri" |> shouldEqual true

    [<Test>]
    let ``A qualified reference is not captured by a local record with the same last segment`` () =
        let rendered =
            generateFromSource
                """namespace TestMe

open WoofWare.Myriad.Plugins

type Uri =
    {
        Foo : int
    }

[<ArgParser>]
type TopLevel =
    {
        Address : System.Uri
    }
"""
            |> renderOrFail

        rendered.Contains "Foo" |> shouldEqual false
        rendered.Contains "System.Uri" |> shouldEqual true

    [<Test>]
    let ``A qualified case payload is not captured by a local record with the same last segment`` () =
        // `System.Uri` is not a record defined alongside the union, so this must be rejected
        // (rather than lowering the local record `Uri` and constructing `BadDu.Fetch { Foo = ... }`
        // where a `System.Uri` is required).
        """namespace TestMe

open WoofWare.Myriad.Plugins

type Uri =
    {
        Foo : int
    }

[<ArgParser>]
type BadDu =
    | Fetch of System.Uri
"""
        |> shouldRejectWith
            "Case Fetch of [<ArgParser>] union BadDu must have a payload which is a record defined alongside the union."

    [<Test>]
    let ``A qualified reference is not captured by a local flag DU with the same last segment`` () =
        // `External.Enabled` is some foreign type the generator cannot parse; treating it as the
        // local flag DU `Enabled` would emit `Enabled.On`/`Enabled.Off` into an
        // `External.Enabled` field. Rejection is the correct outcome.
        let exc =
            Assert.Throws<exn> (fun () ->
                generateFromSource
                    """namespace TestMe

open WoofWare.Myriad.Plugins

type Enabled =
    | [<ArgumentFlag true>] On
    | [<ArgumentFlag false>] Off

[<ArgParser>]
type FlagClash =
    {
        Mode : External.Enabled
    }
"""
                |> ignore<SynModuleOrNamespace list>
            )

        exc.Message.Contains "Could not decide how to parse" |> shouldEqual true

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

    // ------------------------------------------------------------------------------------------
    // Default-value attributes. `[<ArgumentDefaultFunction>]` and
    // `[<ArgumentDefaultEnvironmentVariable>]` are only meaningful on a `Choice<'a, 'a>` field: a
    // successful parse reports whether the value was user-supplied (Choice1Of2) or defaulted
    // (Choice2Of2). On a bare (non-Choice) field the default cannot be surfaced, so honouring it
    // silently would defeat the point; the generator used to accept the attribute and drop it,
    // leaving the field required. It must be rejected at generation time instead.

    [<Test>]
    let ``ArgumentDefaultFunction on a bare flag DU field is rejected`` () =
        // The motivating repro from the issue: DryRun is a flag DU, not Choice<DryRunMode, DryRunMode>.
        """namespace TestMe

open WoofWare.Myriad.Plugins

type DryRunMode =
    | [<ArgumentFlag false>] Wet
    | [<ArgumentFlag true>] Dry

[<ArgParser>]
type BareFlagDefault =
    {
        [<ArgumentDefaultFunction>]
        DryRun : DryRunMode
    }
"""
        |> shouldRejectWith (defaultAttrOnNonChoice "DryRun")

    [<Test>]
    let ``ArgumentDefaultEnvironmentVariable on a bare scalar field is rejected`` () =
        // The sibling default attribute shares the gap.
        """namespace TestMe

open WoofWare.Myriad.Plugins

[<ArgParser>]
type BareScalarDefault =
    {
        [<ArgumentDefaultEnvironmentVariable "MY_ENV_VAR">]
        Count : int
    }
"""
        |> shouldRejectWith (defaultAttrOnNonChoice "Count")

    [<Test>]
    let ``A default attribute on a record-typed field is rejected`` () =
        // Record- and union-typed fields never reach the Choice-parsing path, so the attribute
        // was dropped silently there too.
        """namespace TestMe

open WoofWare.Myriad.Plugins

type ChildRecord =
    {
        Thing : int
    }

[<ArgParser>]
type ParentRecord =
    {
        [<ArgumentDefaultFunction>]
        Child : ChildRecord
    }
"""
        |> shouldRejectWith (defaultAttrOnNonChoice "Child")

    [<Test>]
    let ``A default attribute on a positional field is rejected`` () =
        """namespace TestMe

open WoofWare.Myriad.Plugins

[<ArgParser>]
type PositionalDefault =
    {
        [<PositionalArgs>]
        [<ArgumentDefaultFunction>]
        Rest : string list
    }
"""
        |> shouldRejectWith (defaultAttrOnPositional "Rest")

    [<Test>]
    let ``ArgumentDefaultValue on a bare scalar field is rejected`` () =
        // The literal-default attribute inherits the Choice-only requirement of its siblings.
        """namespace TestMe

open WoofWare.Myriad.Plugins

[<ArgParser>]
type BareScalarLiteralDefault =
    {
        [<ArgumentDefaultValue 3>]
        Count : int
    }
"""
        |> shouldRejectWith (defaultAttrOnNonChoice "Count")

    [<Test>]
    let ``ArgumentDefaultValue on a positional field is rejected`` () =
        """namespace TestMe

open WoofWare.Myriad.Plugins

[<ArgParser>]
type PositionalLiteralDefault =
    {
        [<PositionalArgs>]
        [<ArgumentDefaultValue 3>]
        Rest : Choice<int, int> list
    }
"""
        |> shouldRejectWith (defaultAttrOnPositional "Rest")

    [<Test>]
    let ``A default attribute on a Choice field generates successfully`` () =
        // The valid form: the generator must still accept it.
        let modules =
            generateFromSource
                """namespace TestMe

open WoofWare.Myriad.Plugins

[<ArgParser>]
type WithDefault =
    {
        [<ArgumentDefaultFunction>]
        Count : Choice<int, int>
    }

    static member DefaultCount () = 4
"""

        // One namespace for the embedded runtime module, one for the generated parser module.
        List.length modules |> shouldEqual 2

    [<Test>]
    let ``ArgumentDefaultValue on a Choice field generates successfully`` () =
        let modules =
            generateFromSource
                """namespace TestMe

open WoofWare.Myriad.Plugins

[<ArgParser>]
type WithLiteralDefault =
    {
        [<ArgumentDefaultValue 4>]
        Count : Choice<int, int>
    }
"""

        // One namespace for the embedded runtime module, one for the generated parser module.
        List.length modules |> shouldEqual 2

    [<Test>]
    let ``ArgumentDefaultValue null generates successfully`` () =
        // `null` is a written-out literal like any other -- F# admits it as an object-valued
        // attribute argument -- so it must not fall through to the "unrecognised" rejection. It is
        // the one literal whose help text cannot be rendered by calling `ToString` on it.
        let modules =
            generateFromSource
                """namespace TestMe

open WoofWare.Myriad.Plugins

[<ArgParser>]
type WithNullDefault =
    {
        [<ArgumentDefaultValue null>]
        Missing : Choice<string, string>
    }
"""

        // One namespace for the embedded runtime module, one for the generated parser module.
        List.length modules |> shouldEqual 2

    /// F#'s context-sensitive constants: valid attribute arguments, but their value depends on
    /// where they are written.
    let sourceIdentifiers : TestCaseData list =
        [ "__LINE__" ; "__SOURCE_FILE__" ; "__SOURCE_DIRECTORY__" ]
        |> List.map TestCaseData

    [<TestCaseSource(nameof sourceIdentifiers)>]
    let ``ArgumentDefaultValue with a context-sensitive constant is rejected`` (constant : string) =
        // We splice the attribute's expression into the generated file, so one of these would be
        // evaluated *there* rather than at the user's own attribute: __SOURCE_FILE__ would name the
        // generated file. Worse, we emit the default in two places (the help text and the
        // defaulting step), so __LINE__ would advertise one default and store another.
        $"""namespace TestMe

open WoofWare.Myriad.Plugins

[<ArgParser>]
type ContextSensitiveDefault =
    {{
        [<ArgumentDefaultValue(%s{constant})>]
        Where : Choice<string, string>
    }}
"""
        |> shouldRejectWith (contextSensitiveDefault "Where" constant)

    [<Test>]
    let ``ArgumentDefaultValue sees through redundant parentheses`` () =
        // F#'s attribute syntax already requires one pair of parentheses around a non-literal
        // argument, and the user may add more; the check must not be fooled by the extra layer.
        """namespace TestMe

open WoofWare.Myriad.Plugins

[<ArgParser>]
type DoublyParenthesisedDefault =
    {
        [<ArgumentDefaultValue((__SOURCE_FILE__))>]
        Where : Choice<string, string>
    }
"""
        |> shouldRejectWith (contextSensitiveDefault "Where" "__SOURCE_FILE__")

    /// The two ways to name a constant rather than write one out.
    let identifierDefaults : TestCaseData list =
        [
            // A [<Literal>] binding, opened into scope.
            "Sentinel"
            // A qualified [<Literal>] binding, or an enum case.
            "Consts.Sentinel"
        ]
        |> List.map TestCaseData

    [<TestCaseSource(nameof identifierDefaults)>]
    let ``ArgumentDefaultValue naming a constant is rejected`` (name : string) =
        // The generated module hoists every `open` in the file above the parser, so a name resolves
        // there against a different set of bindings than at the attribute: a later `open` which
        // shadows an earlier one silently changes which constant the default means, and a file-local
        // module abbreviation is not in scope at all. We have no type checker, so we cannot tell
        // which binding was meant; refuse to guess.
        $"""namespace TestMe

open WoofWare.Myriad.Plugins

[<ArgParser>]
type NamedDefault =
    {{
        [<ArgumentDefaultValue(%s{name})>]
        Which : Choice<string, string>
    }}
"""
        |> shouldRejectWith (namedDefault "Which" name)

    [<Test>]
    let ``ArgumentDefaultValue with an unrecognised expression is rejected`` () =
        // We recognise the constant forms rather than hunting for bad ones, so an expression we
        // have not anticipated is refused instead of being spliced sight-unseen. (F# would reject
        // this attribute argument too, since it is not a compile-time constant.)
        """namespace TestMe

open WoofWare.Myriad.Plugins

[<ArgParser>]
type ComputedDefault =
    {
        [<ArgumentDefaultValue(3 + 4)>]
        Count : Choice<int, int>
    }
"""
        |> shouldRejectWith
            "Field 'Count' has an [<ArgumentDefaultValue>] whose value we do not recognise as a constant. We reproduce the value in the generated file rather than evaluating it at your attribute, so we accept only a literal written out in full (optionally parenthesised). Use [<ArgumentDefaultFunction>] for anything else: that function is evaluated in your own file."

    // Exactly one source may supply a field's default. With two, the generator would have to
    // invent a precedence order which is invisible at the use site, so it refuses instead.

    /// Every ordered pair drawn from the three default-supplying attributes, including each
    /// attribute with itself: two spellings of the same source are just as ambiguous as two
    /// different sources.
    let conflictingDefaultAttrs : TestCaseData list =
        let attrs =
            [
                "ArgumentDefaultFunction"
                "ArgumentDefaultValue 3"
                "ArgumentDefaultEnvironmentVariable \"MY_ENV_VAR\""
            ]

        attrs
        |> List.collect (fun a -> attrs |> List.map (fun b -> TestCaseData (a, b)))

    [<TestCaseSource(nameof conflictingDefaultAttrs)>]
    let ``Two default attributes on one field are rejected`` (first : string, second : string) =
        let source =
            [
                "namespace TestMe"
                ""
                "open WoofWare.Myriad.Plugins"
                ""
                "[<ArgParser>]"
                "type TwoDefaults ="
                "    {"
                $"        [<%s{first}>]"
                $"        [<%s{second}>]"
                "        Count : Choice<int, int>"
                "    }"
                ""
                "    static member DefaultCount () = 4"
                ""
            ]
            |> String.concat "\n"

        source
        |> shouldRejectWith
            "Expected Choice to be annotated with at most one ArgumentDefaultFunction or similar, but it was annotated with multiple. Field: Count"

    // ------------------------------------------------------------------------------------------
    // Data-free unions are argument *values*, spelled by case name (`--blah=a`), rather than sets
    // of alternative arguments: with no arguments to tell its cases apart, no command line could
    // select among them. The classification is by shape, so the shapes on either side of the
    // boundary must be rejected clearly.

    [<Test>]
    let ``A data-free union field generates successfully`` () =
        let modules =
            generateFromSource
                """namespace TestMe

open WoofWare.Myriad.Plugins

type FooDto =
    | A
    | B

[<ArgParser>]
type Args =
    {
        Blah : FooDto
    }
"""

        // One namespace for the embedded runtime module, one for the generated parser module.
        List.length modules |> shouldEqual 2

    [<Test>]
    let ``A data-free union may not be the tagged type`` () =
        """namespace TestMe

open WoofWare.Myriad.Plugins

[<ArgParser>]
type FooDto =
    | A
    | B
"""
        |> shouldRejectWith
            "No case of [<ArgParser>] union FooDto has any data, so it is an enumerated value rather than a set of alternative argument sets: an empty command line could not choose between its cases. Use it as the type of a field of an [<ArgParser>] record instead, where it is supplied as `--field-name=a`."

    [<Test>]
    let ``A data-free case alongside cases with payloads is rejected`` () =
        """namespace TestMe

open WoofWare.Myriad.Plugins

type FooArgs =
    {
        Foo : int
    }

[<ArgParser>]
type BadDu =
    | FooCase of FooArgs
    | BarCase
"""
        |> shouldRejectWith
            "Case BarCase of [<ArgParser>] union BadDu has no data. A union whose cases *all* have no data is parsed as an enumerated value, and a union of alternative argument sets needs a record payload on every case; a mixture of the two is not yet supported."

    [<Test>]
    let ``Enumerated case names which differ only by case are rejected`` () =
        // The generated parser matches values case-insensitively, exactly as the scanner matches
        // argument names, so `--blah=ab` would otherwise silently pick whichever case is first.
        """namespace TestMe

open WoofWare.Myriad.Plugins

type FooDto =
    | Ab
    | AB

[<ArgParser>]
type Args =
    {
        Blah : FooDto
    }
"""
        |> shouldRejectWith
            "Conflicting case names detected in the data-free union FooDto, whose cases are argument values (values are matched case-insensitively):\nThe value 'Ab' is claimed by cases: Ab; AB"

    [<Test>]
    let ``Case names the generated parser distinguishes are not collisions`` () =
        // OrdinalIgnoreCase, not ToUpperInvariant keying: "s" and "ſ" (long s) uppercase to
        // the same string but are distinct to the generated parser, so they do not collide.
        let modules =
            generateFromSource
                """namespace TestMe

open WoofWare.Myriad.Plugins

type FooDto =
    | S
    | ``ſ``

[<ArgParser>]
type Args =
    {
        Blah : FooDto
    }
"""

        List.length modules |> shouldEqual 2

    [<Test>]
    let ``ArgumentFlag on a union which does not have two cases is rejected`` () =
        // Without this check the attributes are silently ignored, and the union is parsed as an
        // enumerated value instead.
        """namespace TestMe

open WoofWare.Myriad.Plugins

type FooDto =
    | [<ArgumentFlag true>] A
    | [<ArgumentFlag false>] B
    | C

[<ArgParser>]
type Args =
    {
        Blah : FooDto
    }
"""
        |> shouldRejectWith
            "[<ArgumentFlag>] must be placed on both cases of a two-case discriminated union, with opposite argument values on each case."

    [<Test>]
    let ``Negation is not available on an enumerated field`` () =
        """namespace TestMe

open WoofWare.Myriad.Plugins

type FooDto =
    | A
    | B

[<ArgParser>]
type Args =
    {
        [<ArgumentNegateWithPrefix>]
        Blah : FooDto
    }
"""
        |> shouldRejectWith
            "[<ArgumentNegateWithPrefix>] can only be applied to boolean or flag DU fields, but was applied to field Blah of type FooDto"

    [<Test>]
    let ``A map without a key-value separator is rejected`` () =
        // There is no defensible default: which character is safe depends on what the key type
        // can spell, and guessing wrong silently misparses rather than failing.
        """namespace TestMe

open WoofWare.Myriad.Plugins

[<ArgParser>]
type Args =
    {
        Blah : Map<string, string>
    }
"""
        |> shouldRejectWith
            "Field 'Blah' has type map<string, string>, so it requires an [<ArgumentKeyValueSeparator>] attribute giving the character which separates a key from its value within one entry. There is no default: which separator is safe depends on what your keys can spell."

    [<Test>]
    let ``An entry separator without a key-value separator is rejected`` () =
        """namespace TestMe

open WoofWare.Myriad.Plugins

[<ArgParser>]
type Args =
    {
        [<ArgumentMapEntrySeparator ','>]
        Blah : Map<string, string>
    }
"""
        |> shouldRejectWith
            "Field 'Blah' has type map<string, string>, so it requires an [<ArgumentKeyValueSeparator>] attribute giving the character which separates a key from its value within one entry. There is no default: which separator is safe depends on what your keys can spell."

    [<Test>]
    let ``A key-value separator on a non-map field is rejected`` () =
        """namespace TestMe

open WoofWare.Myriad.Plugins

[<ArgParser>]
type Args =
    {
        [<ArgumentKeyValueSeparator ':'>]
        Blah : string
    }
"""
        |> shouldRejectWith
            "[<ArgumentKeyValueSeparator>] can only be applied to map fields, but was applied to field 'Blah' of type string. It controls how one entry of a map is split into a key and a value."

    [<Test>]
    let ``An entry separator on a non-map field is rejected`` () =
        """namespace TestMe

open WoofWare.Myriad.Plugins

[<ArgParser>]
type Args =
    {
        [<ArgumentKeyValueSeparator ':'>]
        [<ArgumentMapEntrySeparator ','>]
        Blah : string list
    }
"""
        |> shouldRejectWith
            "[<ArgumentKeyValueSeparator>] can only be applied to map fields, but was applied to field 'Blah' of type string list. It controls how one entry of a map is split into a key and a value."

    [<Test>]
    let ``The two separators must differ`` () =
        // Otherwise the entry split consumes every separator and no entry can ever contain one.
        """namespace TestMe

open WoofWare.Myriad.Plugins

[<ArgParser>]
type Args =
    {
        [<ArgumentKeyValueSeparator ':'>]
        [<ArgumentMapEntrySeparator ':'>]
        Blah : Map<string, string>
    }
"""
        |> shouldRejectWith
            "Field 'Blah' uses ':' as both its [<ArgumentKeyValueSeparator>] and its [<ArgumentMapEntrySeparator>]. They must differ, or no entry could be split into a key and a value."

    [<Test>]
    let ``An optional map is rejected`` () =
        """namespace TestMe

open WoofWare.Myriad.Plugins

[<ArgParser>]
type Args =
    {
        [<ArgumentKeyValueSeparator ':'>]
        Blah : Map<string, string> option
    }
"""
        |> shouldRejectWith
            "ArgParser does not support optional maps at field Blah: a map is already satisfiable with no arguments, so it is empty rather than absent."

    [<Test>]
    let ``A defaulted map is rejected`` () =
        """namespace TestMe

open WoofWare.Myriad.Plugins

[<ArgParser>]
type Args =
    {
        [<ArgumentKeyValueSeparator ':'>]
        Blah : Choice<Map<string, string>, Map<string, string>>
    }
"""
        |> shouldRejectWith
            "ArgParser does not support choices containing maps at field Blah: a map is already satisfiable with no arguments, so it is empty rather than defaulted."

    [<Test>]
    let ``A list of maps is rejected`` () =
        """namespace TestMe

open WoofWare.Myriad.Plugins

[<ArgParser>]
type Args =
    {
        [<ArgumentKeyValueSeparator ':'>]
        Blah : Map<string, string> list
    }
"""
        |> shouldRejectWith
            "ArgParser does not support lists of maps at field Blah: a map already accumulates across occurrences."

    [<Test>]
    let ``A map with a non-scalar value type is rejected`` () =
        """namespace TestMe

open WoofWare.Myriad.Plugins

[<ArgParser>]
type Args =
    {
        [<ArgumentKeyValueSeparator ':'>]
        Blah : Map<string, string list>
    }
"""
        |> shouldRejectWith
            "ArgParser does not support map values which are themselves lists, options, choices or maps, at field Blah: one entry spells one value."

    [<Test>]
    let ``A map with a non-scalar key type is rejected`` () =
        """namespace TestMe

open WoofWare.Myriad.Plugins

[<ArgParser>]
type Args =
    {
        [<ArgumentKeyValueSeparator ':'>]
        Blah : Map<string option, string>
    }
"""
        |> shouldRejectWith
            "ArgParser does not support map keys which are themselves lists, options, choices or maps, at field Blah: one entry spells one key."

    [<Test>]
    let ``A positional map is rejected`` () =
        """namespace TestMe

open WoofWare.Myriad.Plugins

[<ArgParser>]
type Args =
    {
        [<PositionalArgs>]
        [<ArgumentKeyValueSeparator ':'>]
        Blah : Map<string, string>
    }
"""
        |> shouldRejectWith "Expected positional arg accumulation type to be List, but it was map<string, string>"

    [<Test>]
    let ``A key-value separator on a nested-record field is rejected`` () =
        // The structural branches (a field whose type is an ambient record or union) are taken
        // before the leaf machinery runs, so they must reject these attributes themselves rather
        // than silently ignoring them.
        """namespace TestMe

open WoofWare.Myriad.Plugins

type ChildArgs =
    {
        Thing : int
    }

[<ArgParser>]
type Args =
    {
        [<ArgumentKeyValueSeparator ':'>]
        Child : ChildArgs
    }
"""
        |> shouldRejectWith
            "[<ArgumentKeyValueSeparator>] can only be applied to map fields, but was applied to field 'Child' of type ChildArgs. It controls how one entry of a map is split into a key and a value."

    [<Test>]
    let ``An entry separator on a union-typed field is rejected`` () =
        """namespace TestMe

open WoofWare.Myriad.Plugins

type FetchArgs =
    {
        Url : string
    }

type PushArgs =
    {
        Remote : string
    }

type Command =
    | Fetch of FetchArgs
    | Push of PushArgs

[<ArgParser>]
type Args =
    {
        [<ArgumentMapEntrySeparator ','>]
        Command : Command
    }
"""
        |> shouldRejectWith
            "[<ArgumentMapEntrySeparator>] can only be applied to map fields, but was applied to field 'Command' of type Command. It controls how one occurrence of a map is split into several entries."

    [<Test>]
    let ``Negation is not available on a map field`` () =
        // A map occurrence always carries an encoded entry, so it is never boolean-like however
        // its values are typed.
        """namespace TestMe

open WoofWare.Myriad.Plugins

[<ArgParser>]
type Args =
    {
        [<ArgumentKeyValueSeparator ':'>]
        [<ArgumentNegateWithPrefix>]
        Blah : Map<string, bool>
    }
"""
        |> shouldRejectWith
            "[<ArgumentNegateWithPrefix>] can only be applied to boolean or flag DU fields, but was applied to field Blah of type map<string, bool>"

    [<Test>]
    let ``A key whose enumerated case contains the separator is rejected`` () =
        // Case names are arbitrary identifiers, so a double-backtick name can contain the
        // separator. Such a key has no spelling on any command line.
        """namespace TestMe

open WoofWare.Myriad.Plugins

type Weird =
    | ``a:b``
    | Other

[<ArgParser>]
type Args =
    {
        [<ArgumentKeyValueSeparator ':'>]
        Blah : Map<Weird, string>
    }
"""
        |> shouldRejectWith
            "Field 'Blah' has map key type Weird, whose case 'a:b' cannot be spelled without using a separator (':'). No command line could express that key, so choose a different separator."

    [<Test>]
    let ``A key whose enumerated case contains the entry separator is rejected`` () =
        """namespace TestMe

open WoofWare.Myriad.Plugins

type Weird =
    | ``a,b``
    | Other

[<ArgParser>]
type Args =
    {
        [<ArgumentKeyValueSeparator ':'>]
        [<ArgumentMapEntrySeparator ','>]
        Blah : Map<Weird, string>
    }
"""
        |> shouldRejectWith
            "Field 'Blah' has map key type Weird, whose case 'a,b' cannot be spelled without using a separator (','). No command line could express that key, so choose a different separator."

    [<Test>]
    let ``A value whose enumerated case contains the entry separator is rejected`` () =
        // The entry separator is stripped before keys and values are split apart, so it
        // constrains values too -- unlike the key-value separator, which does not.
        """namespace TestMe

open WoofWare.Myriad.Plugins

type Weird =
    | ``a,b``
    | Other

[<ArgParser>]
type Args =
    {
        [<ArgumentKeyValueSeparator ':'>]
        [<ArgumentMapEntrySeparator ','>]
        Blah : Map<string, Weird>
    }
"""
        |> shouldRejectWith
            "Field 'Blah' has map value type Weird, whose case 'a,b' cannot be spelled without using a separator (','). No command line could express that value, so choose a different separator."

    [<Test>]
    let ``A value whose enumerated case contains the key-value separator is accepted`` () =
        // Splitting at the *first* key-value separator leaves values unconstrained by it.
        let modules =
            generateFromSource
                """namespace TestMe

open WoofWare.Myriad.Plugins

type Weird =
    | ``a:b``
    | Other

[<ArgParser>]
type Args =
    {
        [<ArgumentKeyValueSeparator ':'>]
        Blah : Map<string, Weird>
    }
"""

        List.length modules |> shouldEqual 2

    [<Test>]
    let ``A cased separator does not make an enumerated case unrepresentable`` () =
        // Enumerated values are matched with OrdinalIgnoreCase while the entry is split
        // ordinally, so a case named ``A`` may be spelled ``a``, which avoids the separator 'A'.
        // Only a separator which no spelling can avoid makes a case unrepresentable.
        let modules =
            generateFromSource
                """namespace TestMe

open WoofWare.Myriad.Plugins

type Weird =
    | A
    | Other

[<ArgParser>]
type Args =
    {
        [<ArgumentKeyValueSeparator 'A'>]
        Blah : Map<Weird, string>
    }
"""

        List.length modules |> shouldEqual 2

    [<Test>]
    let ``Separators which between them cover both spellings of a case are rejected`` () =
        // 'a' and 'A' are distinct separators, so the sole case name has no spelling avoiding
        // both of them.
        """namespace TestMe

open WoofWare.Myriad.Plugins

type Weird =
    | A
    | Other

[<ArgParser>]
type Args =
    {
        [<ArgumentKeyValueSeparator 'a'>]
        [<ArgumentMapEntrySeparator 'A'>]
        Blah : Map<Weird, string>
    }
"""
        |> shouldRejectWith
            "Field 'Blah' has map key type Weird, whose case 'A' cannot be spelled without using a separator ('a' or 'A'). No command line could express that key, so choose a different separator."

    [<Test>]
    let ``A separator which no alternative spelling escapes is rejected`` () =
        // Invariant casing would say this case has an alternative spelling: ToUpperInvariant of
        // ſ is 'S'. But the parser matches with OrdinalIgnoreCase, which holds ſ and 'S'
        // distinct, so ſ is the case's only spelling and the separator makes it unreachable.
        """namespace TestMe

open WoofWare.Myriad.Plugins

type Weird =
    | ``ſ``
    | Other

[<ArgParser>]
type Args =
    {
        [<ArgumentKeyValueSeparator 'ſ'>]
        Blah : Map<Weird, string>
    }
"""
        |> shouldRejectWith
            "Field 'Blah' has map key type Weird, whose case 'ſ' cannot be spelled without using a separator ('ſ'). No command line could express that key, so choose a different separator."

    [<Test>]
    let ``A separator some alternative spelling escapes is accepted`` () =
        // Both the lower and the upper case of Greek mu are separators here, so modelling the
        // accepted spellings with invariant casing would conclude this case is unreachable. But
        // OrdinalIgnoreCase also holds the micro sign equal to mu, and that is neither case of
        // it, so the key does have a spelling and generation must succeed.
        let modules =
            generateFromSource
                """namespace TestMe

open WoofWare.Myriad.Plugins

type Weird =
    | ``μ``
    | Other

[<ArgParser>]
type Args =
    {
        [<ArgumentKeyValueSeparator 'μ'>]
        [<ArgumentMapEntrySeparator 'Μ'>]
        Blah : Map<Weird, string>
    }
"""

        List.length modules |> shouldEqual 2

    [<Test>]
    let ``A surrogate separator is rejected`` () =
        // The low surrogate here is half of the case name's own encoding. Splitting on it would
        // cut a character in two, and the per-code-unit reasoning about which spellings are
        // available stops being valid, so refuse the separator outright.
        """namespace TestMe

open WoofWare.Myriad.Plugins

type Weird =
    | ``𐐀``
    | Other

[<ArgParser>]
type Args =
    {
        [<ArgumentKeyValueSeparator ':'>]
        [<ArgumentMapEntrySeparator '\uDC00'>]
        Blah : Map<Weird, string>
    }
"""
        |> shouldRejectWith
            "Field 'Blah' uses the unpaired surrogate U+DC00 as its [<ArgumentMapEntrySeparator>]. A separator must be a whole character: splitting on half of a surrogate pair would cut a character in two."

    [<Test>]
    let ``A supplementary character in a case name is not confused for a separator`` () =
        // The case name is a surrogate pair; an ordinary separator cannot occur inside it, because
        // a supplementary character encodes to surrogates only.
        let modules =
            generateFromSource
                """namespace TestMe

open WoofWare.Myriad.Plugins

type Weird =
    | ``𐐀``
    | Other

[<ArgParser>]
type Args =
    {
        [<ArgumentKeyValueSeparator ':'>]
        Blah : Map<Weird, string>
    }
"""

        List.length modules |> shouldEqual 2

    /// A field whose type is another argument record contributes that record's whole set of
    /// arguments, each named by its own field. There is no single argument for an
    /// [<ArgumentLongForm>] to rename, and the attribute was previously computed and then dropped
    /// on the floor when the structural branch was taken -- leaving an author who reached for it a
    /// parser with names they did not ask for and no indication why.
    let private longFormOnStructural (field : string) (ty : string) : string =
        $"Field '%s{field}' has an [<ArgumentLongForm>], but its type %s{ty} is an argument record or a discriminated union of alternative argument sets, so it contributes a whole set of arguments rather than one. [<ArgumentLongForm>] renames a single argument, and there is none here to rename: the names come from the fields of %s{ty} itself. Put the attribute on the field you mean to rename."

    [<Test>]
    let ``ArgumentLongForm on a sub-record field is rejected`` () =
        """namespace TestMe

open WoofWare.Myriad.Plugins

type Child =
    {
        Blah : int
    }

[<ArgParser>]
type Args =
    {
        [<ArgumentLongForm "renamed">]
        A : Child
    }
"""
        |> shouldRejectWith (longFormOnStructural "A" "Child")

    [<Test>]
    let ``ArgumentLongForm on a union-typed field is rejected`` () =
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

type Mode =
    | FooCase of FooArgs
    | BarCase of BarArgs

[<ArgParser>]
type Args =
    {
        [<ArgumentLongForm "renamed">]
        A : Mode
    }
"""
        |> shouldRejectWith (longFormOnStructural "A" "Mode")

    /// Several aliases are no more meaningful than one.
    [<Test>]
    let ``Several ArgumentLongForms on a sub-record field are rejected`` () =
        """namespace TestMe

open WoofWare.Myriad.Plugins

type Child =
    {
        Blah : int
    }

[<ArgParser>]
type Args =
    {
        [<ArgumentLongForm "renamed">]
        [<ArgumentLongForm "r">]
        A : Child
    }
"""
        |> shouldRejectWith (longFormOnStructural "A" "Child")

    /// The rejection is about the *field's* attribute, not about the child's: a sub-record whose
    /// own fields carry long forms is entirely ordinary.
    [<Test>]
    let ``ArgumentLongForm inside a sub-record is still accepted`` () =
        let modules =
            generateFromSource
                """namespace TestMe

open WoofWare.Myriad.Plugins

type Child =
    {
        [<ArgumentLongForm "renamed">]
        Blah : int
    }

[<ArgParser>]
type Args =
    {
        A : Child
    }
"""

        List.length modules |> shouldEqual 2

    /// [<ArgumentPrefix>] namespaces a whole subtree, so it is meaningful only on a field which
    /// *has* a subtree. Everywhere else it would be silently dropped, leaving the author with a
    /// parser whose argument names are not the ones they asked for.
    let private prefixOnLeaf (field : string) (ty : string) : string =
        $"[<ArgumentPrefix>] can only be applied to a field whose type is another [<ArgParser>]-schema record or a discriminated union of alternative argument sets, but was applied to field '%s{field}' of type %s{ty}. It renames every argument contributed by that field's subtree by prepending a namespace (e.g. [<ArgumentPrefix \"foo\">] on a field whose type is a record containing `Blah : string` turns --blah into --foo-blah); a leaf field has no subtree to rename. To change this one argument's name, use [<ArgumentLongForm>] instead."

    let private badPrefix (field : string) (prefix : string) : string =
        $"[<ArgumentPrefix>] on field '%s{field}' must be a non-empty string which does not contain '=' and does not start or end with '-' (the generated parser inserts the separating '-' itself), but got '%s{prefix}'. The prefix is used exactly as written, so spell it as you want it to appear on the command line, without the leading '--'."

    [<Test>]
    let ``ArgumentPrefix on a leaf field is rejected`` () =
        """namespace TestMe

open WoofWare.Myriad.Plugins

[<ArgParser>]
type Args =
    {
        [<ArgumentPrefix "foo">]
        A : int
    }
"""
        |> shouldRejectWith (prefixOnLeaf "A" "int32")

    [<Test>]
    let ``ArgumentPrefix on a Map field is rejected`` () =
        """namespace TestMe

open WoofWare.Myriad.Plugins

[<ArgParser>]
type Args =
    {
        [<ArgumentPrefix "foo">]
        [<ArgumentKeyValueSeparator ':'>]
        A : Map<string, string>
    }
"""
        |> shouldRejectWith (prefixOnLeaf "A" "map<string, string>")

    [<Test>]
    let ``ArgumentPrefix on a positional field is rejected`` () =
        """namespace TestMe

open WoofWare.Myriad.Plugins

[<ArgParser>]
type Args =
    {
        [<ArgumentPrefix "foo">]
        [<PositionalArgs>]
        A : string list
    }
"""
        |> shouldRejectWith
            "[<ArgumentPrefix>] was applied to field 'A', which carries [<PositionalArgs>]. A positional-args field has no subtree of nested arguments to namespace. If you want positional args nested under a prefix, move the [<PositionalArgs>] field into a sub-record and put the [<ArgumentPrefix>] on the record-typed field which holds it."

    /// The structural branches are taken before any leaf machinery runs, so a field which is both
    /// prefixed and positional must be caught before the dispatch: otherwise the record-typed case
    /// would prefix the subtree and drop the [<PositionalArgs>] without a word.
    [<Test>]
    let ``ArgumentPrefix beside PositionalArgs on a sub-record field is rejected`` () =
        """namespace TestMe

open WoofWare.Myriad.Plugins

type Child =
    {
        Blah : int
    }

[<ArgParser>]
type Args =
    {
        [<ArgumentPrefix "foo">]
        [<PositionalArgs>]
        A : Child
    }
"""
        |> shouldRejectWith
            "[<ArgumentPrefix>] was applied to field 'A', which carries [<PositionalArgs>]. A positional-args field has no subtree of nested arguments to namespace. If you want positional args nested under a prefix, move the [<PositionalArgs>] field into a sub-record and put the [<ArgumentPrefix>] on the record-typed field which holds it."

    [<Test>]
    let ``ArgumentPrefix on a union case is rejected`` () =
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
type Args =
    | [<ArgumentPrefix "foo">] FooCase of FooArgs
    | BarCase of BarArgs
"""
        |> shouldRejectWith
            "[<ArgumentPrefix>] was applied to case 'FooCase' of [<ArgParser>] union 'Args', but it belongs on a field. A union's cases are alternatives, so their argument names must already be distinct from one another, and a prefix here would buy no disambiguation. To namespace every case's arguments at once, put the [<ArgumentPrefix>] on the field whose type is 'Args'."

    [<Test>]
    let ``A non-literal ArgumentPrefix is rejected`` () =
        """namespace TestMe

open WoofWare.Myriad.Plugins

type Child =
    {
        Blah : int
    }

[<ArgParser>]
type Args =
    {
        [<ArgumentPrefix(SomeConstant)>]
        A : Child
    }
"""
        |> shouldRejectWith
            "[<ArgumentPrefix>] on field 'A' must be a string literal written out in full, but its value names something (SomeConstant) instead. The prefix is combined with every argument name in that field's subtree as the parser is generated, so it has to be known then; the generated file also hoists every `open` in your source above the parser, so a name need not resolve there to what it means here."

    /// The prefix is concatenated into every name in the subtree, so a prefix which no token could
    /// address makes the entire subtree unaddressable. Reject it at the prefix rather than letting
    /// the resulting names fail the ordinary name checks, where the reported name would be the
    /// concatenation and the author would have to work backwards to the cause.
    [<TestCase "">]
    [<TestCase "has=equals">]
    [<TestCase "-leading">]
    [<TestCase "trailing-">]
    [<TestCase "-">]
    let ``Malformed ArgumentPrefix values are rejected`` (prefix : string) =
        $"""namespace TestMe

open WoofWare.Myriad.Plugins

type Child =
    {{
        Blah : int
    }}

[<ArgParser>]
type Args =
    {{
        [<ArgumentPrefix "%s{prefix}">]
        A : Child
    }}
"""
        |> shouldRejectWith (badPrefix "A" prefix)

    /// The complement of the case-by-case rejections above: a prefix is accepted exactly when it
    /// is non-empty, contains no '=', and has no edge dash. Searching the space beats enumerating
    /// it, because the interesting failures are the ones neither of us thought to write down.
    [<Test>]
    let ``A prefix is accepted exactly when it is well-formed`` () =
        let source (prefix : string) : string =
            $"""namespace TestMe

open WoofWare.Myriad.Plugins

type Child =
    {{
        Blah : int
    }}

[<ArgParser>]
type Args =
    {{
        [<ArgumentPrefix "%s{prefix}">]
        A : Child
    }}
"""

        // Restricted to characters which survive being written into F# source unescaped: the
        // property is about the generator's validation, not about F# string literals.
        let prefixChar =
            Gen.elements ([ 'a' .. 'e' ] @ [ 'A' ; 'B' ] @ [ '-' ; '=' ; '_' ; '.' ; '0' ])

        let prefixes =
            Gen.listOf prefixChar
            |> Gen.map (fun cs -> System.String (Array.ofList cs))
            |> Arb.fromGen

        Prop.forAll
            prefixes
            (fun prefix ->
                let wellFormed =
                    prefix <> ""
                    && not (prefix.Contains "=")
                    && not (prefix.StartsWith ("-", System.StringComparison.Ordinal))
                    && not (prefix.EndsWith ("-", System.StringComparison.Ordinal))

                if wellFormed then
                    generateFromSource (source prefix) |> List.length = 2
                else
                    let exc =
                        Assert.Throws<exn> (fun () ->
                            generateFromSource (source prefix) |> ignore<SynModuleOrNamespace list>
                        )

                    exc.Message = badPrefix "A" prefix
            )
        |> Check.QuickThrowOnFailure

    /// The point of the feature: two copies of one sub-record, which collide without prefixes, are
    /// accepted with distinct ones.
    [<Test>]
    let ``The same sub-record may be embedded twice under distinct prefixes`` () =
        let modules =
            generateFromSource
                """namespace TestMe

open WoofWare.Myriad.Plugins

type Endpoint =
    {
        Host : string
        Port : int
    }

[<ArgParser>]
type Args =
    {
        [<ArgumentPrefix "src">]
        Source : Endpoint
        [<ArgumentPrefix "dst">]
        Dest : Endpoint
    }
"""

        List.length modules |> shouldEqual 2

    /// ... and the prefixed names still take part in the ordinary conflict detection rather than
    /// bypassing it: equal prefixes collide exactly as bare names do.
    [<Test>]
    let ``Two copies of a sub-record under the same prefix still conflict`` () =
        """namespace TestMe

open WoofWare.Myriad.Plugins

type Endpoint =
    {
        Host : string
    }

[<ArgParser>]
type Args =
    {
        [<ArgumentPrefix "same">]
        Source : Endpoint
        [<ArgumentPrefix "same">]
        Dest : Endpoint
    }
"""
        |> shouldRejectWith
            "Conflicting argument names detected (names are matched case-insensitively):\nThe argument name '--same-host' is claimed by: '--same-host' (field 'Host'); '--same-host' (field 'Host')"

    /// The generation-time name checks compare under the scanner's own case-insensitive equality,
    /// so a prefixed name must reach them as its semantic spelling and not as a rendering of it:
    /// `é` and `É` differ exactly where `é` and `É` collide, so a schema which is
    /// broken at runtime would otherwise sail through generation.
    [<Test>]
    let ``A non-ASCII collision between prefixes differing only by case is detected`` () =
        """namespace TestMe

open WoofWare.Myriad.Plugins

type Endpoint =
    {
        Host : string
    }

[<ArgParser>]
type Args =
    {
        [<ArgumentPrefix "é">]
        Source : Endpoint
        [<ArgumentPrefix "É">]
        Dest : Endpoint
    }
"""
        |> shouldRejectWith
            "Conflicting argument names detected (names are matched case-insensitively):\nThe argument name '--é-host' is claimed by: '--é-host' (field 'Host'); '--É-host' (field 'Host')"

    /// A prefix can manufacture a collision which is present in neither subtree alone.
    [<Test>]
    let ``A prefixed name colliding with a sibling's bare name is rejected`` () =
        """namespace TestMe

open WoofWare.Myriad.Plugins

type Child =
    {
        Bar : int
    }

[<ArgParser>]
type Args =
    {
        [<ArgumentPrefix "foo">]
        A : Child
        FooBar : int
    }
"""
        |> shouldRejectWith
            "Conflicting argument names detected (names are matched case-insensitively):\nThe argument name '--foo-bar' is claimed by: '--foo-bar' (field 'Bar'); '--foo-bar' (field 'FooBar')"

    /// The reserved-name check sees the prefixed name, not the bare one: `--help` is reserved, but
    /// `--foo-help` is an ordinary name.
    [<Test>]
    let ``A prefix rescues an otherwise-reserved name`` () =
        let modules =
            generateFromSource
                """namespace TestMe

open WoofWare.Myriad.Plugins

type Child =
    {
        Help : int
    }

[<ArgParser>]
type Args =
    {
        [<ArgumentPrefix "foo">]
        A : Child
    }
"""

        List.length modules |> shouldEqual 2

    /// Prefixing does not disturb the recursion guard: a self-referential schema still fails as
    /// such, rather than looping while it concatenates prefixes.
    [<Test>]
    let ``A prefixed recursive schema is still rejected as recursive`` () =
        """namespace TestMe

open WoofWare.Myriad.Plugins

[<ArgParser>]
type Args =
    {
        [<ArgumentPrefix "foo">]
        A : Args
    }
"""
        |> shouldRejectWith
            "The [<ArgParser>] schema is recursive: Args -> Args. Argument records and unions may not contain themselves, even indirectly."
