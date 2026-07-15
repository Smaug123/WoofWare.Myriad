namespace WoofWare.Myriad.Plugins.Test

open Fantomas.FCS.Syntax
open NUnit.Framework
open FsUnitTyped
open WoofWare.Whippet.Fantomas
open WoofWare.Myriad.Plugins

/// Generation-time rejection tests: sources which the ArgParser generator must refuse to
/// process, with comprehensible messages. These drive the full generator pipeline over
/// in-memory source, so rejection is asserted automatically; the corresponding cases in
/// ConsumePlugin/ArgParserConflictTests.fs are commented out precisely because a rejected
/// source cannot take part in a build.
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
        // ConsumePlugin/ArgParserConflictTests.fs, test 1.
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
        // ConsumePlugin/ArgParserConflictTests.fs, test 2.
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
        // ConsumePlugin/ArgParserConflictTests.fs, test 3.
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
        // ConsumePlugin/ArgParserConflictTests.fs, test 6.
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
        // ConsumePlugin/ArgParserConflictTests.fs, tests 4 and 5.
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
        // ConsumePlugin/ArgParserConflictTests.fs, test 7: this must generate successfully.
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
