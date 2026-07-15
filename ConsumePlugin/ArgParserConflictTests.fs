namespace ConsumePlugin

open WoofWare.Myriad.Plugins

// This file contains test cases for conflict detection in the ArgParser generator.
// These are expected to FAIL at build time with appropriate error messages.
// Uncomment each section one at a time to test the specific conflict detection.
//
// These cases (and case-insensitive variants of them) are asserted automatically in
// WoofWare.Myriad.Plugins/Test/TestArgParserRejection.fs, which drives the generator over
// in-memory source; the copies here are kept for manual end-to-end confirmation that a real
// build fails.

// ============================================================================
// Test 1: Field named NoFooBar conflicts with FooBar's --no- variant
// ============================================================================
// Expected error: The argument name '--no-foo-bar' is claimed by: the --no- variant of field
// 'FooBar' (which has [<ArgumentNegateWithPrefix>]); '--no-foo-bar' (field 'NoFooBar')

(*
[<ArgParser>]
type ConflictingFieldNames =
    {
        [<ArgumentNegateWithPrefix>]
        FooBar : bool
        NoFooBar : bool
    }
*)

// ============================================================================
// Test 2: ArgumentLongForm "no-foo" conflicts with Foo's --no- variant
// ============================================================================
// Expected error: The argument name '--no-foo' is claimed by: the --no- variant of field
// 'Foo' (which has [<ArgumentNegateWithPrefix>]); '--no-foo' (field 'Bar')

(*
[<ArgParser>]
type ConflictingLongForm =
    {
        [<ArgumentNegateWithPrefix>]
        Foo : bool
        [<ArgumentLongForm "no-foo">]
        Bar : bool
    }
*)

// ============================================================================
// Test 3: Multiple ArgumentLongForm, one conflicts
// ============================================================================
// Expected error: The argument name '--no-verbose' is claimed by: the --no- variant of field
// 'VerboseMode' (which has [<ArgumentNegateWithPrefix>]); '--no-verbose' (field 'Quiet')

(*
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
*)

// ============================================================================
// Test 4: ArgumentNegateWithPrefix on non-boolean field
// ============================================================================
// Expected error: [<ArgumentNegateWithPrefix>] can only be applied to boolean
// or flag DU fields, but was applied to field NotABool of type string

(*
[<ArgParser>]
type InvalidAttributeOnNonBool =
    {
        [<ArgumentNegateWithPrefix>]
        NotABool : string
    }
*)

// ============================================================================
// Test 5: ArgumentNegateWithPrefix on non-flag int field
// ============================================================================
// Expected error: [<ArgumentNegateWithPrefix>] can only be applied to boolean
// or flag DU fields

(*
[<ArgParser>]
type InvalidAttributeOnInt =
    {
        [<ArgumentNegateWithPrefix>]
        NotAFlag : int
    }
*)

// ============================================================================
// Test 6: Complex conflict with custom names
// ============================================================================
// This tests a more complex scenario where a custom ArgumentLongForm creates
// a conflict with a different field's negated form

(*
[<ArgParser>]
type ComplexConflict =
    {
        [<ArgumentLongForm "enable">]
        [<ArgumentNegateWithPrefix>]
        FeatureA : bool

        [<ArgumentLongForm "no-enable">]
        DisableAll : bool
    }
*)

// ============================================================================
// Test 7: Valid usage - no conflicts (this SHOULD compile)
// ============================================================================

[<ArgParser>]
type NoConflict =
    {
        [<ArgumentNegateWithPrefix>]
        EnableFeature : bool

        [<ArgumentNegateWithPrefix>]
        VerboseMode : bool

        NormalField : string
    }
