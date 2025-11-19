namespace ConsumePlugin

open WoofWare.Myriad.Plugins

// This file contains test cases for conflict detection in the ArgParser generator.
// These are expected to FAIL at build time with appropriate error messages.
// Uncomment each section one at a time to test the specific conflict detection.

// ============================================================================
// Test 1: Field named NoFooBar conflicts with FooBar's --no- variant
// ============================================================================
// Expected error: Argument name conflict: '--no-foo-bar' collides with the --no- variant
// of field 'FooBar' (which has [<ArgumentNegateWithPrefix>])

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
// Expected error: Argument name conflict: '--no-foo' collides with the --no- variant
// of field 'Foo' (which has [<ArgumentNegateWithPrefix>])

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
// Expected error: Argument name conflict: '--no-verbose' collides with...

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
