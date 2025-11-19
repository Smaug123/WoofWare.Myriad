namespace ConsumePlugin

open WoofWare.Myriad.Plugins

// Test types for ArgumentNegateWithPrefix functionality

type TestDryRunMode =
    | [<ArgumentFlag false>] Wet
    | [<ArgumentFlag true>] Dry

[<ArgParser true>]
type BoolNegation =
    {
        [<ArgumentNegateWithPrefix>]
        EnableFeature : bool
    }

[<ArgParser true>]
type FlagNegation =
    {
        [<ArgumentNegateWithPrefix>]
        DryRun : TestDryRunMode
    }

[<ArgParser true>]
type MultipleFormsNegation =
    {
        [<ArgumentLongForm "verbose">]
        [<ArgumentLongForm "v">]
        [<ArgumentNegateWithPrefix>]
        VerboseMode : bool
    }

[<ArgParser true>]
type CombinedFeatures =
    {
        [<ArgumentNegateWithPrefix>]
        [<ArgumentDefaultFunction>]
        Verbose : Choice<bool, bool>

        [<ArgumentNegateWithPrefix>]
        [<ArgumentHelpText "Enable debug mode">]
        Debug : bool

        NormalBool : bool
    }

    static member DefaultVerbose () = false
