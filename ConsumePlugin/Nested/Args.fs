namespace ConsumePlugin

open WoofWare.Myriad.Plugins

/// Regression tests, two in one.
/// This file shares its base name with Args.fs and generates parsers into the same namespace:
/// were the embedded runtime module named after the input file, the two generated files would
/// both define it and fail to compile (FS0248). And its positional sink has two long forms,
/// both of which must reach the sink in keyed syntax.
[<ArgParser>]
type SameBaseNameArgs =
    {
        Value : int
        [<ArgumentLongForm "rest">]
        [<ArgumentLongForm "others">]
        [<PositionalArgs>]
        Rest : string list
    }
