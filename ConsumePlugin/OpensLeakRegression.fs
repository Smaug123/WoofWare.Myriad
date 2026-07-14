// Regression test for the opens leak: `open` declarations from one namespace block of an input
// file must not appear in generated code for a *different* namespace block. The bait below is a
// relative `open RelativeOpenBait`, which resolves only from within ConsumePlugin.OpensLeak; if
// it leaked into the generated module for ConsumePlugin.OpensLeakVictim, this project would fail
// to compile.
namespace ConsumePlugin.OpensLeak

module internal RelativeOpenBait =
    let internal bait : int = 1

namespace ConsumePlugin.OpensLeak

open RelativeOpenBait

type UsesTheBait =
    {
        Ignored : int
    }

    static member internal Use () : int = bait

namespace ConsumePlugin.OpensLeakVictim

open WoofWare.Myriad.Plugins

[<ArgParser>]
type LeakArgs =
    {
        Foo : int
    }
