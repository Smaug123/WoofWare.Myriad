namespace ConsumePlugin

open WoofWare.Myriad.Plugins

type PrefixedChild =
    {
        Thing1 : int
        Thing2 : string
    }

/// The basic case: the child's arguments are namespaced, so `--thing1` becomes `--foo-thing1`.
[<ArgParser true>]
type PrefixedParent =
    {
        [<ArgumentPrefix "foo">]
        Child : PrefixedChild
        AndAnother : bool
    }

type Endpoint =
    {
        Host : string
        Port : int
    }

/// The motivating example: the same sub-record embedded twice. Without distinct prefixes the two
/// copies' arguments would collide and generation would fail.
[<ArgParser true>]
type Transfer =
    {
        [<ArgumentPrefix "src">]
        Source : Endpoint

        [<ArgumentPrefix "dst">]
        Dest : Endpoint
    }

type PrefixedGrandchild =
    {
        Leaf : int
    }

type PrefixedMiddle =
    {
        [<ArgumentPrefix "inner">]
        Grandchild : PrefixedGrandchild

        /// A sibling of the prefixed field, to check that the inner prefix does not leak sideways.
        Sibling : int
    }

/// Prefixes compose from the outside in: `Leaf` is spelled `--outer-inner-leaf`, and `Sibling`,
/// which carries no prefix of its own, still inherits the outer one as `--outer-sibling`.
[<ArgParser true>]
type PrefixedNested =
    {
        [<ArgumentPrefix "outer">]
        Middle : PrefixedMiddle
    }

/// The other side of the composition-associativity check: one prefix spelled with the separator
/// written into it must give exactly the arguments that two nested prefixes give.
[<ArgParser true>]
type PrefixedFlattened =
    {
        [<ArgumentPrefix "outer-inner">]
        Grandchild : PrefixedGrandchild
    }

type UnprefixedMiddle =
    {
        Grandchild : PrefixedGrandchild
        Sibling : int
    }

/// An unprefixed sub-record beneath a prefixed one still has the outer prefix applied to its whole
/// subtree: `--outer-leaf` and `--outer-sibling`.
[<ArgParser true>]
type PrefixedThroughUnprefixed =
    {
        [<ArgumentPrefix "outer">]
        Middle : UnprefixedMiddle
    }

type ChildWithLongForms =
    {
        [<ArgumentLongForm "renamed">]
        [<ArgumentLongForm "r">]
        Original : int
    }

/// The prefix applies to explicit [<ArgumentLongForm>] aliases too, so the field is spelled
/// `--pre-renamed` or `--pre-r`, and never `--renamed`.
[<ArgParser true>]
type PrefixedLongForms =
    {
        [<ArgumentPrefix "pre">]
        Child : ChildWithLongForms
    }

type ChildWithNegation =
    {
        [<ArgumentNegateWithPrefix>]
        EnableFeature : bool
    }

/// Negation composes outside the prefix: `--no-flags-enable-feature`.
[<ArgParser true>]
type PrefixedNegation =
    {
        [<ArgumentPrefix "flags">]
        Child : ChildWithNegation
    }

type ChildWithPositionals =
    {
        Thing1 : int

        [<PositionalArgs>]
        Rest : string list
    }

/// A positional sink inside a prefixed sub-record keeps its keyed alias, which is prefixed like
/// any other name: `--pos-rest=x` addresses the sink explicitly.
[<ArgParser true>]
type PrefixedPositionals =
    {
        [<ArgumentPrefix "pos">]
        Child : ChildWithPositionals
    }

type PrefixedMapChild =
    {
        [<ArgumentKeyValueSeparator ':'>]
        [<ArgumentMapEntrySeparator ','>]
        Entries : Map<string, string>
    }

/// The key/value and entry separators govern how one occurrence's payload is split, which is
/// orthogonal to the outer name: `--m-entries=a:b,c:d`.
[<ArgParser true>]
type PrefixedMap =
    {
        [<ArgumentPrefix "m">]
        Child : PrefixedMapChild
    }

type PrefixedAutoMode =
    {
        Quiet : bool option
    }

type PrefixedManualMode =
    {
        Level : int
    }

type PrefixedMode =
    | Auto of PrefixedAutoMode
    | Manual of PrefixedManualMode

/// A field whose type is a union of alternative argument sets: every case's arguments are
/// namespaced, so `--level` becomes `--mode-level`.
[<ArgParser true>]
type PrefixedUnion =
    {
        [<ArgumentPrefix "mode">]
        Mode : PrefixedMode
    }
