namespace WoofWare.Myriad.Plugins

open System

/// Attribute indicating a record type to which the "build arg parser" Myriad
/// generator should apply during build.
///
/// If you supply isExtensionMethod = true, you will get extension methods.
/// These can only be consumed from F#, but the benefit is that they don't use up the module name
/// (since by default we create a module called "{TypeName}").
type ArgParserAttribute (isExtensionMethod : bool) =
    inherit Attribute ()

    /// The default value of `isExtensionMethod`, the optional argument to the ArgParserAttribute constructor.
    static member DefaultIsExtensionMethod = false

    /// Shorthand for the "isExtensionMethod = false" constructor; see documentation there for details.
    new () = ArgParserAttribute ArgParserAttribute.DefaultIsExtensionMethod

/// Attribute indicating that this field shall accumulate all unmatched args,
/// as well as any that appear after a bare `--`.
///
/// Set `includeFlagLike = true` to include args that begin `--` in the
/// positional args.
/// (By default, `includeFlagLike = false` and we throw when encountering
/// an argument which looks like a flag but which we don't recognise.)
/// We will still interpret `--help` as requesting help, unless it comes after
/// a standalone `--` separator.
///
/// If the type of the PositionalArgs field is `Choice<'a, 'a>`, then we will
/// tell you whether each arg came before or after a standalone `--` separator.
/// For example, `MyApp foo bar -- baz` with PositionalArgs of `Choice<string, string>`
/// would yield `Choice1Of2 foo, Choice1Of2 bar, Choice2Of2 baz`.
type PositionalArgsAttribute (includeFlagLike : bool) =
    inherit Attribute ()

    /// The default value of `isExtensionMethod`, the optional argument to the ArgParserAttribute constructor.
    static member DefaultIncludeFlagLike = false

    /// Shorthand for the "includeFlagLike = false" constructor; see documentation there for details.
    new () = PositionalArgsAttribute PositionalArgsAttribute.DefaultIncludeFlagLike

/// Attribute indicating that this field shall have a default value derived
/// from calling an appropriately named static method on the type.
///
/// This attribute can only be placed on fields of type `Choice<_, _>` where both type parameters
/// are the same.
/// After a successful parse, the value is Choice1Of2 if the user supplied an input,
/// or Choice2Of2 if the input was obtained by calling the default function.
///
/// The static method we call for field `FieldName : 'a` is `DefaultFieldName : unit -> 'a`.
type ArgumentDefaultFunctionAttribute () =
    inherit Attribute ()

/// Attribute indicating that this field shall have a default value derived
/// from an environment variable (whose name you give in the attribute constructor).
///
/// This attribute can only be placed on fields of type `Choice<_, _>` where both type parameters
/// are the same.
/// After a successful parse, the value is Choice1Of2 if the user supplied an input,
/// or Choice2Of2 if the input was obtained by pulling a value from `Environment.GetEnvironmentVariable`.
type ArgumentDefaultEnvironmentVariableAttribute (envVar : string) =
    inherit Attribute ()

/// Attribute indicating that this field shall have the given constant default value.
/// This is shorthand for `[<ArgumentDefaultFunction>]` in the common case where the default function
/// would simply return a constant.
///
/// This attribute can only be placed on fields of type `Choice<_, _>` where both type parameters
/// are the same.
/// After a successful parse, the value is Choice1Of2 if the user supplied an input,
/// or Choice2Of2 if the input was this constant.
///
/// The value you supply is reproduced in the generated code, so it must have the field's
/// element type: `[<ArgumentDefaultValue 3>]` on a `Choice<int, int>`, but
/// `[<ArgumentDefaultValue 3L>]` on a `Choice<int64, int64>`. A mismatch is a compile error in
/// the generated file rather than at the attribute itself.
///
/// The value must be a literal written out in full. We reproduce it in the generated file rather
/// than evaluating it at your attribute, so anything whose meaning depends on where it is written
/// is rejected:
///
/// * A name standing for a constant (a `[<Literal>]` binding, an enum case) is rejected. The
///   generated file hoists every `open` in your source above the parser, so a name need not resolve
///   to the same binding there as here -- a later `open` which shadows an earlier one would silently
///   change the default's value.
/// * F#'s context-sensitive constants (`__LINE__`, `__SOURCE_FILE__`, `__SOURCE_DIRECTORY__`) are
///   rejected: they would be resolved against the generated file instead of yours.
///
/// Use `[<ArgumentDefaultFunction>]` for any of those, and for anything which is not a constant at
/// all (in particular a discriminated union case, such as a flag DU's): that function is evaluated
/// in your own file, so it means what you wrote.
///
/// `null` is a literal like any other here. Note though that F# will only let you write it if your
/// own project is not checking nullness (an `obj`-typed attribute argument does not admit null when
/// it is), and that the `FS3559` warning, if you have turned it on, fires on the attribute too.
type ArgumentDefaultValueAttribute (defaultValue : obj) =
    inherit Attribute ()

/// Attribute indicating that this field or type shall have the given help text, when `--help` is invoked
/// or when a parse error causes us to print help text.
/// When applied to a record type, the help text appears at the top of the help output, before the field descriptions.
/// When applied to a field, the help text appears next to that field's description.
type ArgumentHelpTextAttribute (helpText : string) =
    inherit Attribute ()

/// Attribute indicating that this field should be parsed with a ParseExact method on its type.
/// For example, on a TimeSpan field, with [<ArgumentParseExact @"hh\:mm\:ss">], we will call
/// `TimeSpan.ParseExact (s, @"hh\:mm\:ss", CultureInfo.CurrentCulture).
type ParseExactAttribute (format : string) =
    inherit Attribute ()

/// Attribute indicating that this field should be parsed in the invariant culture, rather than the
/// default current culture.
/// For example, on a TimeSpan field, with [<InvariantCulture>] and [<ArgumentParseExact @"hh\:mm\:ss">], we will call
/// `TimeSpan.ParseExact (s, @"hh\:mm\:ss", CultureInfo.InvariantCulture).
type InvariantCultureAttribute () =
    inherit Attribute ()

/// Attribute placed on a field of a two-case no-data discriminated union, indicating that this is "basically a bool".
/// For example: `type DryRun = | [<ArgumentFlag true>] Dry | [<ArgumentFlag false>] Wet`
/// A record with `{ DryRun : DryRun }` will then be parsed like `{ DryRun : bool }` (so the user supplies `--dry-run`),
/// but that you get this strongly-typed value directly in the code (so you `match args.DryRun with | DryRun.Dry ...`).
///
/// You must put this attribute on both cases of the discriminated union, with opposite values in each case.
type ArgumentFlagAttribute (flagValue : bool) =
    inherit Attribute ()

/// Attribute placed on a field of a record to specify a different long form from the default. If you place this
/// attribute, you won't get the default: ArgFoo would normally be expressed as `--arg-foo`, but if you instead
/// say `[<ArgumentLongForm "thingy-blah">]` or `[<ArgumentLongForm "thingy">]`, you instead use `--thingy-blah`
/// or `--thingy` respectively.
///
/// You can place this argument multiple times.
///
/// Omit the initial `--` that you expect the user to type.
[<AttributeUsage(AttributeTargets.Field, AllowMultiple = true)>]
type ArgumentLongForm (s : string) =
    inherit Attribute ()

/// Attribute specifying the character which separates a key from its value within one entry of a
/// `Map`-typed field. This attribute is mandatory on `Map` fields, and is rejected anywhere else:
/// there is no default, because the right separator depends on what the key and value can spell.
///
/// For example, `[<ArgumentKeyValueSeparator ':'>] MyMap : Map<string, string>` accepts
/// `--my-map=foo:bar`, giving the single entry `"foo" => "bar"`. The field may be supplied
/// repeatedly, and the entries are combined; supplying the same key twice is an error.
///
/// The entry is split at the *first* occurrence of the separator, so a value may itself contain
/// the separator (`--my-map=foo:bar:baz` gives `"foo" => "bar:baz"`), but a key may not. Every
/// `Map` whose keys avoid the separator is therefore expressible. Choose a separator which cannot
/// appear in a key: for `int` or data-free-union keys any punctuation will do, but note that e.g.
/// `TimeSpan` spells itself with colons and `Guid` with hyphens.
///
/// If you need to express keys which contain every character, take a `string list` field and do
/// the splitting yourself.
[<AttributeUsage(AttributeTargets.Field, AllowMultiple = false)>]
type ArgumentKeyValueSeparatorAttribute (separator : char) =
    inherit Attribute ()

/// Attribute specifying the character which separates one entry from the next within a single
/// occurrence of a `Map`-typed field. This attribute is optional, and is rejected on any field
/// which is not a `Map`.
///
/// For example, `[<ArgumentKeyValueSeparator ':'>] [<ArgumentMapEntrySeparator ','>]` accepts
/// `--my-map=foo:bar,quux:baz`, giving two entries from one occurrence. Without this attribute,
/// each occurrence carries exactly one entry, so the user must write
/// `--my-map=foo:bar --my-map=quux:baz`.
///
/// Supplying this attribute costs expressiveness: the entry separator is stripped out before
/// keys and values are split apart, so neither a key nor a value may contain it. (Without it,
/// only keys are constrained, by the key-value separator.) Omit it unless the terser command
/// line is worth that.
[<AttributeUsage(AttributeTargets.Field, AllowMultiple = false)>]
type ArgumentMapEntrySeparatorAttribute (separator : char) =
    inherit Attribute ()

/// Attribute indicating that this boolean or flag field should accept `--no-` prefix for negation.
/// When this attribute is present on a boolean or flag DU field, the generated parser will accept
/// both --field-name and --no-field-name as argument forms.
///
/// For boolean fields:
///   --field-name (or --field-name=true) sets the value to true
///   --no-field-name (or --no-field-name=true) sets the value to false
///   --field-name=false sets the value to false
///   --no-field-name=false sets the value to true
///
/// For flag DU fields with [<ArgumentFlag>]:
///   --field-name (or --field-name=true) sets to the case marked with [<ArgumentFlag true>]
///   --no-field-name (or --no-field-name=true) sets to the case marked with [<ArgumentFlag false>]
///   --field-name=false sets to the [<ArgumentFlag false>] case
///   --no-field-name=false sets to the [<ArgumentFlag true>] case
///
/// This attribute can only be applied to bool fields or flag DU fields (two-case DUs with [<ArgumentFlag>]).
[<AttributeUsage(AttributeTargets.Field, AllowMultiple = false)>]
type ArgumentNegateWithPrefixAttribute () =
    inherit Attribute ()
