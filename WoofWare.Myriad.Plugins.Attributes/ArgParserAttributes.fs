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
type PositionalArgsAttribute () =
    inherit Attribute ()

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

/// Attribute indicating that this field shall have the given help text, when `--help` is invoked
/// or when a parse error causes us to print help text.
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
