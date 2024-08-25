namespace WoofWare.Myriad.Plugins

open System

/// Attribute indicating a record type to which the "build arg parser" Myriad
/// generator should apply during build.
type ArgParserAttribute () =
    inherit Attribute ()

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
