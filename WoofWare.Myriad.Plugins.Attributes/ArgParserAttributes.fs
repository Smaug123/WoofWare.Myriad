namespace WoofWare.Myriad.Plugins

open System

/// Attribute indicating a record type to which the "build arg parser" Myriad
/// generator should apply during build.
type ArgParserAttribute () =
    inherit Attribute ()

type PositionalArgsAttribute () =
    inherit Attribute ()

type ArgumentDefaultFunctionAttribute () =
    inherit Attribute ()

type ArgumentDefaultEnvironmentVariableAttribute (envVar : string) =
    inherit Attribute ()
