namespace ConsumePlugin

open System
open WoofWare.Myriad.Plugins

type ConstructorOptionAlias<'value> = 'value option

[<GenerateRecordConstructor>]
type MixedConstructorRecord =
    {
        FirstRequired : string
        FirstOptional : int option
        SecondRequired : string
        SecondOptional : bool option
        ThirdRequired : string
    }

[<GenerateRecordConstructor>]
type AllRequiredConstructorRecord =
    {
        First : string
        Second : string
        Third : string
    }

[<GenerateRecordConstructor>]
type AllOptionalConstructorRecord =
    {
        Number : int option
        Text : string option
    }

[<GenerateRecordConstructor>]
type NestedOptionConstructorRecord =
    {
        Id : Guid
        PostfixNested : int option option
        PrefixOption : Option<string>
    }

[<GenerateRecordConstructor>]
type GenericConstructorRecord<'required, 'optional> =
    {
        Required : 'required
        Optional : 'optional option
        RequiredList : 'required list
    }

[<GenerateRecordConstructor>]
type GenericAllOptionalConstructorRecord<'value> =
    {
        Value : 'value option
    }

[<GenerateRecordConstructor>]
type internal InternalConstructorRecord =
    {
        Required : string
        Optional : int option
    }

[<GenerateRecordConstructor>]
type TupleAndFunctionConstructorRecord =
    {
        Pair : int * string
        Transform : int -> string
        OptionalTransform : (int -> string) option
    }

[<GenerateRecordConstructor>]
type NonSyntacticOptionConstructorRecord =
    {
        Alias : ConstructorOptionAlias<int>
        ValueOption : string voption
        Nullable : Nullable<int>
        Optional : decimal option
    }

[<GenerateRecordConstructor>]
type NameHazardConstructorRecord =
    {
        ``type`` : string
        arg_0 : string
        Arg_0 : string
        ``two words`` : string
        ``two-words`` : string option
    }

namespace RecordConstructorFirstInput

type Token = | FirstToken of int

namespace RecordConstructorSecondInput

type Token = | SecondToken of string

namespace ConsumePlugin.FirstScopedOpen

open RecordConstructorFirstInput

[<WoofWare.Myriad.Plugins.GenerateRecordConstructor>]
type ScopedOpenRecord =
    {
        Token : Token
        Optional : int option
    }

namespace ConsumePlugin.SecondScopedOpen

open RecordConstructorSecondInput

[<WoofWare.Myriad.Plugins.GenerateRecordConstructor>]
type ScopedOpenRecord =
    {
        Token : Token
        Optional : int option
    }
