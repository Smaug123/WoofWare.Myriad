namespace SomeNamespace

open WoofWare.Myriad.Plugins

[<GenerateMock>]
type IPublicType =
    abstract Mem1 : string * int -> unit
    abstract Mem2 : string -> int

[<GenerateMock>]
type internal InternalType =
    abstract Mem1 : string * int -> unit
    abstract Mem2 : string -> int

[<GenerateMock>]
type private PrivateType =
    abstract Mem1 : string * int -> unit
    abstract Mem2 : string -> int

[<GenerateMock>]
type VeryPublicType<'a, 'b> =
    abstract Mem1 : 'a -> 'b
