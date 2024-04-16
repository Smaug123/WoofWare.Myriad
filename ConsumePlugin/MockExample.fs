namespace SomeNamespace

open System
open WoofWare.Myriad.Plugins

[<GenerateMock>]
type IPublicType =
    abstract Mem1 : string * int -> string list
    abstract Mem2 : string -> int
    abstract Mem3 : x : int * ?ct : System.Threading.CancellationToken -> string

[<GenerateMock false>]
type IPublicTypeInternalFalse =
    abstract Mem1 : string * int -> string list
    abstract Mem2 : string -> int
    abstract Mem3 : x : int * ?ct : System.Threading.CancellationToken -> string

[<GenerateMock>]
type internal InternalType =
    abstract Mem1 : string * int -> unit
    abstract Mem2 : string -> int

[<GenerateMock>]
type private PrivateType =
    abstract Mem1 : string * int -> unit
    abstract Mem2 : string -> int

[<GenerateMock false>]
type private PrivateTypeInternalFalse =
    abstract Mem1 : string * int -> unit
    abstract Mem2 : string -> int

[<GenerateMock>]
type VeryPublicType<'a, 'b> =
    abstract Mem1 : 'a -> 'b

[<GenerateMock>]
type Curried<'a> =
    abstract Mem1 : int -> 'a -> string
    abstract Mem2 : int * string -> 'a -> string
    abstract Mem3 : (int * string) -> 'a -> string
    abstract Mem4 : (int * string) -> ('a * int) -> string
    abstract Mem5 : x : int * string -> ('a * int) -> string
    abstract Mem6 : int * string -> y : 'a * int -> string

[<GenerateMock>]
type TypeWithInterface =
    inherit IDisposable
    abstract Mem1 : int -> string
