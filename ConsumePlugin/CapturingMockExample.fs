namespace SomeNamespace.CapturingMock

open System
open WoofWare.Myriad.Plugins

[<GenerateCapturingMock>]
type IPublicType =
    abstract Mem1 : string * int -> string list
    abstract Mem2 : string -> int
    abstract Mem3 : x : int * ?ct : System.Threading.CancellationToken -> string

[<GenerateCapturingMock false>]
type IPublicTypeInternalFalse =
    abstract Mem1 : string * int -> string list
    abstract Mem2 : string -> int
    abstract Mem3 : x : int * ?ct : System.Threading.CancellationToken -> string

[<GenerateCapturingMock>]
type internal InternalType =
    abstract Mem1 : string * int -> unit
    abstract Mem2 : string -> int

[<GenerateCapturingMock>]
type private PrivateType =
    abstract Mem1 : string * int -> unit
    abstract Mem2 : string -> int

[<GenerateCapturingMock false>]
type private PrivateTypeInternalFalse =
    abstract Mem1 : string * int -> unit
    abstract Mem2 : string -> int

[<GenerateCapturingMock>]
type VeryPublicType<'a, 'b> =
    abstract Mem1 : 'a -> 'b

[<GenerateCapturingMock>]
type Curried<'a> =
    abstract Mem1 : int -> 'a -> string
    abstract Mem2 : int * string -> 'a -> string
    abstract Mem3 : (int * string) -> 'a -> string
    abstract Mem4 : (int * string) -> ('a * int) -> string
    abstract Mem5 : x : int * string -> ('a * int) -> string
    abstract Mem6 : int * string -> y : 'a * int -> string

[<GenerateCapturingMock>]
type TypeWithInterface =
    inherit IDisposable
    abstract Mem1 : string option -> string[] Async
    abstract Mem2 : unit -> string[] Async

[<GenerateCapturingMock>]
type TypeWithProperties =
    inherit IDisposable
    abstract Mem1 : string option -> string[] Async
    abstract Prop1 : int
    abstract Prop2 : unit Async
