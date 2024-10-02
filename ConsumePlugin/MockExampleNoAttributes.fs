namespace SomeNamespace

open System

type IPublicTypeNoAttr =
    abstract Mem1 : string * int -> string list
    abstract Mem2 : string -> int
    abstract Mem3 : x : int * ?ct : System.Threading.CancellationToken -> string

type IPublicTypeInternalFalseNoAttr =
    abstract Mem1 : string * int -> string list
    abstract Mem2 : string -> int
    abstract Mem3 : x : int * ?ct : System.Threading.CancellationToken -> string

type internal InternalTypeNoAttr =
    abstract Mem1 : string * int -> unit
    abstract Mem2 : string -> int

type private PrivateTypeNoAttr =
    abstract Mem1 : string * int -> unit
    abstract Mem2 : string -> int

type private PrivateTypeInternalFalseNoAttr =
    abstract Mem1 : string * int -> unit
    abstract Mem2 : string -> int

type VeryPublicTypeNoAttr<'a, 'b> =
    abstract Mem1 : 'a -> 'b

type CurriedNoAttr<'a> =
    abstract Mem1 : int -> 'a -> string
    abstract Mem2 : int * string -> 'a -> string
    abstract Mem3 : (int * string) -> 'a -> string
    abstract Mem4 : (int * string) -> ('a * int) -> string
    abstract Mem5 : x : int * string -> ('a * int) -> string
    abstract Mem6 : int * string -> y : 'a * int -> string

type TypeWithInterfaceNoAttr =
    inherit IDisposable
    abstract Mem1 : string option -> string[] Async
    abstract Mem2 : unit -> string[] Async
