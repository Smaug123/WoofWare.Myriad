namespace SomeNamespace

open System
open WoofWare.Myriad.Plugins

/// Mock record type for an interface
type internal PublicTypeMock =
    {
        Mem1 : string * int -> string list
        Mem2 : string -> int
        Mem3 : int * option<System.Threading.CancellationToken> -> string
    }

    /// An implementation where every method throws.
    static member Empty : PublicTypeMock =
        {
            Mem1 = (fun _ -> raise (System.NotImplementedException "Unimplemented mock function: Mem1"))
            Mem2 = (fun _ -> raise (System.NotImplementedException "Unimplemented mock function: Mem2"))
            Mem3 = (fun _ -> raise (System.NotImplementedException "Unimplemented mock function: Mem3"))
        }

    interface IPublicType with
        member this.Mem1 (arg_0_0, arg_0_1) = this.Mem1 (arg_0_0, arg_0_1)
        member this.Mem2 arg_0_0 = this.Mem2 (arg_0_0)
        member this.Mem3 (arg_0_0, arg_0_1) = this.Mem3 (arg_0_0, arg_0_1)
namespace SomeNamespace

open System
open WoofWare.Myriad.Plugins

/// Mock record type for an interface
type public PublicTypeInternalFalseMock =
    {
        Mem1 : string * int -> string list
        Mem2 : string -> int
        Mem3 : int * option<System.Threading.CancellationToken> -> string
    }

    /// An implementation where every method throws.
    static member Empty : PublicTypeInternalFalseMock =
        {
            Mem1 = (fun _ -> raise (System.NotImplementedException "Unimplemented mock function: Mem1"))
            Mem2 = (fun _ -> raise (System.NotImplementedException "Unimplemented mock function: Mem2"))
            Mem3 = (fun _ -> raise (System.NotImplementedException "Unimplemented mock function: Mem3"))
        }

    interface IPublicTypeInternalFalse with
        member this.Mem1 (arg_0_0, arg_0_1) = this.Mem1 (arg_0_0, arg_0_1)
        member this.Mem2 arg_0_0 = this.Mem2 (arg_0_0)
        member this.Mem3 (arg_0_0, arg_0_1) = this.Mem3 (arg_0_0, arg_0_1)
namespace SomeNamespace

open System
open WoofWare.Myriad.Plugins

/// Mock record type for an interface
type internal InternalTypeMock =
    {
        Mem1 : string * int -> unit
        Mem2 : string -> int
    }

    /// An implementation where every method throws.
    static member Empty : InternalTypeMock =
        {
            Mem1 = (fun _ -> raise (System.NotImplementedException "Unimplemented mock function: Mem1"))
            Mem2 = (fun _ -> raise (System.NotImplementedException "Unimplemented mock function: Mem2"))
        }

    interface InternalType with
        member this.Mem1 (arg_0_0, arg_0_1) = this.Mem1 (arg_0_0, arg_0_1)
        member this.Mem2 arg_0_0 = this.Mem2 (arg_0_0)
namespace SomeNamespace

open System
open WoofWare.Myriad.Plugins

/// Mock record type for an interface
type private PrivateTypeMock =
    {
        Mem1 : string * int -> unit
        Mem2 : string -> int
    }

    /// An implementation where every method throws.
    static member Empty : PrivateTypeMock =
        {
            Mem1 = (fun _ -> raise (System.NotImplementedException "Unimplemented mock function: Mem1"))
            Mem2 = (fun _ -> raise (System.NotImplementedException "Unimplemented mock function: Mem2"))
        }

    interface PrivateType with
        member this.Mem1 (arg_0_0, arg_0_1) = this.Mem1 (arg_0_0, arg_0_1)
        member this.Mem2 arg_0_0 = this.Mem2 (arg_0_0)
namespace SomeNamespace

open System
open WoofWare.Myriad.Plugins

/// Mock record type for an interface
type private PrivateTypeInternalFalseMock =
    {
        Mem1 : string * int -> unit
        Mem2 : string -> int
    }

    /// An implementation where every method throws.
    static member Empty : PrivateTypeInternalFalseMock =
        {
            Mem1 = (fun _ -> raise (System.NotImplementedException "Unimplemented mock function: Mem1"))
            Mem2 = (fun _ -> raise (System.NotImplementedException "Unimplemented mock function: Mem2"))
        }

    interface PrivateTypeInternalFalse with
        member this.Mem1 (arg_0_0, arg_0_1) = this.Mem1 (arg_0_0, arg_0_1)
        member this.Mem2 arg_0_0 = this.Mem2 (arg_0_0)
namespace SomeNamespace

open System
open WoofWare.Myriad.Plugins

/// Mock record type for an interface
type internal VeryPublicTypeMock<'a, 'b> =
    {
        Mem1 : 'a -> 'b
    }

    /// An implementation where every method throws.
    static member Empty () : VeryPublicTypeMock<'a, 'b> =
        {
            Mem1 = (fun _ -> raise (System.NotImplementedException "Unimplemented mock function: Mem1"))
        }

    interface VeryPublicType<'a, 'b> with
        member this.Mem1 arg_0_0 = this.Mem1 (arg_0_0)
namespace SomeNamespace

open System
open WoofWare.Myriad.Plugins

/// Mock record type for an interface
type internal CurriedMock<'a> =
    {
        Mem1 : int -> 'a -> string
        Mem2 : int * string -> 'a -> string
        Mem3 : (int * string) -> 'a -> string
        Mem4 : (int * string) -> ('a * int) -> string
        Mem5 : int * string -> ('a * int) -> string
        Mem6 : int * string -> 'a * int -> string
    }

    /// An implementation where every method throws.
    static member Empty () : CurriedMock<'a> =
        {
            Mem1 = (fun _ -> raise (System.NotImplementedException "Unimplemented mock function: Mem1"))
            Mem2 = (fun _ -> raise (System.NotImplementedException "Unimplemented mock function: Mem2"))
            Mem3 = (fun _ -> raise (System.NotImplementedException "Unimplemented mock function: Mem3"))
            Mem4 = (fun _ -> raise (System.NotImplementedException "Unimplemented mock function: Mem4"))
            Mem5 = (fun _ -> raise (System.NotImplementedException "Unimplemented mock function: Mem5"))
            Mem6 = (fun _ -> raise (System.NotImplementedException "Unimplemented mock function: Mem6"))
        }

    interface Curried<'a> with
        member this.Mem1 arg_0_0 arg_1_0 = this.Mem1 (arg_0_0) (arg_1_0)
        member this.Mem2 (arg_0_0, arg_0_1) arg_1_0 = this.Mem2 (arg_0_0, arg_0_1) (arg_1_0)
        member this.Mem3 ((arg_0_0, arg_0_1)) arg_1_0 = this.Mem3 (arg_0_0, arg_0_1) (arg_1_0)

        member this.Mem4 ((arg_0_0, arg_0_1)) ((arg_1_0, arg_1_1)) =
            this.Mem4 (arg_0_0, arg_0_1) (arg_1_0, arg_1_1)

        member this.Mem5 (arg_0_0, arg_0_1) ((arg_1_0, arg_1_1)) =
            this.Mem5 (arg_0_0, arg_0_1) (arg_1_0, arg_1_1)

        member this.Mem6 (arg_0_0, arg_0_1) (arg_1_0, arg_1_1) =
            this.Mem6 (arg_0_0, arg_0_1) (arg_1_0, arg_1_1)
namespace SomeNamespace

open System
open WoofWare.Myriad.Plugins

/// Mock record type for an interface
type internal TypeWithInterfaceMock =
    {
        /// Implementation of IDisposable.Dispose
        Dispose : unit -> unit
        Mem1 : string option -> string[] Async
        Mem2 : unit -> string[] Async
    }

    /// An implementation where every method throws.
    static member Empty : TypeWithInterfaceMock =
        {
            Dispose = (fun () -> ())
            Mem1 = (fun _ -> raise (System.NotImplementedException "Unimplemented mock function: Mem1"))
            Mem2 = (fun _ -> raise (System.NotImplementedException "Unimplemented mock function: Mem2"))
        }

    interface TypeWithInterface with
        member this.Mem1 arg_0_0 = this.Mem1 (arg_0_0)
        member this.Mem2 () = this.Mem2 (())

    interface System.IDisposable with
        member this.Dispose () : unit = this.Dispose ()
