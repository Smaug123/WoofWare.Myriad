namespace WoofWare.Myriad.Plugins.Test

open System
open SomeNamespace.CapturingMock
open NUnit.Framework
open FsUnitTyped

[<TestFixture>]
module TestCapturingMockGenerator =

    [<Test>]
    let ``Example of use: IPublicType`` () =
        let mock : IPublicType =
            { PublicTypeMock.Empty () with
                Mem1 = fun (s, count) -> List.replicate count s
            }
            :> _

        let _ =
            Assert.Throws<NotImplementedException> (fun () -> mock.Mem2 "hi" |> ignore<int>)

        mock.Mem1 ("hi", 3) |> shouldEqual [ "hi" ; "hi" ; "hi" ]

    [<Test>]
    let ``Example of use: curried args`` () =
        let mock : Curried<_> =
            { CurriedMock.Empty () with
                Mem1 = fun i c -> Array.replicate i c |> String
                Mem2 = fun (i, s) c -> String.concat $"%c{c}" (List.replicate i s)
                Mem3 = fun (i, s) c -> String.concat $"%c{c}" (List.replicate i s)
            }
            :> _

        mock.Mem1 3 'a' |> shouldEqual "aaa"
        mock.Mem2 (3, "hi") 'a' |> shouldEqual "hiahiahi"
        mock.Mem3 (3, "hi") 'a' |> shouldEqual "hiahiahi"

    [<Test>]
    let ``Example of use: properties`` () =
        let mock : TypeWithProperties =
            { TypeWithPropertiesMock.Empty () with
                Mem1 = fun i -> async { return Option.toArray i }
                Prop1 = fun () -> 44
            }
            :> _

        mock.Mem1 (Some "hi") |> Async.RunSynchronously |> shouldEqual [| "hi" |]

        mock.Prop1 |> shouldEqual 44

    [<Test>]
    let ``Example of curried use`` () =
        let mock' =
            { CurriedMock<string>.Empty () with
                Mem1 =
                    fun x y ->
                        x |> shouldEqual 3
                        y |> shouldEqual "hello"
                        "it's me"
            }

        let mock = mock' :> Curried<_>

        mock.Mem1 3 "hello" |> shouldEqual "it's me"

        lock mock'.Calls.Mem1 (fun () -> Seq.toList mock'.Calls.Mem1)
        |> List.exactlyOne
        |> shouldEqual
            {
                bar = 3
                Arg1 = "hello"
            }

    [<Test>]
    let ``Example of use IAsyncDisposable`` () =
        let mock' =
            { TypeWithAsyncDisposableMock.Empty () with
                Mem1 = fun i -> async { return Option.toArray i }
                Mem2 = fun () -> async { return [||] }
            }

        let mock = mock' :> TypeWithAsyncDisposable

        mock.Mem1 (Some "hi") |> Async.RunSynchronously |> shouldEqual [| "hi" |]
        mock.Mem2 () |> Async.RunSynchronously |> shouldEqual [||]

        // Test that DisposeAsync returns a completed ValueTask
        let asyncDisposable = mock :> IAsyncDisposable
        let valueTask = asyncDisposable.DisposeAsync ()
        valueTask.IsCompleted |> shouldEqual true

        // Verify calls were captured
        lock mock'.Calls.Mem1 (fun () -> Seq.toList mock'.Calls.Mem1)
        |> List.exactlyOne
        |> shouldEqual (Some "hi")

        lock mock'.Calls.Mem2 (fun () -> Seq.toList mock'.Calls.Mem2)
        |> List.exactlyOne
        |> shouldEqual ()

    [<Test>]
    let ``Example of use: Both IDisposable and IAsyncDisposable`` () =
        let mutable disposed = false
        let mutable disposedAsync = false

        let mock' =
            { TypeWithBothDisposablesMock.Empty () with
                Dispose = fun () -> disposed <- true
                DisposeAsync =
                    fun () ->
                        disposedAsync <- true
                        System.Threading.Tasks.ValueTask ()
                Mem1 = fun s -> s.Length
            }

        let mock = mock' :> TypeWithBothDisposables

        mock.Mem1 "hello" |> shouldEqual 5
        mock.Mem1 "world" |> shouldEqual 5

        // Test IDisposable.Dispose
        (mock :> IDisposable).Dispose ()
        disposed |> shouldEqual true

        // Test IAsyncDisposable.DisposeAsync
        let valueTask = (mock :> IAsyncDisposable).DisposeAsync ()
        valueTask.IsCompleted |> shouldEqual true
        disposedAsync |> shouldEqual true

        // Verify calls were captured
        lock mock'.Calls.Mem1 (fun () -> Seq.toList mock'.Calls.Mem1)
        |> shouldEqual [ "hello" ; "world" ]
