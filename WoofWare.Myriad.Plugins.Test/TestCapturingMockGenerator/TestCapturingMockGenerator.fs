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
            { PublicTypeMock.Empty with
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
            { TypeWithPropertiesMock.Empty with
                Mem1 = fun i -> async { return Option.toArray i }
                Prop1 = fun () -> 44
            }
            :> _

        mock.Mem1 (Some "hi") |> Async.RunSynchronously |> shouldEqual [| "hi" |]

        mock.Prop1 |> shouldEqual 44

    [<Test>]
    let ``Example of curried use`` () =
        let mock =
            { CurriedMock<string>.Empty () with
                Mem1 =
                    fun x y ->
                        x |> shouldEqual 3
                        y |> shouldEqual "hello"
                        "it's me"
            }

        mock.Mem1 3 "hello" |> shouldEqual "it's me"

        lock mock.Mem1_Calls (fun () -> Seq.toList mock.Mem1_Calls)
        |> List.exactlyOne
        |> shouldEqual
            {
                bar = 3
                Arg1 = "hello"
            }
