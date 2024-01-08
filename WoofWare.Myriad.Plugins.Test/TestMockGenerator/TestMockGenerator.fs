namespace WoofWare.Myriad.Plugins.Test

open System
open SomeNamespace
open NUnit.Framework
open FsUnitTyped

[<TestFixture>]
module TestMockGenerator =

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
