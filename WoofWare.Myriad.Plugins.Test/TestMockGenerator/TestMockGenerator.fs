namespace WoofWare.Myriad.Plugins.Test

open System
open SomeNamespace
open NUnit.Framework
open FsUnitTyped

[<TestFixture>]
module TestMockGenerator =

    [<Test>]
    let ``Example of use: IPublicType`` () =
        let mock =
            { PublicTypeMock.Empty with
                Mem1 = fun (s, count) -> List.replicate count s
            }

        let _ =
            Assert.Throws<NotImplementedException> (fun () -> mock.Mem2 "hi" |> ignore<int>)

        mock.Mem1 ("hi", 3) |> shouldEqual [ "hi" ; "hi" ; "hi" ]
