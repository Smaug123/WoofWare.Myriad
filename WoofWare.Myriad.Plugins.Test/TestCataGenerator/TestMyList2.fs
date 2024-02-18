namespace WoofWare.Myriad.Plugins.Test

open NUnit.Framework
open FsCheck
open FsUnitTyped
open ConsumePlugin

[<TestFixture>]
module TestMyList2 =

    let idCata : MyList2Cata<_> =
        {
            MyList2 =
                { new MyList2CataCase<_> with
                    member _.Nil = MyList2.Nil

                    member _.Cons head tail = MyList2.Cons (head, tail)
                }

        }

    [<Test>]
    let ``Cata works`` () =
        let property (x : MyList2) = MyList2Cata.runMyList2 idCata x = x

        Check.QuickThrowOnFailure property
