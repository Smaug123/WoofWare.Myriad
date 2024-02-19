namespace WoofWare.Myriad.Plugins.Test

open NUnit.Framework
open FsCheck
open FsUnitTyped
open ConsumePlugin

[<TestFixture>]
module TestMyList2 =

    let idCata<'a> : MyList2Cata<'a, _> =
        {
            MyList2 =
                { new MyList2CataCase<'a, _> with
                    member _.Nil = MyList2.Nil

                    member _.Cons (head : 'a) (tail : MyList2<'a>) = MyList2.Cons (head, tail)
                }
        }

    [<Test>]
    let ``Cata works`` () =
        let property (x : MyList2<int>) = MyList2Cata.runMyList2 idCata x = x

        Check.QuickThrowOnFailure property
