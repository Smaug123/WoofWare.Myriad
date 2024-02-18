namespace WoofWare.Myriad.Plugins.Test

open NUnit.Framework
open FsCheck
open FsUnitTyped
open ConsumePlugin

[<TestFixture>]
module TestMyList =

    let idCata : MyListCata<_> =
        {
            MyList =
                { new MyListCataCase<_> with
                    member _.Nil = MyList.Nil

                    member _.Cons head tail =
                        MyList.Cons
                            {
                                Head = head
                                Tail = tail
                            }
                }

        }

    [<Test>]
    let ``Cata works`` () =
        let property (x : MyList) = MyListCata.runMyList idCata x = x

        Check.QuickThrowOnFailure property

    let toListCata =
        {
            MyList =
                { new MyListCataCase<int list> with
                    member _.Nil = []
                    member _.Cons (head : int) (tail : int list) = head :: tail
                }
        }

    let toListViaCata (l : MyList) : int list = MyListCata.runMyList toListCata l


    [<Test>]
    let ``Example of a fold converting to a new data structure`` () =
        let rec toListNaive (l : MyList) : int list =
            match l with
            | MyList.Nil -> []
            | MyList.Cons {
                              Head = head
                              Tail = tail
                          } -> head :: toListNaive tail

        Check.QuickThrowOnFailure (fun l -> toListNaive l = toListViaCata l)

    [<Test>]
    let ``Example of equivalence with FoldBack`` () =
        let baseCase = 0L
        let atLeaf (head : int) (tail : int64) : int64 = int64 head + tail

        let sumCata =
            {
                MyList =
                    { new MyListCataCase<int64> with
                        member _.Nil = baseCase
                        member _.Cons (head : int) (tail : int64) = atLeaf head tail
                    }
            }

        let viaCata (l : MyList) : int64 = MyListCata.runMyList sumCata l

        let viaFold (l : MyList) : int64 =
            // choose your favourite "to list" method - here I use the cata
            // but that could have been done naively
            (toListViaCata l, baseCase)
            ||> List.foldBack (fun elt state -> atLeaf elt state)

        let property (l : MyList) = viaCata l = viaFold l

        Check.QuickThrowOnFailure property
