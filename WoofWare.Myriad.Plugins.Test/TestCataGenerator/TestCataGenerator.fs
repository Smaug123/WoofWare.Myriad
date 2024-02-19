namespace WoofWare.Myriad.Plugins.Test

open System.Threading
open NUnit.Framework
open FsUnitTyped
open ConsumePlugin
open FsCheck

[<TestFixture>]
module TestCataGenerator =
    let idCata<'a, 'b> : TreeCata<'a, 'b, _, _> =
        {
            Tree =
                { new TreeCataCase<_, _, _, _> with
                    member _.Const x y = Const (x, y)
                    member _.Pair x y z = Pair (x, y, z)
                    member _.Sequential xs = Sequential xs
                    member _.Builder x b = Builder (x, b)
                }
            TreeBuilder =
                { new TreeBuilderCataCase<_, _, _, _> with
                    member _.Child x = Child x
                    member _.Parent x = Parent x
                }
        }

    [<Test>]
    let ``Example`` () =
        let x =
            Tree.Pair (Tree.Const (Const.Verbatim 0, "hi"), Tree.Const (Const.String "", "bye"), PairOpKind.ThenDoSeq)

        TreeCata.runTree idCata x |> shouldEqual x


    [<Test>]
    let ``Cata works`` () =
        let builderCases = ref 0

        let property (x : Tree<int, string>) =
            match x with
            | Tree.Builder _ -> Interlocked.Increment builderCases |> ignore
            | _ -> ()

            TreeCata.runTree idCata x = x

        Check.QuickThrowOnFailure property
        builderCases.Value |> shouldBeGreaterThan 10
