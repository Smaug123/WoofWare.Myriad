namespace WoofWare.Myriad.Plugins.Test

open System.Threading
open NUnit.Framework
open FsUnitTyped
open ConsumePluginNoAttr
open FsCheck

[<TestFixture>]
module TestCataGeneratorNoAttr =
    let idCata<'a, 'b> : TreeNoAttrCata<'a, 'b, _, _> =
        {
            TreeNoAttr =
                { new TreeNoAttrCataCase<_, _, _, _> with
                    member _.Const x y = Const (x, y)
                    member _.Pair x y z = Pair (x, y, z)
                    member _.Sequential xs = Sequential xs
                    member _.Builder x b = Builder (x, b)
                }
            TreeBuilderNoAttr =
                { new TreeBuilderNoAttrCataCase<_, _, _, _> with
                    member _.Child x = Child x
                    member _.Parent x = Parent x
                }
        }

    [<Test>]
    let ``Example`` () =
        let x =
            TreeNoAttr.Pair (
                TreeNoAttr.Const (ConstNoAttr.Verbatim 0, "hi"),
                TreeNoAttr.Const (ConstNoAttr.String "", "bye"),
                PairOpKindNoAttr.ThenDoSeq
            )

        TreeNoAttrCata.runTreeNoAttr idCata x |> shouldEqual x


    [<Test>]
    let ``Cata works`` () =
        let builderCases = ref 0

        let property (x : TreeNoAttr<int, string>) =
            match x with
            | TreeNoAttr.Builder _ -> Interlocked.Increment builderCases |> ignore
            | _ -> ()

            TreeNoAttrCata.runTreeNoAttr idCata x = x

        Check.QuickThrowOnFailure property
        builderCases.Value |> shouldBeGreaterThan 10
