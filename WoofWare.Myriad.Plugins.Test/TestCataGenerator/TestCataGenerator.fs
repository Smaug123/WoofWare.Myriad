namespace WoofWare.Myriad.Plugins.Test

open NUnit.Framework
open FsUnitTyped
open ConsumePlugin
open FsCheck

[<TestFixture>]
module TestCataGenerator =
    let idCata : Cata<_, _> =
        {
            Expr =
                { new ExprCata<_, _> with
                    member _.Const x = Const x
                    member _.Pair x y z = Pair (x, y, z)
                    member _.Sequential xs = Sequential xs
                    member _.Builder x b = Builder (x, b)
                }
            ExprBuilder =
                { new ExprBuilderCata<_, _> with
                    member _.Child x = Child x
                    member _.Parent x = Parent x
                }
        }

    [<Test>]
    let ``Example`` () =
        let x =
            Expr.Pair (Expr.Const (Const.Int 0), Expr.Const (Const.String ""), PairOpKind.ThenDoSeq)

        ExprCata.runExpr idCata x |> shouldEqual x


    [<Test>]
    let ``Cata works`` () =
        let property (x : Expr) = ExprCata.runExpr idCata x = x
        Check.QuickThrowOnFailure property
