namespace ConsumePlugin

open WoofWare.Myriad.Plugins

type Const =
    | Int of int
    | String of string

type PairOpKind =
    | NormalSeq
    | ThenDoSeq

[<CreateCatamorphism>]
type Expr =
    | Const of Const
    | Pair of Expr * Expr * PairOpKind
    | Sequential of Expr list
    | Builder of Expr * ExprBuilder

and ExprBuilder =
    | Child of ExprBuilder
    | Parent of Expr
