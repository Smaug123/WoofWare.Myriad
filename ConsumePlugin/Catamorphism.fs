namespace ConsumePlugin

open WoofWare.Myriad.Plugins

type Const =
    | Int of int
    | String of string

type PairOpKind =
    | NormalSeq
    | ThenDoSeq

[<CreateCatamorphism "TreeCata">]
type Tree =
    | Const of Const
    | Pair of Tree * Tree * PairOpKind
    | Sequential of Tree list
    | Builder of Tree * TreeBuilder

and TreeBuilder =
    | Child of TreeBuilder
    | Parent of Tree
