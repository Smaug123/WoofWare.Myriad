namespace ConsumePlugin

open WoofWare.Myriad.Plugins

type Const<'a> =
    | Verbatim of 'a
    | String of string

type PairOpKind =
    | NormalSeq
    | ThenDoSeq

[<CreateCatamorphism "TreeCata">]
type Tree<'a> =
    | Const of Const<'a>
    | Pair of Tree<'a> * Tree<'a> * PairOpKind
    | Sequential of Tree<'a> list
    | Builder of Tree<'a> * TreeBuilder<'a>

and TreeBuilder<'a> =
    | Child of TreeBuilder<'a>
    | Parent of Tree<'a>
