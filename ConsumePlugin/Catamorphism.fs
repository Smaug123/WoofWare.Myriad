namespace ConsumePlugin

open WoofWare.Myriad.Plugins

type Const<'a> =
    | Verbatim of 'a
    | String of string

type PairOpKind =
    | NormalSeq
    | ThenDoSeq

[<CreateCatamorphism "TreeCata">]
type Tree<'a, 'b> =
    | Const of Const<'a> * 'b
    | Pair of Tree<'a, 'b> * Tree<'a, 'b> * PairOpKind
    | Sequential of Tree<'a, 'b> list
    | Builder of Tree<'a, 'b> * TreeBuilder<'b, 'a>

and TreeBuilder<'b, 'a> =
    | Child of TreeBuilder<'b, 'a>
    | Parent of Tree<'a, 'b>
