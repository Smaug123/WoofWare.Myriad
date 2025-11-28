namespace ConsumePluginNoAttr

type ConstNoAttr<'a> =
    | Verbatim of 'a
    | String of string

type PairOpKindNoAttr =
    | NormalSeq
    | ThenDoSeq

type TreeNoAttr<'a, 'b> =
    | Const of ConstNoAttr<'a> * 'b
    | Pair of TreeNoAttr<'a, 'b> * TreeNoAttr<'a, 'b> * PairOpKindNoAttr
    | Sequential of TreeNoAttr<'a, 'b> list
    | Builder of TreeNoAttr<'a, 'b> * TreeBuilderNoAttr<'b, 'a>

and TreeBuilderNoAttr<'b, 'a> =
    | Child of TreeBuilderNoAttr<'b, 'a>
    | Parent of TreeNoAttr<'a, 'b>
