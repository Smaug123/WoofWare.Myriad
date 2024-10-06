// Source generated by Whippet. Changes will be overwritten on next build.
namespace ConsumePlugin

open WoofWare.Myriad.Plugins

/// Description of how to combine cases during a fold
type TreeBuilderCataCase<'b, 'a, 'TreeBuilder, 'Tree, 'PairOpKind, 'Const> =
    /// How to operate on the Child case
    abstract Child: 'TreeBuilder -> 'TreeBuilder
    /// How to operate on the Parent case
    abstract Parent: 'Tree -> 'TreeBuilder

/// Description of how to combine cases during a fold
type TreeCataCase<'a, 'b, 'TreeBuilder, 'Tree, 'PairOpKind, 'Const> =
    /// How to operate on the Const case
    abstract Const: 'Const -> 'b -> 'Tree
    /// How to operate on the Pair case
    abstract Pair: 'Tree -> 'Tree -> 'PairOpKind -> 'Tree
    /// How to operate on the Sequential case
    abstract Sequential: 'Tree list -> 'Tree
    /// How to operate on the Builder case
    abstract Builder: 'Tree -> 'TreeBuilder -> 'Tree

/// Description of how to combine cases during a fold
type PairOpKindCataCase<'TreeBuilder, 'Tree, 'PairOpKind, 'Const> =
    /// How to operate on the NormalSeq case
    abstract NormalSeq: 'PairOpKind
    /// How to operate on the ThenDoSeq case
    abstract ThenDoSeq: 'PairOpKind

/// Description of how to combine cases during a fold
type ConstCataCase<'a, 'TreeBuilder, 'Tree, 'PairOpKind, 'Const> =
    /// How to operate on the Verbatim case
    abstract Verbatim: 'a -> 'Const
    /// How to operate on the String case
    abstract String: string -> 'Const

/// Specifies how to perform a fold (catamorphism) over the type Tree and its friends.
type TreeCata<'b, 'a, 'TreeBuilder, 'Tree, 'PairOpKind, 'Const> =
    {
        /// How to perform a fold (catamorphism) over the type TreeBuilder
        TreeBuilder: TreeBuilderCataCase<'b, 'a, 'TreeBuilder, 'Tree, 'PairOpKind, 'Const>
        /// How to perform a fold (catamorphism) over the type Tree
        Tree: TreeCataCase<'a, 'b, 'TreeBuilder, 'Tree, 'PairOpKind, 'Const>
        /// How to perform a fold (catamorphism) over the type PairOpKind
        PairOpKind: PairOpKindCataCase<'TreeBuilder, 'Tree, 'PairOpKind, 'Const>
        /// How to perform a fold (catamorphism) over the type Const
        Const: ConstCataCase<'a, 'TreeBuilder, 'Tree, 'PairOpKind, 'Const>
    }

/// Methods to perform a catamorphism over the type Tree
[<RequireQualifiedAccess>]
module TreeCata =
    [<RequireQualifiedAccess>]
    type private Instruction<'b, 'a> =
        | Process__TreeBuilder of TreeBuilder<'b, 'a>
        | Process__Tree of Tree<'a, 'b>
        | Process__PairOpKind of PairOpKind
        | Process__Const of Const<'a>
        | TreeBuilder_Child
        | TreeBuilder_Parent
        | Tree_Const of 'b
        | Tree_Pair
        | Tree_Sequential of int
        | Tree_Builder

    let private loop
        (cata: TreeCata<'b, 'a, 'TreeBuilder, 'Tree, 'PairOpKind, 'Const>)
        (instructions: ResizeArray<Instruction<'b, 'a>>)
        =
        let constStack = ResizeArray<'Const>()
        let pairOpKindStack = ResizeArray<'PairOpKind>()
        let treeStack = ResizeArray<'Tree>()
        let treeBuilderStack = ResizeArray<'TreeBuilder>()

        while instructions.Count > 0 do
            let currentInstruction = instructions.[instructions.Count - 1]
            instructions.RemoveAt(instructions.Count - 1)

            match currentInstruction with
            | Instruction.Process__TreeBuilder x ->
                match x with
                | TreeBuilder.Child(arg0_0) ->
                    instructions.Add Instruction.TreeBuilder_Child
                    instructions.Add(Instruction.Process__TreeBuilder arg0_0)
                | TreeBuilder.Parent(arg0_0) ->
                    instructions.Add Instruction.TreeBuilder_Parent
                    instructions.Add(Instruction.Process__Tree arg0_0)
            | Instruction.Process__Tree x ->
                match x with
                | Tree.Const(arg0_0, arg1_0) ->
                    instructions.Add(Instruction.Tree_Const(arg1_0))
                    instructions.Add(Instruction.Process__Const arg0_0)
                | Tree.Pair(arg0_0, arg1_0, arg2_0) ->
                    instructions.Add Instruction.Tree_Pair
                    instructions.Add(Instruction.Process__Tree arg0_0)
                    instructions.Add(Instruction.Process__Tree arg1_0)
                    instructions.Add(Instruction.Process__PairOpKind arg2_0)
                | Tree.Sequential(arg0_0) ->
                    instructions.Add(Instruction.Tree_Sequential((List.length arg0_0)))

                    for elt in arg0_0 do
                        instructions.Add(Instruction.Process__Tree elt)
                | Tree.Builder(arg0_0, arg1_0) ->
                    instructions.Add Instruction.Tree_Builder
                    instructions.Add(Instruction.Process__Tree arg0_0)
                    instructions.Add(Instruction.Process__TreeBuilder arg1_0)
            | Instruction.Process__PairOpKind x ->
                match x with
                | PairOpKind.NormalSeq -> cata.PairOpKind.NormalSeq |> pairOpKindStack.Add
                | PairOpKind.ThenDoSeq -> cata.PairOpKind.ThenDoSeq |> pairOpKindStack.Add
            | Instruction.Process__Const x ->
                match x with
                | Const.Verbatim(arg0_0) -> cata.Const.Verbatim arg0_0 |> constStack.Add
                | Const.String(arg0_0) -> cata.Const.String arg0_0 |> constStack.Add
            | Instruction.TreeBuilder_Child ->
                let arg0_0 = treeBuilderStack.[treeBuilderStack.Count - 1]
                treeBuilderStack.RemoveAt(treeBuilderStack.Count - 1)
                cata.TreeBuilder.Child arg0_0 |> treeBuilderStack.Add
            | Instruction.TreeBuilder_Parent ->
                let arg0_0 = treeStack.[treeStack.Count - 1]
                treeStack.RemoveAt(treeStack.Count - 1)
                cata.TreeBuilder.Parent arg0_0 |> treeBuilderStack.Add
            | Instruction.Tree_Const arg1_0 ->
                let arg0_0 = constStack.[constStack.Count - 1]
                constStack.RemoveAt(constStack.Count - 1)
                cata.Tree.Const arg0_0 arg1_0 |> treeStack.Add
            | Instruction.Tree_Pair ->
                let arg0_0 = treeStack.[treeStack.Count - 1]
                treeStack.RemoveAt(treeStack.Count - 1)
                let arg1_0 = treeStack.[treeStack.Count - 1]
                treeStack.RemoveAt(treeStack.Count - 1)
                let arg2_0 = pairOpKindStack.[pairOpKindStack.Count - 1]
                pairOpKindStack.RemoveAt(pairOpKindStack.Count - 1)
                cata.Tree.Pair arg0_0 arg1_0 arg2_0 |> treeStack.Add
            | Instruction.Tree_Sequential arg0_0 ->
                let arg0_0_len = arg0_0

                let arg0_0 =
                    seq {
                        for i = treeStack.Count - 1 downto treeStack.Count - arg0_0 do
                            yield treeStack.[i]
                    }
                    |> Seq.toList

                treeStack.RemoveRange(treeStack.Count - arg0_0_len, arg0_0_len)
                cata.Tree.Sequential arg0_0 |> treeStack.Add
            | Instruction.Tree_Builder ->
                let arg0_0 = treeStack.[treeStack.Count - 1]
                treeStack.RemoveAt(treeStack.Count - 1)
                let arg1_0 = treeBuilderStack.[treeBuilderStack.Count - 1]
                treeBuilderStack.RemoveAt(treeBuilderStack.Count - 1)
                cata.Tree.Builder arg0_0 arg1_0 |> treeStack.Add

        treeBuilderStack, treeStack, pairOpKindStack, constStack

    /// Execute the catamorphism.
    let runTreeBuilder
        (cata: TreeCata<'b, 'a, 'TreeBuilderRet, 'TreeRet, 'PairOpKindRet, 'ConstRet>)
        (x: TreeBuilder<'b, 'a>)
        : 'TreeBuilderRet =
        let instructions = ResizeArray()
        instructions.Add(Instruction.Process__TreeBuilder x)

        let treeBuilderRetStack, treeRetStack, pairOpKindRetStack, constRetStack =
            loop cata instructions

        Seq.exactlyOne treeBuilderRetStack

    /// Execute the catamorphism.
    let runTree
        (cata: TreeCata<'b, 'a, 'TreeBuilderRet, 'TreeRet, 'PairOpKindRet, 'ConstRet>)
        (x: Tree<'b, 'a>)
        : 'TreeRet =
        let instructions = ResizeArray()
        instructions.Add(Instruction.Process__Tree x)

        let treeBuilderRetStack, treeRetStack, pairOpKindRetStack, constRetStack =
            loop cata instructions

        Seq.exactlyOne treeRetStack

    /// Execute the catamorphism.
    let runPairOpKind
        (cata: TreeCata<'b, 'a, 'TreeBuilderRet, 'TreeRet, 'PairOpKindRet, 'ConstRet>)
        (x: PairOpKind<'b, 'a>)
        : 'PairOpKindRet =
        let instructions = ResizeArray()
        instructions.Add(Instruction.Process__PairOpKind x)

        let treeBuilderRetStack, treeRetStack, pairOpKindRetStack, constRetStack =
            loop cata instructions

        Seq.exactlyOne pairOpKindRetStack

    /// Execute the catamorphism.
    let runConst
        (cata: TreeCata<'b, 'a, 'TreeBuilderRet, 'TreeRet, 'PairOpKindRet, 'ConstRet>)
        (x: Const<'b, 'a>)
        : 'ConstRet =
        let instructions = ResizeArray()
        instructions.Add(Instruction.Process__Const x)

        let treeBuilderRetStack, treeRetStack, pairOpKindRetStack, constRetStack =
            loop cata instructions

        Seq.exactlyOne constRetStack

