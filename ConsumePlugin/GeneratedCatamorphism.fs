namespace ConsumePlugin

open WoofWare.Myriad.Plugins

/// Description of how to combine cases during a fold
type TreeBuilderCataCase<'b, 'a, 'TreeBuilder, 'Tree> =
    /// How to operate on the Child case
    abstract Child : 'TreeBuilder -> 'TreeBuilder
    /// How to operate on the Parent case
    abstract Parent : 'Tree -> 'TreeBuilder

/// Description of how to combine cases during a fold
type TreeCataCase<'a, 'b, 'TreeBuilder, 'Tree> =
    /// How to operate on the Const case
    abstract Const : Const<'a> -> 'b -> 'Tree
    /// How to operate on the Pair case
    abstract Pair : 'Tree -> 'Tree -> PairOpKind -> 'Tree
    /// How to operate on the Sequential case
    abstract Sequential : 'Tree list -> 'Tree
    /// How to operate on the Builder case
    abstract Builder : 'Tree -> 'TreeBuilder -> 'Tree

/// Specifies how to perform a fold (catamorphism) over the type Tree and its friends.
type TreeCata<'b, 'a, 'TreeBuilder, 'Tree> =
    {
        /// How to perform a fold (catamorphism) over the type TreeBuilder
        TreeBuilder : TreeBuilderCataCase<'b, 'a, 'TreeBuilder, 'Tree>
        /// How to perform a fold (catamorphism) over the type Tree
        Tree : TreeCataCase<'a, 'b, 'TreeBuilder, 'Tree>
    }

/// Methods to perform a catamorphism over the type Tree
[<RequireQualifiedAccess>]
module TreeCata =
    [<RequireQualifiedAccess>]
    type private Instruction<'b, 'a> =
        | Process__TreeBuilder of TreeBuilder<'b, 'a>
        | Process__Tree of Tree<'a, 'b>
        | TreeBuilder_Child
        | TreeBuilder_Parent
        | Tree_Pair of PairOpKind
        | Tree_Sequential of int
        | Tree_Builder

    let private loop (cata : TreeCata<'b, 'a, 'TreeBuilder, 'Tree>) (instructions : ResizeArray<Instruction<'b, 'a>>) =
        let treeStack = ResizeArray<'Tree> ()
        let treeBuilderStack = ResizeArray<'TreeBuilder> ()

        while instructions.Count > 0 do
            let currentInstruction = instructions.[instructions.Count - 1]
            instructions.RemoveAt (instructions.Count - 1)

            match currentInstruction with
            | Instruction.Process__TreeBuilder x ->
                match x with
                | TreeBuilder.Child (arg0_0) ->
                    instructions.Add Instruction.TreeBuilder_Child
                    instructions.Add (Instruction.Process__TreeBuilder arg0_0)
                | TreeBuilder.Parent (arg0_0) ->
                    instructions.Add Instruction.TreeBuilder_Parent
                    instructions.Add (Instruction.Process__Tree arg0_0)
            | Instruction.Process__Tree x ->
                match x with
                | Tree.Const (arg0_0, arg1_0) -> cata.Tree.Const arg0_0 arg1_0 |> treeStack.Add
                | Tree.Pair (arg0_0, arg1_0, arg2_0) ->
                    instructions.Add (Instruction.Tree_Pair (arg2_0))
                    instructions.Add (Instruction.Process__Tree arg0_0)
                    instructions.Add (Instruction.Process__Tree arg1_0)
                | Tree.Sequential (arg0_0) ->
                    instructions.Add (Instruction.Tree_Sequential ((List.length arg0_0)))

                    for elt in arg0_0 do
                        instructions.Add (Instruction.Process__Tree elt)
                | Tree.Builder (arg0_0, arg1_0) ->
                    instructions.Add Instruction.Tree_Builder
                    instructions.Add (Instruction.Process__Tree arg0_0)
                    instructions.Add (Instruction.Process__TreeBuilder arg1_0)
            | Instruction.TreeBuilder_Child ->
                let arg0_0 = treeBuilderStack.[treeBuilderStack.Count - 1]
                treeBuilderStack.RemoveAt (treeBuilderStack.Count - 1)
                cata.TreeBuilder.Child arg0_0 |> treeBuilderStack.Add
            | Instruction.TreeBuilder_Parent ->
                let arg0_0 = treeStack.[treeStack.Count - 1]
                treeStack.RemoveAt (treeStack.Count - 1)
                cata.TreeBuilder.Parent arg0_0 |> treeBuilderStack.Add
            | Instruction.Tree_Pair arg2_0 ->
                let arg0_0 = treeStack.[treeStack.Count - 1]
                treeStack.RemoveAt (treeStack.Count - 1)
                let arg1_0 = treeStack.[treeStack.Count - 1]
                treeStack.RemoveAt (treeStack.Count - 1)
                cata.Tree.Pair arg0_0 arg1_0 arg2_0 |> treeStack.Add
            | Instruction.Tree_Sequential arg0_0 ->
                let arg0_0_len = arg0_0

                let arg0_0 =
                    seq {
                        for i = treeStack.Count - 1 downto treeStack.Count - arg0_0 do
                            yield treeStack.[i]
                    }
                    |> Seq.toList

                treeStack.RemoveRange (treeStack.Count - arg0_0_len, arg0_0_len)
                cata.Tree.Sequential arg0_0 |> treeStack.Add
            | Instruction.Tree_Builder ->
                let arg0_0 = treeStack.[treeStack.Count - 1]
                treeStack.RemoveAt (treeStack.Count - 1)
                let arg1_0 = treeBuilderStack.[treeBuilderStack.Count - 1]
                treeBuilderStack.RemoveAt (treeBuilderStack.Count - 1)
                cata.Tree.Builder arg0_0 arg1_0 |> treeStack.Add

        treeBuilderStack, treeStack

    /// Execute the catamorphism.
    let runTreeBuilder
        (cata : TreeCata<'b, 'a, 'TreeBuilderRet, 'TreeRet>)
        (x : TreeBuilder<'b, 'a>)
        : 'TreeBuilderRet
        =
        let instructions = ResizeArray ()
        instructions.Add (Instruction.Process__TreeBuilder x)
        let treeBuilderRetStack, treeRetStack = loop cata instructions
        Seq.exactlyOne treeBuilderRetStack

    /// Execute the catamorphism.
    let runTree (cata : TreeCata<'b, 'a, 'TreeBuilderRet, 'TreeRet>) (x : Tree<'a, 'b>) : 'TreeRet =
        let instructions = ResizeArray ()
        instructions.Add (Instruction.Process__Tree x)
        let treeBuilderRetStack, treeRetStack = loop cata instructions
        Seq.exactlyOne treeRetStack
