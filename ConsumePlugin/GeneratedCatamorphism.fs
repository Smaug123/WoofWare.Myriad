//------------------------------------------------------------------------------
//        This code was generated by myriad.
//        Changes to this file will be lost when the code is regenerated.
//------------------------------------------------------------------------------





namespace ConsumePlugin

open WoofWare.Myriad.Plugins

/// Description of how to combine cases during a fold
type ExprCata<'Expr, 'ExprBuilder> =
    /// How to operate on the Const case
    abstract Const : Const -> 'Expr
    /// How to operate on the Pair case
    abstract Pair : 'Expr -> 'Expr -> PairOpKind -> 'Expr
    /// How to operate on the Sequential case
    abstract Sequential : 'Expr list -> 'Expr
    /// How to operate on the Builder case
    abstract Builder : 'Expr -> 'ExprBuilder -> 'Expr

/// Description of how to combine cases during a fold
type ExprBuilderCata<'Expr, 'ExprBuilder> =
    /// How to operate on the Child case
    abstract Child : 'ExprBuilder -> 'ExprBuilder
    /// How to operate on the Parent case
    abstract Parent : 'Expr -> 'ExprBuilder

/// Specifies how to perform a fold (catamorphism) over the type Expr and its friends.
type Cata<'Expr, 'ExprBuilder> =
    {
        /// How to perform a fold (catamorphism) over the type Expr
        Expr : ExprCata<'Expr, 'ExprBuilder>
        /// How to perform a fold (catamorphism) over the type ExprBuilder
        ExprBuilder : ExprBuilderCata<'Expr, 'ExprBuilder>
    }

/// Methods to perform a catamorphism over the type Expr
[<RequireQualifiedAccess>]
module ExprCata =
    [<RequireQualifiedAccess>]
    type private Instruction =
        | Process__Expr of Expr
        | Process__ExprBuilder of ExprBuilder
        | Expr_Pair of PairOpKind
        | Expr_Sequential of int
        | Expr_Builder
        | ExprBuilder_Child
        | ExprBuilder_Parent

    let private loop (cata : Cata<_, _>) (instructions : ResizeArray<Instruction>) =
        let exprBuilderStack = ResizeArray ()
        let exprStack = ResizeArray ()

        while instructions.Count > 0 do
            let currentInstruction = instructions.[instructions.Count - 1]
            instructions.RemoveAt (instructions.Count - 1)

            match currentInstruction with
            | Instruction.Process__Expr x ->
                match x with
                | Expr.Const (arg0) -> cata.Expr.Const arg0 |> exprStack.Add
                | Expr.Pair (arg0, arg1, arg2) ->
                    instructions.Add (Instruction.Expr_Pair (arg2))
                    instructions.Add (Instruction.Process__Expr arg0)
                    instructions.Add (Instruction.Process__Expr arg1)
                | Expr.Sequential (n0) ->
                    instructions.Add (Instruction.Expr_Sequential ((List.length n0)))

                    for elt in n0 do
                        instructions.Add (Instruction.Process__Expr elt)
                | Expr.Builder (arg0, arg1) ->
                    instructions.Add Instruction.Expr_Builder
                    instructions.Add (Instruction.Process__Expr arg0)
                    instructions.Add (Instruction.Process__ExprBuilder arg1)
            | Instruction.Process__ExprBuilder x ->
                match x with
                | ExprBuilder.Child (arg0) ->
                    instructions.Add Instruction.ExprBuilder_Child
                    instructions.Add (Instruction.Process__ExprBuilder arg0)
                | ExprBuilder.Parent (arg0) ->
                    instructions.Add Instruction.ExprBuilder_Parent
                    instructions.Add (Instruction.Process__Expr arg0)
            | Instruction.Expr_Pair (arg2) ->
                let arg0 = exprStack.[exprStack.Count - 1]
                exprStack.RemoveAt (exprStack.Count - 1)
                let arg1 = exprStack.[exprStack.Count - 1]
                exprStack.RemoveAt (exprStack.Count - 1)
                cata.Expr.Pair arg0 arg1 arg2 |> exprStack.Add
            | Instruction.Expr_Sequential (n0) ->
                let n0_len = n0

                let n0 =
                    seq {
                        for i = exprStack.Count - 1 downto exprStack.Count - n0 do
                            yield exprStack.[i]
                    }
                    |> Seq.toList

                exprStack.RemoveRange (exprStack.Count - n0_len, n0_len)
                cata.Expr.Sequential n0 |> exprStack.Add
            | Instruction.Expr_Builder ->
                let arg0 = exprStack.[exprStack.Count - 1]
                exprStack.RemoveAt (exprStack.Count - 1)
                let arg1 = exprBuilderStack.[exprBuilderStack.Count - 1]
                exprBuilderStack.RemoveAt (exprBuilderStack.Count - 1)
                cata.Expr.Builder arg0 arg1 |> exprStack.Add
            | Instruction.ExprBuilder_Child ->
                let arg0 = exprBuilderStack.[exprBuilderStack.Count - 1]
                exprBuilderStack.RemoveAt (exprBuilderStack.Count - 1)
                cata.ExprBuilder.Child arg0 |> exprBuilderStack.Add
            | Instruction.ExprBuilder_Parent ->
                let arg0 = exprStack.[exprStack.Count - 1]
                exprStack.RemoveAt (exprStack.Count - 1)
                cata.ExprBuilder.Parent arg0 |> exprBuilderStack.Add

        exprStack, exprBuilderStack

    /// Execute the catamorphism.
    let runExpr (cata : Cata<'ExprRet, 'ExprBuilderRet>) (x : Expr) : 'ExprRet =
        let instructions = ResizeArray ()
        instructions.Add (Instruction.Process__Expr x)
        let exprRetStack, exprBuilderRetStack = loop cata instructions
        Seq.exactlyOne exprRetStack

    /// Execute the catamorphism.
    let runExprBuilder (cata : Cata<'ExprRet, 'ExprBuilderRet>) (x : ExprBuilder) : 'ExprBuilderRet =
        let instructions = ResizeArray ()
        instructions.Add (Instruction.Process__ExprBuilder x)
        let exprRetStack, exprBuilderRetStack = loop cata instructions
        Seq.exactlyOne exprBuilderRetStack
