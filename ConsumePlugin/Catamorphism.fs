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

and [<CreateCatamorphism>] ExprBuilder =
    | Child of ExprBuilder
    | Parent of Expr

// Say that CreateCatamorphism-tagged types form the set T.
// Assert that each U in T is a discriminated union.
// For each type U in T, assign a generic parameter 'ret<U>.
// For each U:
//   * Define the type [U]Cata, generic on all the parameters {'ret<U> : U in T}.
//   * For each DU case C in type U:
//     * create a method in [U]Cata, whose return value is 'ret<U> and whose args are the fields of the case C
//     * any occurrence in a field of an input value of type equal to any element of T (say type V) is replaced by 'ret<V>
// Finally, define a type Cata<{'ret<U> for U in T}>
// with one member for each U, namely of type [U]Cata<{'ret<U> for U in T}>.
type ExprCata<'builderRet, 'ret> =
    abstract Const : Const -> 'ret
    abstract Pair : 'ret -> 'ret -> PairOpKind -> 'ret
    abstract Sequential : 'ret list -> 'ret
    abstract Builder : 'ret -> 'builderRet -> 'ret

type ExprBuilderCata<'builderRet, 'ret> =
    abstract Child : 'builderRet -> 'builderRet
    abstract Parent : 'ret -> 'builderRet

type Cata<'bret, 'ret> =
    {
        Expr : ExprCata<'bret, 'ret>
        Builder : ExprBuilderCata<'bret, 'ret>
    }

// Then we can create the noddy non-tail-rec implementation of `apply`.
// For each U in T, define apply{U}, generic on every {'ret<U> for U in T}, taking a Cata and a U and returning a 'ret<U>.
// The body of apply{U} is given by matching on the cases of U.
module Cata =
    let rec apply<'bret, 'ret> (cata : Cata<'bret, 'ret>) (e : Expr) : 'ret =
        match e with
        | Const c -> cata.Expr.Const c
        | Pair(expr, expr1, pairOpKind) -> cata.Expr.Pair (apply cata expr) (apply cata expr1) pairOpKind
        | Sequential exprs -> exprs |> List.map (apply cata) |> cata.Expr.Sequential
        | Builder(expr, exprBuilder) -> cata.Expr.Builder (apply cata expr) (applyB cata exprBuilder)

    and applyB<'bret, 'ret> (cata : Cata<'bret, 'ret>) (e : ExprBuilder) : 'bret =
        match e with
        | Child b -> cata.Builder.Child (applyB cata b)
        | Parent p -> cata.Builder.Parent (apply cata p)

// The tail-recursive version is harder.
module TailRecCata =
    [<RequireQualifiedAccess>]
    type private Instruction =
        | ProcessExpr of Expr
        | ProcessBuilder of ExprBuilder
        | Pair of PairOpKind
        | Sequential of int
        | Builder
        | Child
        | Parent

    let private loop (cata : Cata<_, _>) (instructions : ResizeArray<_>) =
        let resultsStack = ResizeArray ()
        let builderResultsStack = ResizeArray ()

        while instructions.Count > 0 do
            let currentInstruction = instructions.[instructions.Count - 1]
            instructions.RemoveAt (instructions.Count - 1)

            match currentInstruction with
            | Instruction.ProcessBuilder builder ->
                match builder with
                | Child exprBuilder ->
                    instructions.Add Instruction.Child
                    instructions.Add (Instruction.ProcessBuilder exprBuilder)
                | Parent expr ->
                    instructions.Add Instruction.Parent
                    instructions.Add (Instruction.ProcessExpr expr)
            | Instruction.ProcessExpr currentExpr ->
                match currentExpr with
                | Const c ->
                    resultsStack.Add (cata.Expr.Const c)
                | Pair(expr, expr1, pairOpKind) ->
                    instructions.Add (Instruction.Pair pairOpKind)
                    instructions.Add (Instruction.ProcessExpr expr1)
                    instructions.Add (Instruction.ProcessExpr expr)
                | Sequential exprs ->
                    instructions.Add (Instruction.Sequential (List.length exprs))
                    for expr in exprs do
                        instructions.Add (Instruction.ProcessExpr expr)
                | Builder(expr, exprBuilder) ->
                    instructions.Add Instruction.Builder
                    instructions.Add (Instruction.ProcessExpr expr)
                    instructions.Add (Instruction.ProcessBuilder exprBuilder)
            | Instruction.Pair pairOpKind ->
                let expr = resultsStack.[resultsStack.Count - 1]
                let expr1 = resultsStack.[resultsStack.Count - 2]
                resultsStack.RemoveRange (resultsStack.Count - 2, 2)
                cata.Expr.Pair expr expr1 pairOpKind
                |> resultsStack.Add
            | Instruction.Sequential count ->
                let values =
                    seq {
                        for i = resultsStack.Count - 1 downto resultsStack.Count - count do
                            yield resultsStack.[i]
                    }
                    |> Seq.toList
                resultsStack.RemoveRange (resultsStack.Count - count, count)
                cata.Expr.Sequential values
                |> resultsStack.Add
            | Instruction.Builder ->
                let expr = resultsStack.[resultsStack.Count - 1]
                resultsStack.RemoveAt (resultsStack.Count - 1)
                let exprBuilder = builderResultsStack.[builderResultsStack.Count - 1]
                builderResultsStack.RemoveAt (builderResultsStack.Count - 1)
                cata.Expr.Builder expr exprBuilder
                |> resultsStack.Add
            | Instruction.Child ->
                let exprBuilder = builderResultsStack.[builderResultsStack.Count - 1]
                builderResultsStack.RemoveAt (builderResultsStack.Count - 1)
                cata.Builder.Child exprBuilder
                |> builderResultsStack.Add
            | Instruction.Parent ->
                let expr = resultsStack.[resultsStack.Count - 1]
                resultsStack.RemoveAt (resultsStack.Count - 1)
                cata.Builder.Parent expr
                |> builderResultsStack.Add

        resultsStack, builderResultsStack

    let go (cata : Cata<'bret, 'ret>) (e : Expr) : 'ret =
        let instructions = ResizeArray ()
        instructions.Add (Instruction.ProcessExpr e)

        let resultsStack, builderResultsStack = loop cata instructions

        if builderResultsStack.Count > 0 then failwith "logic error"
        Seq.exactlyOne resultsStack

    let goBuilder (cata : Cata<'bret, 'ret>) (e : ExprBuilder) : 'bret =
        let instructions = ResizeArray ()
        instructions.Add (Instruction.ProcessBuilder e)

        let resultsStack, builderResultsStack = loop cata instructions

        if resultsStack.Count > 0 then failwith "logic error"
        Seq.exactlyOne builderResultsStack
