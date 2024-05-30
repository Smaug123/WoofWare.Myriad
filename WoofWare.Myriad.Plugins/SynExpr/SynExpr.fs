namespace WoofWare.Myriad.Plugins

open Fantomas.FCS.Syntax
open Fantomas.FCS.SyntaxTrivia
open Myriad.Core
open Myriad.Core.Ast
open Fantomas.FCS.Text.Range

type internal CompExprBinding =
    | LetBang of varName : string * rhs : SynExpr
    | Let of varName : string * rhs : SynExpr
    | Use of varName : string * rhs : SynExpr
    | Do of body : SynExpr

[<RequireQualifiedAccess>]
module internal SynExpr =

    /// {f} {x}
    let applyFunction (f : SynExpr) (x : SynExpr) : SynExpr = SynExpr.CreateApp (f, x)

    /// {f} {x}
    let applyTo (x : SynExpr) (f : SynExpr) : SynExpr = SynExpr.CreateApp (f, x)

    /// {expr} |> {func}
    let pipeThroughFunction (func : SynExpr) (expr : SynExpr) : SynExpr =
        SynExpr.CreateAppInfix (
            SynExpr.CreateLongIdent (
                SynLongIdent.SynLongIdent (
                    [ Ident.Create "op_PipeRight" ],
                    [],
                    [ Some (IdentTrivia.OriginalNotation "|>") ]
                )
            ),
            expr
        )
        |> applyTo func

    /// if {cond} then {trueBranch} else {falseBranch}
    /// Note that this function puts the trueBranch last, for pipelining convenience:
    /// we assume that the `else` branch is more like an error case and is less interesting.
    let ifThenElse (cond : SynExpr) (falseBranch : SynExpr) (trueBranch : SynExpr) : SynExpr =
        SynExpr.IfThenElse (
            cond,
            trueBranch,
            Some falseBranch,
            DebugPointAtBinding.Yes range0,
            false,
            range0,
            {
                IfKeyword = range0
                IsElif = false
                ThenKeyword = range0
                ElseKeyword = Some range0
                IfToThenRange = range0
            }
        )

    /// try {body} with | {exc} as exc -> {handler}
    let pipeThroughTryWith (exc : SynPat) (handler : SynExpr) (body : SynExpr) : SynExpr =
        let clause =
            SynMatchClause.Create (SynPat.As (exc, SynPat.CreateNamed (Ident.Create "exc"), range0), None, handler)

        SynExpr.TryWith (
            body,
            [ clause ],
            range0,
            DebugPointAtTry.Yes range0,
            DebugPointAtWith.Yes range0,
            {
                TryKeyword = range0
                TryToWithRange = range0
                WithKeyword = range0
                WithToEndRange = range0
            }
        )

    /// {a} = {b}
    let equals (a : SynExpr) (b : SynExpr) =
        SynExpr.CreateAppInfix (
            SynExpr.CreateLongIdent (
                SynLongIdent.SynLongIdent (
                    Ident.CreateLong "op_Equality",
                    [],
                    [ Some (IdentTrivia.OriginalNotation "=") ]
                )
            ),
            a
        )
        |> applyTo b

    /// {a} + {b}
    let plus (a : SynExpr) (b : SynExpr) =
        SynExpr.CreateAppInfix (
            SynExpr.CreateLongIdent (
                SynLongIdent.SynLongIdent (
                    Ident.CreateLong "op_Addition",
                    [],
                    [ Some (IdentTrivia.OriginalNotation "+") ]
                )
            ),
            a
        )
        |> applyTo b

    let rec stripOptionalParen (expr : SynExpr) : SynExpr =
        match expr with
        | SynExpr.Paren (expr, _, _, _) -> stripOptionalParen expr
        | expr -> expr

    /// {obj}.{meth} {arg}
    let callMethodArg (meth : string) (arg : SynExpr) (obj : SynExpr) : SynExpr =
        SynExpr.DotGet (
            obj,
            range0,
            SynLongIdent.SynLongIdent (id = [ Ident.Create meth ], dotRanges = [], trivia = [ None ]),
            range0
        )
        |> applyTo arg

    /// {obj}.{meth}()
    let callMethod (meth : string) (obj : SynExpr) : SynExpr =
        callMethodArg meth (SynExpr.CreateConst SynConst.Unit) obj

    let callGenericMethod (meth : string) (ty : LongIdent) (obj : SynExpr) : SynExpr =
        SynExpr.TypeApp (
            SynExpr.DotGet (obj, range0, SynLongIdent.Create [ meth ], range0),
            range0,
            [ SynType.LongIdent (SynLongIdent.CreateFromLongIdent ty) ],
            [],
            Some range0,
            range0,
            range0
        )
        |> applyTo (SynExpr.CreateConst SynConst.Unit)

    /// {obj}.{meth}<ty>()
    let callGenericMethod' (meth : string) (ty : string) (obj : SynExpr) : SynExpr =
        SynExpr.TypeApp (
            SynExpr.DotGet (obj, range0, SynLongIdent.Create [ meth ], range0),
            range0,
            [ SynType.CreateLongIdent ty ],
            [],
            Some range0,
            range0,
            range0
        )
        |> applyTo (SynExpr.CreateConst SynConst.Unit)

    let index (property : SynExpr) (obj : SynExpr) : SynExpr =
        SynExpr.DotIndexedGet (obj, property, range0, range0)

    /// (fun {varName} -> {body})
    let createLambda (varName : string) (body : SynExpr) : SynExpr =
        let parsedDataPat = [ SynPat.CreateNamed (Ident.Create varName) ]

        SynExpr.Lambda (
            false,
            false,
            SynSimplePats.Create [ SynSimplePat.CreateId (Ident.Create varName) ],
            body,
            Some (parsedDataPat, body),
            range0,
            {
                ArrowRange = Some range0
            }
        )
        |> SynExpr.CreateParen

    let reraise : SynExpr =
        SynExpr.CreateIdent (Ident.Create "reraise")
        |> applyTo (SynExpr.CreateConst SynConst.Unit)

    /// {body} |> fun a -> Async.StartAsTask (a, ?cancellationToken=ct)
    let startAsTask (ct : SynLongIdent) (body : SynExpr) =
        let lambda =
            [
                SynExpr.CreateLongIdent (SynLongIdent.CreateString "a")
                equals
                    (SynExpr.LongIdent (true, SynLongIdent.CreateString "cancellationToken", None, range0))
                    (SynExpr.CreateLongIdent ct)
            ]
            |> SynExpr.CreateParenedTuple
            |> applyFunction (SynExpr.CreateLongIdent (SynLongIdent.Create [ "Async" ; "StartAsTask" ]))
            |> createLambda "a"

        pipeThroughFunction lambda body

    let createLongIdent (ident : string list) : SynExpr =
        SynExpr.CreateLongIdent (SynLongIdent.Create ident)

    let createLongIdent' (ident : Ident list) : SynExpr =
        SynExpr.CreateLongIdent (SynLongIdent.CreateFromLongIdent ident)

    let createLet (bindings : SynBinding list) (body : SynExpr) : SynExpr =
        SynExpr.LetOrUse (false, false, bindings, body, range0, SynExprLetOrUseTrivia.empty)

    let createMatch (matchOn : SynExpr) (cases : SynMatchClause list) : SynExpr = SynExpr.CreateMatch (matchOn, cases)

    let typeAnnotate (ty : SynType) (expr : SynExpr) : SynExpr = SynExpr.CreateTyped (expr, ty)

    /// {compExpr} { {lets} ; return {ret} }
    let createCompExpr (compExpr : string) (retBody : SynExpr) (lets : CompExprBinding list) : SynExpr =
        let retStatement = SynExpr.YieldOrReturn ((false, true), retBody, range0)

        let contents : SynExpr =
            (retStatement, List.rev lets)
            ||> List.fold (fun state binding ->
                match binding with
                | LetBang (lhs, rhs) ->
                    SynExpr.LetOrUseBang (
                        DebugPointAtBinding.Yes range0,
                        false,
                        true,
                        SynPat.CreateNamed (Ident.Create lhs),
                        rhs,
                        [],
                        state,
                        range0,
                        {
                            EqualsRange = Some range0
                        }
                    )
                | Let (lhs, rhs) ->
                    createLet [ SynBinding.Let (pattern = SynPat.CreateNamed (Ident.Create lhs), expr = rhs) ] state
                | Use (lhs, rhs) ->
                    SynExpr.LetOrUse (
                        false,
                        true,
                        [ SynBinding.Let (pattern = SynPat.CreateNamed (Ident.Create lhs), expr = rhs) ],
                        state,
                        range0,
                        {
                            SynExprLetOrUseTrivia.InKeyword = None
                        }
                    )
                | Do body -> SynExpr.CreateSequential [ SynExpr.Do (body, range0) ; state ]
            )

        SynExpr.CreateApp (
            SynExpr.CreateIdent (Ident.Create compExpr),
            SynExpr.ComputationExpr (false, contents, range0)
        )

    /// {expr} |> Async.AwaitTask
    let awaitTask (expr : SynExpr) : SynExpr =
        expr
        |> pipeThroughFunction (SynExpr.CreateLongIdent (SynLongIdent.Create [ "Async" ; "AwaitTask" ]))

    /// {ident}.ToString ()
    /// with special casing for some types like DateTime
    let toString (ty : SynType) (ident : SynExpr) =
        match ty with
        | DateOnly -> ident |> callMethodArg "ToString" (SynExpr.CreateConstString "yyyy-MM-dd")
        | DateTime ->
            ident
            |> callMethodArg "ToString" (SynExpr.CreateConstString "yyyy-MM-ddTHH:mm:ss")
        | _ -> callMethod "ToString" ident

    let upcast' (ty : SynType) (e : SynExpr) = SynExpr.Upcast (e, ty, range0)

    /// {ident} - {rhs}
    let minus (ident : SynLongIdent) (rhs : SynExpr) : SynExpr =
        SynExpr.CreateApp (
            SynExpr.CreateAppInfix (
                SynExpr.CreateLongIdent (
                    SynLongIdent.SynLongIdent (
                        [ Ident.Create "op_Subtraction" ],
                        [],
                        [ Some (IdentTrivia.OriginalNotation "-") ]
                    )
                ),
                SynExpr.CreateLongIdent ident
            ),
            rhs
        )

    /// {ident} - {n}
    let minusN (ident : SynLongIdent) (n : int) : SynExpr =
        minus ident (SynExpr.CreateConst (SynConst.Int32 n))

    /// {y} > {x}
    let greaterThan (x : SynExpr) (y : SynExpr) : SynExpr =
        SynExpr.CreateApp (
            SynExpr.CreateAppInfix (
                SynExpr.CreateLongIdent (
                    SynLongIdent.SynLongIdent (
                        [ Ident.Create "op_GreaterThan" ],
                        [],
                        [ Some (IdentTrivia.OriginalNotation ">") ]
                    )
                ),
                y
            ),
            x
        )

    /// {y} >= {x}
    let greaterThanOrEqual (x : SynExpr) (y : SynExpr) : SynExpr =
        SynExpr.CreateAppInfix (
            SynExpr.CreateLongIdent (
                SynLongIdent.SynLongIdent (
                    [ Ident.Create "op_GreaterThanOrEqual" ],
                    [],
                    [ Some (IdentTrivia.OriginalNotation ">=") ]
                )
            ),
            y
        )
        |> applyTo x
