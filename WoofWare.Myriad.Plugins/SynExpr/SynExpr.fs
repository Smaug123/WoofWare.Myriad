namespace WoofWare.Myriad.Plugins

open Fantomas.FCS.Syntax
open Fantomas.FCS.SyntaxTrivia
open Fantomas.FCS.Text.Range

[<AutoOpen>]
module internal SynExprExtensions =
    type SynExpr with
        static member CreateConst (s : string) : SynExpr =
            SynExpr.Const (SynConst.Create s, range0)

        static member CreateConst () : SynExpr = SynExpr.Const (SynConst.Unit, range0)

        static member CreateConst (b : bool) : SynExpr = SynExpr.Const (SynConst.Bool b, range0)

        static member CreateConst (c : char) : SynExpr =
            // apparent Myriad bug: `IndexOf '?'` gets formatted as `IndexOf ?` which is clearly wrong
            SynExpr.App (
                ExprAtomicFlag.NonAtomic,
                false,
                SynExpr.Ident (Ident.create "char"),
                SynExpr.CreateConst (int c),
                range0
            )
            |> fun e -> SynExpr.Paren (e, range0, Some range0, range0)

        static member CreateConst (i : int32) : SynExpr =
            SynExpr.Const (SynConst.Int32 i, range0)

[<RequireQualifiedAccess>]
module internal SynExpr =

    /// {f} {x}
    let applyFunction (f : SynExpr) (x : SynExpr) : SynExpr =
        SynExpr.App (ExprAtomicFlag.NonAtomic, false, f, x, range0)

    /// {f} {x}
    let inline applyTo (x : SynExpr) (f : SynExpr) : SynExpr = applyFunction f x

    let inline private createAppInfix (f : SynExpr) (x : SynExpr) =
        SynExpr.App (ExprAtomicFlag.NonAtomic, true, f, x, range0)

    let inline createLongIdent'' (ident : SynLongIdent) : SynExpr =
        SynExpr.LongIdent (false, ident, None, range0)

    let inline createLongIdent' (ident : Ident list) : SynExpr =
        createLongIdent'' (SynLongIdent.create ident)

    let inline createLongIdent (ident : string list) : SynExpr =
        createLongIdent' (ident |> List.map Ident.create)

    /// {expr} |> {func}
    let pipeThroughFunction (func : SynExpr) (expr : SynExpr) : SynExpr =
        createAppInfix (createLongIdent'' SynLongIdent.pipe) expr |> applyTo func

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
            SynMatchClause.create (SynPat.As (exc, SynPat.named "exc", range0)) handler

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
        createAppInfix (createLongIdent'' SynLongIdent.eq) a |> applyTo b

    /// {a} && {b}
    let booleanAnd (a : SynExpr) (b : SynExpr) =
        createAppInfix (createLongIdent'' SynLongIdent.booleanAnd) a |> applyTo b

    /// {a} || {b}
    let booleanOr (a : SynExpr) (b : SynExpr) =
        createAppInfix (createLongIdent'' SynLongIdent.booleanOr) a |> applyTo b

    /// {a} + {b}
    let plus (a : SynExpr) (b : SynExpr) =
        createAppInfix (createLongIdent'' SynLongIdent.plus) a |> applyTo b

    /// {a} * {b}
    let times (a : SynExpr) (b : SynExpr) =
        createAppInfix (createLongIdent'' SynLongIdent.times) a |> applyTo b

    let rec stripOptionalParen (expr : SynExpr) : SynExpr =
        match expr with
        | SynExpr.Paren (expr, _, _, _) -> stripOptionalParen expr
        | expr -> expr

    let dotGet (field : string) (obj : SynExpr) : SynExpr =
        SynExpr.DotGet (
            obj,
            range0,
            SynLongIdent.SynLongIdent (id = [ Ident.create field ], dotRanges = [], trivia = [ None ]),
            range0
        )

    /// {obj}.{meth} {arg}
    let callMethodArg (meth : string) (arg : SynExpr) (obj : SynExpr) : SynExpr = dotGet meth obj |> applyTo arg

    /// {obj}.{meth}()
    let callMethod (meth : string) (obj : SynExpr) : SynExpr =
        callMethodArg meth (SynExpr.CreateConst ()) obj

    let typeApp (types : SynType list) (operand : SynExpr) =
        SynExpr.TypeApp (operand, range0, types, List.replicate (types.Length - 1) range0, Some range0, range0, range0)

    /// {obj}.{meth}<types,...>()
    let callGenericMethod (meth : SynLongIdent) (types : SynType list) (obj : SynExpr) : SynExpr =
        SynExpr.DotGet (obj, range0, meth, range0)
        |> typeApp types
        |> applyTo (SynExpr.CreateConst ())

    /// {obj}.{meth}<ty>()
    let callGenericMethod' (meth : string) (ty : string) (obj : SynExpr) : SynExpr =
        callGenericMethod (SynLongIdent.createS meth) [ SynType.createLongIdent' [ ty ] ] obj

    let inline index (property : SynExpr) (obj : SynExpr) : SynExpr =
        SynExpr.DotIndexedGet (obj, property, range0, range0)

    let inline arrayIndexRange (start : SynExpr option) (endRange : SynExpr option) (arr : SynExpr) : SynExpr =
        SynExpr.DotIndexedGet (
            arr,
            (SynExpr.IndexRange (start, range0, endRange, range0, range0, range0)),
            range0,
            range0
        )

    let inline paren (e : SynExpr) : SynExpr =
        SynExpr.Paren (e, range0, Some range0, range0)

    /// (fun {varName} -> {body})
    let createLambda (varName : string) (body : SynExpr) : SynExpr =
        let parsedDataPat = [ SynPat.named varName ]

        SynExpr.Lambda (
            false,
            false,
            SynSimplePats.create [ SynSimplePat.createId (Ident.create varName) ],
            body,
            Some (parsedDataPat, body),
            range0,
            {
                ArrowRange = Some range0
            }
        )
        |> paren

    let createThunk (body : SynExpr) : SynExpr =
        SynExpr.Lambda (
            false,
            false,
            SynSimplePats.create [],
            body,
            Some ([ SynPat.unit ], body),
            range0,
            {
                ArrowRange = Some range0
            }
        )
        |> paren

    let inline createIdent (s : string) : SynExpr = SynExpr.Ident (Ident (s, range0))

    let inline createIdent' (i : Ident) : SynExpr = SynExpr.Ident i

    let tupleNoParen (args : SynExpr list) : SynExpr =
        SynExpr.Tuple (false, args, List.replicate (args.Length - 1) range0, range0)

    let inline tuple (args : SynExpr list) = args |> tupleNoParen |> paren

    /// {body} |> fun a -> Async.StartAsTask (a, ?cancellationToken=ct)
    let startAsTask (ct : Ident) (body : SynExpr) =
        let lambda =
            [
                createIdent "a"
                equals
                    (SynExpr.LongIdent (true, SynLongIdent.createS "cancellationToken", None, range0))
                    (createIdent' ct)
            ]
            |> tuple
            |> applyFunction (createLongIdent [ "Async" ; "StartAsTask" ])
            |> createLambda "a"

        pipeThroughFunction lambda body

    let inline createForEach (pat : SynPat) (enumExpr : SynExpr) (body : SynExpr) : SynExpr =
        SynExpr.ForEach (
            DebugPointAtFor.No,
            DebugPointAtInOrTo.No,
            SeqExprOnly.SeqExprOnly false,
            true,
            pat,
            enumExpr,
            body,
            range0
        )

    let inline createLet (bindings : SynBinding list) (body : SynExpr) : SynExpr =
        SynExpr.LetOrUse (false, false, bindings, body, range0, SynExprLetOrUseTrivia.empty)

    let inline createDo (body : SynExpr) : SynExpr = SynExpr.Do (body, range0)

    let inline createMatch (matchOn : SynExpr) (cases : SynMatchClause list) : SynExpr =
        SynExpr.Match (
            DebugPointAtBinding.Yes range0,
            matchOn,
            cases,
            range0,
            {
                MatchKeyword = range0
                WithKeyword = range0
            }
        )

    let typeAnnotate (ty : SynType) (expr : SynExpr) : SynExpr = SynExpr.Typed (expr, ty, range0)

    let inline createNew (ty : SynType) (args : SynExpr) : SynExpr =
        SynExpr.New (false, ty, paren args, range0)

    let inline createWhile (cond : SynExpr) (body : SynExpr) : SynExpr =
        SynExpr.While (DebugPointAtWhile.Yes range0, cond, body, range0)

    let inline createNull () : SynExpr = SynExpr.Null range0

    let reraise : SynExpr = createIdent "reraise" |> applyTo (SynExpr.CreateConst ())

    let sequential (exprs : SynExpr list) : SynExpr =
        exprs
        |> List.reduce (fun a b -> SynExpr.Sequential (DebugPointAtSequential.SuppressNeither, false, a, b, range0))

    let listLiteral (elts : SynExpr list) : SynExpr =
        SynExpr.ArrayOrListComputed (false, sequential elts, range0)

    let arrayLiteral (elts : SynExpr list) : SynExpr =
        SynExpr.ArrayOrListComputed (true, sequential elts, range0)

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
                        SynPat.named lhs,
                        rhs,
                        [],
                        state,
                        range0,
                        {
                            EqualsRange = Some range0
                        }
                    )
                | Let (lhs, rhs) -> createLet [ SynBinding.basic [ Ident.create lhs ] [] rhs ] state
                | Use (lhs, rhs) ->
                    SynExpr.LetOrUse (
                        false,
                        true,
                        [ SynBinding.basic [ Ident.create lhs ] [] rhs ],
                        state,
                        range0,
                        {
                            SynExprLetOrUseTrivia.InKeyword = None
                        }
                    )
                | Do body -> sequential [ SynExpr.Do (body, range0) ; state ]
            )

        applyFunction (createIdent compExpr) (SynExpr.ComputationExpr (false, contents, range0))

    /// {expr} |> Async.AwaitTask
    let awaitTask (expr : SynExpr) : SynExpr =
        expr |> pipeThroughFunction (createLongIdent [ "Async" ; "AwaitTask" ])

    /// {ident}.ToString ()
    /// with special casing for some types like DateTime
    let toString (ty : SynType) (ident : SynExpr) =
        match ty with
        | DateOnly -> ident |> callMethodArg "ToString" (SynExpr.CreateConst "yyyy-MM-dd")
        | DateTime -> ident |> callMethodArg "ToString" (SynExpr.CreateConst "yyyy-MM-ddTHH:mm:ss")
        | _ -> callMethod "ToString" ident

    let upcast' (ty : SynType) (e : SynExpr) = SynExpr.Upcast (e, ty, range0)

    /// {ident} - {rhs}
    let minus (ident : SynLongIdent) (rhs : SynExpr) : SynExpr =
        createAppInfix (createLongIdent'' SynLongIdent.sub) (createLongIdent'' ident)
        |> applyTo rhs

    /// {ident} - {n}
    let minusN (ident : SynLongIdent) (n : int) : SynExpr = minus ident (SynExpr.CreateConst n)

    /// {y} > {x}
    let greaterThan (x : SynExpr) (y : SynExpr) : SynExpr =
        createAppInfix (createLongIdent'' SynLongIdent.gt) y |> applyTo x

    /// {y} < {x}
    let lessThan (x : SynExpr) (y : SynExpr) : SynExpr =
        createAppInfix (createLongIdent'' SynLongIdent.lt) y |> applyTo x

    /// {y} >= {x}
    let greaterThanOrEqual (x : SynExpr) (y : SynExpr) : SynExpr =
        createAppInfix (createLongIdent'' SynLongIdent.geq) y |> applyTo x

    /// {y} <= {x}
    let lessThanOrEqual (x : SynExpr) (y : SynExpr) : SynExpr =
        createAppInfix (createLongIdent'' SynLongIdent.leq) y |> applyTo x

    /// {x} :: {y}
    let listCons (x : SynExpr) (y : SynExpr) : SynExpr =
        createAppInfix
            (SynExpr.LongIdent (
                false,
                SynLongIdent.SynLongIdent (
                    [ Ident.create "op_ColonColon" ],
                    [],
                    [ Some (IdentTrivia.OriginalNotation "::") ]
                ),
                None,
                range0
            ))
            (tupleNoParen [ x ; y ])
        |> paren

    let assign (lhs : SynLongIdent) (rhs : SynExpr) : SynExpr = SynExpr.LongIdentSet (lhs, rhs, range0)

    let assignIndex (lhs : SynExpr) (index : SynExpr) (rhs : SynExpr) : SynExpr =
        SynExpr.DotIndexedSet (lhs, index, rhs, range0, range0, range0)
