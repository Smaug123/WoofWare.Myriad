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

    /// {expr} |> {func}
    let pipeThroughFunction (func : SynExpr) (expr : SynExpr) : SynExpr =
        SynExpr.CreateApp (
            SynExpr.CreateAppInfix (
                SynExpr.CreateLongIdent (
                    SynLongIdent.SynLongIdent (
                        [ Ident.Create "op_PipeRight" ],
                        [],
                        [ Some (IdentTrivia.OriginalNotation "|>") ]
                    )
                ),
                expr
            ),
            func
        )

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
        SynExpr.CreateApp (
            SynExpr.CreateAppInfix (
                SynExpr.CreateLongIdent (
                    SynLongIdent.SynLongIdent (
                        Ident.CreateLong "op_Equality",
                        [],
                        [ Some (IdentTrivia.OriginalNotation "=") ]
                    )
                ),
                a
            ),
            b
        )

    /// {a} + {b}
    let plus (a : SynExpr) (b : SynExpr) =
        SynExpr.CreateApp (
            SynExpr.CreateAppInfix (
                SynExpr.CreateLongIdent (
                    SynLongIdent.SynLongIdent (
                        Ident.CreateLong "op_Addition",
                        [],
                        [ Some (IdentTrivia.OriginalNotation "+") ]
                    )
                ),
                a
            ),
            b
        )

    let stripOptionalParen (expr : SynExpr) : SynExpr =
        match expr with
        | SynExpr.Paren (expr, _, _, _) -> expr
        | expr -> expr

    /// Given e.g. "byte", returns "System.Byte".
    let qualifyPrimitiveType (typeName : string) : LongIdent =
        match typeName with
        | "float32" -> [ "System" ; "Single" ]
        | "float" -> [ "System" ; "Double" ]
        | "byte"
        | "uint8" -> [ "System" ; "Byte" ]
        | "sbyte" -> [ "System" ; "SByte" ]
        | "int16" -> [ "System" ; "Int16" ]
        | "int" -> [ "System" ; "Int32" ]
        | "int64" -> [ "System" ; "Int64" ]
        | "uint16" -> [ "System" ; "UInt16" ]
        | "uint"
        | "uint32" -> [ "System" ; "UInt32" ]
        | "uint64" -> [ "System" ; "UInt64" ]
        | _ -> failwith $"Unable to identify a parsing function `string -> %s{typeName}`"
        |> List.map Ident.Create

    /// {obj}.{meth} {arg}
    let callMethodArg (meth : string) (arg : SynExpr) (obj : SynExpr) : SynExpr =
        SynExpr.CreateApp (
            SynExpr.DotGet (
                obj,
                range0,
                SynLongIdent.SynLongIdent (id = [ Ident.Create meth ], dotRanges = [], trivia = [ None ]),
                range0
            ),
            arg
        )

    /// {obj}.{meth}()
    let callMethod (meth : string) (obj : SynExpr) : SynExpr =
        callMethodArg meth (SynExpr.CreateConst SynConst.Unit) obj

    /// {obj}.{meth}<ty>()
    let callGenericMethod (meth : string) (ty : string) (obj : SynExpr) : SynExpr =
        SynExpr.CreateApp (
            SynExpr.TypeApp (
                SynExpr.DotGet (obj, range0, SynLongIdent.Create [ meth ], range0),
                range0,
                [ SynType.CreateLongIdent ty ],
                [],
                Some range0,
                range0,
                range0
            ),
            SynExpr.CreateConst SynConst.Unit
        )

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
        SynExpr.CreateApp (SynExpr.CreateIdent (Ident.Create "reraise"), SynExpr.CreateConst SynConst.Unit)

    /// {body} |> fun a -> Async.StartAsTask (a, ?cancellationToken=ct)
    let startAsTask (body : SynExpr) =
        let lambda =
            SynExpr.CreateApp (
                SynExpr.CreateLongIdent (SynLongIdent.Create [ "Async" ; "StartAsTask" ]),
                SynExpr.CreateParenedTuple
                    [
                        SynExpr.CreateLongIdent (SynLongIdent.CreateString "a")
                        equals
                            (SynExpr.LongIdent (true, SynLongIdent.CreateString "cancellationToken", None, range0))
                            (SynExpr.CreateLongIdent (SynLongIdent.CreateString "ct"))
                    ]
            )
            |> createLambda "a"

        pipeThroughFunction lambda body

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
                    SynExpr.LetOrUse (
                        false,
                        false,
                        [ SynBinding.Let (pattern = SynPat.CreateNamed (Ident.Create lhs), expr = rhs) ],
                        state,
                        range0,
                        {
                            SynExprLetOrUseTrivia.InKeyword = None
                        }
                    )
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
                | Do body -> SynExpr.Do (body, range0)
            )

        SynExpr.CreateApp (
            SynExpr.CreateIdent (Ident.Create compExpr),
            SynExpr.ComputationExpr (false, contents, range0)
        )

    /// {expr} |> Async.AwaitTask
    let awaitTask (expr : SynExpr) : SynExpr =
        expr
        |> pipeThroughFunction (SynExpr.CreateLongIdent (SynLongIdent.Create [ "Async" ; "AwaitTask" ]))
