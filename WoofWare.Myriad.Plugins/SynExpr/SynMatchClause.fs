namespace WoofWare.Myriad.Plugins

open Fantomas.FCS.Syntax
open Fantomas.FCS.Text.Range

[<RequireQualifiedAccess>]
module internal SynMatchClause =
    let create (lhs : SynPat) (rhs : SynExpr) : SynMatchClause =
        SynMatchClause.SynMatchClause (
            lhs,
            None,
            rhs,
            range0,
            DebugPointAtTarget.Yes,
            {
                ArrowRange = Some range0
                BarRange = Some range0
            }
        )

    let withWhere (where : SynExpr) (m : SynMatchClause) : SynMatchClause =
        match m with
        | SynMatchClause (synPat, _, resultExpr, range, debugPointAtTarget, synMatchClauseTrivia) ->
            SynMatchClause (synPat, Some where, resultExpr, range, debugPointAtTarget, synMatchClauseTrivia)
