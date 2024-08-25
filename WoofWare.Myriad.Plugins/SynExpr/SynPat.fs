namespace WoofWare.Myriad.Plugins

open Fantomas.FCS.Syntax
open Fantomas.FCS.Text.Range

[<RequireQualifiedAccess>]
module internal SynPat =
    let inline paren (pat : SynPat) : SynPat = SynPat.Paren (pat, range0)

    let inline annotateTypeNoParen (ty : SynType) (pat : SynPat) = SynPat.Typed (pat, ty, range0)

    let inline annotateType (ty : SynType) (pat : SynPat) = paren (annotateTypeNoParen ty pat)

    let inline named (s : string) : SynPat =
        SynPat.Named (SynIdent.SynIdent (Ident (s, range0), None), false, None, range0)

    let inline namedI (i : Ident) : SynPat =
        SynPat.Named (SynIdent.SynIdent (i, None), false, None, range0)

    let inline identWithArgs (i : LongIdent) (args : SynArgPats) : SynPat =
        SynPat.LongIdent (SynLongIdent.create i, None, None, args, None, range0)

    let inline tupleNoParen (elements : SynPat list) : SynPat =
        match elements with
        | [] -> failwith "Can't tuple no elements in a pattern"
        | [ p ] -> p
        | elements -> SynPat.Tuple (false, elements, List.replicate (elements.Length - 1) range0, range0)

    let inline tuple (elements : SynPat list) : SynPat = tupleNoParen elements |> paren

    let inline createConst (c : SynConst) = SynPat.Const (c, range0)

    let unit = createConst SynConst.Unit

    let createNull = SynPat.Null range0

    let emptyList = SynPat.ArrayOrList (false, [], range0)

    let listCons (lhs : SynPat) (rhs : SynPat) =
        SynPat.ListCons (
            lhs,
            rhs,
            range0,
            {
                ColonColonRange = range0
            }
        )

    let emptyArray = SynPat.ArrayOrList (true, [], range0)
