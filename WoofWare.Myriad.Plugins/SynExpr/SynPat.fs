namespace WoofWare.Myriad.Plugins

open Fantomas.FCS.Syntax
open Fantomas.FCS.Text.Range

[<RequireQualifiedAccess>]
module internal SynPat =

    let inline annotateType (ty : SynType) (pat : SynPat) =
        SynPat.Paren (SynPat.Typed (pat, ty, range0), range0)

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

    let inline paren (pat : SynPat) : SynPat = SynPat.Paren (pat, range0)

    let inline tuple (elements : SynPat list) : SynPat = tupleNoParen elements |> paren

    let unit = SynPat.Const (SynConst.Unit, range0)

    let createNull = SynPat.Null range0
