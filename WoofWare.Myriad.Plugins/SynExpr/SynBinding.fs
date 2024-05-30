namespace WoofWare.Myriad.Plugins

open Fantomas.FCS.Syntax
open Fantomas.FCS.SyntaxTrivia
open Fantomas.FCS.Xml
open Fantomas.FCS.Text.Range

[<RequireQualifiedAccess>]
module internal SynBinding =

    let rec private stripParen (pat : SynPat) =
        match pat with
        | SynPat.Paren (p, _) -> stripParen p
        | _ -> pat

    let rec private getName (pat : SynPat) : Ident option =
        match stripParen pat with
        | SynPat.Named (SynIdent.SynIdent (name, _), _, _, _) -> Some name
        | SynPat.Wild _ -> None
        | SynPat.Typed (pat, _, _) -> getName pat
        | SynPat.LongIdent (SynLongIdent.SynLongIdent (longIdent, _, _), _, _, _, _, _) ->
            match longIdent with
            | [ x ] -> Some x
            | _ -> failwithf "got long ident %O ; can only get the name of a long ident with one component" longIdent
        | _ -> failwithf "unrecognised pattern: %+A" pat

    let basic (name : SynLongIdent) (args : SynPat list) (body : SynExpr) : SynBinding =
        let valInfo : SynValInfo =
            args
            |> List.map (fun pat -> [ SynArgInfo.SynArgInfo (SynAttributes.Empty, false, getName pat) ])
            |> fun x -> SynValInfo.SynValInfo (x, SynArgInfo.SynArgInfo ([], false, None))

        SynBinding.SynBinding (
            None,
            SynBindingKind.Normal,
            false,
            false,
            [],
            PreXmlDoc.Empty,
            SynValData.SynValData (None, valInfo, None),
            SynPat.LongIdent (name, None, None, SynArgPats.Pats args, None, range0),
            None,
            body,
            range0,
            DebugPointAtBinding.Yes range0,
            SynExpr.synBindingTriviaZero false
        )

    let withAccessibility (acc : SynAccess option) (binding : SynBinding) : SynBinding =
        match binding with
        | SynBinding (_, kind, inl, mut, attrs, xml, valData, headPat, returnInfo, expr, range, debugPoint, trivia) ->
            SynBinding (acc, kind, inl, mut, attrs, xml, valData, headPat, returnInfo, expr, range, debugPoint, trivia)

    let withXmlDoc (doc : PreXmlDoc) (binding : SynBinding) : SynBinding =
        match binding with
        | SynBinding (acc, kind, inl, mut, attrs, _, valData, headPat, returnInfo, expr, range, debugPoint, trivia) ->
            SynBinding (acc, kind, inl, mut, attrs, doc, valData, headPat, returnInfo, expr, range, debugPoint, trivia)

    let withReturnAnnotation (ty : SynType) (binding : SynBinding) : SynBinding =
        match binding with
        | SynBinding (acc, kind, inl, mut, attrs, doc, valData, headPat, _, expr, range, debugPoint, trivia) ->
            let retInfo =
                SynBindingReturnInfo.SynBindingReturnInfo (
                    ty,
                    range0,
                    [],
                    {
                        ColonRange = Some range0
                    }
                )

            SynBinding (
                acc,
                kind,
                inl,
                mut,
                attrs,
                doc,
                valData,
                headPat,
                Some retInfo,
                expr,
                range,
                debugPoint,
                trivia
            )
