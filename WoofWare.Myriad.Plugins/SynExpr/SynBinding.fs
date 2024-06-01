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
        | SynPat.Typed (pat, _, _) -> getName pat
        | SynPat.LongIdent (SynLongIdent.SynLongIdent (longIdent, _, _), _, _, _, _, _) ->
            match longIdent with
            | [ x ] -> Some x
            | _ -> failwithf "got long ident %O ; can only get the name of a long ident with one component" longIdent
        | _ -> None

    let private getArgInfo (pat : SynPat) : SynArgInfo list =
        // TODO: this only copes with one layer of tupling
        match stripParen pat with
        | SynPat.Tuple (_, pats, _, _) -> pats |> List.map (fun pat -> SynArgInfo.SynArgInfo ([], false, getName pat))
        | pat -> [ SynArgInfo.SynArgInfo (SynAttributes.Empty, false, getName pat) ]

    let triviaZero (isMember : bool) =
        {
            SynBindingTrivia.EqualsRange = Some range0
            InlineKeyword = None
            LeadingKeyword =
                if isMember then
                    SynLeadingKeyword.Member range0
                else
                    SynLeadingKeyword.Let range0
        }

    let basic (name : LongIdent) (args : SynPat list) (body : SynExpr) : SynBinding =
        let valInfo : SynValInfo =
            args
            |> List.map getArgInfo
            |> fun x -> SynValInfo.SynValInfo (x, SynArgInfo.SynArgInfo ([], false, None))

        SynBinding.SynBinding (
            None,
            SynBindingKind.Normal,
            false,
            false,
            [],
            PreXmlDoc.Empty,
            SynValData.SynValData (None, valInfo, None),
            SynPat.identWithArgs name (SynArgPats.Pats args),
            None,
            body,
            range0,
            DebugPointAtBinding.Yes range0,
            triviaZero false
        )

    let withAccessibility (acc : SynAccess option) (binding : SynBinding) : SynBinding =
        match binding with
        | SynBinding (_, kind, inl, mut, attrs, xml, valData, headPat, returnInfo, expr, range, debugPoint, trivia) ->
            let headPat =
                match headPat with
                | SynPat.LongIdent (ident, extra, options, argPats, _, range) ->
                    SynPat.LongIdent (ident, extra, options, argPats, acc, range)
                | _ -> failwithf "unrecognised head pattern: %O" headPat

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

    let makeInline (binding : SynBinding) : SynBinding =
        match binding with
        | SynBinding (acc, kind, _, mut, attrs, doc, valData, headPat, ret, expr, range, debugPoint, trivia) ->
            SynBinding (
                acc,
                kind,
                true,
                mut,
                attrs,
                doc,
                valData,
                headPat,
                ret,
                expr,
                range,
                debugPoint,
                { trivia with
                    InlineKeyword = Some range0
                }
            )

    let makeStaticMember (binding : SynBinding) : SynBinding =
        let memberFlags =
            {
                SynMemberFlags.IsInstance = false
                SynMemberFlags.IsDispatchSlot = false
                SynMemberFlags.IsOverrideOrExplicitImpl = false
                SynMemberFlags.IsFinal = false
                SynMemberFlags.GetterOrSetterIsCompilerGenerated = false
                SynMemberFlags.MemberKind = SynMemberKind.Member
            }

        match binding with
        | SynBinding (acc, kind, inl, mut, attrs, doc, valData, headPat, ret, expr, range, debugPoint, trivia) ->
            let valData =
                match valData with
                | SynValData.SynValData (_, valInfo, _) -> SynValData.SynValData (Some memberFlags, valInfo, None)

            let trivia =
                { trivia with
                    LeadingKeyword = SynLeadingKeyword.StaticMember (range0, range0)
                }

            SynBinding (acc, kind, inl, mut, attrs, doc, valData, headPat, ret, expr, range, debugPoint, trivia)

    let makeInstanceMember (binding : SynBinding) : SynBinding =
        let memberFlags =
            {
                SynMemberFlags.IsInstance = true
                SynMemberFlags.IsDispatchSlot = false
                SynMemberFlags.IsOverrideOrExplicitImpl = true
                SynMemberFlags.IsFinal = false
                SynMemberFlags.GetterOrSetterIsCompilerGenerated = false
                SynMemberFlags.MemberKind = SynMemberKind.Member
            }

        match binding with
        | SynBinding (acc, kind, inl, mut, attrs, doc, valData, headPat, ret, expr, range, debugPoint, trivia) ->
            let valData =
                match valData with
                | SynValData.SynValData (_, valInfo, _) -> SynValData.SynValData (Some memberFlags, valInfo, None)

            let trivia =
                { trivia with
                    LeadingKeyword = SynLeadingKeyword.Member range0
                }

            SynBinding (acc, kind, inl, mut, attrs, doc, valData, headPat, ret, expr, range, debugPoint, trivia)
