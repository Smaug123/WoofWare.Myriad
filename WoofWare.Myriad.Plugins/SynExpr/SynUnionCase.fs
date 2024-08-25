namespace WoofWare.Myriad.Plugins

open Fantomas.FCS.Syntax
open Fantomas.FCS.Text.Range
open Fantomas.FCS.Xml
open Fantomas.FCS.SyntaxTrivia

type internal UnionCase<'Ident> =
    {
        Fields : SynFieldData<'Ident> list
        Attrs : SynAttribute list
        Ident : Ident
    }

[<RequireQualifiedAccess>]
module internal UnionCase =
    let mapIdentFields<'a, 'b> (f : 'a -> 'b) (unionCase : UnionCase<'a>) : UnionCase<'b> =
        {
            Fields = unionCase.Fields |> List.map (SynField.mapIdent f)
            Attrs = unionCase.Attrs
            Ident = unionCase.Ident
        }

[<RequireQualifiedAccess>]
module internal SynUnionCase =
    let extract (SynUnionCase (attrs, id, caseType, _, _, _, _)) : UnionCase<Ident option> =
        match caseType with
        | SynUnionCaseKind.FullType _ -> failwith "WoofWare.Myriad does not support FullType union cases."
        | SynUnionCaseKind.Fields fields ->

        let fields = fields |> List.map SynField.extract

        let id =
            match id with
            | SynIdent.SynIdent (ident, _) -> ident

        // As far as I can tell, there's no way to get any attributes here? :shrug:
        let attrs = attrs |> List.collect (fun l -> l.Attributes)

        {
            Fields = fields
            Attrs = attrs
            Ident = id
        }

    let create (case : UnionCase<Ident>) : SynUnionCase =
        let fields =
            case.Fields
            |> List.map (fun field ->
                SynField.SynField (
                    SynAttributes.ofAttrs field.Attrs,
                    false,
                    Some field.Ident,
                    field.Type,
                    false,
                    PreXmlDoc.Empty,
                    None,
                    range0,
                    {
                        LeadingKeyword = None
                    }
                )
            )

        SynUnionCase.SynUnionCase (
            SynAttributes.ofAttrs case.Attrs,
            SynIdent.SynIdent (case.Ident, None),
            SynUnionCaseKind.Fields fields,
            PreXmlDoc.Empty,
            None,
            range0,
            {
                BarRange = Some range0
            }
        )
