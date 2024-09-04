namespace WoofWare.Myriad.Plugins

open Fantomas.FCS.Syntax
open Fantomas.FCS.Text.Range
open Fantomas.FCS.Xml
open Fantomas.FCS.SyntaxTrivia

/// This is generic on whether each field of this case must be named.
type UnionCase<'ident> =
    {
        Name : Ident
        XmlDoc : PreXmlDoc option
        Access : SynAccess option
        Attributes : SynAttribute list
        Fields : SynFieldData<'ident> list
    }

[<RequireQualifiedAccess>]
module internal SynUnionCase =
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
            SynAttributes.ofAttrs case.Attributes,
            SynIdent.SynIdent (case.Name, None),
            SynUnionCaseKind.Fields fields,
            case.XmlDoc |> Option.defaultValue PreXmlDoc.Empty,
            case.Access,
            range0,
            {
                BarRange = Some range0
            }
        )
