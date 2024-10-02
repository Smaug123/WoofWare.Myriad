namespace WoofWare.Myriad.Plugins

open Fantomas.FCS.Syntax
open Fantomas.FCS.Text.Range
open Fantomas.FCS.Xml
open Fantomas.FCS.SyntaxTrivia

/// Represents everything you need to know about a union case.
/// This is generic on whether each field of this case must be named.
type UnionCase<'ident> =
    {
        /// The name of the case: e.g. `| Foo of blah` has this being `Foo`.
        Name : Ident
        /// Any docstring associated with this case.
        XmlDoc : PreXmlDoc option
        /// Any accessibility modifier: e.g. `type Foo = private | Blah`.
        Access : SynAccess option
        /// Attributes on the case: for example, `| [<Attr>] Foo of blah`.
        Attributes : SynAttribute list
        /// The data contained within the case: for example, `[blah]` in `| Foo of blah`.
        Fields : SynFieldData<'ident> list
    }

[<RequireQualifiedAccess>]
module internal SynUnionCase =
    let create (case : UnionCase<Ident option>) : SynUnionCase =
        let fields =
            case.Fields
            |> List.map (fun field ->
                SynField.SynField (
                    SynAttributes.ofAttrs field.Attrs,
                    false,
                    field.Ident,
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
            SynIdent.createI case.Name,
            SynUnionCaseKind.Fields fields,
            case.XmlDoc |> Option.defaultValue PreXmlDoc.Empty,
            case.Access,
            range0,
            {
                BarRange = Some range0
            }
        )
