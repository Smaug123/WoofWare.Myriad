namespace WoofWare.Myriad.Plugins

open Fantomas.FCS.Syntax

type internal UnionCase<'Ident> =
    {
        Fields : SynFieldData<'Ident> list
        Attrs : SynAttribute list
        Ident : Ident
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
