namespace WoofWare.Myriad.Plugins

open Fantomas.FCS.Text.Range
open Fantomas.FCS.Syntax
open Fantomas.FCS.SyntaxTrivia
open Fantomas.FCS.Xml

type internal SynFieldData<'Ident> =
    {
        Attrs : SynAttribute list
        Ident : 'Ident
        Type : SynType
    }

[<RequireQualifiedAccess>]
module internal SynField =
    /// Get the useful information out of a SynField.
    let extract (SynField (attrs, _, id, fieldType, _, _, _, _, _)) : SynFieldData<Ident option> =
        {
            Attrs = attrs |> List.collect (fun l -> l.Attributes)
            Ident = id
            Type = fieldType
        }

    let mapIdent<'a, 'b> (f : 'a -> 'b) (x : SynFieldData<'a>) : SynFieldData<'b> =
        let ident = f x.Ident

        {
            Attrs = x.Attrs
            Ident = ident
            Type = x.Type
        }

    /// Throws if the field has no identifier.
    let extractWithIdent (f : SynField) : SynFieldData<Ident> =
        f
        |> extract
        |> mapIdent (fun ident ->
            match ident with
            | None -> failwith "expected field identifier to have a value, but it did not"
            | Some i -> i
        )

    let make (data : SynFieldData<Ident option>) : SynField =
        let attrs : SynAttributeList list =
            data.Attrs
            |> List.map (fun l ->
                {
                    Attributes = [ l ]
                    Range = range0
                }
            )

        SynField.SynField (
            attrs,
            false,
            data.Ident,
            data.Type,
            false,
            PreXmlDoc.Empty,
            None,
            range0,
            SynFieldTrivia.Zero
        )

    let withDocString (doc : PreXmlDoc) (f : SynField) : SynField =
        match f with
        | SynField (attributes, isStatic, idOpt, fieldType, isMutable, _, accessibility, range, trivia) ->
            SynField (attributes, isStatic, idOpt, fieldType, isMutable, doc, accessibility, range, trivia)
