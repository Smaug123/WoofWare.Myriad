namespace WoofWare.Myriad.Plugins

open Fantomas.FCS.Syntax
open Fantomas.FCS.Xml
open Fantomas.FCS.Text.Range

[<RequireQualifiedAccess>]
module internal SynComponentInfo =
    let inline createLong (name : LongIdent) =
        SynComponentInfo.SynComponentInfo ([], None, [], name, PreXmlDoc.Empty, false, None, range0)

    let inline create (name : Ident) = createLong [ name ]

    let inline withDocString (doc : PreXmlDoc) (i : SynComponentInfo) : SynComponentInfo =
        match i with
        | SynComponentInfo.SynComponentInfo (attrs, typars, constraints, name, _, postfix, access, range) ->
            SynComponentInfo (attrs, typars, constraints, name, doc, postfix, access, range)

    let inline setGenerics (typars : SynTyparDecls option) (i : SynComponentInfo) : SynComponentInfo =
        match i with
        | SynComponentInfo.SynComponentInfo (attrs, _, constraints, name, doc, postfix, access, range) ->
            SynComponentInfo (attrs, typars, constraints, name, doc, postfix, access, range)

    let inline withGenerics (typars : SynTyparDecl list) (i : SynComponentInfo) : SynComponentInfo =
        let inner =
            if typars.IsEmpty then
                None
            else
                Some (SynTyparDecls.PostfixList (typars, [], range0))

        setGenerics inner i

    let inline setAccessibility (acc : SynAccess option) (i : SynComponentInfo) : SynComponentInfo =
        match i with
        | SynComponentInfo.SynComponentInfo (attrs, typars, constraints, name, doc, postfix, _, range) ->
            SynComponentInfo.SynComponentInfo (attrs, typars, constraints, name, doc, postfix, acc, range)

    let inline withAccessibility (acc : SynAccess) (i : SynComponentInfo) : SynComponentInfo =
        setAccessibility (Some acc) i

    let inline addAttributes (attrs : SynAttribute list) (i : SynComponentInfo) : SynComponentInfo =
        match i with
        | SynComponentInfo.SynComponentInfo (oldAttrs, typars, constraints, name, doc, postfix, acc, range) ->
            let attrs =
                {
                    SynAttributeList.Attributes = attrs
                    SynAttributeList.Range = range0
                }

            SynComponentInfo.SynComponentInfo ((attrs :: oldAttrs), typars, constraints, name, doc, postfix, acc, range)
