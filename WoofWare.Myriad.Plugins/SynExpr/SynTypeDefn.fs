namespace WoofWare.Myriad.Plugins

open Fantomas.FCS.Syntax
open Fantomas.FCS.SyntaxTrivia
open Fantomas.FCS.Text.Range

[<RequireQualifiedAccess>]
module internal SynTypeDefn =

    let inline create (componentInfo : SynComponentInfo) (repr : SynTypeDefnRepr) : SynTypeDefn =
        SynTypeDefn.SynTypeDefn (
            componentInfo,
            repr,
            [],
            None,
            range0,
            {
                LeadingKeyword = SynTypeDefnLeadingKeyword.Type range0
                EqualsRange = Some range0
                WithKeyword = None
            }
        )

    let inline withMemberDefns (members : SynMemberDefn list) (r : SynTypeDefn) : SynTypeDefn =
        match r with
        | SynTypeDefn (typeInfo, typeRepr, _, ctor, range, trivia) ->
            SynTypeDefn.SynTypeDefn (typeInfo, typeRepr, members, ctor, range, trivia)

    let getName (defn : SynTypeDefn) : LongIdent =
        match defn with
        | SynTypeDefn (SynComponentInfo.SynComponentInfo (_, _, _, id, _, _, _, _), _, _, _, _, _) -> id

    let getAttribute (attrName : string) (defn : SynTypeDefn) : SynAttribute option =
        match defn with
        | SynTypeDefn (SynComponentInfo.SynComponentInfo (attrs, _, _, _, _, _, _, _), _, _, _, _, _) ->
            attrs
            |> List.collect (fun a -> a.Attributes)
            |> List.tryFind (fun i ->
                match i.TypeName with
                | SynLongIdent.SynLongIdent (id, _, _) ->
                    let name = List.last(id).idText
                    name = attrName || name + "Attribute" = attrName
            )

    let hasAttribute (attrName : string) (defn : SynTypeDefn) : bool =
        getAttribute attrName defn |> Option.isSome
