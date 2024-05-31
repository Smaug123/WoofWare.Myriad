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
