namespace WoofWare.Myriad.Plugins

open Fantomas.FCS.Syntax
open Fantomas.FCS.Text.Range

[<RequireQualifiedAccess>]
module internal SynTypeDefnRepr =

    let inline interfaceType (mems : SynMemberDefns) : SynTypeDefnRepr =
        SynTypeDefnRepr.ObjectModel (SynTypeDefnKind.Unspecified, mems, range0)

    /// Indicates the body of a `type Foo with {body}` extension type declaration.
    let inline augmentation () : SynTypeDefnRepr =
        SynTypeDefnRepr.ObjectModel (SynTypeDefnKind.Augmentation range0, [], range0)

    let inline unionWithAccess (implAccess : SynAccess option) (cases : SynUnionCase list) : SynTypeDefnRepr =
        SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.Union (implAccess, cases, range0), range0)

    let inline union (cases : SynUnionCase list) : SynTypeDefnRepr = unionWithAccess None cases

    let inline recordWithAccess (implAccess : SynAccess option) (fields : SynField list) : SynTypeDefnRepr =
        SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.Record (implAccess, fields, range0), range0)

    let inline record (fields : SynField list) : SynTypeDefnRepr = recordWithAccess None fields
