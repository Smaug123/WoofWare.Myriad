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

    let inline union (cases : SynUnionCase list) : SynTypeDefnRepr =
        SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.Union (None, cases, range0), range0)

    let inline record (fields : SynField list) : SynTypeDefnRepr =
        SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.Record (None, fields, range0), range0)
