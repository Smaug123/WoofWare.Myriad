namespace WoofWare.Myriad.Plugins

open Fantomas.FCS.Syntax
open Fantomas.FCS.Text.Range

[<RequireQualifiedAccess>]
module internal SynAttribute =
    let inline create (typeName : SynLongIdent) (arg : SynExpr) : SynAttribute =
        {
            TypeName = typeName
            ArgExpr = arg
            Target = None
            AppliesToGetterAndSetter = false
            Range = range0
        }

    let internal compilationRepresentation : SynAttribute =
        [ "CompilationRepresentationFlags" ; "ModuleSuffix" ]
        |> SynExpr.createLongIdent
        |> SynExpr.paren
        |> create (SynLongIdent.createS "CompilationRepresentation")

    let internal requireQualifiedAccess : SynAttribute =
        create (SynLongIdent.createS "RequireQualifiedAccess") (SynExpr.CreateConst ())

    let internal autoOpen : SynAttribute =
        create (SynLongIdent.createS "AutoOpen") (SynExpr.CreateConst ())
