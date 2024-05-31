namespace WoofWare.Myriad.Plugins

open Fantomas.FCS.Syntax
open Fantomas.FCS.Text.Range

[<RequireQualifiedAccess>]
module internal SynAttribute =
    let internal compilationRepresentation : SynAttribute =
        {
            TypeName = SynLongIdent.createS "CompilationRepresentation"
            ArgExpr =
                [ "CompilationRepresentationFlags" ; "ModuleSuffix" ]
                |> SynExpr.createLongIdent
                |> SynExpr.paren
            Target = None
            AppliesToGetterAndSetter = false
            Range = range0
        }

    let internal requireQualifiedAccess : SynAttribute =
        {
            TypeName = SynLongIdent.createS "RequireQualifiedAccess"
            ArgExpr = SynExpr.CreateConst ()
            Target = None
            AppliesToGetterAndSetter = false
            Range = range0
        }

    let internal autoOpen : SynAttribute =
        {
            TypeName = SynLongIdent.createS "AutoOpen"
            ArgExpr = SynExpr.CreateConst ()
            Target = None
            AppliesToGetterAndSetter = false
            Range = range0
        }
