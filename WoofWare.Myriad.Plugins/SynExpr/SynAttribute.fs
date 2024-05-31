namespace WoofWare.Myriad.Plugins

open Fantomas.FCS.Syntax
open Fantomas.FCS.Text.Range
open Myriad.Core

[<RequireQualifiedAccess>]
module internal SynAttribute =
    let internal compilationRepresentation : SynAttribute =
        {
            TypeName = SynLongIdent.createS "CompilationRepresentation"
            ArgExpr =
                SynExpr.CreateLongIdent (
                    false,
                    SynLongIdent.createS' [ "CompilationRepresentationFlags" ; "ModuleSuffix" ],
                    None
                )
                |> SynExpr.CreateParen
            Target = None
            AppliesToGetterAndSetter = false
            Range = range0
        }

    let internal autoOpen : SynAttribute =
        {
            TypeName = SynLongIdent.createS "AutoOpen"
            ArgExpr = SynExpr.CreateConst SynConst.Unit
            Target = None
            AppliesToGetterAndSetter = false
            Range = range0
        }
