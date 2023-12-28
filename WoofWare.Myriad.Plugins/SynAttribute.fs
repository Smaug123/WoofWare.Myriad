namespace WoofWare.Myriad.Plugins

open Fantomas.FCS.Syntax
open Fantomas.FCS.Text.Range
open Myriad.Core

[<RequireQualifiedAccess>]
module internal SynAttribute =
    let internal compilationRepresentation : SynAttribute =
        {
            TypeName = SynLongIdent.CreateString "CompilationRepresentation"
            ArgExpr =
                SynExpr.CreateLongIdent (
                    false,
                    SynLongIdent.Create [ "CompilationRepresentationFlags" ; "ModuleSuffix" ],
                    None
                )
                |> SynExpr.CreateParen
            Target = None
            AppliesToGetterAndSetter = false
            Range = range0
        }
