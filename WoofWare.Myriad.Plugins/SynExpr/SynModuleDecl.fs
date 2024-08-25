namespace WoofWare.Myriad.Plugins

open Fantomas.FCS.Syntax
open Fantomas.FCS.SyntaxTrivia
open Fantomas.FCS.Text.Range

[<RequireQualifiedAccess>]
module internal SynModuleDecl =

    let inline openAny (ident : SynOpenDeclTarget) : SynModuleDecl = SynModuleDecl.Open (ident, range0)

    let inline createLets (bindings : SynBinding list) : SynModuleDecl =
        SynModuleDecl.Let (false, bindings, range0)

    let inline createLet (binding : SynBinding) : SynModuleDecl = createLets [ binding ]

    let inline createTypes (tys : SynTypeDefn list) : SynModuleDecl = SynModuleDecl.Types (tys, range0)

    let nestedModule (info : SynComponentInfo) (decls : SynModuleDecl list) : SynModuleDecl =
        SynModuleDecl.NestedModule (
            info,
            false,
            decls,
            false,
            range0,
            {
                ModuleKeyword = Some range0
                EqualsRange = Some range0
            }
        )
