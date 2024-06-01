namespace WoofWare.Myriad.Plugins

open Fantomas.FCS.Syntax
open Fantomas.FCS.SyntaxTrivia
open Fantomas.FCS.Xml
open Fantomas.FCS.Text.Range

[<RequireQualifiedAccess>]
module internal SynModuleOrNamespace =

    let createNamespace (name : LongIdent) (decls : SynModuleDecl list) =
        SynModuleOrNamespace.SynModuleOrNamespace (
            name,
            false,
            SynModuleOrNamespaceKind.DeclaredNamespace,
            decls,
            PreXmlDoc.Empty,
            [],
            None,
            range0,
            {
                LeadingKeyword = SynModuleOrNamespaceLeadingKeyword.Namespace range0
            }
        )
