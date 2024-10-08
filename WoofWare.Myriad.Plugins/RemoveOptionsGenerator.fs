namespace WoofWare.Myriad.Plugins

open System
open Fantomas.FCS.Syntax
open Fantomas.FCS.Xml
open WoofWare.Whippet.Fantomas

[<RequireQualifiedAccess>]
module internal RemoveOptionsGenerator =
    open Fantomas.FCS.Text.Range

    let private removeOption (s : SynField) : SynField =
        let (SynField.SynField (synAttributeLists,
                                isStatic,
                                identOption,
                                fieldType,
                                isMutable,
                                preXmlDoc,
                                synAccessOption,
                                range,
                                trivia)) =
            s

        let newType =
            match fieldType with
            | OptionType innerType -> innerType
            | _ -> fieldType

        SynField.SynField (
            synAttributeLists,
            isStatic,
            identOption,
            newType,
            isMutable,
            preXmlDoc,
            synAccessOption,
            range,
            trivia
        )

    let createType
        (xmlDoc : PreXmlDoc option)
        (accessibility : SynAccess option)
        (generics : SynTyparDecls option)
        (fields : SynField list)
        : SynModuleDecl
        =
        let fields : SynField list = fields |> List.map removeOption
        let name = Ident.create "Short"

        let record =
            {
                Name = name
                Fields = fields
                Members = None
                XmlDoc = xmlDoc
                Generics = generics
                TypeAccessibility = accessibility
                ImplAccessibility = None
                Attributes = []
            }

        let typeDecl = RecordType.ToAst record

        SynModuleDecl.Types ([ typeDecl ], range0)

    let createMaker (withOptionsType : LongIdent) (withoutOptionsType : Ident) (fields : SynFieldData<Ident> list) =
        let xmlDoc = PreXmlDoc.create "Remove the optional members of the input."

        let inputArg = Ident.create "input"
        let functionName = Ident.create "shorten"

        let body =
            fields
            |> List.map (fun fieldData ->
                let accessor =
                    SynExpr.LongIdent (
                        false,
                        SynLongIdent ([ inputArg ; fieldData.Ident ], [ range0 ], []),
                        None,
                        range0
                    )

                let body =
                    match fieldData.Type with
                    | OptionType _ ->
                        accessor
                        |> SynExpr.pipeThroughFunction (
                            SynExpr.applyFunction
                                (SynExpr.createLongIdent [ "Option" ; "defaultWith" ])
                                (SynExpr.createLongIdent' (
                                    [ withoutOptionsType ]
                                    @ [ Ident.create (sprintf "Default%s" fieldData.Ident.idText) ]
                                ))
                        )
                    | _ -> accessor

                SynLongIdent.createI fieldData.Ident, body
            )
            |> AstHelper.instantiateRecord

        SynBinding.basic
            [ functionName ]
            [
                SynPat.named inputArg.idText
                |> SynPat.annotateType (SynType.LongIdent (SynLongIdent.createI withoutOptionsType))
            ]
            body
        |> SynBinding.withXmlDoc xmlDoc
        |> SynBinding.withReturnAnnotation (SynType.LongIdent (SynLongIdent.create withOptionsType))
        |> SynModuleDecl.createLet

    let createRecordModule (namespaceId : LongIdent) (typeDefn : RecordType) =
        let fieldData = typeDefn.Fields |> List.map SynField.extractWithIdent

        let decls =
            [
                createType typeDefn.XmlDoc typeDefn.TypeAccessibility typeDefn.Generics typeDefn.Fields
                createMaker [ Ident.create "Short" ] typeDefn.Name fieldData
            ]

        let xmlDoc =
            sprintf "Module containing an option-truncated version of the %s type" typeDefn.Name.idText
            |> PreXmlDoc.create

        let info =
            SynComponentInfo.create typeDefn.Name
            |> SynComponentInfo.withDocString xmlDoc
            |> SynComponentInfo.addAttributes [ SynAttribute.compilationRepresentation ]
            |> SynComponentInfo.addAttributes [ SynAttribute.requireQualifiedAccess ]

        SynModuleDecl.nestedModule info decls
        |> List.singleton
        |> SynModuleOrNamespace.createNamespace namespaceId

open Myriad.Core

/// Myriad generator that stamps out a record with option types stripped
/// from the fields at the top level.
[<MyriadGenerator("remove-options")>]
type RemoveOptionsGenerator () =

    interface IMyriadGenerator with
        member _.ValidInputExtensions = [ ".fs" ]

        member _.Generate (context : GeneratorContext) =
            let ast, _ =
                Ast.fromFilename context.InputFilename |> Async.RunSynchronously |> Array.head

            let records = Ast.getRecords ast

            let namespaceAndRecords =
                records
                |> List.collect (fun (ns, ty) ->
                    ty
                    |> List.filter (fun record ->
                        record.Attributes
                        |> List.exists (fun attr ->
                            attr.TypeName.LongIdent
                            |> List.last
                            |> _.idText
                            |> fun s ->
                                if s.EndsWith ("Attribute", StringComparison.Ordinal) then
                                    s
                                else
                                    $"%s{s}Attribute"
                            |> (=) typeof<RemoveOptionsAttribute>.Name
                        )
                    )
                    |> List.map (fun ty -> ns, ty)
                )

            let modules =
                namespaceAndRecords
                |> List.map (fun (ns, record) -> RemoveOptionsGenerator.createRecordModule ns record)

            Output.Ast modules
