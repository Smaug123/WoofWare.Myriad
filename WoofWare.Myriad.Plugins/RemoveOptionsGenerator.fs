namespace WoofWare.Myriad.Plugins

open Fantomas.FCS.Syntax
open Fantomas.FCS.Xml

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

    // TODO: this option seems a bit odd
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
                Accessibility = accessibility
                Attributes = []
            }

        let typeDecl = AstHelper.defineRecordType record

        SynModuleDecl.Types ([ typeDecl ], range0)

    let createMaker (withOptionsType : LongIdent) (withoutOptionsType : LongIdent) (fields : SynFieldData<Ident> list) =
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
                                    withoutOptionsType
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
                |> SynPat.annotateType (SynType.LongIdent (SynLongIdent.create withoutOptionsType))
            ]
            body
        |> SynBinding.withXmlDoc xmlDoc
        |> SynBinding.withReturnAnnotation (SynType.LongIdent (SynLongIdent.create withOptionsType))
        |> SynModuleDecl.createLet

    let createRecordModule (namespaceId : LongIdent) (typeDefn : SynTypeDefn) =
        let (SynTypeDefn (synComponentInfo, synTypeDefnRepr, _members, _implicitCtor, _, _)) =
            typeDefn

        let (SynComponentInfo (_attributes, typeParams, _constraints, recordId, doc, _preferPostfix, _access, _)) =
            synComponentInfo

        match synTypeDefnRepr with
        | SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.Record (accessibility, fields, _range), _) ->
            let fieldData = fields |> List.map SynField.extractWithIdent

            let decls =
                [
                    createType (Some doc) accessibility typeParams fields
                    createMaker [ Ident.create "Short" ] recordId fieldData
                ]

            let xmlDoc =
                recordId
                |> Seq.map (fun i -> i.idText)
                |> String.concat "."
                |> sprintf "Module containing an option-truncated version of the %s type"
                |> PreXmlDoc.create

            let info =
                SynComponentInfo.createLong recordId
                |> SynComponentInfo.withDocString xmlDoc
                |> SynComponentInfo.addAttributes [ SynAttribute.compilationRepresentation ]
                |> SynComponentInfo.addAttributes [ SynAttribute.requireQualifiedAccess ]

            SynModuleDecl.nestedModule info decls
            |> List.singleton
            |> SynModuleOrNamespace.createNamespace namespaceId
        | _ -> failwithf "Not a record type"

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

            let records = Ast.extractRecords ast

            let namespaceAndRecords =
                records
                |> List.choose (fun (ns, types) ->
                    match types |> List.filter Ast.hasAttribute<RemoveOptionsAttribute> with
                    | [] -> None
                    | types -> Some (ns, types)
                )

            let modules =
                namespaceAndRecords
                |> List.collect (fun (ns, records) ->
                    records
                    |> List.map (fun record ->
                        let recordModule = RemoveOptionsGenerator.createRecordModule ns record
                        recordModule
                    )
                )

            Output.Ast modules
