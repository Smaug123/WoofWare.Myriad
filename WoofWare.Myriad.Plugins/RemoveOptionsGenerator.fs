namespace WoofWare.Myriad.Plugins

open Fantomas.FCS.Syntax
open Fantomas.FCS.SyntaxTrivia
open Fantomas.FCS.Xml
open Myriad.Core

[<RequireQualifiedAccess>]
module internal RemoveOptionsGenerator =
    open Fantomas.FCS.Text.Range
    open Myriad.Core.Ast

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
        =
        let fields : SynField list = fields |> List.map removeOption
        let name = Ident.Create "Short"

        let record =
            {
                Name = name
                Fields = fields
                Members = None
                XmlDoc = xmlDoc
                Generics = generics
                Accessibility = accessibility
            }

        let typeDecl = AstHelper.defineRecordType record

        SynModuleDecl.Types ([ typeDecl ], range0)

    let createMaker (withOptionsType : LongIdent) (withoutOptionsType : LongIdent) (fields : SynFieldData<Ident> list) =
        let xmlDoc = PreXmlDoc.Create " Remove the optional members of the input."

        let returnInfo =
            SynBindingReturnInfo.Create (SynType.LongIdent (SynLongIdent.CreateFromLongIdent withOptionsType))

        let inputArg = Ident.Create "input"
        let functionName = Ident.Create "shorten"

        let inputVal =
            SynValData.SynValData (
                None,
                SynValInfo.SynValInfo ([ [ SynArgInfo.CreateId functionName ] ], SynArgInfo.Empty),
                Some inputArg
            )

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
                        SynExpr.CreateApp (
                            SynExpr.CreateAppInfix (
                                SynExpr.LongIdent (
                                    false,
                                    SynLongIdent.SynLongIdent (
                                        [ Ident.Create "op_PipeRight" ],
                                        [],
                                        [ Some (IdentTrivia.OriginalNotation "|>") ]
                                    ),
                                    None,
                                    range0
                                ),
                                accessor
                            ),
                            SynExpr.CreateApp (
                                SynExpr.CreateLongIdent (SynLongIdent.CreateString "Option.defaultWith"),
                                SynExpr.CreateLongIdent (
                                    SynLongIdent.CreateFromLongIdent (
                                        withoutOptionsType
                                        @ [ Ident.Create (sprintf "Default%s" fieldData.Ident.idText) ]
                                    )
                                )
                            )
                        )
                    | _ -> accessor

                (SynLongIdent.CreateFromLongIdent [ fieldData.Ident ], true), Some body
            )
            |> AstHelper.instantiateRecord

        let pattern =
            SynPat.LongIdent (
                SynLongIdent.CreateFromLongIdent [ functionName ],
                None,
                None,
                SynArgPats.Pats
                    [
                        SynPat.CreateTyped (
                            SynPat.CreateNamed inputArg,
                            SynType.LongIdent (SynLongIdent.CreateFromLongIdent withoutOptionsType)
                        )
                        |> SynPat.CreateParen
                    ],
                None,
                range0
            )

        let binding =
            SynBinding.Let (
                isInline = false,
                isMutable = false,
                xmldoc = xmlDoc,
                returnInfo = returnInfo,
                expr = body,
                valData = inputVal,
                pattern = pattern
            )

        SynModuleDecl.CreateLet [ binding ]

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
                    createMaker [ Ident.Create "Short" ] recordId fieldData
                ]

            let attributes =
                [
                    SynAttributeList.Create (SynAttribute.RequireQualifiedAccess ())
                    SynAttributeList.Create SynAttribute.compilationRepresentation
                ]

            let xmlDoc =
                recordId
                |> Seq.map (fun i -> i.idText)
                |> String.concat "."
                |> sprintf " Module containing an option-truncated version of the %s type"
                |> PreXmlDoc.Create

            let info =
                SynComponentInfo.Create (recordId, attributes = attributes, xmldoc = xmlDoc)

            let mdl = SynModuleDecl.CreateNestedModule (info, decls)

            SynModuleOrNamespace.CreateNamespace (namespaceId, decls = [ mdl ])
        | _ -> failwithf "Not a record type"

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
