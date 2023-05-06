namespace MyriadPlugin

open System
open FSharp.Compiler.Syntax
open FSharp.Compiler.Xml
open Myriad.Core

/// Attribute indicating a record type to which the "Remove Options" Myriad
/// generator should apply during build.
type RemoveOptionsAttribute () =
    inherit Attribute ()

module internal Create =
    open FSharp.Compiler.Text.Range
    open Myriad.Core.Ast

    let isOptionIdent (ident : SynLongIdent) : bool =
        match ident.LongIdent with
        | [ i ] when String.Equals (i.idText, "option", StringComparison.OrdinalIgnoreCase) -> true
        // TODO: consider Microsoft.FSharp.Option or whatever it is
        | _ -> false

    let (|OptionIdent|_|) (ident : SynLongIdent) =
        if isOptionIdent ident then Some () else None

    let private removeOption (s : SynField) : SynField =
        let (SynField.SynField (synAttributeLists,
                                isStatic,
                                identOption,
                                fieldType,
                                isMutable,
                                preXmlDoc,
                                synAccessOption,
                                range)) =
            s

        let newType =
            match fieldType with
            | SynType.App (SynType.LongIdent OptionIdent, _, [ innerType ], _, _, _, _) -> innerType
            | _ -> fieldType

        SynField.SynField (
            synAttributeLists,
            isStatic,
            identOption,
            newType,
            isMutable,
            preXmlDoc,
            synAccessOption,
            range
        )

    let createCreate (xmlDoc : PreXmlDoc option) (fields : SynField list) =
        let fields : SynField list = fields |> List.map removeOption
        let name = Ident.Create "Short"

        let typeDecl : SynTypeDefn =
            match xmlDoc with
            | None -> SynTypeDefn.CreateRecord (name, fields)
            | Some xmlDoc -> SynTypeDefn.CreateRecord (name, fields, xmldoc = xmlDoc)

        SynModuleDecl.Types ([ typeDecl ], range0)

    let createRecordModule (namespaceId : LongIdent) (typeDefn : SynTypeDefn) =
        let (SynTypeDefn (synComponentInfo, synTypeDefnRepr, _members, _implicitCtor, _, _)) =
            typeDefn

        let (SynComponentInfo (_attributes, _typeParams, _constraints, recordId, doc, _preferPostfix, _access, _)) =
            synComponentInfo

        match synTypeDefnRepr with
        | SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.Record (_accessibility, recordFields, _recordRange), _) ->

            let create = createCreate (Some doc) recordFields

            let decls = [ yield create ]

            let compilationRepresentation : SynAttribute =
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

            let attributes =
                [
                    SynAttributeList.Create (SynAttribute.RequireQualifiedAccess ())
                    SynAttributeList.Create compilationRepresentation
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
                        let recordModule = Create.createRecordModule ns record
                        recordModule
                    )
                )

            Output.Ast modules
