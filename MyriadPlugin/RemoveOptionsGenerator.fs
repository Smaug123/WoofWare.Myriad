namespace MyriadPlugin

open System
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
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

    let (|OptionType|_|) (fieldType : SynType) =
        match fieldType with
        | SynType.App (SynType.LongIdent ident, _, [ innerType ], _, _, _, _) when isOptionIdent ident ->
            Some innerType
        | _ -> None

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
            range
        )

    // TODO: this option seems a bit odd
    let createType (xmlDoc : PreXmlDoc option) (fields : SynField list) =
        let fields : SynField list = fields |> List.map removeOption
        let name = Ident.Create "Short"

        let typeDecl : SynTypeDefn =
            match xmlDoc with
            | None -> SynTypeDefn.CreateRecord (name, fields)
            | Some xmlDoc -> SynTypeDefn.CreateRecord (name, fields, xmldoc = xmlDoc)

        SynModuleDecl.Types ([ typeDecl ], range0)

    let createMaker (withOptionsType : LongIdent) (withoutOptionsType : LongIdent) (fields : SynField list) =
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
            |> List.map (fun (SynField (_, _, id, fieldType, _, _, _, _)) ->
                let id =
                    match id with
                    | None -> failwith "Expected record field to have an identifying name"
                    | Some id -> id

                let accessor =
                    SynExpr.DotGet (
                        SynExpr.CreateIdent inputArg,
                        range0,
                        SynLongIdent.CreateFromLongIdent [ id ],
                        range0
                    )

                let body =
                    match fieldType with
                    | OptionType _ ->
                        SynExpr.CreateApp (
                            SynExpr.CreateAppInfix (
                                SynExpr.LongIdent (false, SynLongIdent.SynLongIdent (
                                    [Ident.Create "op_PipeRight"], [], [Some (IdentTrivia.OriginalNotation "|>")]
                                ), None, range0),
                                accessor
                            ),
                            SynExpr.CreateApp (
                                SynExpr.CreateLongIdent (SynLongIdent.CreateString "Option.defaultValue"),
                                SynExpr.CreateParen (
                                SynExpr.CreateApp (
                                    SynExpr.CreateLongIdent (SynLongIdent.CreateFromLongIdent (withoutOptionsType @ [Ident.Create (sprintf "Default%s" id.idText)])),
                                    SynExpr.CreateUnit
                                )
                                )
                            )
                        )
                    | _ -> accessor

                (SynLongIdent.CreateFromLongIdent [ id ], true), Some body
            )
            |> SynExpr.CreateRecord

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

        let (SynComponentInfo (_attributes, _typeParams, _constraints, recordId, doc, _preferPostfix, _access, _)) =
            synComponentInfo

        match synTypeDefnRepr with
        | SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.Record (_accessibility, recordFields, _recordRange), _) ->

            let decls =
                [
                    createType (Some doc) recordFields
                    createMaker [ Ident.Create "Short" ] recordId recordFields
                ]

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
