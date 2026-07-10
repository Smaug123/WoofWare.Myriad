namespace WoofWare.Myriad.Plugins

open Fantomas.FCS.Syntax
open Fantomas.FCS.Xml
open WoofWare.Whippet.Fantomas

/// Whether a record field needs a constructor parameter.
type internal ConstructorFieldKind<'fieldType> =
    | Required of 'fieldType
    | Optional

/// The part of a record field needed to plan its constructor.
type internal ConstructorField<'fieldName, 'fieldType> =
    {
        Name : 'fieldName
        Kind : ConstructorFieldKind<'fieldType>
    }

/// A parameter accepted by a generated record constructor.
type internal ConstructorParameter<'fieldType> =
    {
        Name : string
        Type : 'fieldType
    }

/// The value used to initialize a field in a generated record constructor.
type internal ConstructorInitializer =
    | FromParameter of string
    | UseNone

/// The planned initializer for one record field.
type internal ConstructorAssignment<'fieldName> =
    {
        FieldName : 'fieldName
        Initializer : ConstructorInitializer
    }

/// A syntax-independent description of a generated record constructor.
type internal ConstructorPlan<'fieldName, 'fieldType> =
    {
        Parameters : ConstructorParameter<'fieldType> list
        Assignments : ConstructorAssignment<'fieldName> list
    }

/// A record selected for generation together with the syntax in scope at its declaration.
type internal RecordSelection =
    {
        NamespaceId : LongIdent
        Opens : SynOpenDeclTarget list
        Record : RecordType
    }

[<RequireQualifiedAccess>]
module internal ConstructorPlan =

    /// Plan a constructor from fields in declaration order.
    let create (fields : ConstructorField<'fieldName, 'fieldType> list) : ConstructorPlan<'fieldName, 'fieldType> =
        let parameters =
            fields
            |> List.choose (fun field ->
                match field.Kind with
                | Required fieldType -> Some fieldType
                | Optional -> None
            )
            |> List.mapi (fun index fieldType ->
                {
                    Name = $"arg_{index}"
                    Type = fieldType
                }
            )

        let assignments, _ =
            fields
            |> List.mapFold
                (fun requiredIndex field ->
                    match field.Kind with
                    | Optional ->
                        {
                            FieldName = field.Name
                            Initializer = UseNone
                        },
                        requiredIndex
                    | Required _ ->
                        {
                            FieldName = field.Name
                            Initializer = FromParameter $"arg_{requiredIndex}"
                        },
                        requiredIndex + 1
                )
                0

        {
            Parameters = parameters
            Assignments = assignments
        }

[<RequireQualifiedAccess>]
module internal RecordConstructorGenerator =

    let private isOptionType (fieldType : SynType) : bool =
        match fieldType with
        | OptionType _ -> true
        | SynType.App (SynType.LongIdent (SynLongIdent (identifiers, _, _)), _, [ _ ], _, _, _, _) ->
            match identifiers |> List.map _.idText with
            | [ "Microsoft" ; "FSharp" ; "Core" ; "option" ]
            | [ "Microsoft" ; "FSharp" ; "Core" ; "Option" ]
            | [ "global" ; "Microsoft" ; "FSharp" ; "Core" ; "option" ]
            | [ "global" ; "Microsoft" ; "FSharp" ; "Core" ; "Option" ] -> true
            | _ -> false
        | _ -> false

    let private toConstructorField (field : SynFieldData<Ident>) : ConstructorField<Ident, SynType> =
        {
            Name = field.Ident
            Kind =
                if isOptionType field.Type then
                    Optional
                else
                    Required field.Type
        }

    let private returnType (record : RecordType) : SynType =
        match record.Generics with
        | None -> SynType.createLongIdent [ record.Name ]
        | Some generics ->
            generics.TyparDecls
            |> List.map (fun (SynTyparDecl (_, typar)) -> SynType.var typar)
            |> SynType.app' (SynType.createLongIdent [ record.Name ])

    /// Classify a parsed record and plan its constructor.
    let plan (record : RecordType) : ConstructorPlan<Ident, SynType> =
        record.Fields
        |> List.map SynField.extractWithIdent
        |> List.map toConstructorField
        |> ConstructorPlan.create

    let private createBinding (record : RecordType) : SynModuleDecl =
        let plan = plan record

        let parameters =
            match plan.Parameters with
            | [] -> [ SynPat.unit ]
            | parameters ->
                parameters
                |> List.map (fun parameter -> SynPat.named parameter.Name |> SynPat.annotateType parameter.Type)

        let body =
            plan.Assignments
            |> List.map (fun assignment ->
                let initializer =
                    match assignment.Initializer with
                    | FromParameter parameterName -> SynExpr.createIdent parameterName
                    | UseNone -> SynExpr.createIdent "None"

                SynLongIdent.createI assignment.FieldName, initializer
            )
            |> SynExpr.createRecord None

        body
        |> SynBinding.basic [ Ident.create "create" ] parameters
        |> SynBinding.withXmlDoc (
            PreXmlDoc.create $"Create a value of type %s{record.Name.idText} with every optional field set to None."
        )
        |> SynBinding.withReturnAnnotation (returnType record)
        |> SynModuleDecl.createLet

    let private createModule
        (namespaceId : LongIdent)
        (opens : SynOpenDeclTarget list)
        (record : RecordType)
        : SynModuleOrNamespace
        =
        let accessibility =
            match record.TypeAccessibility with
            | Some accessibility -> Some accessibility
            | None -> record.ImplAccessibility

        let moduleInfo =
            SynComponentInfo.create record.Name
            |> SynComponentInfo.withDocString (
                PreXmlDoc.create $"Functions for constructing the %s{record.Name.idText} record."
            )
            |> SynComponentInfo.setAccessibility accessibility
            |> SynComponentInfo.addAttributes
                [ SynAttribute.requireQualifiedAccess ; SynAttribute.compilationRepresentation ]

        [
            yield! opens |> List.map SynModuleDecl.openAny
            yield SynModuleDecl.nestedModule moduleInfo [ createBinding record ]
        ]
        |> SynModuleOrNamespace.createNamespace namespaceId

    let private typeName (typeDefn : SynTypeDefn) : string =
        typeDefn |> SynTypeDefn.getName |> List.map _.idText |> String.concat "."

    let private toRecord (typeDefn : SynTypeDefn) : RecordType option =
        match typeDefn with
        | SynTypeDefn.SynTypeDefn (componentInfo,
                                   SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.Record (representationAccess, fields, _),
                                                           _),
                                   members,
                                   _,
                                   _,
                                   _) -> Some (RecordType.OfRecord componentInfo members representationAccess fields)
        | _ -> None

    let private isPrivateAccess (access : SynAccess option) : bool =
        match access with
        | Some (SynAccess.Private _) -> true
        | _ -> false

    let private selectType
        (namespaceId : LongIdent)
        (opens : SynOpenDeclTarget list)
        (typeDefn : SynTypeDefn)
        : RecordSelection option
        =
        match SynTypeDefn.getAttribute typeof<GenerateRecordConstructorAttribute>.Name typeDefn with
        | None -> None
        | Some _ ->
            let name = typeName typeDefn

            let record =
                typeDefn
                |> toRecord
                |> Option.defaultWith (fun () ->
                    failwith $"[<GenerateRecordConstructor>] may only be applied to record types. Type: %s{name}"
                )

            if
                isPrivateAccess record.TypeAccessibility
                || isPrivateAccess record.ImplAccessibility
            then
                failwith
                    $"[<GenerateRecordConstructor>] cannot be applied to a record with a private representation. Type: %s{name}"

            {
                NamespaceId = namespaceId
                Opens = opens
                Record = record
            }
            |> Some

    let rec private rejectAnnotationsNestedInModules (containingPath : LongIdent) (decls : SynModuleDecl list) : unit =
        for decl in decls do
            match decl with
            | SynModuleDecl.Types (typeDefns, _) ->
                for typeDefn in typeDefns do
                    match SynTypeDefn.getAttribute typeof<GenerateRecordConstructorAttribute>.Name typeDefn with
                    | None -> ()
                    | Some _ ->
                        let name =
                            containingPath @ SynTypeDefn.getName typeDefn
                            |> List.map _.idText
                            |> String.concat "."

                        failwith
                            $"[<GenerateRecordConstructor>] may only be applied to namespace-level records. Nested type: %s{name}"
            | SynModuleDecl.NestedModule (SynComponentInfo (_, _, _, moduleName, _, _, _, _), _, nestedDecls, _, _, _) ->
                rejectAnnotationsNestedInModules (containingPath @ moduleName) nestedDecls
            | _ -> ()

    let private selectNamespace (namespaceId : LongIdent) (decls : SynModuleDecl list) : RecordSelection list =
        let rec loop (opens : SynOpenDeclTarget list) (selectedRev : RecordSelection list) remainingDecls =
            match remainingDecls with
            | [] -> List.rev selectedRev
            | decl :: rest ->
                match decl with
                | SynModuleDecl.Open (target, _) -> loop (opens @ [ target ]) selectedRev rest
                | SynModuleDecl.Types (typeDefns, _) ->
                    let newlySelected = typeDefns |> List.choose (selectType namespaceId opens)
                    loop opens (List.rev newlySelected @ selectedRev) rest
                | SynModuleDecl.NestedModule (SynComponentInfo (_, _, _, moduleName, _, _, _, _),
                                              _,
                                              nestedDecls,
                                              _,
                                              _,
                                              _) ->
                    rejectAnnotationsNestedInModules (namespaceId @ moduleName) nestedDecls
                    loop opens selectedRev rest
                | _ -> loop opens selectedRev rest

        loop [] [] decls

    /// Select and validate namespace-level records annotated for constructor generation.
    let select (ast : ParsedInput) : RecordSelection list =
        match ast with
        | ParsedInput.ImplFile (ParsedImplFileInput (_, _, _, _, _, modules, _, _, _)) ->
            modules
            |> List.collect (fun (SynModuleOrNamespace (namespaceId, _, kind, decls, _, _, _, _, _)) ->
                match kind with
                | SynModuleOrNamespaceKind.DeclaredNamespace
                | SynModuleOrNamespaceKind.GlobalNamespace -> selectNamespace namespaceId decls
                | SynModuleOrNamespaceKind.NamedModule
                | SynModuleOrNamespaceKind.AnonModule ->
                    rejectAnnotationsNestedInModules namespaceId decls
                    []
            )
        | ParsedInput.SigFile _ -> []

    /// Generate constructor modules from an already-parsed F# input.
    let generate (ast : ParsedInput) : SynModuleOrNamespace list =
        ast
        |> select
        |> List.map (fun selection -> createModule selection.NamespaceId selection.Opens selection.Record)

open Myriad.Core

/// Myriad generator that creates a record constructor which takes the required fields
/// and initializes the optional fields to `None`.
[<MyriadGenerator("record-constructor")>]
type RecordConstructorGenerator () =

    interface IMyriadGenerator with
        member _.ValidInputExtensions = [ ".fs" ]

        member _.Generate (context : GeneratorContext) =
            let ast, _ =
                Ast.fromFilename context.InputFilename |> Async.RunSynchronously |> Array.head

            ast |> RecordConstructorGenerator.generate |> Output.Ast
