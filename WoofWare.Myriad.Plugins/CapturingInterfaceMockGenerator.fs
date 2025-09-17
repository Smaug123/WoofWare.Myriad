namespace WoofWare.Myriad.Plugins

open System
open Fantomas.FCS.Syntax
open Fantomas.FCS.Xml
open WoofWare.Whippet.Fantomas

type internal CapturingInterfaceMockOutputSpec =
    {
        IsInternal : bool
    }

type private CallField =
    | ArgsObject of Ident * SynTypeDefn * SynTyparDecls option
    | Original of SynType

[<RequireQualifiedAccess>]
module internal CapturingInterfaceMockGenerator =
    open Fantomas.FCS.Text.Range

    [<RequireQualifiedAccess>]
    type private KnownInheritance = | IDisposable

    /// Expects the input `args` list to have more than one element.
    let private createTypeForArgs
        (spec : CapturingInterfaceMockOutputSpec)
        (memberName : Ident)
        (generics : SynTyparDecls option)
        (args : TupledArg list)
        : Ident * SynTypeDefn
        =
        let name = memberName.idText + "Call" |> Ident.create

        let access =
            if spec.IsInternal then
                SynAccess.Internal range0
            else
                SynAccess.Public range0

        let recordFields =
            args
            |> List.mapi (fun i tupledArg ->
                {
                    SynFieldData.Ident =
                        match tupledArg.Args with
                        | [ arg ] -> arg.Id
                        | _ -> None
                        |> Option.defaultValue (Ident.create $"Arg%i{i}")
                        |> Some
                    Attrs = []
                    Type =
                        tupledArg.Args
                        |> List.map (fun pi -> pi.Type)
                        |> SynType.tupleNoParen
                        |> Option.get
                }
                |> SynField.make
            )

        let record =
            {
                Name = name
                Fields = recordFields
                Members = None
                XmlDoc = Some (PreXmlDoc.create $"A single call to the %s{memberName.idText} method")
                Generics = generics
                TypeAccessibility = Some access
                ImplAccessibility = None
                Attributes = []
            }

        let typeDecl = AstHelper.defineRecordType record

        name, typeDecl

    let private buildType (x : ParameterInfo) : SynType =
        if x.IsOptional then
            SynType.app "option" [ x.Type ]
        else
            x.Type

    let private constructMemberSinglePlace (tuple : TupledArg) : SynType =
        tuple.Args
        |> List.map buildType
        |> SynType.tupleNoParen
        |> Option.defaultWith (fun () -> failwith "no-arg functions not supported yet")
        |> if tuple.HasParen then SynType.paren else id

    let rec private collectGenerics' (ty : SynType) : Ident list =
        match ty with
        | SynType.Var (typar = SynTypar (ident = typar)) -> [ typar ]
        | SynType.HashConstraint (innerType = ty)
        | SynType.WithGlobalConstraints (typeName = ty)
        | SynType.Paren (innerType = ty)
        | SynType.MeasurePower (baseMeasure = ty)
        | SynType.SignatureParameter (usedType = ty)
        | SynType.Array (elementType = ty) -> collectGenerics' ty
        | SynType.StaticConstant _
        | SynType.StaticConstantNamed _
        | SynType.StaticConstantExpr _
        | SynType.FromParseError _
        | SynType.Anon _
        | SynType.LongIdent _ -> []
        | SynType.LongIdentApp (typeArgs = tys)
        | SynType.App (typeArgs = tys) -> tys |> List.collect collectGenerics'
        | SynType.Tuple (path = path) ->
            path
            |> List.collect (fun seg ->
                match seg with
                | SynTupleTypeSegment.Type ty -> collectGenerics' ty
                | SynTupleTypeSegment.Star _
                | SynTupleTypeSegment.Slash _ -> []
            )
        | SynType.AnonRecd (fields = fields) -> fields |> List.collect (fun (_, ty) -> collectGenerics' ty)
        | SynType.Fun (argType = t1 ; returnType = t2)
        | SynType.Or (lhsType = t1 ; rhsType = t2) -> collectGenerics' t1 @ collectGenerics' t2

    let private collectGenerics (ty : SynType) =
        collectGenerics' ty |> List.distinctBy _.idText

    /// Builds the record field for the mock object, and also if applicable a type representing a single call to
    /// that object (packaging up the args of the call).
    let private constructMember (spec : CapturingInterfaceMockOutputSpec) (mem : MemberInfo) : SynField * CallField =
        let inputType = mem.Args |> List.map constructMemberSinglePlace

        let funcType = SynType.toFun inputType mem.ReturnType

        let field =
            {
                Type = funcType
                Attrs = []
                Ident = Some mem.Identifier
            }
            |> SynField.make
            |> SynField.withDocString (mem.XmlDoc |> Option.defaultValue PreXmlDoc.Empty)

        let argsType =
            match mem.Args with
            | [] -> failwith "expected args in member"
            | [ ty ] ->
                ty.Args
                |> List.map _.Type
                |> SynType.tupleNoParen
                |> Option.get
                |> CallField.Original
            | args ->
                let genericsUsed =
                    args
                    |> List.collect (fun arg -> arg.Args |> List.map _.Type |> List.collect collectGenerics)
                    |> List.distinctBy _.idText

                let genericsUsed =
                    match genericsUsed with
                    | [] -> None
                    | genericsUsed ->
                        genericsUsed
                        |> List.map (fun i ->
                            SynTyparDecl.SynTyparDecl ([], SynTypar.SynTypar (i, TyparStaticReq.None, false))
                        )
                        |> fun l -> SynTyparDecls.PostfixList (l, [], range0)
                        |> Some

                let name, defn = createTypeForArgs spec mem.Identifier genericsUsed args
                CallField.ArgsObject (name, defn, genericsUsed)

        field, argsType

    let constructProperty (prop : PropertyInfo) : SynField =
        {
            Attrs = []
            Ident = Some prop.Identifier
            Type = SynType.toFun [ SynType.unit ] prop.Type
        }
        |> SynField.make
        |> SynField.withDocString (prop.XmlDoc |> Option.defaultValue PreXmlDoc.Empty)

    let createType
        (spec : CapturingInterfaceMockOutputSpec)
        (name : string)
        (interfaceType : InterfaceType)
        (xmlDoc : PreXmlDoc)
        : SynModuleDecl option * SynModuleDecl
        =
        let fields =
            interfaceType.Members
            |> List.map (constructMember spec)
            |> List.append (
                interfaceType.Properties
                |> List.map constructProperty
                |> List.map (Tuple.withRight (CallField.Original SynType.unit))
            )

        let inherits =
            interfaceType.Inherits
            |> Seq.map (fun ty ->
                match ty with
                | SynType.LongIdent (SynLongIdent.SynLongIdent (name, _, _)) ->
                    match name |> List.map _.idText with
                    | [] -> failwith "Unexpected empty identifier in inheritance declaration"
                    | [ "IDisposable" ]
                    | [ "System" ; "IDisposable" ] -> KnownInheritance.IDisposable
                    | _ -> failwithf $"Unrecognised inheritance identifier: %+A{name}"
                | x -> failwithf $"Unrecognised type in inheritance: %+A{x}"
            )
            |> Set.ofSeq

        // TODO: for each field, if there are multiple arguments to the member, stamp out a new type to represent them;
        // then store that type name in this list alongside the field name
        let fields =
            fields
            |> List.map (fun (SynField (idOpt = idOpt) as f, extraType) ->
                let fieldName =
                    match idOpt with
                    | None -> failwith $"unexpectedly got a field with no identifier: %O{f}"
                    | Some idOpt -> idOpt.idText

                f, extraType, fieldName
            )

        let failwithNotImplemented (fieldName : string) =
            let failString = SynExpr.CreateConst $"Unimplemented mock function: %s{fieldName}"

            SynExpr.createLongIdent [ "System" ; "NotImplementedException" ]
            |> SynExpr.applyTo failString
            |> SynExpr.paren
            |> SynExpr.applyFunction (SynExpr.createIdent "raise")
            |> SynExpr.createLambda "_"

        let constructorReturnType =
            match interfaceType.Generics with
            | None -> SynType.createLongIdent' [ name ]
            | Some generics ->

            let generics =
                generics.TyparDecls
                |> List.map (fun (SynTyparDecl (_, typar)) -> SynType.var typar)

            SynType.app name generics

        let emptyRecordFieldInstantiations =
            let interfaceExtras =
                if inherits.Contains KnownInheritance.IDisposable then
                    let unitFun = SynExpr.createThunk (SynExpr.CreateConst ())

                    [ SynLongIdent.createS "Dispose", unitFun ]
                else
                    []

            let originalMembers =
                fields
                |> List.map (fun (_, _, fieldName) -> SynLongIdent.createS fieldName, failwithNotImplemented fieldName)

            let callsArrays =
                fields
                |> List.map (fun (_field, _, fieldName) ->
                    let name = SynLongIdent.createS $"%s{fieldName}_Calls"

                    let init =
                        SynExpr.createIdent "ResizeArray" |> SynExpr.applyTo (SynExpr.CreateConst ())

                    name, init
                )

            interfaceExtras @ originalMembers @ callsArrays

        let staticMemberEmpty =
            SynBinding.basic
                [ Ident.create "Empty" ]
                (if interfaceType.Generics.IsNone then
                     []
                 else
                     [ SynPat.unit ])
                (SynExpr.createRecord None emptyRecordFieldInstantiations)
            |> SynBinding.withXmlDoc (PreXmlDoc.create "An implementation where every non-unit method throws.")
            |> SynBinding.withReturnAnnotation constructorReturnType
            |> SynMemberDefn.staticMember

        let recordFields =
            let extras =
                if inherits.Contains KnownInheritance.IDisposable then
                    {
                        Attrs = []
                        Ident = Some (Ident.create "Dispose")
                        Type = SynType.funFromDomain SynType.unit SynType.unit
                    }
                    |> SynField.make
                    |> SynField.withDocString (PreXmlDoc.create "Implementation of IDisposable.Dispose")
                    |> List.singleton
                else
                    []

            let nonExtras =
                fields
                |> List.collect (fun (field, callType, fieldName) ->
                    let callField =
                        match callType with
                        | CallField.Original ty ->
                            {
                                Attrs = []
                                Ident = Some (fieldName + "_Calls" |> Ident.create)
                                Type = SynType.app "ResizeArray" [ ty ]
                            }
                            |> SynField.make
                            |> SynField.withDocString (
                                PreXmlDoc.create
                                    "Additions to this ResizeArray are locked on itself. For maximum safety, lock on this field before reading it."
                            )
                        | CallField.ArgsObject (argsObjectName, _, generics) ->
                            {
                                Attrs = []
                                Ident = Some (fieldName + "_Calls" |> Ident.create)
                                Type =
                                    match generics with
                                    | None -> SynType.named argsObjectName.idText
                                    | Some generics ->
                                        generics.TyparDecls
                                        |> List.map (fun (SynTyparDecl.SynTyparDecl (_, typar)) -> SynType.var typar)
                                        |> SynType.app' (
                                            SynType.createLongIdent' [ $"%s{name}Calls" ; argsObjectName.idText ]
                                        )
                                    |> List.singleton
                                    |> SynType.app "ResizeArray"
                            }
                            |> SynField.make

                    [ field ; callField ]
                )

            extras @ nonExtras

        let interfaceMembers =
            let members =
                interfaceType.Members
                |> List.map (fun memberInfo ->
                    let headArgs =
                        memberInfo.Args
                        |> List.mapi (fun i tupledArgs ->
                            let args =
                                tupledArgs.Args
                                |> List.mapi (fun j ty ->
                                    match ty.Type with
                                    | UnitType -> SynPat.unit
                                    | _ -> SynPat.named $"arg_%i{i}_%i{j}"
                                )

                            match args with
                            | [] -> failwith "somehow got no args at all"
                            | [ arg ] -> arg
                            | args -> SynPat.tuple args
                            |> fun i -> if tupledArgs.HasParen then SynPat.paren i else i
                        )

                    let body =
                        let tuples =
                            memberInfo.Args
                            |> List.mapi (fun i args ->
                                args.Args
                                |> List.mapi (fun j arg ->
                                    match arg.Type with
                                    | UnitType -> SynExpr.CreateConst ()
                                    | _ -> SynExpr.createIdent $"arg_%i{i}_%i{j}"
                                )
                                |> SynExpr.tuple
                            )

                        match tuples |> List.rev with
                        | [] -> failwith "expected args but got none"
                        | last :: rest ->

                        (last, rest)
                        ||> List.fold SynExpr.applyTo
                        |> SynExpr.applyFunction (
                            SynExpr.createLongIdent' [ Ident.create "this" ; memberInfo.Identifier ]
                        )

                    SynBinding.basic [ Ident.create "this" ; memberInfo.Identifier ] headArgs body
                    |> SynMemberDefn.memberImplementation
                )

            let properties =
                interfaceType.Properties
                |> List.map (fun pi ->
                    SynExpr.createLongIdent' [ Ident.create "this" ; pi.Identifier ]
                    |> SynExpr.applyTo (SynExpr.CreateConst ())
                    |> SynBinding.basic [ Ident.create "this" ; pi.Identifier ] []
                    |> SynMemberDefn.memberImplementation
                )

            let interfaceName =
                let baseName = SynType.createLongIdent interfaceType.Name

                match interfaceType.Generics with
                | None -> baseName
                | Some generics ->
                    let generics =
                        match generics with
                        | SynTyparDecls.PostfixList (decls, _, _) -> decls
                        | SynTyparDecls.PrefixList (decls, _) -> decls
                        | SynTyparDecls.SinglePrefix (decl, _) -> [ decl ]
                        |> List.map (fun (SynTyparDecl (_, typar)) -> SynType.var typar)

                    SynType.app' baseName generics

            SynMemberDefn.Interface (interfaceName, Some range0, Some (members @ properties), range0)

        let access =
            match interfaceType.Accessibility, spec.IsInternal with
            | Some (SynAccess.Public _), true
            | None, true -> SynAccess.Internal range0
            | Some (SynAccess.Public _), false -> SynAccess.Public range0
            | None, false -> SynAccess.Public range0
            | Some (SynAccess.Internal _), _ -> SynAccess.Internal range0
            | Some (SynAccess.Private _), _ -> SynAccess.Private range0

        let extraInterfaces =
            inherits
            |> Seq.map (fun inheritance ->
                match inheritance with
                | KnownInheritance.IDisposable ->
                    let mem =
                        SynExpr.createLongIdent [ "this" ; "Dispose" ]
                        |> SynExpr.applyTo (SynExpr.CreateConst ())
                        |> SynBinding.basic [ Ident.create "this" ; Ident.create "Dispose" ] [ SynPat.unit ]
                        |> SynBinding.withReturnAnnotation SynType.unit
                        |> SynMemberDefn.memberImplementation

                    SynMemberDefn.Interface (
                        SynType.createLongIdent' [ "System" ; "IDisposable" ],
                        Some range0,
                        Some [ mem ],
                        range0
                    )
            )
            |> Seq.toList

        let record =
            {
                Name = Ident.create name
                Fields = recordFields
                Members = Some ([ staticMemberEmpty ; interfaceMembers ] @ extraInterfaces)
                XmlDoc = Some xmlDoc
                Generics = interfaceType.Generics
                TypeAccessibility = Some access
                ImplAccessibility = None
                Attributes = []
            }

        let typeDecl = AstHelper.defineRecordType record

        let callsModule =
            fields
            |> List.choose (fun (_, field, _) ->
                match field with
                | CallField.Original _ -> None
                | CallField.ArgsObject (_, callType, _) -> Some callType
            )
            |> function
                | [] -> None
                | l ->
                    SynModuleDecl.Types (l, range0)
                    |> List.singleton
                    |> SynModuleDecl.nestedModule (
                        SynComponentInfo.create (Ident.create $"%s{name}Calls")
                        |> SynComponentInfo.withAccessibility access
                    )
                    |> Some

        (callsModule, SynModuleDecl.Types ([ typeDecl ], range0))

    let createRecord
        (namespaceId : LongIdent)
        (opens : SynOpenDeclTarget list)
        (interfaceType : SynTypeDefn, spec : CapturingInterfaceMockOutputSpec)
        : SynModuleOrNamespace
        =
        let interfaceType = AstHelper.parseInterface interfaceType

        let docString = PreXmlDoc.create "Mock record type for an interface"

        let name =
            List.last interfaceType.Name
            |> _.idText
            |> fun s ->
                if s.StartsWith 'I' && s.Length > 1 && Char.IsUpper s.[1] then
                    s.Substring 1
                else
                    s
            |> fun s -> s + "Mock"

        let callsTypes, typeDecl = createType spec name interfaceType docString

        [
            yield! opens |> List.map SynModuleDecl.openAny
            match callsTypes with
            | None -> ()
            | Some c -> yield c
            yield typeDecl
        ]
        |> SynModuleOrNamespace.createNamespace namespaceId

open Myriad.Core

/// Myriad generator that creates a record which implements the given interface,
/// but with every field mocked out.
[<MyriadGenerator("capturing-interface-mock")>]
type CapturingInterfaceMockGenerator () =

    interface IMyriadGenerator with
        member _.ValidInputExtensions = [ ".fs" ]

        member _.Generate (context : GeneratorContext) =
            let targetedTypes =
                MyriadParamParser.render context.AdditionalParameters
                |> Map.map (fun _ v -> v.Split '!' |> Array.toList |> List.map DesiredGenerator.Parse)

            let ast, _ =
                Ast.fromFilename context.InputFilename |> Async.RunSynchronously |> Array.head

            let types = Ast.getTypes ast

            let namespaceAndInterfaces =
                types
                |> List.choose (fun (ns, types) ->
                    types
                    |> List.choose (fun typeDef ->
                        match SynTypeDefn.getAttribute typeof<GenerateCapturingMockAttribute>.Name typeDef with
                        | None ->
                            let name = SynTypeDefn.getName typeDef |> List.map _.idText |> String.concat "."

                            match Map.tryFind name targetedTypes with
                            | Some desired ->
                                desired
                                |> List.tryPick (fun generator ->
                                    match generator with
                                    | DesiredGenerator.CapturingInterfaceMock arg ->
                                        let spec =
                                            {
                                                IsInternal =
                                                    arg
                                                    |> Option.defaultValue
                                                        GenerateCapturingMockAttribute.DefaultIsInternal
                                            }

                                        Some (typeDef, spec)
                                    | _ -> None
                                )
                            | _ -> None

                        | Some attr ->
                            let arg =
                                match SynExpr.stripOptionalParen attr.ArgExpr with
                                | SynExpr.Const (SynConst.Bool value, _) -> value
                                | SynExpr.Const (SynConst.Unit, _) -> GenerateCapturingMockAttribute.DefaultIsInternal
                                | arg ->
                                    failwith
                                        $"Unrecognised argument %+A{arg} to [<%s{nameof GenerateCapturingMockAttribute}>]. Literals are not supported. Use `true` or `false` (or unit) only."

                            let spec =
                                {
                                    IsInternal = arg
                                }

                            Some (typeDef, spec)
                    )
                    |> function
                        | [] -> None
                        | ty -> Some (ns, ty)
                )

            let opens = AstHelper.extractOpens ast

            let modules =
                namespaceAndInterfaces
                |> List.collect (fun (ns, records) ->
                    records |> List.map (CapturingInterfaceMockGenerator.createRecord ns opens)
                )

            Output.Ast modules
