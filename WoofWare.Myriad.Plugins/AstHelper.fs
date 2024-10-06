namespace WoofWare.Myriad.Plugins

open Fantomas.FCS.Syntax
open Fantomas.FCS.Text.Range
open WoofWare.Whippet.Fantomas

/// Anything that is part of an ADT.
/// A record is a product of stuff; this type represents one of those stuffs.
type internal AdtNode =
    {
        Type : SynType
        Name : Ident option
        /// An ordered list, so you can look up any given generic within `this.Type`
        /// to discover what its index is in the parent DU which defined it.
        GenericsOfParent : SynTyparDecl list
    }

/// A DU is a sum of products (e.g. `type Thing = Foo of a * b`);
/// similarly a record is a product.
/// This type represents a product in that sense.
type internal AdtProduct =
    {
        Name : SynIdent
        Fields : AdtNode list
        /// This AdtProduct represents a product in which there might be
        /// some bound type parameters. This field lists the bound
        /// type parameters in the order they appeared on the parent type.
        Generics : SynTyparDecl list
    }

[<RequireQualifiedAccess>]
module internal AstHelper =

    let isEnum (SynTypeDefn.SynTypeDefn (_, repr, _, _, _, _)) : bool =
        match repr with
        | SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.Enum _, _) -> true
        | _ -> false

    let instantiateRecord (fields : (SynLongIdent * SynExpr) list) : SynExpr =
        let fields =
            fields
            |> List.map (fun (rfn, synExpr) -> SynExprRecordField ((rfn, true), Some range0, Some synExpr, None))

        SynExpr.Record (None, None, fields, range0)

    let defineRecordType (record : RecordType) : SynTypeDefn =
        let name =
            SynComponentInfo.create record.Name
            |> SynComponentInfo.setAccessibility record.TypeAccessibility
            |> match record.XmlDoc with
               | None -> id
               | Some doc -> SynComponentInfo.withDocString doc
            |> SynComponentInfo.setGenerics record.Generics

        SynTypeDefnRepr.recordWithAccess record.ImplAccessibility (Seq.toList record.Fields)
        |> SynTypeDefn.create name
        |> SynTypeDefn.withMemberDefns (defaultArg record.Members SynMemberDefns.Empty)

    let rec private extractOpensFromDecl (moduleDecls : SynModuleDecl list) : SynOpenDeclTarget list =
        moduleDecls
        |> List.choose (fun moduleDecl ->
            match moduleDecl with
            | SynModuleDecl.Open (target, _) -> Some target
            | _ -> None
        )

    let extractOpens (ast : ParsedInput) : SynOpenDeclTarget list =
        match ast with
        | ParsedInput.ImplFile (ParsedImplFileInput (_, _, _, _, _, modules, _, _, _)) ->
            modules
            |> List.collect (fun (SynModuleOrNamespace (_, _, _, decls, _, _, _, _, _)) -> extractOpensFromDecl decls)
        | _ -> []

    let rec convertSigParam (ty : SynType) : ParameterInfo * bool =
        match ty with
        | SynType.Paren (inner, _) ->
            let result, _ = convertSigParam inner
            result, true
        | SynType.LongIdent (SynLongIdent.SynLongIdent (ident, _, _)) ->
            {
                Attributes = []
                IsOptional = false
                Id = None
                Type = SynType.createLongIdent ident
            },
            false
        | SynType.SignatureParameter (attrs, opt, id, usedType, _) ->
            let attrs = attrs |> List.collect (fun attrs -> attrs.Attributes)

            {
                Attributes = attrs
                IsOptional = opt
                Id = id
                Type = usedType
            },
            false
        | SynType.Var (typar, _) ->
            {
                Attributes = []
                IsOptional = false
                Id = None
                Type = SynType.var typar
            },
            false
        | _ -> failwithf "expected SignatureParameter, got: %+A" ty

    let rec extractTupledTypes (tupleType : SynTupleTypeSegment list) : TupledArg =
        match tupleType with
        | [] ->
            {
                HasParen = false
                Args = []
            }
        | [ SynTupleTypeSegment.Type param ] ->
            let converted, hasParen = convertSigParam param

            {
                HasParen = hasParen
                Args = [ converted ]
            }
        | SynTupleTypeSegment.Type param :: SynTupleTypeSegment.Star _ :: rest ->
            let rest = extractTupledTypes rest
            let converted, _ = convertSigParam param

            {
                HasParen = false
                Args = converted :: rest.Args
            }
        | _ -> failwithf "Didn't have alternating type-and-star in interface member definition: %+A" tupleType

    /// Returns the args (where these are tuple types if curried) in order, and the return type.
    let rec getType (ty : SynType) : (SynType * bool) list * SynType =
        match ty with
        | SynType.Paren (ty, _) -> getType ty
        | SynType.Fun (argType, returnType, _, _) ->
            let args, ret = getType returnType
            // TODO this code is clearly wrong
            let (inputArgs, inputRet), hasParen =
                match argType with
                | SynType.Paren (argType, _) -> getType argType, true
                | _ -> getType argType, false

            ((SynType.toFun (List.map fst inputArgs) inputRet), hasParen) :: args, ret
        | _ -> [], ty

    let private parseMember (slotSig : SynValSig) (flags : SynMemberFlags) : Choice<MemberInfo, PropertyInfo> =
        if not flags.IsInstance then
            failwith "member was not an instance member"

        let propertyAccessors =
            match flags.MemberKind with
            | SynMemberKind.Member -> None
            | SynMemberKind.PropertyGet -> Some PropertyAccessors.Get
            | SynMemberKind.PropertySet -> Some PropertyAccessors.Set
            | SynMemberKind.PropertyGetSet -> Some PropertyAccessors.GetSet
            | kind -> failwithf "Unrecognised member kind: %+A" kind

        match slotSig with
        | SynValSig (attrs,
                     SynIdent.SynIdent (ident, _),
                     _typeParams,
                     synType,
                     _arity,
                     isInline,
                     isMutable,
                     xmlDoc,
                     accessibility,
                     synExpr,
                     _,
                     _) ->

            match synExpr with
            | Some _ -> failwith "literal members are not supported"
            | None -> ()

            let attrs = attrs |> List.collect _.Attributes

            let args, ret = getType synType

            let args =
                args
                |> List.map (fun (args, hasParen) ->
                    match args with
                    | SynType.Tuple (false, path, _) -> extractTupledTypes path
                    | SynType.SignatureParameter _ ->
                        let arg, hasParen = convertSigParam args

                        {
                            HasParen = hasParen
                            Args = [ arg ]
                        }
                    | SynType.LongIdent (SynLongIdent (ident, _, _)) ->
                        {
                            HasParen = false
                            Args =
                                {
                                    Attributes = []
                                    IsOptional = false
                                    Id = None
                                    Type = SynType.createLongIdent ident
                                }
                                |> List.singleton
                        }
                    | SynType.Var (typar, _) ->
                        {
                            HasParen = false
                            Args =
                                {
                                    Attributes = []
                                    IsOptional = false
                                    Id = None
                                    Type = SynType.var typar
                                }
                                |> List.singleton
                        }
                    | arg ->
                        {
                            HasParen = false
                            Args =
                                {
                                    Attributes = []
                                    IsOptional = false
                                    Id = None
                                    Type = arg
                                }
                                |> List.singleton
                        }
                    |> fun ty ->
                        { ty with
                            HasParen = ty.HasParen || hasParen
                        }
                )

            match propertyAccessors with
            | None ->
                {
                    ReturnType = ret
                    Args = args
                    Identifier = ident
                    Attributes = attrs
                    XmlDoc = Some xmlDoc
                    Accessibility = accessibility
                    IsInline = isInline
                    IsMutable = isMutable
                }
                |> Choice1Of2
            | Some accessors ->
                {
                    Type = ret
                    Accessibility = accessibility
                    Attributes = attrs
                    XmlDoc = Some xmlDoc
                    Accessors = accessors
                    IsInline = isInline
                    Identifier = ident
                }
                |> Choice2Of2

    /// Assumes that the input type is an ObjectModel, i.e. a `type Foo = member ...`
    let parseInterface (interfaceType : SynTypeDefn) : InterfaceType =
        let (SynTypeDefn (SynComponentInfo (attrs, typars, _, interfaceName, _, _, accessibility, _),
                          synTypeDefnRepr,
                          _,
                          _,
                          _,
                          _)) =
            interfaceType

        let attrs = attrs |> List.collect (fun s -> s.Attributes)

        let members, inherits =
            match synTypeDefnRepr with
            | SynTypeDefnRepr.ObjectModel (_kind, members, _) ->
                members
                |> List.map (fun defn ->
                    match defn with
                    | SynMemberDefn.AbstractSlot (slotSig, flags, _, _) -> Choice1Of2 (parseMember slotSig flags)
                    | SynMemberDefn.Inherit (baseType, _asIdent, _) -> Choice2Of2 baseType
                    | _ -> failwith $"Unrecognised member definition: %+A{defn}"
                )
            | _ -> failwith $"Unrecognised SynTypeDefnRepr for an interface type: %+A{synTypeDefnRepr}"
            |> List.partitionChoice

        let members, properties = members |> List.partitionChoice

        {
            Members = members
            Properties = properties
            Name = interfaceName
            Inherits = inherits
            Attributes = attrs
            Generics = typars
            Accessibility = accessibility
        }

    let getUnionCases
        (SynTypeDefn.SynTypeDefn (info, repr, _, _, _, _))
        : AdtProduct list * SynTyparDecl list * SynAccess option
        =
        let typars, access =
            match info with
            | SynComponentInfo (_, typars, _, _, _, _, access, _) -> typars, access

        let typars =
            match typars with
            | None -> []
            | Some (SynTyparDecls.PrefixList (decls, _)) -> decls
            | Some (SynTyparDecls.SinglePrefix (l, _)) -> [ l ]
            | Some (SynTyparDecls.PostfixList (decls, constraints, _)) ->
                if not constraints.IsEmpty then
                    failwith "Constrained type parameters not currently supported"

                decls

        match repr with
        | SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.Union (_, cases, _), _) ->
            let cases =
                cases
                |> List.map (fun (SynUnionCase.SynUnionCase (_, ident, kind, _, _, _, _)) ->
                    match kind with
                    | SynUnionCaseKind.FullType _ -> failwith "FullType union cases not supported"
                    | SynUnionCaseKind.Fields fields ->
                        {
                            Name = ident
                            Fields =
                                fields
                                |> List.map (fun (SynField.SynField (_, _, id, ty, _, _, _, _, _)) ->
                                    {
                                        Type = ty
                                        Name = id
                                        GenericsOfParent = typars
                                    }
                                )
                            Generics = typars
                        }
                )

            cases, typars, access
        | _ -> failwithf "Failed to get union cases for type that was: %+A" repr

    let getRecordFields (SynTypeDefn.SynTypeDefn (typeInfo, repr, _, _, _, _)) : AdtNode list =
        let (SynComponentInfo.SynComponentInfo (typeParams = typars)) = typeInfo

        let typars =
            match typars with
            | None -> []
            | Some (SynTyparDecls.PrefixList (decls, _)) -> decls
            | Some (SynTyparDecls.SinglePrefix (l, _)) -> [ l ]
            | Some (SynTyparDecls.PostfixList (decls, constraints, _)) ->
                if not constraints.IsEmpty then
                    failwith "Constrained type parameters not currently supported"

                decls

        match repr with
        | SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.Record (_, fields, _), _) ->
            fields
            |> List.map (fun (SynField.SynField (_, _, ident, ty, _, _, _, _, _)) ->
                {
                    Name = ident
                    Type = ty
                    GenericsOfParent = typars
                }
            )
        | _ -> failwithf "Failed to get record elements for type that was: %+A" repr
