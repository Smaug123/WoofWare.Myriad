namespace WoofWare.Myriad.Plugins

open Fantomas.FCS.Syntax
open Fantomas.FCS.Text.Range
open Fantomas.FCS.Xml

type internal ParameterInfo =
    {
        Attributes : SynAttribute list
        IsOptional : bool
        Id : Ident option
        Type : SynType
    }

type internal TupledArg =
    {
        HasParen : bool
        Args : ParameterInfo list
    }

type internal MemberInfo =
    {
        ReturnType : SynType
        Accessibility : SynAccess option
        /// Each element of this list is a list of args in a tuple, or just one arg if not a tuple.
        Args : TupledArg list
        Identifier : Ident
        Attributes : SynAttribute list
        XmlDoc : PreXmlDoc option
        IsInline : bool
        IsMutable : bool
    }

[<RequireQualifiedAccess>]
type internal PropertyAccessors =
    | Get
    | Set
    | GetSet

type internal PropertyInfo =
    {
        Type : SynType
        Accessibility : SynAccess option
        Attributes : SynAttribute list
        XmlDoc : PreXmlDoc option
        Accessors : PropertyAccessors
        IsInline : bool
        Identifier : Ident
    }

type internal InterfaceType =
    {
        Attributes : SynAttribute list
        Name : LongIdent
        Inherits : SynType list
        Members : MemberInfo list
        Properties : PropertyInfo list
        Generics : SynTyparDecls option
        Accessibility : SynAccess option
    }

type internal RecordType =
    {
        Name : Ident
        Fields : SynField list
        /// Any additional members which are not record fields.
        Members : SynMemberDefns option
        XmlDoc : PreXmlDoc option
        Generics : SynTyparDecls option
        Accessibility : SynAccess option
        Attributes : SynAttribute list
    }

    /// Parse from the AST.
    static member OfRecord
        (sci : SynComponentInfo)
        (smd : SynMemberDefns)
        (access : SynAccess option)
        (recordFields : SynField list)
        : RecordType
        =
        match sci with
        | SynComponentInfo.SynComponentInfo (attrs, typars, _, longId, doc, _, access2, _) ->
            if access <> access2 then
                failwith $"TODO what's happened, two different accessibility modifiers: %O{access} and %O{access2}"

            {
                Name = List.last longId
                Fields = recordFields
                Members = if smd.IsEmpty then None else Some smd
                XmlDoc = if doc.IsEmpty then None else Some doc
                Generics = typars
                Accessibility = access
                Attributes = attrs |> List.collect (fun l -> l.Attributes)
            }

/// Methods for manipulating UnionCase.
[<RequireQualifiedAccess>]
module UnionCase =
    /// Construct our structured `UnionCase` from an FCS `SynUnionCase`: extract everything
    /// we care about from the AST representation.
    let ofSynUnionCase (case : SynUnionCase) : UnionCase<Ident option> =
        match case with
        | SynUnionCase.SynUnionCase (attributes, ident, caseType, xmlDoc, access, _, _) ->

        let ident =
            match ident with
            | SynIdent.SynIdent (ident, _) -> ident

        let fields =
            match caseType with
            | SynUnionCaseKind.Fields cases -> cases
            | SynUnionCaseKind.FullType _ -> failwith "unexpected FullType union"

        {
            Name = ident
            XmlDoc = if xmlDoc.IsEmpty then None else Some xmlDoc
            Access = access
            Attributes = attributes |> List.collect (fun t -> t.Attributes)
            Fields = fields |> List.map SynField.extract
        }

    /// Functorial `map`.
    let mapIdentFields<'a, 'b> (f : 'a -> 'b) (unionCase : UnionCase<'a>) : UnionCase<'b> =
        {
            Attributes = unionCase.Attributes
            Name = unionCase.Name
            Access = unionCase.Access
            XmlDoc = unionCase.XmlDoc
            Fields = unionCase.Fields |> List.map (SynField.mapIdent f)
        }

/// Everything you need to know about a discriminated union definition.
type internal UnionType =
    {
        /// The name of the DU: for example, `type Foo = | Blah` has this being `Foo`.
        Name : Ident
        /// Any additional members which are not union cases.
        Members : SynMemberDefns option
        /// Any docstring associated with the DU itself (not its cases).
        XmlDoc : PreXmlDoc option
        /// Generic type parameters this DU takes: `type Foo<'a> = | ...`.
        Generics : SynTyparDecls option
        /// Attributes of the DU (not its cases): `[<Attr>] type Foo = | ...`
        Attributes : SynAttribute list
        /// Accessibility modifier of the DU: `type private Foo = ...`
        Accessibility : SynAccess option
        /// The actual DU cases themselves.
        Cases : UnionCase<Ident option> list
    }

    static member OfUnion
        (sci : SynComponentInfo)
        (smd : SynMemberDefns)
        (access : SynAccess option)
        (cases : SynUnionCase list)
        : UnionType
        =
        match sci with
        | SynComponentInfo.SynComponentInfo (attrs, typars, _, longId, doc, _, access2, _) ->
            if access <> access2 then
                failwith $"TODO what's happened, two different accessibility modifiers: %O{access} and %O{access2}"

            {
                Name = List.last longId
                Members = if smd.IsEmpty then None else Some smd
                XmlDoc = if doc.IsEmpty then None else Some doc
                Generics = typars
                Attributes = attrs |> List.collect (fun l -> l.Attributes)
                Accessibility = access
                Cases = cases |> List.map UnionCase.ofSynUnionCase
            }

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
            |> SynComponentInfo.setAccessibility record.Accessibility
            |> match record.XmlDoc with
               | None -> id
               | Some doc -> SynComponentInfo.withDocString doc
            |> SynComponentInfo.setGenerics record.Generics

        SynTypeDefnRepr.record (Seq.toList record.Fields)
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
