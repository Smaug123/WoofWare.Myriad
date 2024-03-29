namespace WoofWare.Myriad.Plugins

open Fantomas.FCS.Syntax
open Fantomas.FCS.SyntaxTrivia
open Fantomas.FCS.Text.Range
open Fantomas.FCS.Xml
open Myriad.Core.AstExtensions

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
        Members : MemberInfo list
        Properties : PropertyInfo list
        Generics : SynTyparDecls option
        Accessibility : SynAccess option
    }

type internal RecordType =
    {
        Name : Ident
        Fields : SynField seq
        Members : SynMemberDefns option
        XmlDoc : PreXmlDoc option
        Generics : SynTyparDecls option
        Accessibility : SynAccess option
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

    let instantiateRecord (fields : (RecordFieldName * SynExpr option) list) : SynExpr =
        let fields =
            fields
            |> List.map (fun (rfn, synExpr) -> SynExprRecordField (rfn, Some range0, synExpr, None))

        SynExpr.Record (None, None, fields, range0)

    let defineRecordType (record : RecordType) : SynTypeDefn =
        let repr =
            SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.Record (None, Seq.toList record.Fields, range0), range0)

        let name =
            SynComponentInfo.Create (
                [ record.Name ],
                ?xmldoc = record.XmlDoc,
                ?parameters = record.Generics,
                access = record.Accessibility
            )

        let trivia : SynTypeDefnTrivia =
            {
                LeadingKeyword = SynTypeDefnLeadingKeyword.Type range0
                EqualsRange = Some range0
                WithKeyword = Some range0
            }

        SynTypeDefn (name, repr, defaultArg record.Members SynMemberDefns.Empty, None, range0, trivia)

    let isOptionIdent (ident : SynLongIdent) : bool =
        match ident.LongIdent with
        | [ i ] when System.String.Equals (i.idText, "option", System.StringComparison.OrdinalIgnoreCase) -> true
        // TODO: consider Microsoft.FSharp.Option or whatever it is
        | _ -> false

    let isListIdent (ident : SynLongIdent) : bool =
        match ident.LongIdent with
        | [ i ] when System.String.Equals (i.idText, "list", System.StringComparison.OrdinalIgnoreCase) -> true
        // TODO: consider FSharpList or whatever it is
        | _ -> false

    let isArrayIdent (ident : SynLongIdent) : bool =
        match ident.LongIdent with
        | [ i ] when
            System.String.Equals (i.idText, "array", System.StringComparison.OrdinalIgnoreCase)
            || System.String.Equals (i.idText, "[]", System.StringComparison.Ordinal)
            ->
            true
        | _ -> false

    let isResponseIdent (ident : SynLongIdent) : bool =
        match ident.LongIdent |> List.map _.idText with
        | [ "Response" ]
        | [ "RestEase" ; "Response" ] -> true
        | _ -> false

    let isMapIdent (ident : SynLongIdent) : bool =
        match ident.LongIdent |> List.map _.idText with
        | [ "Map" ] -> true
        | _ -> false

    let isReadOnlyDictionaryIdent (ident : SynLongIdent) : bool =
        match ident.LongIdent |> List.map _.idText with
        | [ "IReadOnlyDictionary" ]
        | [ "Generic" ; "IReadOnlyDictionary" ]
        | [ "Collections" ; "Generic" ; "IReadOnlyDictionary" ]
        | [ "System" ; "Collections" ; "Generic" ; "IReadOnlyDictionary" ] -> true
        | _ -> false

    let isDictionaryIdent (ident : SynLongIdent) : bool =
        match ident.LongIdent |> List.map _.idText with
        | [ "Dictionary" ]
        | [ "Generic" ; "Dictionary" ]
        | [ "Collections" ; "Generic" ; "Dictionary" ]
        | [ "System" ; "Collections" ; "Generic" ; "Dictionary" ] -> true
        | _ -> false

    let isIDictionaryIdent (ident : SynLongIdent) : bool =
        match ident.LongIdent |> List.map _.idText with
        | [ "IDictionary" ]
        | [ "Generic" ; "IDictionary" ]
        | [ "Collections" ; "Generic" ; "IDictionary" ]
        | [ "System" ; "Collections" ; "Generic" ; "IDictionary" ] -> true
        | _ -> false

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
        | SynType.LongIdent ident ->
            {
                Attributes = []
                IsOptional = false
                Id = None
                Type = SynType.CreateLongIdent ident
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
                Type = SynType.Var (typar, range0)
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

    let toFun (inputs : SynType list) (ret : SynType) : SynType =
        (ret, List.rev inputs)
        ||> List.fold (fun ty input -> SynType.CreateFun (input, ty))

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

            ((toFun (List.map fst inputArgs) inputRet), hasParen) :: args, ret
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
                                    Type = SynType.CreateLongIdent (SynLongIdent.CreateFromLongIdent ident)
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
                                    Type = SynType.Var (typar, range0)
                                }
                                |> List.singleton
                        }
                    | _ -> failwith $"Unrecognised args in interface method declaration: %+A{args}"
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

        let members, properties =
            match synTypeDefnRepr with
            | SynTypeDefnRepr.ObjectModel (_kind, members, _) ->
                members
                |> List.map (fun defn ->
                    match defn with
                    | SynMemberDefn.AbstractSlot (slotSig, flags, _, _) -> parseMember slotSig flags
                    | _ -> failwith $"Unrecognised member definition: %+A{defn}"
                )
            | _ -> failwith $"Unrecognised SynTypeDefnRepr for an interface type: %+A{synTypeDefnRepr}"
            |> List.partitionChoice

        {
            Members = members
            Properties = properties
            Name = interfaceName
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

[<AutoOpen>]
module internal SynTypePatterns =
    let (|OptionType|_|) (fieldType : SynType) =
        match fieldType with
        | SynType.App (SynType.LongIdent ident, _, [ innerType ], _, _, _, _) when AstHelper.isOptionIdent ident ->
            Some innerType
        | _ -> None

    let (|ListType|_|) (fieldType : SynType) =
        match fieldType with
        | SynType.App (SynType.LongIdent ident, _, [ innerType ], _, _, _, _) when AstHelper.isListIdent ident ->
            Some innerType
        | _ -> None

    let (|ArrayType|_|) (fieldType : SynType) =
        match fieldType with
        | SynType.App (SynType.LongIdent ident, _, [ innerType ], _, _, _, _) when AstHelper.isArrayIdent ident ->
            Some innerType
        | SynType.Array (1, innerType, _) -> Some innerType
        | _ -> None

    let (|RestEaseResponseType|_|) (fieldType : SynType) =
        match fieldType with
        | SynType.App (SynType.LongIdent ident, _, [ innerType ], _, _, _, _) when AstHelper.isResponseIdent ident ->
            Some innerType
        | _ -> None

    let (|DictionaryType|_|) (fieldType : SynType) =
        match fieldType with
        | SynType.App (SynType.LongIdent ident, _, [ key ; value ], _, _, _, _) when AstHelper.isDictionaryIdent ident ->
            Some (key, value)
        | _ -> None

    let (|IDictionaryType|_|) (fieldType : SynType) =
        match fieldType with
        | SynType.App (SynType.LongIdent ident, _, [ key ; value ], _, _, _, _) when AstHelper.isIDictionaryIdent ident ->
            Some (key, value)
        | _ -> None

    let (|IReadOnlyDictionaryType|_|) (fieldType : SynType) =
        match fieldType with
        | SynType.App (SynType.LongIdent ident, _, [ key ; value ], _, _, _, _) when
            AstHelper.isReadOnlyDictionaryIdent ident
            ->
            Some (key, value)
        | _ -> None

    let (|MapType|_|) (fieldType : SynType) =
        match fieldType with
        | SynType.App (SynType.LongIdent ident, _, [ key ; value ], _, _, _, _) when AstHelper.isMapIdent ident ->
            Some (key, value)
        | _ -> None

    /// Returns the string name of the type.
    let (|PrimitiveType|_|) (fieldType : SynType) =
        match fieldType with
        | SynType.LongIdent ident ->
            match ident.LongIdent with
            | [ i ] ->
                [ "string" ; "float" ; "int" ; "bool" ; "char" ]
                |> List.tryFind (fun s -> s = i.idText)
            | _ -> None
        | _ -> None

    let (|String|_|) (fieldType : SynType) : unit option =
        match fieldType with
        | SynType.LongIdent ident ->
            match ident.LongIdent with
            | [ i ] ->
                [ "string" ]
                |> List.tryFind (fun s -> s = i.idText)
                |> Option.map ignore<string>
            | _ -> None
        | _ -> None

    let (|Byte|_|) (fieldType : SynType) : unit option =
        match fieldType with
        | SynType.LongIdent ident ->
            match ident.LongIdent with
            | [ i ] -> [ "byte" ] |> List.tryFind (fun s -> s = i.idText) |> Option.map ignore<string>
            | _ -> None
        | _ -> None

    let (|Guid|_|) (fieldType : SynType) : unit option =
        match fieldType with
        | SynType.LongIdent ident ->
            match ident.LongIdent |> List.map (fun i -> i.idText) with
            | [ "System" ; "Guid" ]
            | [ "Guid" ] -> Some ()
            | _ -> None
        | _ -> None

    let (|HttpResponseMessage|_|) (fieldType : SynType) : unit option =
        match fieldType with
        | SynType.LongIdent ident ->
            match ident.LongIdent |> List.map (fun i -> i.idText) with
            | [ "System" ; "Net" ; "Http" ; "HttpResponseMessage" ]
            | [ "Net" ; "Http" ; "HttpResponseMessage" ]
            | [ "Http" ; "HttpResponseMessage" ]
            | [ "HttpResponseMessage" ] -> Some ()
            | _ -> None
        | _ -> None

    let (|HttpContent|_|) (fieldType : SynType) : unit option =
        match fieldType with
        | SynType.LongIdent ident ->
            match ident.LongIdent |> List.map (fun i -> i.idText) with
            | [ "System" ; "Net" ; "Http" ; "HttpContent" ]
            | [ "Net" ; "Http" ; "HttpContent" ]
            | [ "Http" ; "HttpContent" ]
            | [ "HttpContent" ] -> Some ()
            | _ -> None
        | _ -> None

    let (|Stream|_|) (fieldType : SynType) : unit option =
        match fieldType with
        | SynType.LongIdent ident ->
            match ident.LongIdent |> List.map (fun i -> i.idText) with
            | [ "System" ; "IO" ; "Stream" ]
            | [ "IO" ; "Stream" ]
            | [ "Stream" ] -> Some ()
            | _ -> None
        | _ -> None

    let (|NumberType|_|) (fieldType : SynType) =
        match fieldType with
        | SynType.LongIdent ident ->
            match ident.LongIdent with
            | [ i ] -> [ "string" ; "float" ; "int" ; "bool" ] |> List.tryFind (fun s -> s = i.idText)
            | _ -> None
        | _ -> None

    let (|DateOnly|_|) (fieldType : SynType) =
        match fieldType with
        | SynType.LongIdent (SynLongIdent.SynLongIdent (ident, _, _)) ->
            match ident |> List.map (fun i -> i.idText) with
            | [ "System" ; "DateOnly" ]
            | [ "DateOnly" ] -> Some ()
            | _ -> None
        | _ -> None

    let (|DateTime|_|) (fieldType : SynType) =
        match fieldType with
        | SynType.LongIdent (SynLongIdent.SynLongIdent (ident, _, _)) ->
            match ident |> List.map (fun i -> i.idText) with
            | [ "System" ; "DateTime" ]
            | [ "DateTime" ] -> Some ()
            | _ -> None
        | _ -> None

    let (|Uri|_|) (fieldType : SynType) =
        match fieldType with
        | SynType.LongIdent (SynLongIdent.SynLongIdent (ident, _, _)) ->
            match ident |> List.map (fun i -> i.idText) with
            | [ "System" ; "Uri" ]
            | [ "Uri" ] -> Some ()
            | _ -> None
        | _ -> None

    let (|Task|_|) (fieldType : SynType) : SynType option =
        match fieldType with
        | SynType.App (SynType.LongIdent (SynLongIdent.SynLongIdent (ident, _, _)), _, args, _, _, _, _) ->
            match ident |> List.map (fun i -> i.idText) with
            | [ "Task" ]
            | [ "Tasks" ; "Task" ]
            | [ "Threading" ; "Tasks" ; "Task" ]
            | [ "System" ; "Threading" ; "Tasks" ; "Task" ] ->
                match args with
                | [ arg ] -> Some arg
                | _ -> failwithf "Expected Task to be applied to exactly one arg, but got: %+A" args
            | _ -> None
        | _ -> None
