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

/// A generator which synthesizes an identifier -- rather than reusing one from the user's source --
/// has to spell it so the generated file reads it back as itself. Fantomas prints an `Ident` exactly
/// as its `idText` reads, and reproduces backticks only when it can slice the original source text
/// at the node's real range; a node built here has no such text behind it, so a name which needed
/// backticks at its declaration loses them, and the generated file does not parse.
[<RequireQualifiedAccess>]
module internal BacktickIdent =

    /// F#'s lexer accepts each of these bare in a record-construction label -- so `Ast.parse` below
    /// would say they're fine -- but the real compiler reserves them "for future use" and emits
    /// FS0046, a warning by default but an error under `--warnaserror` (which this repo enables).
    /// Fantomas's own parser doesn't model this distinction, so it can't be asked; this list was
    /// instead obtained by compiling each candidate word from the F# keyword reference
    /// (https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/keyword-reference) bare
    /// in this exact position with the actual compiler (`dotnet fsi`) and keeping the ones that
    /// warned. That reference is not itself definitive -- three of its listed words (`const`,
    /// `event`, `external`) no longer trigger the warning at all -- which is exactly why this was
    /// verified against the compiler rather than transcribed from the page.
    let private reservedForFutureUse =
        set
            [
                "break"
                "checked"
                "component"
                "constraint"
                "continue"
                "include"
                "mixin"
                "parallel"
                "process"
                "protected"
                "pure"
                "sealed"
                "tailcall"
                "trait"
                "virtual"
            ]

    /// Whether `ident` is safe to splice in bare wherever the generated file wants a single
    /// identifier. F#'s lexer treats a number of shapes as meaningful bare tokens in *other* grammar
    /// positions but not as a plain identifier.
    ///
    /// The probe deliberately uses the record-label position for every caller, including the ones
    /// which emit a member name rather than a label. That position is the tightest available: it
    /// admits exactly one identifier and nothing else, so the parser accepts the probe only if
    /// `ident` really is one token. A probe in the position a caller actually emits into can be far
    /// weaker -- `Owner.%s{ident} ()` would happily parse `Owner.Defaultspace name ()` as an
    /// application of `Owner.Defaultspace` to `name`, and report success for a name which is
    /// nothing of the sort. Being tighter than a caller needs only means backticking something
    /// which did not require it, and backticks are a legal alternative spelling of any identifier,
    /// so that is always safe.
    ///
    /// This is still not exhaustive: a name built to smuggle extra syntax into the probe (e.g. one
    /// containing a block comment, `A (*x*)`) can make the probe parse successfully as a *different*,
    /// shorter label than `ident`, without the parser or `reservedForFutureUse` ever seeing anything
    /// wrong. Deliberately left unfixed: such a name is not a real identifier anyone would write, and
    /// the failure mode if it ever occurred is the same one being fixed here -- generated code that
    /// doesn't compile -- not silent corruption.
    let isValidBare (ident : string) : bool =
        if reservedForFutureUse.Contains ident then
            false
        else

        try
            Ast.parse $"module M\ntype T = {{ %s{ident} : int }}\nlet _ = {{ %s{ident} = 1 }}"
            |> ignore<ParsedInput>

            true
        with _ ->
            false

    /// Re-backtick an identifier we are about to emit, if it needs backticks to be read back as
    /// itself -- exactly as its declaration needed them, if it had any (backticking is always a
    /// legal alternative spelling of any identifier, so this is safe to apply unconditionally to
    /// whatever `isValidBare` rejects).
    let escape (ident : string) : string =
        if isValidBare ident then ident else "``" + ident + "``"

[<RequireQualifiedAccess>]
module internal AstHelper =

    let isEnum (SynTypeDefn.SynTypeDefn (_, repr, _, _, _, _)) : bool =
        match repr with
        | SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.Enum _, _) -> true
        | _ -> false

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

    let rec internal extractOpensFromDecl (moduleDecls : SynModuleDecl list) : SynOpenDeclTarget list =
        moduleDecls
        |> List.choose (fun moduleDecl ->
            match moduleDecl with
            | SynModuleDecl.Open (target, _) -> Some target
            | _ -> None
        )

    /// Extract the `open` declarations which are in scope for generated code emitted into
    /// namespace `ns`: the opens of every top-level namespace/module block of the file with
    /// exactly that name.
    ///
    /// Opens from differently-named blocks must not leak in: a relative `open` which is valid in
    /// one namespace block can be invalid (or resolve to something else) in another. Two blocks
    /// with the same name share a resolution context, so it is sound to union their opens.
    ///
    /// This is the right lookup for generators whose type discovery only visits the top level of
    /// each block (Whippet's `Ast.getTypes`): the reported namespace is always exactly a block
    /// name. A generator which descends into nested modules (CataGenerator) must instead track
    /// the opens which are lexically in scope at the point of the type; matching by name cannot
    /// reconstruct that.
    ///
    /// (Note: this deliberately does not share a name with WoofWare.Whippet.Fantomas's
    /// `AstHelper.extractOpens`, which shadows this module's members at generator call sites and
    /// extracts opens from *every* block in the file.)
    let extractOpensForNamespace (ns : LongIdent) (ast : ParsedInput) : SynOpenDeclTarget list =
        let nsName = ns |> List.map _.idText

        match ast with
        | ParsedInput.ImplFile (ParsedImplFileInput (_, _, _, _, _, modules, _, _, _)) ->
            modules
            |> List.collect (fun (SynModuleOrNamespace (longId, _, _, decls, _, _, _, _, _)) ->
                let blockName = longId |> List.map _.idText

                let sameName =
                    List.length blockName = List.length nsName
                    && List.forall2
                        (fun (a : string) (b : string) -> System.String.Equals (a, b, System.StringComparison.Ordinal))
                        blockName
                        nsName

                if sameName then extractOpensFromDecl decls else []
            )
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
