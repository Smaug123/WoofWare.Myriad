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

type internal MemberInfo =
    {
        ReturnType : SynType
        Accessibility : SynAccess option
        /// Each element of this list is a list of args in a tuple, or just one arg if not a tuple.
        Args : ParameterInfo list list
        Identifier : Ident
        Attributes : SynAttribute list
        XmlDoc : PreXmlDoc option
        IsInline : bool
        IsMutable : bool
    }

type internal InterfaceType =
    {
        Attributes : SynAttribute list
        Name : LongIdent
        Members : MemberInfo list
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
        // TODO: consider FSharpList or whatever it is
        | [ i ] ->
            printfn $"Not array: %s{i.idText}"
            false
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

    let rec convertSigParam (ty : SynType) : ParameterInfo =
        match ty with
        | SynType.Paren (inner, _) -> convertSigParam inner
        | SynType.LongIdent ident ->
            {
                Attributes = []
                IsOptional = false
                Id = None
                Type = SynType.CreateLongIdent ident
            }
        | SynType.SignatureParameter (attrs, opt, id, usedType, _) ->
            let attrs = attrs |> List.collect (fun attrs -> attrs.Attributes)

            {
                Attributes = attrs
                IsOptional = opt
                Id = id
                Type = usedType
            }
        | SynType.Var (typar, _) ->
            {
                Attributes = []
                IsOptional = false
                Id = None
                Type = SynType.Var (typar, range0)
            }
        | _ -> failwithf "expected SignatureParameter, got: %+A" ty

    let rec extractTupledTypes (tupleType : SynTupleTypeSegment list) : ParameterInfo list =
        match tupleType with
        | [] -> []
        | [ SynTupleTypeSegment.Type param ] -> [ convertSigParam param ]
        | SynTupleTypeSegment.Type param :: SynTupleTypeSegment.Star _ :: rest ->
            convertSigParam param :: extractTupledTypes rest
        | _ -> failwithf "Didn't have alternating type-and-star in interface member definition: %+A" tupleType

    let toFun (inputs : SynType list) (ret : SynType) : SynType =
        (ret, List.rev inputs)
        ||> List.fold (fun ty input -> SynType.CreateFun (input, ty))

    /// Returns the args (where these are tuple types if curried) in order, and the return type.
    let rec getType (ty : SynType) : SynType list * SynType =
        match ty with
        | SynType.Paren (ty, _) -> getType ty
        | SynType.Fun (argType, returnType, _, _) ->
            let args, ret = getType returnType
            let inputArgs, inputRet = getType argType
            (toFun inputArgs inputRet) :: args, ret
        | _ -> [], ty

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

        let members =
            match synTypeDefnRepr with
            | SynTypeDefnRepr.ObjectModel (_kind, members, _) ->
                members
                |> List.map (fun defn ->
                    match defn with
                    | SynMemberDefn.AbstractSlot (slotSig, flags, _, _) ->
                        match flags.MemberKind with
                        | SynMemberKind.Member -> ()
                        | kind -> failwithf "Unrecognised member kind: %+A" kind

                        if not flags.IsInstance then
                            failwith "member was not an instance member"

                        match slotSig with
                        | SynValSig (attrs,
                                     SynIdent.SynIdent (ident, _),
                                     _typeParams,
                                     synType,
                                     arity,
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

                            let attrs = attrs |> List.collect (fun attr -> attr.Attributes)

                            let args, ret = getType synType

                            let args =
                                args
                                |> List.map (fun args ->
                                    match args with
                                    | SynType.SignatureParameter _ -> [ convertSigParam args ]
                                    | SynType.Tuple (false, path, _) -> extractTupledTypes path
                                    | SynType.LongIdent (SynLongIdent (ident, _, _)) ->
                                        {
                                            Attributes = []
                                            IsOptional = false
                                            Id = None
                                            Type = SynType.CreateLongIdent (SynLongIdent.CreateFromLongIdent ident)
                                        }
                                        |> List.singleton
                                    | SynType.Var (typar, _) ->
                                        {
                                            Attributes = []
                                            IsOptional = false
                                            Id = None
                                            Type = SynType.Var (typar, range0)
                                        }
                                        |> List.singleton
                                    | _ -> failwith $"Unrecognised args in interface method declaration: %+A{args}"
                                )

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
                    | _ -> failwith $"Unrecognised member definition: %+A{defn}"
                )
            | _ -> failwith $"Unrecognised SynTypeDefnRepr for an interface type: %+A{synTypeDefnRepr}"

        {
            Members = members
            Name = interfaceName
            Attributes = attrs
            Generics = typars
            Accessibility = accessibility
        }


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

    /// Returns the string name of the type.
    let (|PrimitiveType|_|) (fieldType : SynType) =
        match fieldType with
        | SynType.LongIdent ident ->
            match ident.LongIdent with
            | [ i ] -> [ "string" ; "float" ; "int" ; "bool" ] |> List.tryFind (fun s -> s = i.idText)
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
