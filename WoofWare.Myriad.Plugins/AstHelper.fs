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
        Arity : SynArgInfo list
        Args : ParameterInfo list
        Identifier : Ident
        Attributes : SynAttribute list
        XmlDoc : PreXmlDoc option
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
        | _ -> failwithf "expected SignatureParameter, got: %+A" ty

    let rec extractTupledTypes (tupleType : SynTupleTypeSegment list) : ParameterInfo list =
        match tupleType with
        | [] -> []
        | [ SynTupleTypeSegment.Type param ] -> [ convertSigParam param ]
        | SynTupleTypeSegment.Type param :: SynTupleTypeSegment.Star _ :: rest ->
            convertSigParam param :: extractTupledTypes rest
        | _ -> failwithf "Didn't have alternating type-and-star in interface member definition: %+A" tupleType

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
                            if isInline then
                                failwith "inline members not supported"

                            if isMutable then
                                failwith "mutable members not supported"

                            match accessibility with
                            | Some (SynAccess.Internal _)
                            | Some (SynAccess.Private _) -> failwith "only public members are supported"
                            | _ -> ()

                            match synExpr with
                            | Some _ -> failwith "literal members are not supported"
                            | None -> ()

                            let arity =
                                match arity with
                                | SynValInfo ([ curriedArgs ], SynArgInfo ([], false, _)) -> curriedArgs
                                | SynValInfo (curriedArgs, SynArgInfo ([], false, _)) ->
                                    failwithf "only tupled arguments are currently supported, but got: %+A" curriedArgs
                                | SynValInfo (_, info) ->
                                    failwithf
                                        "only bare return values like `Task<foo>` are supported, but got: %+A"
                                        info

                            let attrs = attrs |> List.collect (fun attr -> attr.Attributes)

                            let args, ret =
                                match synType with
                                | SynType.Fun (argType, returnType, _, _) -> argType, returnType
                                | _ ->
                                    failwithf
                                        "Expected a return type of a generic Task; bad signature was: %+A"
                                        synType

                            let args =
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
                                | _ -> failwithf "Unrecognised args in interface method declaration: %+A" args

                            {
                                ReturnType = ret
                                Arity = arity
                                Args = args
                                Identifier = ident
                                Attributes = attrs
                                XmlDoc = Some xmlDoc
                            }
                    | _ -> failwithf "Unrecognised member definition: %+A" defn
                )
            | _ -> failwithf "Unrecognised SynTypeDefnRepr for an interface type: %+A" synTypeDefnRepr

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
