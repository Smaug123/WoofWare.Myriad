namespace WoofWare.Myriad.Plugins

open System
open System.Text
open Fantomas.FCS.Syntax
open Fantomas.FCS.Text.Range
open TypeEquality
open WoofWare.Whippet.Fantomas

type internal ArgParserOutputSpec =
    {
        ExtensionMethods : bool
    }

type internal FlagDu =
    {
        Name : Ident
        Case1Name : Ident
        Case2Name : Ident
        /// Hopefully this is simply the const bool True or False, but it might e.g. be a literal
        Case1Arg : SynExpr
        /// Hopefully this is simply the const bool True or False, but it might e.g. be a literal
        Case2Arg : SynExpr
    }

    static member FromBoolean (flagDu : FlagDu) (value : SynExpr) =
        SynExpr.ifThenElse
            (SynExpr.equals value flagDu.Case1Arg)
            (SynExpr.createLongIdent' [ flagDu.Name ; flagDu.Case2Name ])
            (SynExpr.createLongIdent' [ flagDu.Name ; flagDu.Case1Name ])

/// The default value of an argument which admits default values can be pulled from different sources.
/// This defines which source a particular default value comes from.
type private ArgumentDefaultSpec =
    /// From parsing the environment variable with the given name (e.g. "WOOFWARE_DISABLE_FOO" or whatever).
    | EnvironmentVariable of name : SynExpr
    /// From calling the static member `{typeWeParseInto}.Default{name}()`
    /// For example, if `type MyArgs = { Thing : Choice<int, int> }`, then
    /// we would use `MyArgs.DefaultThing () : int`.
    ///
    | FunctionCall of name : Ident

type private Accumulation<'choice> =
    | Required
    | Optional
    | Choice of 'choice
    | List of Accumulation<'choice>

type private ParseFunction<'acc> =
    {
        FieldName : Ident
        TargetVariable : Ident
        /// Any of the forms in this set are acceptable, but make sure they all start with a dash, or we might
        /// get confused with positional args or something! I haven't thought that hard about this.
        /// In the default case, this is `Const("arg-name")` for the `ArgName : blah` field; note that we have
        /// omitted the initial `--` that will be required at runtime.
        ArgForm : SynExpr list
        /// If this is a boolean-like field (e.g. a bool or a flag DU), the help text should look a bit different:
        /// we should lie to the user about the value of the cases there.
        /// Similarly, if we're reading from an environment variable with the laxer parsing rules of accepting e.g.
        /// "0" instead of "false", we need to know if we're reading a bool.
        /// In that case, `boolCases` is Some, and contains the construction of the flag (or boolean, in which case
        /// you get no data).
        BoolCases : Choice<FlagDu, unit> option
        Help : SynExpr option
        /// A function string -> %TargetType%, where TargetVariable is probably a `%TargetType% option`.
        /// (Depending on `Accumulation`, we'll remove the `option` at the end of the parse, asserting that the
        /// argument was supplied.)
        /// This is allowed to throw if it fails to parse.
        Parser : SynExpr
        /// If `Accumulation` is `List`, then this is the type of the list *element*; analogously for optionals
        /// and choices and so on.
        TargetType : SynType
        Accumulation : 'acc
    }

    /// A SynExpr of type `string` which we can display to the user at generated-program runtime to display all
    /// the ways they can refer to this arg.
    member arg.HumanReadableArgForm : SynExpr =
        let formatString = List.replicate arg.ArgForm.Length "--%s" |> String.concat " / "

        (SynExpr.applyFunction (SynExpr.createIdent "sprintf") (SynExpr.CreateConst formatString), arg.ArgForm)
        ||> List.fold SynExpr.applyFunction
        |> SynExpr.paren


module internal ShibaGenerator =
    open SynTypePatterns

    type RecognisedType =
        | Union of UnionType
        | Record of RecordType

        member this.Name : Ident =
            match this with
            | Union unionType -> unionType.Name
            | Record recordType -> recordType.Name

    /// Some types don't have in-progress equivalents (e.g. a no-data DU, which is "basically primitive");
    /// hence the `option`.
    let createInProgressRecognisedType
        (flagDuNames : string list)
        (allKnownTypeIdents : string list)
        (ty : RecognisedType)
        : RecordType option
        =
        let getInProgressTypeName (ty : LongIdent) : SynType =
            // TODO: this is super jank
            let ident = List.last ty

            if List.contains ident.idText flagDuNames then
                // Flag DUs have no in-progress form as such
                SynType.createLongIdent ty |> SynType.option
            elif List.contains ident.idText allKnownTypeIdents then
                SynType.createLongIdent [ ident.idText + "_InProgress" |> Ident.create ]
            else
                // TODO: this is just nonsense, probably
                SynType.createLongIdent ty |> SynType.option

        let makeType (attrs : SynAttribute list) (ty : SynType) (id : Ident) : SynField option =
            match ty with
            | ChoiceType [ left ; right ] ->
                if not (SynType.provablyEqual left right) then
                    failwith
                        $"ArgParser was unable to prove types %O{left} and %O{right} to be equal in a Choice. We require them to be equal."

                {
                    Attrs = []
                    Ident = Some id
                    Type = SynType.option left
                }
                |> SynField.make
                |> Some
            | ChoiceType _ ->
                failwith
                    $"Only `Choice`s with exactly two args are supported, and they must have the same type on each side (field name: %s{id.idText})"
            | ListType contents ->
                // TODO: jank conditional
                if
                    attrs
                    |> List.exists (fun x -> List.last(x.TypeName.LongIdent).idText.StartsWith "PositionalArgs")
                then
                    // Omit positional args, they are treated in the Finalise
                    None
                else

                {
                    Attrs = []
                    Ident = Some id
                    Type =
                        // Parser will take strings later, when finalising
                        SynType.list SynType.string
                }
                |> SynField.make
                |> Some
            | PrimitiveType ty ->
                {
                    Attrs = []
                    Ident = Some id
                    Type = SynType.option (SynType.createLongIdent ty)
                }
                |> SynField.make
                |> Some
            | OptionType ty ->
                {
                    Attrs = []
                    Ident = Some id
                    Type =
                        // an `option` is its own in-progress
                        SynType.option ty
                }
                |> SynField.make
                |> Some
            | SynType.LongIdent (SynLongIdent.SynLongIdent (ident, _, _)) ->
                // Assume this is in-progress
                {
                    Attrs = []
                    Ident = Some id
                    Type = getInProgressTypeName ident
                }
                |> SynField.make
                |> Some
            | ty -> failwith $"TODO: %O{ty}"

        match ty with
        | RecognisedType.Union union ->
            if union.Cases |> List.forall (fun case -> case.Fields.IsEmpty) then
                None
            else

            {
                Name = union.Name.idText + "_InProgress" |> Ident.create
                XmlDoc = PreXmlDoc.create $"A partially-parsed %s{union.Name.idText}." |> Some
                Members =
                    SynExpr.CreateConst "TODO: now construct the object"
                    |> SynBinding.basic
                        [ Ident.create "this" ; Ident.create "Assemble" ]
                        [
                            SynPat.annotateType (SynType.list SynType.string) (SynPat.named "positionals")
                        ]
                    |> SynBinding.withReturnAnnotation (SynType.createLongIdent [ union.Name ])
                    |> SynMemberDefn.memberImplementation
                    |> List.singleton
                    |> Some
                Fields =
                    union.Cases
                    |> List.mapi (fun i data -> i, data)
                    |> List.choose (fun (caseNum, case) ->
                        match case.Fields with
                        | [] ->
                            failwith
                                $"Union type %s{union.Name.idText} has case %s{case.Name.idText} with no data; we require all cases to have exactly one field, or else all cases to be empty."
                        | [ x ] -> makeType x.Attrs x.Type (Ident.create $"Case_%i{caseNum}")
                        | _ ->
                            failwith
                                $"Union type %s{union.Name.idText} has case %s{case.Name.idText} with multiple fields; we require all cases to have exactly one field, or else all cases to be empty. Define a record type to hold the contents."
                    )
                    |> fun l ->
                        if l.IsEmpty then
                            [
                                SynField.make
                                    {
                                        Attrs = []
                                        Ident = Some (Ident.create "_Dummy")
                                        Type = SynType.unit
                                    }
                            ]
                        else
                            l
                Generics =
                    match union.Generics with
                    | None -> None
                    | Some _ -> failwith $"Union type %s{union.Name.idText} had generics, which we don't support."
                TypeAccessibility = Some (SynAccess.Private range0)
                ImplAccessibility = None
                Attributes = []
            }
            |> Some
        | RecognisedType.Record record ->
            {
                Name = record.Name.idText + "_InProgress" |> Ident.create
                Fields =
                    record.Fields
                    |> List.choose (fun (SynField.SynField (attrs, _, id, ty, _, _, _, _, _)) ->
                        match id with
                        | None ->
                            failwith $"expected field in record %s{record.Name.idText} to have a name, but it did not"
                        | Some id -> makeType (SynAttributes.toAttrs attrs) ty id
                    )
                    |> fun l ->
                        if l.IsEmpty then
                            [
                                SynField.make
                                    {
                                        Attrs = []
                                        Ident = Some (Ident.create "_Dummy")
                                        Type = SynType.unit
                                    }
                            ]
                        else
                            l
                Members =
                    SynExpr.CreateConst "TODO: now construct the object"
                    |> SynBinding.basic
                        [ Ident.create "this" ; Ident.create "Assemble" ]
                        [
                            SynPat.annotateType (SynType.list SynType.string) (SynPat.named "positionals")
                        ]
                    |> SynBinding.withReturnAnnotation (SynType.createLongIdent [ record.Name ])
                    |> SynMemberDefn.memberImplementation
                    |> List.singleton
                    |> Some
                XmlDoc = PreXmlDoc.create $"A partially-parsed %s{record.Name.idText}." |> Some
                Generics =
                    match record.Generics with
                    | None -> None
                    | Some _ -> failwith $"Record type %s{record.Name.idText} had generics, which we don't support."
                TypeAccessibility = Some (SynAccess.Private range0)
                ImplAccessibility = None
                Attributes = []
            }
            |> Some

    let createHelpersModule
        (opens : SynOpenDeclTarget list)
        (ns : LongIdent)
        (allUnionTypes : UnionType list)
        (allRecordTypes : RecordType list)
        : SynModuleDecl
        =
        let flagDus =
            allUnionTypes
            |> List.choose (fun ty ->
                match ty.Cases with
                | [ c1 ; c2 ] ->
                    let c1Attr =
                        c1.Attributes
                        |> List.tryPick (fun attr ->
                            match attr.TypeName with
                            | SynLongIdent.SynLongIdent (id, _, _) ->
                                match id |> List.last |> _.idText with
                                | "ArgumentFlagAttribute"
                                | "ArgumentFlag" -> Some (SynExpr.stripOptionalParen attr.ArgExpr)
                                | _ -> None
                        )

                    let c2Attr =
                        c2.Attributes
                        |> List.tryPick (fun attr ->
                            match attr.TypeName with
                            | SynLongIdent.SynLongIdent (id, _, _) ->
                                match id |> List.last |> _.idText with
                                | "ArgumentFlagAttribute"
                                | "ArgumentFlag" -> Some (SynExpr.stripOptionalParen attr.ArgExpr)
                                | _ -> None
                        )

                    match c1Attr, c2Attr with
                    | Some _, None
                    | None, Some _ ->
                        failwith
                            "[<ArgumentFlag>] must be placed on both cases of a two-case discriminated union, with opposite argument values on each case."
                    | None, None -> None
                    | Some c1Attr, Some c2Attr ->

                    // Sanity check where possible
                    match c1Attr, c2Attr with
                    | SynExpr.Const (SynConst.Bool b1, _), SynExpr.Const (SynConst.Bool b2, _) ->
                        if b1 = b2 then
                            failwith
                                "[<ArgumentFlag>] must have opposite argument values on each case in a two-case discriminated union."
                    | _, _ -> ()

                    match c1.Fields, c2.Fields with
                    | [], [] ->
                        {
                            Name = ty.Name
                            Case1Name = c1.Name
                            Case1Arg = c1Attr
                            Case2Name = c2.Name
                            Case2Arg = c2Attr
                        }
                        |> Some
                    | _, _ ->
                        failwith "[<ArgumentFlag>] may only be placed on discriminated union members with no data."
                | _ -> None
            )

        let modName =
            let ns = ns |> List.map _.idText |> String.concat "_"
            Ident.create $"ArgParseHelpers_%s{ns}"

        let modInfo =
            SynComponentInfo.create modName
            |> SynComponentInfo.withAccessibility (SynAccess.Private range0)
            |> SynComponentInfo.withDocString (PreXmlDoc.create $"Helper types for arg parsing")

        let allKnownTypeIdents =
            let uts = allUnionTypes |> List.map _.Name.idText
            let rts = allRecordTypes |> List.map _.Name.idText
            uts @ rts

        let flagDuNames = flagDus |> List.map _.Name.idText

        let reducedRecordTypes =
            allRecordTypes
            |> List.choose (fun rt ->
                // TODO: just split these into different functions and get rid of RecognisedType
                createInProgressRecognisedType flagDuNames allKnownTypeIdents (RecognisedType.Record rt)
                |> Option.map RecordType.ToAst
            )

        let reducedUnionTypes =
            allUnionTypes
            |> List.choose (fun ut ->
                // TODO: just split these into different functions and get rid of RecognisedType
                createInProgressRecognisedType flagDuNames allKnownTypeIdents (RecognisedType.Union ut)
                |> Option.map RecordType.ToAst
            )

        let taggedMod =
            [
                for openStatement in opens do
                    yield SynModuleDecl.openAny openStatement
                yield SynModuleDecl.openAny (SynOpenDeclTarget.ModuleOrNamespace (SynLongIdent.create ns, range0))

                yield (reducedRecordTypes @ reducedUnionTypes) |> SynModuleDecl.createTypes
            ]
            |> SynModuleDecl.nestedModule modInfo

        taggedMod

    // The type for which we're generating args may refer to any of the supplied records/unions.
    let createModule
        (opens : SynOpenDeclTarget list)
        (ns : LongIdent)
        ((taggedType : SynTypeDefn, spec : ArgParserOutputSpec))
        (allUnionTypes : UnionType list)
        (allRecordTypes : RecordType list)
        : SynModuleOrNamespace
        =
        let taggedType =
            match taggedType with
            | SynTypeDefn.SynTypeDefn (sci,
                                       SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.Record (access, fields, _), _),
                                       smd,
                                       _,
                                       _,
                                       _) -> RecordType.OfRecord sci smd access fields
            | _ -> failwith "[<ArgParser>] currently only supports being placed on records."

        let modAttrs, modName =
            if spec.ExtensionMethods then
                [ SynAttribute.autoOpen ], Ident.create (taggedType.Name.idText + "ArgParse")
            else
                [ SynAttribute.requireQualifiedAccess ; SynAttribute.compilationRepresentation ], taggedType.Name

        let modInfo =
            SynComponentInfo.create modName
            |> SynComponentInfo.withDocString (
                PreXmlDoc.create $"Methods to parse arguments for the type %s{taggedType.Name.idText}"
            )
            |> SynComponentInfo.addAttributes modAttrs

        let parseStateIdent = Ident.create $"ParseState_%s{taggedType.Name.idText}"

        let parseStateType =
            [
                SynUnionCase.create
                    {
                        Attributes = []
                        Fields = []
                        Name = Ident.create "AwaitingKey"
                        XmlDoc = Some (PreXmlDoc.create "Ready to consume a key or positional arg")
                        Access = None
                    }
                SynUnionCase.create
                    {
                        Attributes = []
                        Fields =
                            [
                                {
                                    Attrs = []
                                    Ident = Some (Ident.create "key")
                                    Type = SynType.string
                                }
                            ]
                        Name = Ident.create "AwaitingValue"
                        XmlDoc = Some (PreXmlDoc.create "Waiting to receive a value for the key we've already consumed")
                        Access = None
                    }
            ]
            |> SynTypeDefnRepr.union
            |> SynTypeDefn.create (
                SynComponentInfo.create parseStateIdent
                |> SynComponentInfo.setAccessibility (Some (SynAccess.Private range0))
            )
            |> List.singleton
            |> SynModuleDecl.createTypes

        let taggedMod =
            let argsParam =
                SynPat.named "args"
                |> SynPat.annotateType (SynType.appPostfix "list" SynType.string)

            let parsePrime =
                SynExpr.CreateConst "todo"
                |> SynExpr.applyFunction (SynExpr.createIdent "failwith")
                |> SynBinding.basic
                    [ Ident.create "parse'" ]
                    [
                        SynPat.named "getEnvironmentVariable"
                        |> SynPat.annotateType (SynType.funFromDomain SynType.string SynType.string)
                        argsParam
                    ]
                |> SynBinding.withReturnAnnotation (SynType.createLongIdent [ taggedType.Name ])

            let parsePrimeCall =
                if spec.ExtensionMethods then
                    // need to fully qualify
                    [ taggedType.Name ; Ident.create "parse'" ]
                else
                    [ Ident.create "parse'" ]

            let parse =
                SynExpr.createLongIdent' parsePrimeCall
                |> SynExpr.applyTo (SynExpr.createLongIdent [ "System" ; "Environment" ; "GetEnvironmentVariable" ])
                |> SynExpr.applyTo (SynExpr.createIdent "args")
                |> SynBinding.basic [ Ident.create "parse" ] [ argsParam ]
                |> SynBinding.withReturnAnnotation (SynType.createLongIdent [ taggedType.Name ])

            [
                yield parseStateType

                if spec.ExtensionMethods then
                    let bindingPrime = parsePrime |> SynMemberDefn.staticMember

                    let binding = parse |> SynMemberDefn.staticMember

                    let componentInfo =
                        SynComponentInfo.create taggedType.Name
                        |> SynComponentInfo.withDocString (PreXmlDoc.create "Extension methods for argument parsing")

                    let containingType =
                        SynTypeDefnRepr.augmentation ()
                        |> SynTypeDefn.create componentInfo
                        |> SynTypeDefn.withMemberDefns [ bindingPrime ; binding ]

                    yield SynModuleDecl.createTypes [ containingType ]
                else
                    yield SynModuleDecl.createLet parsePrime

                    yield SynModuleDecl.createLet parse
            ]
            |> SynModuleDecl.nestedModule modInfo

        [
            for openStatement in opens do
                yield SynModuleDecl.openAny openStatement
            yield taggedMod
        ]
        |> SynModuleOrNamespace.createNamespace ns

open Myriad.Core

/// Myriad generator that provides a catamorphism for an algebraic data type.
[<MyriadGenerator("arg-parser")>]
type ArgParserGenerator () =

    interface IMyriadGenerator with
        member _.ValidInputExtensions = [ ".fs" ]

        member _.Generate (context : GeneratorContext) =
            let ast, _ =
                Ast.fromFilename context.InputFilename |> Async.RunSynchronously |> Array.head

            let types =
                // Bug in WoofWare.Whippet, probably: we return types in the wrong order
                Ast.getTypes ast |> List.map (fun (ns, types) -> ns, List.rev types)

            let opens = AstHelper.extractOpens ast

            let namespaceAndTypes =
                types
                |> List.collect (fun (ns, types) ->
                    let typeWithAttr =
                        types
                        |> List.choose (fun ty ->
                            match SynTypeDefn.getAttribute typeof<ArgParserAttribute>.Name ty with
                            | None -> None
                            | Some attr ->
                                let arg =
                                    match SynExpr.stripOptionalParen attr.ArgExpr with
                                    | SynExpr.Const (SynConst.Bool value, _) -> value
                                    | SynExpr.Const (SynConst.Unit, _) -> ArgParserAttribute.DefaultIsExtensionMethod
                                    | arg ->
                                        failwith
                                            $"Unrecognised argument %+A{arg} to [<%s{nameof ArgParserAttribute}>]. Literals are not supported. Use `true` or `false` (or unit) only."

                                let spec =
                                    {
                                        ExtensionMethods = arg
                                    }

                                Some (ty, spec)
                        )

                    typeWithAttr
                    |> List.map (fun taggedType ->
                        let unions, records, others =
                            (([], [], []), types)
                            ||> List.fold (fun
                                               (unions, records, others)
                                               (SynTypeDefn.SynTypeDefn (sci, repr, smd, _, _, _) as ty) ->
                                match repr with
                                | SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.Union (access, cases, _), _) ->
                                    UnionType.OfUnion sci smd access cases :: unions, records, others
                                | SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.Record (access, fields, _), _) ->
                                    unions, RecordType.OfRecord sci smd access fields :: records, others
                                | _ -> unions, records, ty :: others
                            )

                        if not others.IsEmpty then
                            failwith
                                $"Error: all types recursively defined together with an ArgParserGenerator type must be discriminated unions or records. %+A{others}"

                        (ns, taggedType, unions, records)
                    )
                )

            let unionsAndRecordsByNs =
                (Map.empty, namespaceAndTypes)
                ||> List.fold (fun types (ns, _, unions, records) ->
                    let nsKey = ns |> List.map _.idText |> String.concat "."

                    types
                    |> Map.change
                        nsKey
                        (fun v ->
                            match v with
                            | None -> Some (unions, records)
                            | Some (u, r) -> Some (unions @ u, records @ r)
                        )
                )

            let helpersMod =
                unionsAndRecordsByNs
                |> Map.toSeq
                |> Seq.map (fun (ns, (unions, records)) ->
                    let unions = unions |> List.distinctBy (fun u -> u.Name.idText)
                    let records = records |> List.distinctBy (fun r -> r.Name.idText)

                    ShibaGenerator.createHelpersModule
                        opens
                        (ns.Split '.' |> Seq.map Ident.create |> List.ofSeq)
                        unions
                        records
                )
                |> Seq.toList
                |> fun l -> [ yield! l ]
                |> SynModuleOrNamespace.createNamespace [ Ident.create "ArgParserHelpers" ]

            let modules =
                namespaceAndTypes
                |> List.map (fun (ns, taggedType, unions, records) ->
                    ShibaGenerator.createModule opens ns taggedType unions records
                )

            Output.Ast (helpersMod :: modules)
