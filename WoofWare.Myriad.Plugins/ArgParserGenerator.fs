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

[<RequireQualifiedAccess>]
type private ChoicePositional =
    | Normal of includeFlagLike : SynExpr option
    | Choice of includeFlagLike : SynExpr option

type private ParseFunctionPositional = ParseFunction<ChoicePositional>
type private ParseFunctionNonPositional = ParseFunction<Accumulation<ArgumentDefaultSpec>>

type private ParserSpec =
    {
        NonPositionals : ParseFunctionNonPositional list
        /// The variable into which positional arguments will be accumulated.
        /// In this case, the TargetVariable is a `ResizeArray` rather than the usual `option`.
        Positionals : ParseFunctionPositional option
    }

type private HasPositional = HasPositional
type private HasNoPositional = HasNoPositional

[<AutoOpen>]
module private TeqUtils =
    let exFalso<'a> (_ : Teq<HasNoPositional, HasPositional>) : 'a = failwith "LOGIC ERROR!"
    let exFalso'<'a> (_ : Teq<HasPositional, HasNoPositional>) : 'a = failwith "LOGIC ERROR!"

[<RequireQualifiedAccess>]
type private ParseTree<'hasPositional> =
    | NonPositionalLeaf of ParseFunctionNonPositional * Teq<'hasPositional, HasNoPositional>
    | PositionalLeaf of ParseFunctionPositional * Teq<'hasPositional, HasPositional>
    /// `assemble` takes the SynExpr's (e.g. each record field contents) corresponding to each `Ident` in
    /// the branch (e.g. each record field name),
    /// and composes them into a `SynExpr` (e.g. the record-typed object).
    | Branch of
        fields : (Ident * ParseTree<HasNoPositional>) list *
        assemble : (Map<string, SynExpr> -> SynExpr) *
        Teq<'hasPositional, HasNoPositional>
    /// `assemble` takes the SynExpr's (e.g. each record field contents) corresponding to each `Ident` in
    /// the branch (e.g. each record field name),
    /// and composes them into a `SynExpr` (e.g. the record-typed object).
    | BranchPos of
        posField : Ident *
        fields : ParseTree<HasPositional> *
        (Ident * ParseTree<HasNoPositional>) list *
        assemble : (Map<string, SynExpr> -> SynExpr) *
        Teq<'hasPositional, HasPositional>

type private ParseTreeEval<'ret> =
    abstract Eval<'a> : ParseTree<'a> -> 'ret

type private ParseTreeCrate =
    abstract Apply<'ret> : ParseTreeEval<'ret> -> 'ret

[<RequireQualifiedAccess>]
module private ParseTreeCrate =
    let make<'a> (p : ParseTree<'a>) =
        { new ParseTreeCrate with
            member _.Apply a = a.Eval p
        }

[<RequireQualifiedAccess>]
module private ParseTree =
    [<RequireQualifiedAccess>]
    type State =
        | Positional of ParseTree<HasPositional> * ParseTree<HasNoPositional> list
        | NoPositional of ParseTree<HasNoPositional> list

    let private cast (t : Teq<'a, 'b>) : Teq<ParseTree<'a>, ParseTree<'b>> = Teq.Cong.believeMe t

    /// The `Ident` here is the field name.
    let branch (assemble : Map<string, SynExpr> -> SynExpr) (subs : (Ident * ParseTreeCrate) list) : ParseTreeCrate =
        let rec go
            (selfIdent : Ident option)
            (acc : (Ident * ParseTree<HasNoPositional>) list, pos : (Ident * ParseTree<HasPositional>) option)
            (subs : (Ident * ParseTreeCrate) list)
            : ParseTreeCrate
            =
            match subs with
            | [] ->
                match pos with
                | None -> ParseTree.Branch (List.rev acc, assemble, Teq.refl) |> ParseTreeCrate.make
                | Some (posField, pos) ->
                    ParseTree.BranchPos (posField, pos, List.rev acc, assemble, Teq.refl)
                    |> ParseTreeCrate.make
            | (fieldName, sub) :: subs ->
                { new ParseTreeEval<_> with
                    member _.Eval (t : ParseTree<'a>) =
                        match t with
                        | ParseTree.NonPositionalLeaf (_, teq)
                        | ParseTree.Branch (_, _, teq) ->
                            go selfIdent (((fieldName, Teq.cast (cast teq) t) :: acc), pos) subs
                        | ParseTree.PositionalLeaf (_, teq)
                        | ParseTree.BranchPos (_, _, _, _, teq) ->
                            match pos with
                            | None -> go selfIdent (acc, Some (fieldName, Teq.cast (cast teq) t)) subs
                            | Some (ident, _) ->
                                failwith
                                    $"Multiple entries tried to claim positional args! %s{ident.idText} and %s{fieldName.idText}"
                }
                |> sub.Apply

        go None ([], None) subs

    let rec accumulatorsNonPos (tree : ParseTree<HasNoPositional>) : ParseFunctionNonPositional list =
        match tree with
        | ParseTree.PositionalLeaf (_, teq) -> exFalso teq
        | ParseTree.BranchPos (_, _, _, _, teq) -> exFalso teq
        | ParseTree.NonPositionalLeaf (pf, _) -> [ pf ]
        | ParseTree.Branch (trees, _, _) -> trees |> List.collect (snd >> accumulatorsNonPos)

    /// Returns the positional arg separately.
    let rec accumulatorsPos
        (tree : ParseTree<HasPositional>)
        : ParseFunctionNonPositional list * ParseFunctionPositional
        =
        match tree with
        | ParseTree.PositionalLeaf (pf, _) -> [], pf
        | ParseTree.NonPositionalLeaf (_, teq) -> exFalso' teq
        | ParseTree.Branch (_, _, teq) -> exFalso' teq
        | ParseTree.BranchPos (_, tree, trees, _, _) ->
            let nonPos = trees |> List.collect (snd >> accumulatorsNonPos)

            let nonPos2, pos = accumulatorsPos tree
            nonPos @ nonPos2, pos

    /// Collect all the ParseFunctions which are necessary to define variables, throwing away
    /// all information relevant to composing the resulting variables into records.
    /// Returns the list of non-positional parsers, and any positional parser that exists.
    let accumulators<'a> (tree : ParseTree<'a>) : ParseFunctionNonPositional list * ParseFunctionPositional option =
        // Sad duplication of some code here, but it was the easiest way to make it type-safe :(
        match tree with
        | ParseTree.PositionalLeaf (pf, _) -> [], Some pf
        | ParseTree.NonPositionalLeaf (pf, _) -> [ pf ], None
        | ParseTree.Branch (trees, _, _) -> trees |> List.collect (snd >> accumulatorsNonPos) |> (fun i -> i, None)
        | ParseTree.BranchPos (_, tree, trees, _, _) ->
            let nonPos = trees |> List.collect (snd >> accumulatorsNonPos)

            let nonPos2, pos = accumulatorsPos tree
            nonPos @ nonPos2, Some pos

        |> fun (nonPos, pos) ->
            let duplicateArgs =
                // This is best-effort. We can't necessarily detect all SynExprs here, but usually it'll be strings.
                Option.toList (pos |> Option.map _.ArgForm) @ (nonPos |> List.map _.ArgForm)
                |> Seq.concat
                |> Seq.choose (fun expr ->
                    match expr |> SynExpr.stripOptionalParen with
                    | SynExpr.Const (SynConst.String (s, _, _), _) -> Some s
                    | _ -> None
                )
                |> List.ofSeq
                |> List.groupBy id
                |> List.choose (fun (key, v) -> if v.Length > 1 then Some key else None)

            match duplicateArgs with
            | [] -> nonPos, pos
            | dups ->
                let dups = dups |> String.concat " "
                failwith $"Duplicate args detected! %s{dups}"

    /// Build the return value.
    let rec instantiate<'a> (tree : ParseTree<'a>) : SynExpr =
        match tree with
        | ParseTree.NonPositionalLeaf (pf, _) -> SynExpr.createIdent' pf.TargetVariable
        | ParseTree.PositionalLeaf (pf, _) -> SynExpr.createIdent' pf.TargetVariable
        | ParseTree.Branch (trees, assemble, _) ->
            trees
            |> List.map (fun (fieldName, contents) ->
                let instantiated = instantiate contents
                fieldName.idText, instantiated
            )
            |> Map.ofList
            |> assemble
        | ParseTree.BranchPos (posField, tree, trees, assemble, _) ->
            let withPos = instantiate tree

            trees
            |> List.map (fun (fieldName, contents) ->
                let instantiated = instantiate contents
                fieldName.idText, instantiated
            )
            |> Map.ofList
            |> Map.add posField.idText withPos
            |> assemble

[<RequireQualifiedAccess>]
module internal ArgParserGenerator =

    /// Convert e.g. "Foo" into "--foo".
    let argify (ident : Ident) : string =
        let result = StringBuilder ()

        for c in ident.idText do
            if Char.IsUpper c then
                result.Append('-').Append (Char.ToLowerInvariant c) |> ignore<StringBuilder>
            else
                result.Append c |> ignore<StringBuilder>

        result.ToString().TrimStart '-'

    let private identifyAsFlag (flagDus : FlagDu list) (ty : SynType) : FlagDu option =
        match ty with
        | SynType.LongIdent (SynLongIdent.SynLongIdent (ident, _, _)) ->
            flagDus
            |> List.tryPick (fun du ->
                let duName = du.Name.idText
                let ident = List.last(ident).idText
                if duName = ident then Some du else None
            )
        | _ -> None

    /// Builds a function or lambda of one string argument, which returns a `ty` (as modified by the `Accumulation`;
    /// for example, maybe it returns a `ty option` or a `ty list`).
    /// The resulting SynType is the type of the *element* being parsed; so if the Accumulation is List, the SynType
    /// is the list element.
    let rec private createParseFunction<'choice>
        (choice : ArgumentDefaultSpec option -> 'choice)
        (flagDus : FlagDu list)
        (fieldName : Ident)
        (attrs : SynAttribute list)
        (ty : SynType)
        : SynExpr * Accumulation<'choice> * SynType
        =
        match ty with
        | String -> SynExpr.createLambda "x" (SynExpr.createIdent "x"), Accumulation.Required, SynType.string
        | PrimitiveType pt ->
            SynExpr.createLambda
                "x"
                (SynExpr.applyFunction
                    (SynExpr.createLongIdent' (pt @ [ Ident.create "Parse" ]))
                    (SynExpr.createIdent "x")),
            Accumulation.Required,
            ty
        | Uri ->
            SynExpr.createLambda
                "x"
                (SynExpr.applyFunction (SynExpr.createLongIdent [ "System" ; "Uri" ]) (SynExpr.createIdent "x")),
            Accumulation.Required,
            ty
        | TimeSpan ->
            let parseExact =
                attrs
                |> List.tryPick (fun attr ->
                    match attr.TypeName with
                    | SynLongIdent.SynLongIdent (idents, _, _) ->
                        match idents |> List.map (fun i -> i.idText) |> List.tryLast with
                        | Some "ParseExactAttribute"
                        | Some "ParseExact" -> Some attr.ArgExpr
                        | _ -> None
                )

            let culture =
                attrs
                |> List.tryPick (fun attr ->
                    match attr.TypeName with
                    | SynLongIdent.SynLongIdent (idents, _, _) ->
                        match idents |> List.map (fun i -> i.idText) |> List.tryLast with
                        | Some "InvariantCultureAttribute"
                        | Some "InvariantCulture" -> Some ()
                        | _ -> None
                )

            let parser =
                match parseExact, culture with
                | None, None ->
                    SynExpr.createIdent "x"
                    |> SynExpr.applyFunction (SynExpr.createLongIdent [ "System" ; "TimeSpan" ; "Parse" ])
                | Some format, None ->
                    [
                        SynExpr.createIdent "x"
                        format
                        SynExpr.createLongIdent [ "System" ; "Globalization" ; "CultureInfo" ; "CurrentCulture" ]
                    ]
                    |> SynExpr.tuple
                    |> SynExpr.applyFunction (SynExpr.createLongIdent [ "System" ; "TimeSpan" ; "ParseExact" ])
                | None, Some () ->
                    [
                        SynExpr.createIdent "x"
                        SynExpr.createLongIdent [ "System" ; "Globalization" ; "CultureInfo" ; "InvariantCulture" ]
                    ]
                    |> SynExpr.tuple
                    |> SynExpr.applyFunction (SynExpr.createLongIdent [ "System" ; "TimeSpan" ; "Parse" ])
                | Some format, Some () ->
                    [
                        SynExpr.createIdent "x"
                        format
                        SynExpr.createLongIdent [ "System" ; "Globalization" ; "CultureInfo" ; "InvariantCulture" ]
                    ]
                    |> SynExpr.tuple
                    |> SynExpr.applyFunction (SynExpr.createLongIdent [ "System" ; "TimeSpan" ; "ParseExact" ])
                |> SynExpr.createLambda "x"

            parser, Accumulation.Required, ty
        | FileInfo ->
            SynExpr.createLambda
                "x"
                (SynExpr.applyFunction
                    (SynExpr.createLongIdent [ "System" ; "IO" ; "FileInfo" ])
                    (SynExpr.createIdent "x")),
            Accumulation.Required,
            ty
        | DirectoryInfo ->
            SynExpr.createLambda
                "x"
                (SynExpr.applyFunction
                    (SynExpr.createLongIdent [ "System" ; "IO" ; "DirectoryInfo" ])
                    (SynExpr.createIdent "x")),
            Accumulation.Required,
            ty
        | OptionType eltTy ->
            let parseElt, acc, childTy =
                createParseFunction choice flagDus fieldName attrs eltTy

            match acc with
            | Accumulation.Optional ->
                failwith
                    $"ArgParser does not support optionals containing options at field %s{fieldName.idText}: %O{ty}"
            | Accumulation.Choice _ ->
                failwith
                    $"ArgParser does not support optionals containing choices at field %s{fieldName.idText}: %O{ty}"
            | Accumulation.List _ ->
                failwith $"ArgParser does not support optional lists at field %s{fieldName.idText}: %O{ty}"
            | Accumulation.Required -> parseElt, Accumulation.Optional, childTy
        | ChoiceType elts ->
            match elts with
            | [ elt1 ; elt2 ] ->
                if not (SynType.provablyEqual elt1 elt2) then
                    failwith
                        $"ArgParser was unable to prove types %O{elt1} and %O{elt2} to be equal in a Choice. We require them to be equal."

                let parseElt, acc, childTy = createParseFunction choice flagDus fieldName attrs elt1

                match acc with
                | Accumulation.Optional ->
                    failwith
                        $"ArgParser does not support choices containing options at field %s{fieldName.idText}: %O{ty}"
                | Accumulation.List _ ->
                    failwith
                        $"ArgParser does not support choices containing lists at field %s{fieldName.idText}: %O{ty}"
                | Accumulation.Choice _ ->
                    failwith
                        $"ArgParser does not support choices containing choices at field %s{fieldName.idText}: %O{ty}"
                | Accumulation.Required ->

                let relevantAttrs =
                    attrs
                    |> List.choose (fun attr ->
                        let (SynLongIdent.SynLongIdent (name, _, _)) = attr.TypeName

                        match name |> List.map _.idText with
                        | [ "ArgumentDefaultFunction" ]
                        | [ "ArgumentDefaultFunctionAttribute" ]
                        | [ "Plugins" ; "ArgumentDefaultFunction" ]
                        | [ "Plugins" ; "ArgumentDefaultFunctionAttribute" ]
                        | [ "Myriad" ; "Plugins" ; "ArgumentDefaultFunction" ]
                        | [ "Myriad" ; "Plugins" ; "ArgumentDefaultFunctionAttribute" ]
                        | [ "WoofWare" ; "Myriad" ; "Plugins" ; "ArgumentDefaultFunction" ]
                        | [ "WoofWare" ; "Myriad" ; "Plugins" ; "ArgumentDefaultFunctionAttribute" ] ->
                            ArgumentDefaultSpec.FunctionCall (Ident.create ("Default" + fieldName.idText))
                            |> Some
                        | [ "ArgumentDefaultEnvironmentVariable" ]
                        | [ "ArgumentDefaultEnvironmentVariableAttribute" ]
                        | [ "Plugins" ; "ArgumentDefaultEnvironmentVariable" ]
                        | [ "Plugins" ; "ArgumentDefaultEnvironmentVariableAttribute" ]
                        | [ "Myriad" ; "Plugins" ; "ArgumentDefaultEnvironmentVariable" ]
                        | [ "Myriad" ; "Plugins" ; "ArgumentDefaultEnvironmentVariableAttribute" ]
                        | [ "WoofWare" ; "Myriad" ; "Plugins" ; "ArgumentDefaultEnvironmentVariable" ]
                        | [ "WoofWare" ; "Myriad" ; "Plugins" ; "ArgumentDefaultEnvironmentVariableAttribute" ] ->

                            ArgumentDefaultSpec.EnvironmentVariable attr.ArgExpr |> Some
                        | _ -> None
                    )

                let relevantAttr =
                    match relevantAttrs with
                    | [] -> None
                    | [ x ] -> Some x
                    | _ ->
                        failwith
                            $"Expected Choice to be annotated with at most one ArgumentDefaultFunction or similar, but it was annotated with multiple. Field: %s{fieldName.idText}"

                parseElt, Accumulation.Choice (choice relevantAttr), childTy
            | elts ->
                let elts = elts |> List.map string<SynType> |> String.concat ", "

                failwith
                    $"ArgParser requires Choice to be of the form Choice<'a, 'a>; that is, two arguments, both the same. For field %s{fieldName.idText}, got: %s{elts}"
        | ListType eltTy ->
            let parseElt, acc, childTy =
                createParseFunction choice flagDus fieldName attrs eltTy

            parseElt, Accumulation.List acc, childTy
        | ty ->
            match identifyAsFlag flagDus ty with
            | None -> failwith $"Could not decide how to parse arguments for field %s{fieldName.idText} of type %O{ty}"
            | Some flagDu ->
                // Parse as a bool, and then do the `if-then` dance.
                let parser =
                    SynExpr.createIdent "x"
                    |> SynExpr.applyFunction (SynExpr.createLongIdent [ "System" ; "Boolean" ; "Parse" ])
                    |> FlagDu.FromBoolean flagDu
                    |> SynExpr.createLambda "x"

                parser, Accumulation.Required, ty

    let rec private toParseSpec
        (counter : int)
        (flagDus : FlagDu list)
        (ambientRecords : RecordType list)
        (finalRecord : RecordType)
        : ParseTreeCrate * int
        =
        finalRecord.Fields
        |> List.iter (fun (SynField.SynField (isStatic = isStatic)) ->
            if isStatic then
                failwith "No static record fields allowed in ArgParserGenerator"
        )

        let counter, contents =
            ((counter, []), finalRecord.Fields)
            ||> List.fold (fun (counter, acc) (SynField.SynField (attrs, _, identOption, fieldType, _, _, _, _, _)) ->
                let attrs = attrs |> List.collect (fun a -> a.Attributes)

                let positionalArgAttr =
                    attrs
                    |> List.tryPick (fun a ->
                        match (List.last a.TypeName.LongIdent).idText with
                        | "PositionalArgsAttribute"
                        | "PositionalArgs" ->
                            match a.ArgExpr with
                            | SynExpr.Const (SynConst.Unit, _) -> Some None
                            | a -> Some (Some a)
                        | _ -> None
                    )

                let parseExactModifier =
                    attrs
                    |> List.tryPick (fun a ->
                        match (List.last a.TypeName.LongIdent).idText with
                        | "ParseExactAttribute"
                        | "ParseExact" -> Some a.ArgExpr
                        | _ -> None
                    )

                let helpText =
                    attrs
                    |> List.tryPick (fun a ->
                        match (List.last a.TypeName.LongIdent).idText with
                        | "ArgumentHelpTextAttribute"
                        | "ArgumentHelpText" -> Some a.ArgExpr
                        | _ -> None
                    )

                let helpText =
                    match parseExactModifier, helpText with
                    | None, ht -> ht
                    | Some pe, None ->
                        SynExpr.createIdent "sprintf"
                        |> SynExpr.applyTo (SynExpr.CreateConst "[Parse format (.NET): %s]")
                        |> SynExpr.applyTo pe
                        |> Some
                    | Some pe, Some ht ->
                        SynExpr.createIdent "sprintf"
                        |> SynExpr.applyTo (SynExpr.CreateConst "%s [Parse format (.NET): %s]")
                        |> SynExpr.applyTo ht
                        |> SynExpr.applyTo pe
                        |> Some

                let ident =
                    match identOption with
                    | None -> failwith "expected args field to have a name, but it did not"
                    | Some i -> i

                let longForms =
                    attrs
                    |> List.choose (fun attr ->
                        match attr.TypeName with
                        | SynLongIdent.SynLongIdent (ident, _, _) ->
                            if (List.last ident).idText = "ArgumentLongForm" then
                                Some attr.ArgExpr
                            else
                                None
                    )
                    |> function
                        | [] -> List.singleton (SynExpr.CreateConst (argify ident))
                        | l -> List.ofSeq l

                let ambientRecordMatch =
                    match fieldType with
                    | SynType.LongIdent (SynLongIdent.SynLongIdent (id, _, _)) ->
                        let target = List.last(id).idText
                        ambientRecords |> List.tryFind (fun r -> r.Name.idText = target)
                    | _ -> None

                match ambientRecordMatch with
                | Some ambient ->
                    // This field has a type we need to obtain from parsing another record.
                    let spec, counter = toParseSpec counter flagDus ambientRecords ambient
                    counter, (ident, spec) :: acc
                | None ->

                match positionalArgAttr with
                | Some includeFlagLike ->
                    let getChoice (spec : ArgumentDefaultSpec option) : unit =
                        match spec with
                        | Some _ ->
                            failwith
                                "Positional Choice args cannot have default values. Remove [<ArgumentDefault*>] from the positional arg."
                        | None -> ()

                    let parser, accumulation, parseTy =
                        createParseFunction<unit> getChoice flagDus ident attrs fieldType

                    let isBoolLike =
                        match parseTy with
                        | PrimitiveType ident when ident |> List.map _.idText = [ "System" ; "Boolean" ] ->
                            Some (Choice2Of2 ())
                        | parseTy -> identifyAsFlag flagDus parseTy |> Option.map Choice1Of2

                    match accumulation with
                    | Accumulation.List (Accumulation.List _) ->
                        failwith "A list of positional args cannot contain lists."
                    | Accumulation.List Accumulation.Optional ->
                        failwith "A list of positional args cannot contain optionals. What would that even mean?"
                    | Accumulation.List (Accumulation.Choice ()) ->
                        {
                            FieldName = ident
                            Parser = parser
                            TargetVariable = Ident.create $"arg_%i{counter}"
                            Accumulation = ChoicePositional.Choice includeFlagLike
                            TargetType = parseTy
                            ArgForm = longForms
                            Help = helpText
                            BoolCases = isBoolLike
                        }
                        |> fun t -> ParseTree.PositionalLeaf (t, Teq.refl)
                    | Accumulation.List Accumulation.Required ->
                        {
                            FieldName = ident
                            Parser = parser
                            TargetVariable = Ident.create $"arg_%i{counter}"
                            Accumulation = ChoicePositional.Normal includeFlagLike
                            TargetType = parseTy
                            ArgForm = longForms
                            Help = helpText
                            BoolCases = isBoolLike
                        }
                        |> fun t -> ParseTree.PositionalLeaf (t, Teq.refl)
                    | Accumulation.Choice _
                    | Accumulation.Optional
                    | Accumulation.Required ->
                        failwith $"Expected positional arg accumulation type to be List, but it was %O{fieldType}"
                    |> ParseTreeCrate.make
                | None ->
                    let getChoice (spec : ArgumentDefaultSpec option) : ArgumentDefaultSpec =
                        match spec with
                        | None ->
                            failwith
                                "Non-positional Choice args must have an `[<ArgumentDefault*>]` attribute on them."
                        | Some spec -> spec

                    let parser, accumulation, parseTy =
                        createParseFunction getChoice flagDus ident attrs fieldType

                    let isBoolLike =
                        match parseTy with
                        | PrimitiveType ident when ident |> List.map _.idText = [ "System" ; "Boolean" ] ->
                            Some (Choice2Of2 ())
                        | parseTy -> identifyAsFlag flagDus parseTy |> Option.map Choice1Of2

                    {
                        FieldName = ident
                        Parser = parser
                        TargetVariable = Ident.create $"arg_%i{counter}"
                        Accumulation = accumulation
                        TargetType = parseTy
                        ArgForm = longForms
                        Help = helpText
                        BoolCases = isBoolLike
                    }
                    |> fun t -> ParseTree.NonPositionalLeaf (t, Teq.refl)
                    |> ParseTreeCrate.make
                |> fun tree -> counter + 1, (ident, tree) :: acc
            )

        let tree =
            contents
            |> List.rev
            |> ParseTree.branch (fun args ->
                args
                |> Map.toList
                |> List.map (fun (ident, expr) -> SynLongIdent.create [ Ident.create ident ], expr)
                |> SynExpr.createRecord None
            )

        tree, counter

    /// let helpText : string = ...
    let private helpText
        (typeName : Ident)
        (positional : ParseFunctionPositional option)
        (args : ParseFunctionNonPositional list)
        : SynBinding
        =
        let describeNonPositional
            (acc : Accumulation<ArgumentDefaultSpec>)
            (flagCases : Choice<FlagDu, unit> option)
            : SynExpr
            =
            match acc with
            | Accumulation.Required -> SynExpr.CreateConst ""
            | Accumulation.Optional -> SynExpr.CreateConst " (optional)"
            | Accumulation.Choice (ArgumentDefaultSpec.EnvironmentVariable var) ->
                // We don't print out the default value in case it's a secret. People often pass secrets
                // through env vars!
                var
                |> SynExpr.pipeThroughFunction (
                    SynExpr.applyFunction
                        (SynExpr.createIdent "sprintf")
                        (SynExpr.CreateConst " (default value populated from env var %s)")
                )
                |> SynExpr.paren
            | Accumulation.Choice (ArgumentDefaultSpec.FunctionCall var) ->
                match flagCases with
                | None -> SynExpr.callMethod var.idText (SynExpr.createIdent' typeName)
                | Some (Choice2Of2 ()) -> SynExpr.callMethod var.idText (SynExpr.createIdent' typeName)
                | Some (Choice1Of2 flagDu) ->
                    // Care required here. The return value from the Default call is not a bool,
                    // but we should display it as such to the user!
                    [
                        SynMatchClause.create
                            (SynPat.identWithArgs [ flagDu.Name ; flagDu.Case1Name ] (SynArgPats.create []))
                            (SynExpr.ifThenElse
                                (SynExpr.equals flagDu.Case1Arg (SynExpr.CreateConst true))
                                (SynExpr.CreateConst "false")
                                (SynExpr.CreateConst "true"))
                        SynMatchClause.create
                            (SynPat.identWithArgs [ flagDu.Name ; flagDu.Case2Name ] (SynArgPats.create []))
                            (SynExpr.ifThenElse
                                (SynExpr.equals flagDu.Case2Arg (SynExpr.CreateConst true))
                                (SynExpr.CreateConst "false")
                                (SynExpr.CreateConst "true"))
                    ]
                    |> SynExpr.createMatch (SynExpr.callMethod var.idText (SynExpr.createIdent' typeName))
                |> SynExpr.pipeThroughFunction (
                    SynExpr.createLambda "x" (SynExpr.callMethod "ToString" (SynExpr.createIdent "x"))
                )
                |> SynExpr.pipeThroughFunction (
                    SynExpr.applyFunction (SynExpr.createIdent "sprintf") (SynExpr.CreateConst " (default value: %s)")
                )
                |> SynExpr.paren
            | Accumulation.List _ -> SynExpr.CreateConst " (can be repeated)"

        let describePositional _ _ =
            SynExpr.CreateConst " (positional args) (can be repeated)"

        /// We may sometimes lie about the type name, if e.g. this is a flag DU which we're pretending is a boolean.
        /// So the `renderTypeName` takes the Accumulation which tells us whether we're lying.
        let toPrintable (describe : 'a -> Choice<FlagDu, unit> option -> SynExpr) (arg : ParseFunction<'a>) : SynExpr =
            let ty =
                match arg.BoolCases with
                | None -> SynType.toHumanReadableString arg.TargetType
                | Some _ -> "bool"

            let helpText =
                match arg.Help with
                | None -> SynExpr.CreateConst ""
                | Some helpText ->
                    SynExpr.applyFunction (SynExpr.createIdent "sprintf") (SynExpr.CreateConst " : %s")
                    |> SynExpr.applyTo (SynExpr.paren helpText)
                    |> SynExpr.paren

            let descriptor = describe arg.Accumulation arg.BoolCases

            SynExpr.applyFunction (SynExpr.createIdent "sprintf") (SynExpr.CreateConst $"%%s  %s{ty}%%s%%s")
            |> SynExpr.applyTo arg.HumanReadableArgForm
            |> SynExpr.applyTo descriptor
            |> SynExpr.applyTo helpText
            |> SynExpr.paren

        args
        |> List.map (toPrintable describeNonPositional)
        |> fun l ->
            match positional with
            | None -> l
            | Some pos -> l @ [ toPrintable describePositional pos ]
        |> SynExpr.listLiteral
        |> SynExpr.pipeThroughFunction (
            SynExpr.applyFunction (SynExpr.createLongIdent [ "String" ; "concat" ]) (SynExpr.CreateConst @"\n")
        )
        |> SynBinding.basic [ Ident.create "helpText" ] [ SynPat.unit ]

    /// `let processKeyValue (key : string) (value : string) : Result<unit, string option> = ...`
    /// Returns a possible error.
    /// A parse failure might not be fatal (e.g. maybe the input was optionally of arity 0, and we failed to do
    /// the parse because in fact the key decided not to take this argument); in that case we return Error None.
    let private processKeyValue
        (argParseErrors : Ident)
        (pos : ParseFunctionPositional option)
        (args : ParseFunctionNonPositional list)
        : SynBinding
        =
        let args =
            args
            |> List.map (fun arg ->
                match arg.Accumulation with
                | Accumulation.Required
                | Accumulation.Choice _
                | Accumulation.Optional ->
                    let multipleErrorMessage =
                        SynExpr.createIdent "sprintf"
                        |> SynExpr.applyTo (SynExpr.CreateConst "Argument '%s' was supplied multiple times: %s and %s")
                        |> SynExpr.applyTo arg.HumanReadableArgForm
                        |> SynExpr.applyTo (SynExpr.createIdent "x" |> SynExpr.callMethod "ToString" |> SynExpr.paren)
                        |> SynExpr.applyTo (
                            SynExpr.createIdent "value" |> SynExpr.callMethod "ToString" |> SynExpr.paren
                        )

                    let performAssignment =
                        [
                            SynExpr.createIdent "value"
                            |> SynExpr.pipeThroughFunction arg.Parser
                            |> SynExpr.pipeThroughFunction (SynExpr.createIdent "Some")
                            |> SynExpr.assign (SynLongIdent.createI arg.TargetVariable)

                            SynExpr.applyFunction (SynExpr.createIdent "Ok") (SynExpr.CreateConst ())
                        ]
                        |> SynExpr.sequential

                    [
                        SynMatchClause.create
                            (SynPat.nameWithArgs "Some" [ SynPat.named "x" ])
                            (SynExpr.sequential
                                [
                                    multipleErrorMessage
                                    |> SynExpr.pipeThroughFunction (
                                        SynExpr.dotGet "Add" (SynExpr.createIdent' argParseErrors)
                                    )
                                    SynExpr.applyFunction (SynExpr.createIdent "Ok") (SynExpr.CreateConst ())
                                ])
                        SynMatchClause.create
                            (SynPat.named "None")
                            (SynExpr.pipeThroughTryWith
                                SynPat.anon
                                (SynExpr.createLongIdent [ "exc" ; "Message" ]
                                 |> SynExpr.pipeThroughFunction (SynExpr.createIdent "Some")
                                 |> SynExpr.pipeThroughFunction (SynExpr.createIdent "Error"))
                                performAssignment)
                    ]
                    |> SynExpr.createMatch (SynExpr.createIdent' arg.TargetVariable)
                | Accumulation.List (Accumulation.List _)
                | Accumulation.List Accumulation.Optional
                | Accumulation.List (Accumulation.Choice _) ->
                    failwith
                        "WoofWare.Myriad invariant violated: expected a list to contain only a Required accumulation. Non-positional lists cannot be optional or Choice, nor can they themselves contain lists."
                | Accumulation.List Accumulation.Required ->
                    [
                        SynExpr.createIdent "value"
                        |> SynExpr.pipeThroughFunction arg.Parser
                        |> SynExpr.pipeThroughFunction (
                            SynExpr.createLongIdent' [ arg.TargetVariable ; Ident.create "Add" ]
                        )
                        SynExpr.CreateConst () |> SynExpr.pipeThroughFunction (SynExpr.createIdent "Ok")
                    ]
                    |> SynExpr.sequential
                |> fun expr -> arg.ArgForm, expr
            )

        let posArg =
            match pos with
            | None -> []
            | Some pos ->
                [
                    SynExpr.createIdent "value"
                    |> SynExpr.pipeThroughFunction pos.Parser
                    |> fun p ->
                        match pos.Accumulation with
                        | ChoicePositional.Choice _ ->
                            p |> SynExpr.pipeThroughFunction (SynExpr.createIdent "Choice1Of2")
                        | ChoicePositional.Normal _ -> p
                    |> SynExpr.pipeThroughFunction (
                        SynExpr.createLongIdent' [ pos.TargetVariable ; Ident.create "Add" ]
                    )
                    SynExpr.CreateConst () |> SynExpr.pipeThroughFunction (SynExpr.createIdent "Ok")
                ]
                |> SynExpr.sequential
                |> fun expr -> pos.ArgForm, expr
                |> List.singleton

        (SynExpr.applyFunction (SynExpr.createIdent "Error") (SynExpr.createIdent "None"), posArg @ args)
        ||> List.fold (fun finalBranch (argForm, arg) ->
            (finalBranch, argForm)
            ||> List.fold (fun finalBranch argForm ->
                arg
                |> SynExpr.ifThenElse
                    (SynExpr.applyFunction
                        (SynExpr.createLongIdent [ "System" ; "String" ; "Equals" ])
                        (SynExpr.tuple
                            [
                                SynExpr.createIdent "key"
                                SynExpr.applyFunction
                                    (SynExpr.applyFunction
                                        (SynExpr.createIdent "sprintf")
                                        (SynExpr.CreateConst "--%s"))
                                    argForm
                                SynExpr.createLongIdent [ "System" ; "StringComparison" ; "OrdinalIgnoreCase" ]
                            ]))
                    finalBranch
            )
        )
        |> SynBinding.basic
            [ Ident.create "processKeyValue" ]
            [
                SynPat.annotateType SynType.string (SynPat.named "key")
                SynPat.annotateType SynType.string (SynPat.named "value")
            ]
        |> SynBinding.withReturnAnnotation (
            SynType.app "Result" [ SynType.unit ; SynType.appPostfix "option" SynType.string ]
        )
        |> SynBinding.withXmlDoc (
            [
                " Processes the key-value pair, returning Error if no key was matched."
                " If the key is an arg which can have arity 1, but throws when consuming that arg, we return Error(<the message>)."
                " This can nevertheless be a successful parse, e.g. when the key may have arity 0."
            ]
            |> PreXmlDoc.create'
        )

    /// `let setFlagValue (key : string) : bool = ...`
    /// The second member of the `flags` list tuple is the constant "true" with which we will interpret the
    /// arity-0 `--foo`. So in the case of a boolean-typed field, this is `true`; in the case of a Flag-typed field,
    /// this is `FlagType.WhicheverCaseHadTrue`.
    let private setFlagValue (argParseErrors : Ident) (flags : (ParseFunction<'a> * SynExpr) list) : SynBinding =
        (SynExpr.CreateConst false, flags)
        ||> List.fold (fun finalExpr (flag, trueCase) ->
            let multipleErrorMessage =
                SynExpr.createIdent "sprintf"
                |> SynExpr.applyTo (SynExpr.CreateConst "Flag '%s' was supplied multiple times")
                |> SynExpr.applyTo flag.HumanReadableArgForm

            let matchFlag =
                [
                    SynMatchClause.create
                        (SynPat.nameWithArgs "Some" [ SynPat.named "x" ])
                        // This is an error, but it's one we can gracefully report at the end.
                        (SynExpr.sequential
                            [
                                multipleErrorMessage
                                |> SynExpr.pipeThroughFunction (
                                    SynExpr.dotGet "Add" (SynExpr.createIdent' argParseErrors)
                                )
                                SynExpr.CreateConst true
                            ])

                    SynMatchClause.create
                        (SynPat.named "None")
                        ([
                            SynExpr.assign
                                (SynLongIdent.createI flag.TargetVariable)
                                (SynExpr.pipeThroughFunction (SynExpr.createIdent "Some") trueCase)
                            SynExpr.CreateConst true
                         ]
                         |> SynExpr.sequential)
                ]
                |> SynExpr.createMatch (SynExpr.createIdent' flag.TargetVariable)

            (finalExpr, flag.ArgForm)
            ||> List.fold (fun finalExpr argForm ->
                SynExpr.ifThenElse
                    (SynExpr.applyFunction
                        (SynExpr.createLongIdent [ "System" ; "String" ; "Equals" ])
                        (SynExpr.tuple
                            [
                                SynExpr.createIdent "key"
                                SynExpr.applyFunction
                                    (SynExpr.applyFunction
                                        (SynExpr.createIdent "sprintf")
                                        (SynExpr.CreateConst "--%s"))
                                    argForm
                                SynExpr.createLongIdent [ "System" ; "StringComparison" ; "OrdinalIgnoreCase" ]
                            ]))
                    finalExpr
                    matchFlag
            )
        )
        |> SynBinding.basic [ Ident.create "setFlagValue" ] [ SynPat.annotateType SynType.string (SynPat.named "key") ]
        |> SynBinding.withReturnAnnotation (SynType.named "bool")
        |> SynBinding.withXmlDoc (PreXmlDoc.create "Returns false if we didn't set a value.")

    /// `let rec go (state : %ParseState%) (args : string list) : unit = ...`
    let private mainLoop
        (parseState : Ident)
        (errorAcc : Ident)
        (leftoverArgAcc : ChoicePositional)
        (leftoverArgs : Ident)
        (leftoverArgParser : SynExpr)
        : SynBinding
        =
        /// `go (AwaitingValue arg) args`
        let recurseValue =
            SynExpr.createIdent "go"
            |> SynExpr.applyTo (
                SynExpr.paren (
                    SynExpr.applyFunction
                        (SynExpr.createLongIdent' [ parseState ; Ident.create "AwaitingValue" ])
                        (SynExpr.createIdent "arg")
                )
            )

        /// `go AwaitingKey args`
        let recurseKey =
            (SynExpr.createIdent "go")
            |> SynExpr.applyTo (SynExpr.createLongIdent' [ parseState ; Ident.create "AwaitingKey" ])
            |> SynExpr.applyTo (SynExpr.createIdent "args")

        /// `failwithf "Unable to process argument ..."`
        let fail =
            SynExpr.createIdent "failwithf"
            |> SynExpr.applyTo (SynExpr.CreateConst "Unable to process argument %s as key %s and value %s")
            |> SynExpr.applyTo (SynExpr.createIdent "arg")
            |> SynExpr.applyTo (SynExpr.createIdent "key")
            |> SynExpr.applyTo (SynExpr.createIdent "value")

        let processAsPositional =
            SynExpr.sequential
                [
                    SynExpr.createIdent "arg"
                    |> SynExpr.pipeThroughFunction leftoverArgParser
                    |> fun p ->
                        match leftoverArgAcc with
                        | ChoicePositional.Normal _ -> p
                        | ChoicePositional.Choice _ ->
                            p |> SynExpr.pipeThroughFunction (SynExpr.createIdent "Choice1Of2")
                    |> SynExpr.pipeThroughFunction (SynExpr.createLongIdent' [ leftoverArgs ; Ident.create "Add" ])

                    recurseKey
                ]

        let posAttr =
            match leftoverArgAcc with
            | ChoicePositional.Choice a
            | ChoicePositional.Normal a -> a

        let notMatched =
            let handleFailure =
                [
                    SynMatchClause.create (SynPat.named "None") fail

                    SynMatchClause.create
                        (SynPat.nameWithArgs "Some" [ SynPat.named "msg" ])
                        (SynExpr.sequential
                            [
                                SynExpr.createIdent "sprintf"
                                |> SynExpr.applyTo (SynExpr.CreateConst "%s (at arg %s)")
                                |> SynExpr.applyTo (SynExpr.createIdent "msg")
                                |> SynExpr.applyTo (SynExpr.createIdent "arg")
                                |> SynExpr.pipeThroughFunction (SynExpr.dotGet "Add" (SynExpr.createIdent' errorAcc))

                                recurseKey
                            ])
                ]
                |> SynExpr.createMatch (SynExpr.createIdent "x")

            match posAttr with
            | None -> handleFailure
            | Some posAttr -> SynExpr.ifThenElse posAttr handleFailure processAsPositional

        let argStartsWithDashes =
            SynExpr.createIdent "arg"
            |> SynExpr.callMethodArg
                "StartsWith"
                (SynExpr.tuple
                    [
                        SynExpr.CreateConst "--"
                        SynExpr.createLongIdent [ "System" ; "StringComparison" ; "Ordinal" ]
                    ])

        let processKey =
            SynExpr.ifThenElse
                argStartsWithDashes
                processAsPositional
                (SynExpr.ifThenElse
                    (SynExpr.equals (SynExpr.createIdent "arg") (SynExpr.CreateConst "--help"))
                    (SynExpr.createLet
                        [
                            SynBinding.basic
                                [ Ident.create "equals" ]
                                []
                                (SynExpr.callMethodArg "IndexOf" (SynExpr.CreateConst '=') (SynExpr.createIdent "arg"))
                        ]
                        (SynExpr.ifThenElse
                            (SynExpr.lessThan (SynExpr.CreateConst 0) (SynExpr.createIdent "equals"))
                            (SynExpr.createLet
                                [
                                    SynBinding.basic
                                        [ Ident.create "key" ]
                                        []
                                        (SynExpr.arrayIndexRange
                                            (Some (SynExpr.CreateConst 0))
                                            (Some (SynExpr.minusN (SynLongIdent.createS "equals") 1))
                                            (SynExpr.createIdent "arg"))
                                    SynBinding.basic
                                        [ Ident.create "value" ]
                                        []
                                        (SynExpr.arrayIndexRange
                                            (Some (SynExpr.plus (SynExpr.createIdent "equals") (SynExpr.CreateConst 1)))
                                            None
                                            (SynExpr.createIdent "arg"))
                                ]
                                (SynExpr.createMatch
                                    (SynExpr.createIdent "processKeyValue"
                                     |> SynExpr.applyTo (SynExpr.createIdent "key")
                                     |> SynExpr.applyTo (SynExpr.createIdent "value"))
                                    [
                                        SynMatchClause.create (SynPat.nameWithArgs "Ok" [ SynPat.unit ]) recurseKey

                                        SynMatchClause.create
                                            (SynPat.nameWithArgs "Error" [ SynPat.named "x" ])
                                            notMatched
                                    ]))
                            (SynExpr.createIdent "args" |> SynExpr.pipeThroughFunction recurseValue)))
                    (SynExpr.createIdent "helpText"
                     |> SynExpr.applyTo (SynExpr.CreateConst ())
                     |> SynExpr.pipeThroughFunction (
                         SynExpr.applyFunction
                             (SynExpr.createIdent "failwithf")
                             (SynExpr.CreateConst @"Help text requested.\n%s")
                     )))

        let processValue =
            // During failure, we've received an optional exception message that happened when we tried to parse
            // the value; it's in the variable `exc`.
            // `fail` is for the case where we're genuinely emitting an error.
            // If we're in `PositionalArgs true` mode, though, we won't call `fail`.
            let fail =
                [
                    SynExpr.createIdent "failwithf"
                    |> SynExpr.applyTo (
                        SynExpr.CreateConst @"Unable to process supplied arg %s. Help text follows.\n%s"
                    )
                    |> SynExpr.applyTo (SynExpr.createIdent "key")
                    |> SynExpr.applyTo (
                        SynExpr.applyFunction (SynExpr.createIdent "helpText") (SynExpr.CreateConst ())
                        |> SynExpr.paren
                    )
                    |> SynMatchClause.create (SynPat.named "None")

                    SynExpr.createIdent "msg"
                    |> SynExpr.pipeThroughFunction (SynExpr.dotGet "Add" (SynExpr.createIdent' errorAcc))
                    |> SynMatchClause.create (SynPat.nameWithArgs "Some" [ SynPat.named "msg" ])
                ]
                |> SynExpr.createMatch (SynExpr.createIdent "exc")

            let onFailure =
                match posAttr with
                | None -> fail
                | Some includeFlagLike ->
                    [
                        SynExpr.createIdent "key"
                        |> SynExpr.pipeThroughFunction leftoverArgParser
                        |> fun i ->
                            match leftoverArgAcc with
                            | ChoicePositional.Choice _ ->
                                i |> SynExpr.pipeThroughFunction (SynExpr.createIdent "Choice1Of2")
                            | ChoicePositional.Normal _ -> i
                        |> SynExpr.pipeThroughFunction (SynExpr.createLongIdent' [ leftoverArgs ; Ident.create "Add" ])

                        SynExpr.createIdent "go"
                        |> SynExpr.applyTo (SynExpr.createLongIdent' [ parseState ; Ident.create "AwaitingKey" ])
                        |> SynExpr.applyTo (SynExpr.listCons (SynExpr.createIdent "arg") (SynExpr.createIdent "args"))
                    ]
                    |> SynExpr.sequential
                    |> SynExpr.ifThenElse includeFlagLike fail

            [
                SynMatchClause.create
                    (SynPat.nameWithArgs "Ok" [ SynPat.unit ])
                    (SynExpr.applyFunction
                        (SynExpr.applyFunction
                            (SynExpr.createIdent "go")
                            (SynExpr.createLongIdent' [ parseState ; Ident.create "AwaitingKey" ]))
                        (SynExpr.createIdent "args"))
                SynMatchClause.create
                    (SynPat.nameWithArgs "Error" [ SynPat.named "exc" ])
                    (SynExpr.ifThenElse
                        (SynExpr.applyFunction (SynExpr.createIdent "setFlagValue") (SynExpr.createIdent "key"))
                        onFailure
                        (SynExpr.createIdent "go"
                         |> SynExpr.applyTo (SynExpr.createLongIdent' [ parseState ; Ident.create "AwaitingKey" ])
                         |> SynExpr.applyTo (SynExpr.listCons (SynExpr.createIdent "arg") (SynExpr.createIdent "args"))))
            ]
            |> SynExpr.createMatch (
                SynExpr.applyFunction
                    (SynExpr.applyFunction (SynExpr.createIdent "processKeyValue") (SynExpr.createIdent "key"))
                    (SynExpr.createIdent "arg")
            )

        let argBody =
            [
                SynMatchClause.create
                    (SynPat.identWithArgs [ parseState ; Ident.create "AwaitingKey" ] (SynArgPats.create []))
                    processKey
                SynMatchClause.create
                    (SynPat.identWithArgs
                        [ parseState ; Ident.create "AwaitingValue" ]
                        (SynArgPats.createNamed [ "key" ]))
                    processValue
            ]
            |> SynExpr.createMatch (SynExpr.createIdent "state")

        let body =
            let trailingArgMessage =
                SynExpr.createIdent "sprintf"
                |> SynExpr.applyTo (
                    SynExpr.CreateConst
                        "Trailing argument %s had no value. Use a double-dash to separate positional args from key-value args."
                )
                |> SynExpr.applyTo (SynExpr.createIdent "key")

            [
                SynMatchClause.create
                    SynPat.emptyList
                    (SynExpr.createMatch
                        (SynExpr.createIdent "state")
                        [
                            SynMatchClause.create
                                (SynPat.identWithArgs [ parseState ; Ident.create "AwaitingKey" ] (SynArgPats.create []))
                                (SynExpr.CreateConst ())
                            SynMatchClause.create
                                (SynPat.identWithArgs
                                    [ parseState ; Ident.create "AwaitingValue" ]
                                    (SynArgPats.createNamed [ "key" ]))
                                (SynExpr.ifThenElse
                                    (SynExpr.applyFunction
                                        (SynExpr.createIdent "setFlagValue")
                                        (SynExpr.createIdent "key"))
                                    (trailingArgMessage
                                     |> SynExpr.pipeThroughFunction (
                                         SynExpr.dotGet "Add" (SynExpr.createIdent' errorAcc)
                                     ))
                                    (SynExpr.CreateConst ()))
                        ])
                SynMatchClause.create
                    (SynPat.listCons (SynPat.createConst (SynConst.Create "--")) (SynPat.named "rest"))
                    (SynExpr.callMethodArg
                        "AddRange"
                        (SynExpr.paren (
                            SynExpr.createIdent "rest"
                            |> SynExpr.pipeThroughFunction (
                                SynExpr.applyFunction (SynExpr.createLongIdent [ "Seq" ; "map" ]) leftoverArgParser
                            )
                            |> fun p ->
                                match leftoverArgAcc with
                                | ChoicePositional.Normal _ -> p
                                | ChoicePositional.Choice _ ->
                                    p
                                    |> SynExpr.pipeThroughFunction (
                                        SynExpr.applyFunction
                                            (SynExpr.createLongIdent [ "Seq" ; "map" ])
                                            (SynExpr.createIdent "Choice2Of2")
                                    )
                        ))
                        (SynExpr.createIdent' leftoverArgs))
                SynMatchClause.create (SynPat.listCons (SynPat.named "arg") (SynPat.named "args")) argBody
            ]
            |> SynExpr.createMatch (SynExpr.createIdent "args")

        let args =
            [
                SynPat.named "state"
                |> SynPat.annotateType (SynType.createLongIdent [ parseState ])
                SynPat.named "args"
                |> SynPat.annotateType (SynType.appPostfix "list" SynType.string)
            ]

        SynBinding.basic [ Ident.create "go" ] args body
        |> SynBinding.withRecursion true

    /// Takes a single argument, `args : string list`, and returns something of the type indicated by `recordType`.
    let createRecordParse
        (parseState : Ident)
        (flagDus : FlagDu list)
        (ambientRecords : RecordType list)
        (recordType : RecordType)
        : SynExpr
        =
        let spec, _ = toParseSpec 0 flagDus ambientRecords recordType
        // For each argument (positional and non-positional), create an accumulator for it.
        let nonPos, pos =
            { new ParseTreeEval<_> with
                member _.Eval tree = ParseTree.accumulators tree
            }
            |> spec.Apply

        let bindings =
            nonPos
            |> List.map (fun pf ->
                match pf.Accumulation with
                | Accumulation.Required
                | Accumulation.Choice _
                | Accumulation.Optional ->
                    SynExpr.createIdent "None"
                    |> SynBinding.basic [ pf.TargetVariable ] []
                    |> SynBinding.withMutability true
                    |> SynBinding.withReturnAnnotation (SynType.appPostfix "option" pf.TargetType)
                | Accumulation.List (Accumulation.List _)
                | Accumulation.List Accumulation.Optional
                | Accumulation.List (Accumulation.Choice _) ->
                    failwith
                        "WoofWare.Myriad invariant violated: expected a list to contain only a Required accumulation. Non-positional lists cannot be optional or Choice, nor can they themselves contain lists."
                | Accumulation.List Accumulation.Required ->
                    SynExpr.createIdent "ResizeArray"
                    |> SynExpr.applyTo (SynExpr.CreateConst ())
                    |> SynBinding.basic [ pf.TargetVariable ] []
                    |> SynBinding.withReturnAnnotation (SynType.appPostfix "ResizeArray" pf.TargetType)
            )

        let bindings, leftoverArgsName, leftoverArgsParser =
            let bindingName, leftoverArgsParser, leftoverArgsType =
                match pos with
                | None ->
                    Ident.create "parser_LeftoverArgs",
                    (SynExpr.createLambda "x" (SynExpr.createIdent "x")),
                    SynType.string
                | Some pf ->
                    match pf.Accumulation with
                    | ChoicePositional.Choice _ ->
                        pf.TargetVariable, pf.Parser, SynType.app "Choice" [ pf.TargetType ; pf.TargetType ]
                    | ChoicePositional.Normal _ -> pf.TargetVariable, pf.Parser, pf.TargetType

            let bindings =
                SynExpr.createIdent "ResizeArray"
                |> SynExpr.applyTo (SynExpr.CreateConst ())
                |> SynBinding.basic [ bindingName ] []
                |> SynBinding.withReturnAnnotation (SynType.appPostfix "ResizeArray" leftoverArgsType)
                |> fun b -> b :: bindings

            bindings, bindingName, leftoverArgsParser

        let argParseErrors = Ident.create "ArgParser_errors"

        let errorCollection : SynBinding =
            SynExpr.createIdent "ResizeArray"
            |> SynExpr.applyTo (SynExpr.CreateConst ())
            |> SynBinding.basic [ argParseErrors ] []

        let helpText = helpText recordType.Name pos nonPos

        let bindings = errorCollection :: helpText :: bindings

        let unchecked =
            SynExpr.createLongIdent [ "Unchecked" ; "defaultof" ]
            |> SynExpr.typeApp [ SynType.anon ]

        // Determine whether any required arg is missing, and freeze args into immutable form.
        let freezeNonPositionalArgs =
            nonPos
            |> List.map (fun pf ->
                match pf.Accumulation with
                | Accumulation.Choice spec ->
                    let getDefaultValue =
                        match spec with
                        | ArgumentDefaultSpec.EnvironmentVariable name ->
                            let result =
                                name
                                |> SynExpr.pipeThroughFunction (SynExpr.createIdent "getEnvironmentVariable")

                            /// Assumes access to a non-null variable `x` containing the string value.
                            let parser =
                                match pf.BoolCases with
                                | Some boolLike ->
                                    let trueCase, falseCase =
                                        match boolLike with
                                        | Choice2Of2 () -> SynExpr.CreateConst true, SynExpr.CreateConst false
                                        | Choice1Of2 flag ->
                                            FlagDu.FromBoolean flag (SynExpr.CreateConst true),
                                            FlagDu.FromBoolean flag (SynExpr.CreateConst false)

                                    // We permit environment variables to be populated with 0 and 1 as well.
                                    SynExpr.ifThenElse
                                        (SynExpr.applyFunction
                                            (SynExpr.createLongIdent [ "System" ; "String" ; "Equals" ])
                                            (SynExpr.tuple
                                                [
                                                    SynExpr.createIdent "x"
                                                    SynExpr.CreateConst "1"
                                                    SynExpr.createLongIdent
                                                        [ "System" ; "StringComparison" ; "OrdinalIgnoreCase" ]
                                                ]))
                                        (SynExpr.ifThenElse
                                            (SynExpr.applyFunction
                                                (SynExpr.createLongIdent [ "System" ; "String" ; "Equals" ])
                                                (SynExpr.tuple
                                                    [
                                                        SynExpr.createIdent "x"
                                                        SynExpr.CreateConst "0"
                                                        SynExpr.createLongIdent
                                                            [ "System" ; "StringComparison" ; "OrdinalIgnoreCase" ]
                                                    ]))
                                            (SynExpr.createIdent "x" |> SynExpr.pipeThroughFunction pf.Parser)
                                            falseCase)
                                        trueCase
                                | None -> (SynExpr.createIdent "x" |> SynExpr.pipeThroughFunction pf.Parser)

                            let errorMessage =
                                SynExpr.createIdent "sprintf"
                                |> SynExpr.applyTo (
                                    SynExpr.CreateConst
                                        "No value was supplied for %s, nor was environment variable %s set"
                                )
                                |> SynExpr.applyTo pf.HumanReadableArgForm
                                |> SynExpr.applyTo name

                            [
                                SynMatchClause.create
                                    (SynPat.named "None")
                                    (SynExpr.sequential
                                        [
                                            errorMessage
                                            |> SynExpr.pipeThroughFunction (
                                                SynExpr.dotGet "Add" (SynExpr.createIdent' argParseErrors)
                                            )
                                            unchecked
                                        ])

                                SynMatchClause.create (SynPat.nameWithArgs "Some" [ SynPat.named "x" ]) parser
                            ]
                            |> SynExpr.createMatch result
                        | ArgumentDefaultSpec.FunctionCall name ->
                            SynExpr.callMethod name.idText (SynExpr.createIdent' recordType.Name)

                    [
                        SynMatchClause.create
                            (SynPat.named "None")
                            (getDefaultValue
                             |> SynExpr.pipeThroughFunction (SynExpr.createIdent "Choice2Of2"))
                        SynMatchClause.create
                            (SynPat.nameWithArgs "Some" [ SynPat.named "x" ])
                            (SynExpr.applyFunction (SynExpr.createIdent "Choice1Of2") (SynExpr.createIdent "x"))
                    ]
                    |> SynExpr.createMatch (SynExpr.createIdent' pf.TargetVariable)
                    |> SynBinding.basic [ pf.TargetVariable ] []
                | Accumulation.Optional ->
                    SynBinding.basic [ pf.TargetVariable ] [] (SynExpr.createIdent' pf.TargetVariable)
                | Accumulation.List (Accumulation.List _)
                | Accumulation.List Accumulation.Optional
                | Accumulation.List (Accumulation.Choice _) ->
                    failwith
                        "WoofWare.Myriad invariant violated: expected a list to contain only a Required accumulation. Non-positional lists cannot be optional or Choice, nor can they themselves contain lists."
                | Accumulation.List Accumulation.Required ->
                    SynBinding.basic
                        [ pf.TargetVariable ]
                        []
                        (SynExpr.createIdent' pf.TargetVariable
                         |> SynExpr.pipeThroughFunction (SynExpr.createLongIdent [ "Seq" ; "toList" ]))
                | Accumulation.Required ->
                    let errorMessage =
                        SynExpr.createIdent "sprintf"
                        |> SynExpr.applyTo (SynExpr.CreateConst "Required argument '%s' received no value")
                        |> SynExpr.applyTo pf.HumanReadableArgForm

                    [
                        SynMatchClause.create
                            (SynPat.named "None")
                            (SynExpr.sequential
                                [
                                    errorMessage
                                    |> SynExpr.pipeThroughFunction (
                                        SynExpr.dotGet "Add" (SynExpr.createIdent' argParseErrors)
                                    )
                                    unchecked
                                ])

                        SynMatchClause.create
                            (SynPat.nameWithArgs "Some" [ SynPat.named "x" ])
                            (SynExpr.createIdent "x")
                    ]
                    |> SynExpr.createMatch (SynExpr.createIdent' pf.TargetVariable)
                    |> SynBinding.basic [ pf.TargetVariable ] []
            )

        let freezePositional =
            match pos with
            | None ->
                // Check if there are leftover args. If there are, throw.
                let errorMessage =
                    SynExpr.createIdent' leftoverArgsName
                    |> SynExpr.pipeThroughFunction (
                        SynExpr.applyFunction
                            (SynExpr.createLongIdent [ "String" ; "concat" ])
                            (SynExpr.CreateConst " ")
                    )
                    |> SynExpr.pipeThroughFunction (
                        SynExpr.applyFunction
                            (SynExpr.createIdent "sprintf")
                            (SynExpr.CreateConst "There were leftover args: %s")
                    )

                SynExpr.ifThenElse
                    (SynExpr.dotGet "Count" (SynExpr.createIdent' leftoverArgsName)
                     |> SynExpr.equals (SynExpr.CreateConst 0))
                    (SynExpr.sequential
                        [
                            errorMessage
                            |> SynExpr.pipeThroughFunction (SynExpr.dotGet "Add" (SynExpr.createIdent' argParseErrors))
                            unchecked
                        ])
                    (SynExpr.CreateConst ())
            | Some _ ->
                SynExpr.createIdent' leftoverArgsName
                |> SynExpr.pipeThroughFunction (SynExpr.createLongIdent [ "Seq" ; "toList" ])
            |> SynBinding.basic [ leftoverArgsName ] []
            |> List.singleton

        let freezeArgs = freezePositional @ freezeNonPositionalArgs

        let retValue =
            let happyPath =
                { new ParseTreeEval<_> with
                    member _.Eval tree = ParseTree.instantiate tree
                }
                |> spec.Apply

            let sadPath =
                SynExpr.createIdent' argParseErrors
                |> SynExpr.pipeThroughFunction (
                    SynExpr.applyFunction (SynExpr.createLongIdent [ "String" ; "concat" ]) (SynExpr.CreateConst @"\n")
                )
                |> SynExpr.pipeThroughFunction (
                    SynExpr.createIdent "failwithf"
                    |> SynExpr.applyTo (SynExpr.CreateConst @"Errors during parse!\n%s")
                )

            let areErrors =
                SynExpr.dotGet "Count" (SynExpr.createIdent' argParseErrors)
                |> SynExpr.equals (SynExpr.CreateConst 0)

            SynExpr.ifThenElse areErrors sadPath happyPath

        let flags =
            nonPos
            |> List.choose (fun pf ->
                match pf.TargetType with
                | PrimitiveType pt ->
                    if (pt |> List.map _.idText) = [ "System" ; "Boolean" ] then
                        Some (pf, SynExpr.CreateConst true)
                    else
                        None
                | ty ->
                    match identifyAsFlag flagDus ty with
                    | Some flag -> (pf, FlagDu.FromBoolean flag (SynExpr.CreateConst true)) |> Some
                    | _ -> None
            )

        let leftoverArgAcc =
            match pos with
            | None -> ChoicePositional.Normal None
            | Some pos -> pos.Accumulation

        [
            SynExpr.createIdent "go"
            |> SynExpr.applyTo (SynExpr.createLongIdent' [ parseState ; Ident.create "AwaitingKey" ])
            |> SynExpr.applyTo (SynExpr.createIdent "args")

            SynExpr.createLet freezeArgs retValue
        ]
        |> SynExpr.sequential
        |> SynExpr.createLet (
            bindings
            @ [
                processKeyValue argParseErrors pos nonPos
                setFlagValue argParseErrors flags
                mainLoop parseState argParseErrors leftoverArgAcc leftoverArgsName leftoverArgsParser
            ]
        )

    // The type for which we're generating args may refer to any of the supplied records/unions.
    let createModule
        (opens : SynOpenDeclTarget list)
        (ns : LongIdent)
        ((taggedType : SynTypeDefn, spec : ArgParserOutputSpec))
        (allUnionTypes : UnionType list)
        (allRecordTypes : RecordType list)
        : SynModuleOrNamespace
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
                createRecordParse parseStateIdent flagDus allRecordTypes taggedType
                |> SynBinding.basic
                    [ Ident.create "parse'" ]
                    [
                        SynPat.named "getEnvironmentVariable"
                        |> SynPat.annotateType (SynType.funFromDomain SynType.string (SynType.option SynType.string))
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
                |> SynExpr.applyTo (
                    SynExpr.paren (
                        SynExpr.createLongIdent [ "System" ; "Environment" ; "GetEnvironmentVariable" ]
                        |> SynExpr.composeWith (SynExpr.createLongIdent [ "Option" ; "ofObj" ])
                    )
                )
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

            let types = Ast.getTypes ast

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

            let modules =
                namespaceAndTypes
                |> List.map (fun (ns, taggedType, unions, records) ->
                    ArgParserGenerator.createModule opens ns taggedType unions records
                )

            Output.Ast modules
