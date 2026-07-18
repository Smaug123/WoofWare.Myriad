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
    /// From calling the static member `{owner}.Default{name}()`, where `owner` is the record
    /// type which declares the field (not necessarily the [<ArgParser>]-tagged root type: the
    /// field may live in a nested record, or in a union case's payload record).
    /// For example, if `type MyArgs = { Thing : Choice<int, int> }`, then
    /// we would use `MyArgs.DefaultThing () : int`.
    | FunctionCall of owner : Ident * name : Ident

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
        /// If true, this boolean/flag field accepts --no- prefix for negation (has [<ArgumentNegateWithPrefix>])
        AcceptsNegation : bool
    }

    /// A SynExpr of type `string` which we can display to the user at generated-program runtime to display all
    /// the ways they can refer to this arg.
    member arg.HumanReadableArgForm : SynExpr =
        if arg.AcceptsNegation then
            // Include both standard and --no- variants
            // E.g., "--foo / --bar / --no-foo / --no-bar"
            let standardFormatString =
                List.replicate arg.ArgForm.Length "--%s" |> String.concat " / "

            let negatedFormatString =
                List.replicate arg.ArgForm.Length "--no-%s" |> String.concat " / "

            let combinedFormatString = standardFormatString + " / " + negatedFormatString

            // Apply all arg forms twice (once for standard, once for negated)
            let allArgForms = arg.ArgForm @ arg.ArgForm

            (SynExpr.applyFunction (SynExpr.createIdent "sprintf") (SynExpr.CreateConst combinedFormatString),
             allArgForms)
            ||> List.fold SynExpr.applyFunction
            |> SynExpr.paren
        else
            // Standard behavior: just --foo / --bar
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
    /// A discriminated-union arg: at runtime, exactly one case is selected by the arguments
    /// which were supplied. `sumId` ties this node to the erased schema's Sum node; `assemble`
    /// builds the union value from the selected case's name and its assembled payload.
    /// Positional args are not permitted inside union cases.
    | Sum of
        sumId : int *
        cases : (Ident * ParseTree<HasNoPositional>) list *
        assemble : (Ident -> SynExpr -> SynExpr) *
        Teq<'hasPositional, HasNoPositional>

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
                        | ParseTree.Branch (_, _, teq)
                        | ParseTree.Sum (_, _, _, teq) ->
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
        | ParseTree.Sum (_, cases, _, _) -> cases |> List.collect (snd >> accumulatorsNonPos)

    /// Returns the positional arg separately.
    let rec accumulatorsPos
        (tree : ParseTree<HasPositional>)
        : ParseFunctionNonPositional list * ParseFunctionPositional
        =
        match tree with
        | ParseTree.PositionalLeaf (pf, _) -> [], pf
        | ParseTree.NonPositionalLeaf (_, teq) -> exFalso' teq
        | ParseTree.Branch (_, _, teq) -> exFalso' teq
        | ParseTree.Sum (_, _, _, teq) -> exFalso' teq
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
        | ParseTree.Sum (_, cases, _, _) -> cases |> List.collect (snd >> accumulatorsNonPos) |> (fun i -> i, None)
        | ParseTree.BranchPos (_, tree, trees, _, _) ->
            let nonPos = trees |> List.collect (snd >> accumulatorsNonPos)

            let nonPos2, pos = accumulatorsPos tree
            nonPos @ nonPos2, Some pos

        |> fun (nonPos, pos) ->
            // Reject argument names which could collide at parse time. The scanner matches names
            // case-insensitively, so this validation must use the same equality; and a name can
            // collide with the `--no-` variant of a negatable argument as well as with another
            // name directly. Only literal forms are checkable here: a form supplied via e.g. a
            // [<Literal>] constant is invisible to the untyped AST, so the generated code
            // re-checks the assembled schema at runtime (WellFormedSchema.checkOrFail).
            let literalForms (exprs : SynExpr list) : string list =
                exprs
                |> List.choose (fun expr ->
                    match expr |> SynExpr.stripOptionalParen with
                    | SynExpr.Const (SynConst.String (s, _, _), _) -> Some s
                    | _ -> None
                )

            // Reject names no token could ever address, given how the scanner tokenises.
            let malformed =
                (nonPos
                 |> List.collect (fun pf ->
                     literalForms pf.ArgForm
                     |> List.choose (fun form ->
                         if form = "" then
                             Some
                                 $"Invalid argument name for field '%s{pf.FieldName.idText}': an empty name's token would be '--', which is the positional separator."
                         elif form.Contains "=" then
                             Some
                                 $"Invalid argument name '%s{form}' for field '%s{pf.FieldName.idText}': a --key=value token splits at its first '=', so this argument could never be addressed."
                         else
                             None
                     )
                 ))
                @ (pos
                   |> Option.toList
                   |> List.collect (fun pf ->
                       literalForms pf.ArgForm
                       |> List.choose (fun form ->
                           if form = "" then
                               Some
                                   $"Invalid argument name for the positional args (field '%s{pf.FieldName.idText}'): an empty name's token would be '--', which is the positional separator."
                           elif form.Contains "=" then
                               Some
                                   $"Invalid argument name '%s{form}' for the positional args (field '%s{pf.FieldName.idText}'): a --key=value token splits at its first '=', so this argument could never be addressed."
                           else
                               None
                       )
                   ))

            match malformed with
            | [] -> ()
            | malformed -> failwith (String.concat "\n" malformed)

            // `--help` always means help (case-insensitively), so no argument may claim it.
            let allLiteralForms =
                (pos |> Option.toList |> List.collect (fun pf -> literalForms pf.ArgForm))
                @ (nonPos |> List.collect (fun pf -> literalForms pf.ArgForm))

            let helpClaims =
                allLiteralForms
                |> List.filter (fun form ->
                    System.String.Equals (form, "help", System.StringComparison.OrdinalIgnoreCase)
                )

            match helpClaims with
            | [] -> ()
            | _ -> failwith "The argument name 'help' is reserved: --help always displays the help text."

            // Every name a `--token` could address, with a description of its claimant, in
            // declaration order.
            let claims : (string * string) list =
                (nonPos
                 |> List.collect (fun pf ->
                     let forms = literalForms pf.ArgForm

                     let plain =
                         forms
                         |> List.map (fun form -> form, $"'--%s{form}' (field '%s{pf.FieldName.idText}')")

                     let negated =
                         if pf.AcceptsNegation then
                             forms
                             |> List.map (fun form ->
                                 $"no-%s{form}",
                                 $"the --no- variant of field '%s{pf.FieldName.idText}' (which has [<ArgumentNegateWithPrefix>])"
                             )
                         else
                             []

                     plain @ negated
                 ))
                @ (pos
                   |> Option.toList
                   |> List.collect (fun pf ->
                       literalForms pf.ArgForm
                       |> List.map (fun form -> form, $"'--%s{form}' (the positional args)")
                   ))

            // Group under the scanner's own equality (OrdinalIgnoreCase), preserving
            // declaration order. This is deliberately not ToUpperInvariant keying, which is a
            // strictly coarser relation: e.g. "s" and "ſ" (long s) uppercase to the same string,
            // but the scanner considers them distinct, so they do not collide.
            let conflicts =
                let indexOf =
                    System.Collections.Generic.Dictionary<string, int> (StringComparer.OrdinalIgnoreCase)

                let buckets = ResizeArray<ResizeArray<string * string>> ()

                for form, claimant in claims do
                    match indexOf.TryGetValue form with
                    | true, index -> buckets.[index].Add ((form, claimant))
                    | false, _ ->
                        indexOf.[form] <- buckets.Count
                        let bucket = ResizeArray ()
                        bucket.Add ((form, claimant))
                        buckets.Add bucket

                buckets
                |> Seq.choose (fun bucket ->
                    if bucket.Count < 2 then
                        None
                    else
                        let form, _ = bucket.[0]

                        bucket
                        |> Seq.map snd
                        |> String.concat "; "
                        |> sprintf "The argument name '--%s' is claimed by: %s" form
                        |> Some
                )
                |> List.ofSeq

            match conflicts with
            | [] -> ()
            | conflicts ->
                let conflictMessages = conflicts |> String.concat "\n"

                failwith
                    $"Conflicting argument names detected (names are matched case-insensitively):\n%s{conflictMessages}"

            nonPos, pos

    /// Assert that this tree contains no positional leaves, so that it can inhabit a union
    /// case; fails with `message` otherwise.
    let assertNoPositional (message : string) (crate : ParseTreeCrate) : ParseTree<HasNoPositional> =
        { new ParseTreeEval<_> with
            member _.Eval (t : ParseTree<'a>) =
                match t with
                | ParseTree.NonPositionalLeaf (_, teq)
                | ParseTree.Branch (_, _, teq)
                | ParseTree.Sum (_, _, _, teq) -> Teq.cast (cast teq) t
                | ParseTree.PositionalLeaf _
                | ParseTree.BranchPos _ -> failwith message
        }
        |> crate.Apply

    /// Does this tree contain any discriminated-union node?
    let rec containsSum<'a> (tree : ParseTree<'a>) : bool =
        match tree with
        | ParseTree.NonPositionalLeaf _
        | ParseTree.PositionalLeaf _ -> false
        | ParseTree.Sum _ -> true
        | ParseTree.Branch (fields, _, _) -> fields |> List.exists (snd >> containsSum)
        | ParseTree.BranchPos (_, posTree, fields, _, _) ->
            containsSum posTree || (fields |> List.exists (snd >> containsSum))

    /// Can this tree be satisfied by supplying no arguments at all? (Defaulted and optional
    /// leaves need nothing; a union needs nothing iff some case needs nothing.)
    let rec emptySatisfiable<'a> (tree : ParseTree<'a>) : bool =
        match tree with
        | ParseTree.NonPositionalLeaf (pf, _) ->
            match pf.Accumulation with
            | Accumulation.Required -> false
            | Accumulation.Optional
            | Accumulation.Choice _
            | Accumulation.List _ -> true
        | ParseTree.PositionalLeaf _ -> true
        | ParseTree.Branch (fields, _, _) -> fields |> List.forall (snd >> emptySatisfiable)
        | ParseTree.BranchPos (_, posTree, fields, _, _) ->
            emptySatisfiable posTree && (fields |> List.forall (snd >> emptySatisfiable))
        | ParseTree.Sum (_, cases, _, _) -> cases |> List.exists (snd >> emptySatisfiable)

    /// For every union node in the tree, at most one case may be satisfiable with no arguments:
    /// were two cases so satisfiable, an empty command line could not choose between them.
    let rec checkSumAmbiguity<'a> (tree : ParseTree<'a>) : unit =
        match tree with
        | ParseTree.NonPositionalLeaf _
        | ParseTree.PositionalLeaf _ -> ()
        | ParseTree.Branch (fields, _, _) -> fields |> List.iter (snd >> checkSumAmbiguity)
        | ParseTree.BranchPos (_, posTree, fields, _, _) ->
            checkSumAmbiguity posTree
            fields |> List.iter (snd >> checkSumAmbiguity)
        | ParseTree.Sum (_, cases, _, _) ->
            cases |> List.iter (snd >> checkSumAmbiguity)

            match cases |> List.filter (snd >> emptySatisfiable) with
            | []
            | [ _ ] -> ()
            | ambiguous ->
                let names =
                    ambiguous |> List.map (fun (name, _) -> name.idText) |> String.concat ", "

                failwith
                    $"Cases %s{names} can all be satisfied without supplying any arguments, so an empty command line cannot choose between them. Make an argument in all but one of them mandatory."

    /// Build the expression for the erased-schema tree mirroring this parse tree. Leaf ids are
    /// assigned in `accumulators` order, which is exactly this walk's traversal order. `rt`
    /// resolves a path inside the embedded runtime module; `listOf` builds a list literal (with
    /// the empty list handled). The positional sink is not part of the erased tree, hence the
    /// option.
    let rec toErasedTreeExpr<'a>
        (rt : string list -> SynExpr)
        (listOf : SynExpr list -> SynExpr)
        (counter : int ref)
        (tree : ParseTree<'a>)
        : SynExpr option
        =
        let product (children : SynExpr list) : SynExpr =
            SynExpr.applyFunction (rt [ "ErasedTree" ; "Product" ]) (SynExpr.paren (listOf children))

        match tree with
        | ParseTree.NonPositionalLeaf _ ->
            let index = counter.Value
            counter.Value <- counter.Value + 1

            SynExpr.applyFunction (rt [ "ErasedTree" ; "Leaf" ]) (SynExpr.CreateConst index)
            |> Some
        | ParseTree.PositionalLeaf _ -> None
        | ParseTree.Branch (fields, _, _) ->
            fields
            |> List.choose (fun (_, child) -> toErasedTreeExpr rt listOf counter child)
            |> product
            |> Some
        | ParseTree.Sum (sumId, cases, _, _) ->
            let caseExprs =
                cases
                |> List.map (fun (caseName, payload) ->
                    let payloadExpr =
                        toErasedTreeExpr rt listOf counter payload |> Option.defaultValue (product [])

                    SynExpr.tuple [ SynExpr.CreateConst caseName.idText ; payloadExpr ]
                )

            SynExpr.applyFunction
                (rt [ "ErasedTree" ; "Sum" ])
                (SynExpr.paren (SynExpr.tuple [ SynExpr.CreateConst sumId ; listOf caseExprs ]))
            |> Some
        | ParseTree.BranchPos (_, posTree, fields, _, _) ->
            // Ordering matches `accumulators`: the sibling fields first, then any named leaves
            // inside the positional subtree.
            let siblings =
                fields
                |> List.choose (fun (_, child) -> toErasedTreeExpr rt listOf counter child)

            let fromPos = toErasedTreeExpr rt listOf counter posTree |> Option.toList
            product (siblings @ fromPos) |> Some

    /// Build the return value. (References the `parser_selection` binding which the generated
    /// code brings into scope on the success path, to choose among Sum cases.)
    let rec instantiate<'a> (tree : ParseTree<'a>) : SynExpr =
        let unwrapRequired (targetVariable : Ident) : SynExpr =
            SynExpr.createMatch
                (SynExpr.createIdent' targetVariable)
                [
                    SynMatchClause.create (SynPat.nameWithArgs "Some" [ SynPat.named "x" ]) (SynExpr.createIdent "x")
                    SynMatchClause.create
                        (SynPat.named "None")
                        (SynExpr.applyFunction
                            (SynExpr.createIdent "failwith")
                            (SynExpr.CreateConst
                                "WoofWare.Myriad internal error in generated parser: required argument missing after successful parse"))
                ]
            |> SynExpr.paren

        match tree with
        | ParseTree.NonPositionalLeaf (pf, _) ->
            // The unwrap happens here, at the use site, rather than eagerly for every slot: the
            // slots of a union's unselected cases are legitimately unpopulated and must never be
            // read.
            match pf.Accumulation with
            | Accumulation.Required
            | Accumulation.Choice _ -> unwrapRequired pf.TargetVariable
            | Accumulation.Optional -> SynExpr.createIdent' pf.TargetVariable
            | Accumulation.List _ ->
                SynExpr.createIdent' pf.TargetVariable
                |> SynExpr.pipeThroughFunction (SynExpr.createLongIdent [ "Seq" ; "toList" ])
                |> SynExpr.paren
        | ParseTree.PositionalLeaf (pf, _) ->
            SynExpr.createIdent' pf.TargetVariable
            |> SynExpr.pipeThroughFunction (SynExpr.createLongIdent [ "Seq" ; "toList" ])
            |> SynExpr.paren
        | ParseTree.Sum (sumId, cases, assemble, _) ->
            let scrutinee =
                SynExpr.createLongIdent [ "Map" ; "tryFind" ]
                |> SynExpr.applyTo (SynExpr.CreateConst sumId)
                |> SynExpr.applyTo (SynExpr.dotGet "Choices" (SynExpr.createIdent "parser_selection"))

            let clauses =
                cases
                |> List.mapi (fun index (caseName, payload) ->
                    SynMatchClause.create
                        (SynPat.nameWithArgs "Some" [ SynPat.createConst (SynConst.Int32 index) ])
                        (assemble caseName (SynExpr.paren (instantiate payload)))
                )

            let fallthrough =
                SynMatchClause.create
                    SynPat.anon
                    (SynExpr.applyFunction
                        (SynExpr.createIdent "failwith")
                        (SynExpr.CreateConst
                            "WoofWare.Myriad internal error in generated parser: no case selected despite a successful parse"))

            SynExpr.createMatch scrutinee (clauses @ [ fallthrough ])
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

    /// A type defined alongside the tagged type is referred to by its bare name, so only a
    /// single-segment reference (possibly parenthesized) may resolve to a local type. Matching
    /// anything less than the complete reference would let e.g. a local union named `Uri`
    /// capture a field of the foreign type `System.Uri`.
    let private localTypeName (ty : SynType) : string option =
        match SynType.stripOptionalParen ty with
        | SynType.LongIdent (SynLongIdent.SynLongIdent ([ ident ], _, _)) -> Some ident.idText
        | _ -> None

    let private identifyAsFlag (flagDus : FlagDu list) (ty : SynType) : FlagDu option =
        match localTypeName ty with
        | Some name -> flagDus |> List.tryFind (fun du -> du.Name.idText = name)
        | None -> None

    /// Builds a function or lambda of one string argument, which returns a `ty` (as modified by the `Accumulation`;
    /// for example, maybe it returns a `ty option` or a `ty list`).
    /// The resulting SynType is the type of the *element* being parsed; so if the Accumulation is List, the SynType
    /// is the list element.
    let rec private createParseFunction<'choice>
        (choice : ArgumentDefaultSpec option -> 'choice)
        (flagDus : FlagDu list)
        (owner : Ident)
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
                createParseFunction choice flagDus owner fieldName attrs eltTy

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

                let parseElt, acc, childTy =
                    createParseFunction choice flagDus owner fieldName attrs elt1

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
                            ArgumentDefaultSpec.FunctionCall (owner, Ident.create ("Default" + fieldName.idText))
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
                createParseFunction choice flagDus owner fieldName attrs eltTy

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

    /// An argument schema must be a finite tree: a record or union which refers to itself, even
    /// indirectly, would expand forever. `ancestors` is the chain of type names currently being
    /// lowered, innermost first; re-entry into any of them is a cycle, which we reject rather
    /// than dying with a stack overflow. (Names are the bare idText, matching the by-name lookup
    /// which resolves ambient type references.)
    let private pushSchemaType (ancestors : string list) (name : Ident) : string list =
        if ancestors |> List.contains name.idText then
            let path = name.idText :: ancestors |> List.rev |> String.concat " -> "

            failwith
                $"The [<ArgParser>] schema is recursive: %s{path}. Argument records and unions may not contain themselves, even indirectly."

        name.idText :: ancestors

    let rec private toParseSpec
        (ancestors : string list)
        (counter : int)
        (flagDus : FlagDu list)
        (ambientUnions : UnionType list)
        (ambientRecords : RecordType list)
        (finalRecord : RecordType)
        : ParseTreeCrate * int
        =
        let ancestors = pushSchemaType ancestors finalRecord.Name

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

                // A default-value attribute is only meaningful on a `Choice<'a, 'a>` field: a
                // successful parse reports whether the value was user-supplied (Choice1Of2) or
                // defaulted (Choice2Of2). The Choice-parsing path is the sole place these
                // attributes are read, so on any other field they would be silently dropped,
                // leaving the field required. Reject them here rather than emitting a parser in
                // which the attribute has no effect.
                let hasDefaultAttr =
                    attrs
                    |> List.exists (fun attr ->
                        match (List.last attr.TypeName.LongIdent).idText with
                        | "ArgumentDefaultFunction"
                        | "ArgumentDefaultFunctionAttribute"
                        | "ArgumentDefaultEnvironmentVariable"
                        | "ArgumentDefaultEnvironmentVariableAttribute" -> true
                        | _ -> false
                    )

                if hasDefaultAttr then
                    match positionalArgAttr, fieldType with
                    | Some _, _ ->
                        failwith
                            $"Field '%s{ident.idText}' is positional, so it cannot carry a default-value attribute ([<ArgumentDefaultFunction>] or [<ArgumentDefaultEnvironmentVariable>]): positional args are collected, not defaulted."
                    | None, ChoiceType _ -> ()
                    | None, _ ->
                        failwith
                            $"Field '%s{ident.idText}' has a default-value attribute ([<ArgumentDefaultFunction>] or [<ArgumentDefaultEnvironmentVariable>]), but its type is not Choice<'a, 'a>. Defaults are surfaced through Choice<'a, 'a> so a successful parse can report whether a value was user-supplied (Choice1Of2) or defaulted (Choice2Of2); a bare field cannot express this. Change the field's type to Choice<'a, 'a>, or remove the attribute."

                let ambientRecordMatch =
                    match localTypeName fieldType with
                    | Some target -> ambientRecords |> List.tryFind (fun r -> r.Name.idText = target)
                    | None -> None

                let ambientUnionMatch =
                    match localTypeName fieldType with
                    | Some target -> ambientUnions |> List.tryFind (fun u -> u.Name.idText = target)
                    | None -> None

                match ambientRecordMatch with
                | Some ambient ->
                    // This field has a type we need to obtain from parsing another record.
                    let spec, counter =
                        toParseSpec ancestors counter flagDus ambientUnions ambientRecords ambient

                    counter, (ident, spec) :: acc
                | None ->

                match ambientUnionMatch with
                | Some union ->
                    // A discriminated union of alternative argument sets: exactly one case's
                    // arguments must be supplied.
                    let spec, counter =
                        unionToParseSpec ancestors counter flagDus ambientUnions ambientRecords union

                    counter, (ident, spec) :: acc
                | None ->

                match positionalArgAttr with
                | Some includeFlagLike ->
                    // Positional fields carrying a default attribute are rejected above, so the
                    // Choice-parsing path only ever reaches this callback with `None`.
                    let getChoice (_ : ArgumentDefaultSpec option) : unit = ()

                    let parser, accumulation, parseTy =
                        createParseFunction<unit> getChoice flagDus finalRecord.Name ident attrs fieldType

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
                            AcceptsNegation = false
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
                            AcceptsNegation = false
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
                        createParseFunction getChoice flagDus finalRecord.Name ident attrs fieldType

                    let isBoolLike =
                        match parseTy with
                        | PrimitiveType ident when ident |> List.map _.idText = [ "System" ; "Boolean" ] ->
                            Some (Choice2Of2 ())
                        | parseTy -> identifyAsFlag flagDus parseTy |> Option.map Choice1Of2

                    let hasNegateAttr =
                        attrs
                        |> List.exists (fun attr ->
                            match attr.TypeName with
                            | SynLongIdent.SynLongIdent (ident, _, _) ->
                                match (List.last ident).idText with
                                | "ArgumentNegateWithPrefixAttribute"
                                | "ArgumentNegateWithPrefix" -> true
                                | _ -> false
                        )

                    let acceptsNegation =
                        if hasNegateAttr then
                            match isBoolLike with
                            | Some _ -> true
                            | None ->
                                failwith
                                    $"[<ArgumentNegateWithPrefix>] can only be applied to boolean or flag DU fields, but was applied to field %s{ident.idText} of type %O{fieldType}"
                        else
                            false

                    {
                        FieldName = ident
                        Parser = parser
                        TargetVariable = Ident.create $"arg_%i{counter}"
                        Accumulation = accumulation
                        TargetType = parseTy
                        ArgForm = longForms
                        Help = helpText
                        BoolCases = isBoolLike
                        AcceptsNegation = acceptsNegation
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
    /// Lower a discriminated union, each of whose cases must carry exactly one field whose type
    /// is a record defined alongside it, into a Sum parse-tree node: exactly one case's
    /// arguments must be supplied at runtime.
    and private unionToParseSpec
        (ancestors : string list)
        (counter : int)
        (flagDus : FlagDu list)
        (ambientUnions : UnionType list)
        (ambientRecords : RecordType list)
        (union : UnionType)
        : ParseTreeCrate * int
        =
        let ancestors = pushSchemaType ancestors union.Name

        let sumId = counter
        let counter = counter + 1

        let counter, cases =
            ((counter, []), union.Cases)
            ||> List.fold (fun (counter, acc) case ->
                let payloadRecord =
                    match case.Fields with
                    | [ field ] ->
                        let payload =
                            match localTypeName field.Type with
                            | Some target -> ambientRecords |> List.tryFind (fun r -> r.Name.idText = target)
                            | None -> None

                        match payload with
                        | Some payload -> payload
                        | None ->
                            failwith
                                $"Case %s{case.Name.idText} of [<ArgParser>] union %s{union.Name.idText} must have a payload which is a record defined alongside the union."
                    | _ ->
                        failwith
                            $"Case %s{case.Name.idText} of [<ArgParser>] union %s{union.Name.idText} must have exactly one field: a record holding that case's arguments."

                let spec, counter =
                    toParseSpec ancestors counter flagDus ambientUnions ambientRecords payloadRecord

                let tree =
                    ParseTree.assertNoPositional
                        $"Positional args are not permitted inside cases of the [<ArgParser>] union %s{union.Name.idText}: the parser could not tell which alternative a positional arg belongs to."
                        spec

                counter, (case.Name, tree) :: acc
            )

        let cases = List.rev cases

        let assemble (caseName : Ident) (payload : SynExpr) : SynExpr =
            SynExpr.applyFunction (SynExpr.createLongIdent' [ union.Name ; caseName ]) payload

        ParseTree.Sum (sumId, cases, assemble, Teq.refl) |> ParseTreeCrate.make, counter

    let private helpText<'hasPositional> (typeHelp : SynExpr option) (tree : ParseTree<'hasPositional>) : SynBinding =
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
            | Accumulation.Choice (ArgumentDefaultSpec.FunctionCall (owner, var)) ->
                match flagCases with
                | None -> SynExpr.callMethod var.idText (SynExpr.createIdent' owner)
                | Some (Choice2Of2 ()) -> SynExpr.callMethod var.idText (SynExpr.createIdent' owner)
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
                    |> SynExpr.createMatch (SynExpr.callMethod var.idText (SynExpr.createIdent' owner))
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
        /// `depth` is the nesting depth in union alternatives; each level indents by two spaces.
        let toPrintable
            (depth : int)
            (describe : 'a -> Choice<FlagDu, unit> option -> SynExpr)
            (arg : ParseFunction<'a>)
            : SynExpr
            =
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

            let indent = String.replicate depth "  "

            SynExpr.applyFunction (SynExpr.createIdent "sprintf") (SynExpr.CreateConst $"%s{indent}%%s  %s{ty}%%s%%s")
            |> SynExpr.applyTo arg.HumanReadableArgForm
            |> SynExpr.applyTo descriptor
            |> SynExpr.applyTo helpText
            |> SynExpr.paren

        // Walk the tree so that a union's alternatives are *grouped* in the help, not flattened
        // into one undifferentiated list: the user must be able to see which arguments go
        // together. Non-positional lines appear in declaration order; the positional-args line,
        // if any, comes last, as it always has. (Positionals cannot coexist with unions, so the
        // two features never interleave.)
        let rec fieldHelpNonPos (depth : int) (tree : ParseTree<HasNoPositional>) : SynExpr list =
            match tree with
            | ParseTree.NonPositionalLeaf (pf, _) -> [ toPrintable depth describeNonPositional pf ]
            | ParseTree.PositionalLeaf (_, teq) -> exFalso teq
            | ParseTree.Branch (fields, _, _) -> fields |> List.collect (fun (_, child) -> fieldHelpNonPos depth child)
            | ParseTree.BranchPos (_, _, _, _, teq) -> exFalso teq
            | ParseTree.Sum (_, cases, _, _) -> sumHelp depth cases

        and sumHelp (depth : int) (cases : (Ident * ParseTree<HasNoPositional>) list) : SynExpr list =
            let indent = String.replicate depth "  "

            SynExpr.CreateConst (indent + "exactly one of the following sets of arguments:")
            :: (cases
                |> List.collect (fun (caseName, case) ->
                    SynExpr.CreateConst (indent + caseName.idText + ":")
                    :: fieldHelpNonPos (depth + 1) case
                ))

        let rec fieldHelpPos (tree : ParseTree<HasPositional>) : SynExpr list * SynExpr =
            match tree with
            | ParseTree.PositionalLeaf (pf, _) -> [], toPrintable 0 describePositional pf
            | ParseTree.NonPositionalLeaf (_, teq) -> exFalso' teq
            | ParseTree.Branch (_, _, teq) -> exFalso' teq
            | ParseTree.BranchPos (_, posTree, fields, _, _) ->
                let nonPos = fields |> List.collect (fun (_, child) -> fieldHelpNonPos 0 child)

                let nonPos2, pos = fieldHelpPos posTree
                nonPos @ nonPos2, pos
            | ParseTree.Sum (_, _, _, teq) -> exFalso' teq

        let fieldHelp =
            match tree with
            | ParseTree.NonPositionalLeaf (pf, _) -> [ toPrintable 0 describeNonPositional pf ]
            | ParseTree.PositionalLeaf (pf, _) -> [ toPrintable 0 describePositional pf ]
            | ParseTree.Branch (fields, _, _) -> fields |> List.collect (fun (_, child) -> fieldHelpNonPos 0 child)
            | ParseTree.BranchPos (_, posTree, fields, _, _) ->
                let nonPos = fields |> List.collect (fun (_, child) -> fieldHelpNonPos 0 child)

                let nonPos2, pos = fieldHelpPos posTree
                nonPos @ nonPos2 @ [ pos ]
            | ParseTree.Sum (_, cases, _, _) -> sumHelp 0 cases

        let allHelp =
            match typeHelp with
            | Some helpExpr ->
                // Prepend type help, followed by blank line, then field help
                [ helpExpr ; SynExpr.CreateConst "" ] @ fieldHelp
            | None ->
                // No type help, just field help
                fieldHelp

        allHelp
        |> SynExpr.listLiteral
        |> SynExpr.pipeThroughFunction (
            SynExpr.applyFunction (SynExpr.createLongIdent [ "String" ; "concat" ]) (SynExpr.CreateConst @"\n")
        )
        |> SynBinding.basic [ Ident.create "helpText" ] [ SynPat.unit ]

    /// Helper to create a negated parser for boolean/flag fields.
    /// Returns a SynExpr that represents: string -> (negated bool or negated flag DU)
    /// For booleans: `fun x -> not (Boolean.Parse x)`
    /// For flag DUs: `fun x -> FlagDu.FromBoolean flagDu (not (Boolean.Parse x))`
    let private createNegatedParser (arg : ParseFunction<'acc>) : SynExpr =
        match arg.BoolCases with
        | None -> failwith $"LOGIC ERROR: createNegatedParser called on non-boolean field %s{arg.FieldName.idText}"
        | Some (Choice2Of2 ()) ->
            // Boolean: parse and negate
            // fun x -> not (System.Boolean.Parse x)
            let parseExpr =
                SynExpr.createIdent "x"
                |> SynExpr.applyFunction (SynExpr.createLongIdent [ "System" ; "Boolean" ; "Parse" ])
                |> SynExpr.paren

            parseExpr
            |> SynExpr.applyFunction (SynExpr.createIdent "not")
            |> SynExpr.createLambda "x"
        | Some (Choice1Of2 flagDu) ->
            // Flag DU: parse as bool, negate, then convert to flag DU
            // fun x -> x |> System.Boolean.Parse |> not |> FlagDu.FromBoolean flagDu
            let parseExpr =
                SynExpr.createIdent "x"
                |> SynExpr.applyFunction (SynExpr.createLongIdent [ "System" ; "Boolean" ; "Parse" ])
                |> SynExpr.paren

            parseExpr
            |> SynExpr.applyFunction (SynExpr.createIdent "not")
            |> FlagDu.FromBoolean flagDu
            |> SynExpr.createLambda "x"

    /// `let processKeyValue (key : string) (value : string) : Result<unit, string option> = ...`
    /// Returns a possible error.
    /// A parse failure might not be fatal (e.g. maybe the input was optionally of arity 0, and we failed to do
    /// the parse because in fact the key decided not to take this argument); in that case we return Error None.
    /// Takes a single argument, `args : string list`, and returns something of the type indicated by `recordType`.
    let private createRecordParse
        (runtimeModule : Ident)
        (typeHelpText : SynExpr option)
        (typeName : Ident)
        (spec : ParseTreeCrate)
        : SynExpr
        =
        // For each argument (positional and non-positional), create an accumulator for it; also
        // check the structural constraints which the runtime's selection semantics rely on.
        let (nonPos, pos), hasSum =
            { new ParseTreeEval<_> with
                member _.Eval tree =
                    ParseTree.checkSumAmbiguity tree
                    ParseTree.accumulators tree, ParseTree.containsSum tree
            }
            |> spec.Apply

        if Option.isSome pos && hasSum then
            failwith
                "Positional args cannot be combined with a discriminated-union arg: the parser could not tell which alternative an unrecognised arg belongs to."

        let bindings =
            nonPos
            |> List.map (fun pf ->
                match pf.Accumulation with
                | Accumulation.Choice _ ->
                    // The slot holds the already-wrapped value: Choice1Of2 for a user-supplied
                    // value, Choice2Of2 for a default.
                    SynExpr.createIdent "None"
                    |> SynBinding.basic [ pf.TargetVariable ] []
                    |> SynBinding.withMutability true
                    |> SynBinding.withReturnAnnotation (
                        SynType.appPostfix "option" (SynType.app "Choice" [ pf.TargetType ; pf.TargetType ])
                    )
                | Accumulation.Required
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

        let helpText =
            { new ParseTreeEval<_> with
                member _.Eval tree = helpText typeHelpText tree
            }
            |> spec.Apply

        let bindings = helpText :: bindings

        /// A fully-qualified reference into the embedded runtime module.
        let rt (path : string list) : SynExpr =
            SynExpr.createLongIdent' (runtimeModule :: List.map Ident.create path)

        let rtPat (path : string list) (args : SynPat list) : SynPat =
            SynPat.identWithArgs (runtimeModule :: List.map Ident.create path) (SynArgPats.create args)

        let rtType (name : string) : SynType =
            SynType.createLongIdent [ runtimeModule ; Ident.create name ]

        let field (name : string) (value : SynExpr) : SynLongIdent * SynExpr = SynLongIdent.createS name, value

        /// Like SynExpr.listLiteral, but a valid expression for the empty list too.
        let listOf (elts : SynExpr list) : SynExpr =
            match elts with
            | [] -> SynExpr.createLongIdent [ "List" ; "empty" ]
            | elts -> SynExpr.listLiteral elts

        // Leaves are identified in the erased schema by their index into `nonPos`; the typed
        // slots (`bindings` above) are looked up by the same index.
        let indexed = List.indexed nonPos

        let schemaVar = Ident.create "parser_schema"

        let internalError (context : string) : SynExpr =
            SynExpr.applyFunction
                (SynExpr.createIdent "failwith")
                (SynExpr.CreateConst ("WoofWare.Myriad internal error in generated parser: " + context))

        let schemaBinding : SynBinding =
            let leaves =
                indexed
                |> List.map (fun (index, pf) ->
                    let requirement =
                        match pf.Accumulation with
                        | Accumulation.Required -> rt [ "ErasedRequirement" ; "Required" ]
                        | Accumulation.Optional -> rt [ "ErasedRequirement" ; "Optional" ]
                        | Accumulation.Choice _ -> rt [ "ErasedRequirement" ; "HasDefault" ]
                        | Accumulation.List _ -> rt [ "ErasedRequirement" ; "Optional" ]

                    let arity =
                        match pf.BoolCases with
                        | Some _ -> rt [ "ErasedArity" ; "BoolLike" ]
                        | None -> rt [ "ErasedArity" ; "One" ]

                    let repeatable =
                        match pf.Accumulation with
                        | Accumulation.List _ -> SynExpr.CreateConst true
                        | Accumulation.Required
                        | Accumulation.Optional
                        | Accumulation.Choice _ -> SynExpr.CreateConst false

                    [
                        field "Id" (SynExpr.CreateConst index)
                        field "Forms" (listOf pf.ArgForm)
                        field "AcceptsNegation" (SynExpr.CreateConst pf.AcceptsNegation)
                        field "Arity" arity
                        field "Repeatable" repeatable
                        field "Requirement" requirement
                        // Help rendering stays in this generated module, so the runtime's
                        // display metadata goes unused.
                        field "TypeDescription" (SynExpr.CreateConst "")
                        field "Help" (SynExpr.createIdent "None")
                    ]
                    |> SynExpr.createRecord None
                )
                |> listOf

            let tree =
                let counter = ref 0

                { new ParseTreeEval<_> with
                    member _.Eval tree =
                        ParseTree.toErasedTreeExpr rt listOf counter tree
                }
                |> spec.Apply
                |> Option.defaultValue (
                    SynExpr.applyFunction (rt [ "ErasedTree" ; "Product" ]) (SynExpr.paren (listOf []))
                )
                |> SynExpr.paren

            let positional =
                match pos with
                | None -> SynExpr.createIdent "None"
                | Some pf ->
                    let flagLike =
                        let includeFlagLike =
                            match pf.Accumulation with
                            | ChoicePositional.Normal fl
                            | ChoicePositional.Choice fl -> fl

                        match includeFlagLike with
                        | None -> rt [ "ErasedFlagLikeBehaviour" ; "Reject" ]
                        | Some cond ->
                            SynExpr.ifThenElse
                                cond
                                (rt [ "ErasedFlagLikeBehaviour" ; "Reject" ])
                                (rt [ "ErasedFlagLikeBehaviour" ; "Collect" ])
                            |> SynExpr.paren


                    // Unlike the leaf literals, this record is not in annotation-directed
                    // position (it flows through `Some`), and its type's module is not open, so
                    // every label must be qualified.
                    let positionalField (name : string) (value : SynExpr) : SynLongIdent * SynExpr =
                        SynLongIdent.create [ runtimeModule ; Ident.create "ErasedPositional" ; Ident.create name ],
                        value

                    [
                        positionalField "Id" (SynExpr.CreateConst (List.length nonPos))
                        positionalField "Forms" (listOf pf.ArgForm)
                        positionalField "FlagLike" flagLike
                        positionalField "TypeDescription" (SynExpr.CreateConst "")
                        positionalField "Help" (SynExpr.createIdent "None")
                    ]
                    |> SynExpr.createRecord None
                    |> SynExpr.paren
                    |> SynExpr.pipeThroughFunction (SynExpr.createIdent "Some")

            [ field "Leaves" leaves ; field "Tree" tree ; field "Positional" positional ]
            |> SynExpr.createRecord None
            |> SynBinding.basic [ schemaVar ] []
            |> SynBinding.withReturnAnnotation (rtType "ErasedSchema")

        let occurrenceField (name : string) : SynExpr =
            SynExpr.dotGet name (SynExpr.createIdent "occurrence")

        /// `Some (sprintf "%s (at arg %s)" exc.Message {source})`, for use inside a `with` handler.
        let conversionError (source : SynExpr) : SynExpr =
            SynExpr.createIdent "sprintf"
            |> SynExpr.applyTo (SynExpr.CreateConst "%s (at arg %s)")
            |> SynExpr.applyTo (SynExpr.createLongIdent [ "exc" ; "Message" ])
            |> SynExpr.applyTo source
            |> SynExpr.paren
            |> SynExpr.pipeThroughFunction (SynExpr.createIdent "Some")

        /// `try {store} ; None with exc -> Some (...)`
        let tryStore (source : SynExpr) (store : SynExpr) : SynExpr =
            SynExpr.sequential [ store ; SynExpr.createIdent "None" ]
            |> SynExpr.pipeThroughTryWith SynPat.anon (conversionError source)

        let storeOccurrenceBinding : SynBinding =
            let branches =
                indexed
                |> List.map (fun (index, pf) ->
                    // The typed value to store, as a function of `value` (the raw string) for
                    // valued occurrences; boolean-like leaves also handle the arity-0 case.
                    let wrapChoice (e : SynExpr) : SynExpr =
                        match pf.Accumulation with
                        | Accumulation.Choice _ ->
                            SynExpr.applyFunction (SynExpr.createIdent "Choice1Of2") (SynExpr.paren e)
                        | Accumulation.Required
                        | Accumulation.Optional
                        | Accumulation.List _ -> e

                    let store (e : SynExpr) : SynExpr =
                        match pf.Accumulation with
                        | Accumulation.List _ ->
                            SynExpr.paren (wrapChoice e)
                            |> SynExpr.applyFunction (
                                SynExpr.createLongIdent' [ pf.TargetVariable ; Ident.create "Add" ]
                            )
                        | Accumulation.Required
                        | Accumulation.Optional
                        | Accumulation.Choice _ ->
                            SynExpr.applyFunction (SynExpr.createIdent "Some") (SynExpr.paren (wrapChoice e))
                            |> SynExpr.assign (SynLongIdent.createI pf.TargetVariable)

                    let valued : SynExpr =
                        match pf.BoolCases with
                        | None ->
                            // Arity one: the value must be present; convert it with the leaf's parser.
                            SynExpr.createIdent "value"
                            |> SynExpr.pipeThroughFunction pf.Parser
                            |> store
                            |> tryStore (occurrenceField "Source")
                        | Some boolCases ->
                            // Parse the boolean, respect negation, then build the target value.
                            let ofBool (b : SynExpr) : SynExpr =
                                match boolCases with
                                | Choice2Of2 () -> b
                                | Choice1Of2 flag -> FlagDu.FromBoolean flag b |> SynExpr.paren

                            let parseAndStore =
                                SynExpr.createLet
                                    [
                                        SynExpr.createIdent "value"
                                        |> SynExpr.applyFunction (
                                            SynExpr.createLongIdent [ "System" ; "Boolean" ; "Parse" ]
                                        )
                                        |> SynBinding.basic [ Ident.create "parsedBool" ] []
                                        SynExpr.ifThenElse
                                            (occurrenceField "Negated")
                                            (SynExpr.createIdent "parsedBool")
                                            (SynExpr.applyFunction
                                                (SynExpr.createIdent "not")
                                                (SynExpr.createIdent "parsedBool"))
                                        |> SynBinding.basic [ Ident.create "parsedBool" ] []
                                    ]
                                    (store (ofBool (SynExpr.createIdent "parsedBool")))

                            parseAndStore |> tryStore (occurrenceField "Source")

                    let body =
                        match pf.BoolCases with
                        | None ->
                            SynExpr.createMatch
                                (occurrenceField "Value")
                                [
                                    SynMatchClause.create (SynPat.nameWithArgs "Some" [ SynPat.named "value" ]) valued
                                    SynMatchClause.create
                                        (SynPat.named "None")
                                        (internalError "arity-one occurrence with no value")
                                ]
                        | Some boolCases ->
                            let ofBool (b : SynExpr) : SynExpr =
                                match boolCases with
                                | Choice2Of2 () -> b
                                | Choice1Of2 flag -> FlagDu.FromBoolean flag b |> SynExpr.paren

                            let wrapChoice (e : SynExpr) : SynExpr =
                                match pf.Accumulation with
                                | Accumulation.Choice _ ->
                                    SynExpr.applyFunction (SynExpr.createIdent "Choice1Of2") (SynExpr.paren e)
                                | Accumulation.Required
                                | Accumulation.Optional
                                | Accumulation.List _ -> e

                            let arityZero =
                                let value =
                                    SynExpr.ifThenElse
                                        (occurrenceField "Negated")
                                        (ofBool (SynExpr.CreateConst true))
                                        (ofBool (SynExpr.CreateConst false))
                                    |> SynExpr.paren

                                SynExpr.sequential [ store value ; SynExpr.createIdent "None" ]

                            SynExpr.createMatch
                                (occurrenceField "Value")
                                [
                                    SynMatchClause.create (SynPat.nameWithArgs "Some" [ SynPat.named "value" ]) valued
                                    SynMatchClause.create (SynPat.named "None") arityZero
                                ]

                    // Non-repeatable leaves: the first occurrence wins; the runtime reports the
                    // duplicate, so a populated slot means we simply do nothing.
                    let guarded =
                        match pf.Accumulation with
                        | Accumulation.List _ -> body
                        | Accumulation.Required
                        | Accumulation.Optional
                        | Accumulation.Choice _ ->
                            SynExpr.createMatch
                                (SynExpr.createIdent' pf.TargetVariable)
                                [
                                    SynMatchClause.create
                                        (SynPat.nameWithArgs "Some" [ SynPat.anon ])
                                        (SynExpr.createIdent "None")
                                    SynMatchClause.create (SynPat.named "None") body
                                ]

                    SynMatchClause.create (SynPat.createConst (SynConst.Int32 index)) guarded
                )

            let fallthrough =
                SynMatchClause.create (SynPat.anon) (internalError "unknown argument id")

            SynExpr.createMatch (occurrenceField "LeafId") (branches @ [ fallthrough ])
            |> SynBinding.basic
                [ Ident.create "parser_storeOccurrence" ]
                [ SynPat.named "occurrence" |> SynPat.annotateType (rtType "ErasedOccurrence") ]
            |> SynBinding.withReturnAnnotation (SynType.appPostfix "option" SynType.string)

        let storePositionalBinding : SynBinding =
            let body =
                match pos with
                | None -> SynExpr.createIdent "None"
                | Some pf ->
                    let converted =
                        let plain = SynExpr.createIdent "value" |> SynExpr.pipeThroughFunction pf.Parser

                        match pf.Accumulation with
                        | ChoicePositional.Normal _ -> plain
                        | ChoicePositional.Choice _ ->
                            SynExpr.ifThenElse
                                (SynExpr.createIdent "afterSeparator")
                                (SynExpr.applyFunction (SynExpr.createIdent "Choice1Of2") (SynExpr.paren plain))
                                (SynExpr.applyFunction (SynExpr.createIdent "Choice2Of2") (SynExpr.paren plain))

                    SynExpr.paren converted
                    |> SynExpr.applyFunction (SynExpr.createLongIdent' [ leftoverArgsName ; Ident.create "Add" ])
                    |> tryStore (SynExpr.createIdent "value")

            body
            |> SynBinding.basic
                [ Ident.create "parser_storePositional" ]
                [
                    SynPat.named "value" |> SynPat.annotateType SynType.string
                    SynPat.named "afterSeparator" |> SynPat.annotateType SynType.bool
                ]
            |> SynBinding.withReturnAnnotation (SynType.appPostfix "option" SynType.string)

        let renderStoredBinding : SynBinding =
            let branches =
                indexed
                |> List.choose (fun (index, pf) ->
                    match pf.Accumulation with
                    | Accumulation.List _ -> None
                    | Accumulation.Choice _ ->
                        // Render the underlying value, not the Choice wrapper, to match the
                        // historical duplicate-argument message.
                        SynExpr.createMatch
                            (SynExpr.createIdent' pf.TargetVariable)
                            [
                                SynMatchClause.create
                                    (SynPat.nameWithArgs
                                        "Some"
                                        [ SynPat.paren (SynPat.nameWithArgs "Choice1Of2" [ SynPat.named "x" ]) ])
                                    (SynExpr.callMethod "ToString" (SynExpr.createIdent "x"))
                                SynMatchClause.create
                                    (SynPat.nameWithArgs
                                        "Some"
                                        [ SynPat.paren (SynPat.nameWithArgs "Choice2Of2" [ SynPat.named "x" ]) ])
                                    (SynExpr.callMethod "ToString" (SynExpr.createIdent "x"))
                                SynMatchClause.create (SynPat.named "None") (SynExpr.CreateConst "<no value>")
                            ]
                        |> SynMatchClause.create (SynPat.createConst (SynConst.Int32 index))
                        |> Some
                    | Accumulation.Required
                    | Accumulation.Optional ->
                        SynExpr.createMatch
                            (SynExpr.createIdent' pf.TargetVariable)
                            [
                                SynMatchClause.create
                                    (SynPat.nameWithArgs "Some" [ SynPat.named "x" ])
                                    (SynExpr.callMethod "ToString" (SynExpr.createIdent "x"))
                                SynMatchClause.create (SynPat.named "None") (SynExpr.CreateConst "<no value>")
                            ]
                        |> SynMatchClause.create (SynPat.createConst (SynConst.Int32 index))
                        |> Some
                )

            let fallthrough =
                SynMatchClause.create (SynPat.anon) (SynExpr.CreateConst "<no value>")

            SynExpr.createMatch (SynExpr.createIdent "leafId") (branches @ [ fallthrough ])
            |> SynBinding.basic
                [ Ident.create "parser_renderStored" ]
                [ SynPat.named "leafId" |> SynPat.annotateType SynType.int ]
            |> SynBinding.withReturnAnnotation SynType.string

        let applyDefaultBinding : SynBinding =
            let branches =
                indexed
                |> List.choose (fun (index, pf) ->
                    match pf.Accumulation with
                    | Accumulation.Required
                    | Accumulation.Optional
                    | Accumulation.List _ -> None
                    | Accumulation.Choice spec ->

                    let storeDefault (e : SynExpr) : SynExpr =
                        SynExpr.applyFunction (SynExpr.createIdent "Choice2Of2") (SynExpr.paren e)
                        |> SynExpr.paren
                        |> SynExpr.applyFunction (SynExpr.createIdent "Some")
                        |> SynExpr.assign (SynLongIdent.createI pf.TargetVariable)

                    let body =
                        match spec with
                        | ArgumentDefaultSpec.FunctionCall (owner, name) ->
                            SynExpr.sequential
                                [
                                    storeDefault (SynExpr.callMethod name.idText (SynExpr.createIdent' owner))
                                    SynExpr.createIdent "None"
                                ]
                        | ArgumentDefaultSpec.EnvironmentVariable name ->
                            // Environment variables permit the laxer boolean grammar: "1" and "0"
                            // as well as the usual literals.
                            let parser =
                                match pf.BoolCases with
                                | Some boolLike ->
                                    let trueCase, falseCase =
                                        match boolLike with
                                        | Choice2Of2 () -> SynExpr.CreateConst true, SynExpr.CreateConst false
                                        | Choice1Of2 flag ->
                                            FlagDu.FromBoolean flag (SynExpr.CreateConst true),
                                            FlagDu.FromBoolean flag (SynExpr.CreateConst false)

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
                                | None -> SynExpr.createIdent "x" |> SynExpr.pipeThroughFunction pf.Parser

                            let missingMessage =
                                SynExpr.createIdent "sprintf"
                                |> SynExpr.applyTo (
                                    SynExpr.CreateConst
                                        "No value was supplied for %s, nor was environment variable %s set"
                                )
                                |> SynExpr.applyTo pf.HumanReadableArgForm
                                |> SynExpr.applyTo name
                                |> SynExpr.paren
                                |> SynExpr.pipeThroughFunction (SynExpr.createIdent "Some")

                            let envFailureMessage =
                                SynExpr.createIdent "sprintf"
                                |> SynExpr.applyTo (SynExpr.CreateConst "%s (from environment variable %s)")
                                |> SynExpr.applyTo (SynExpr.createLongIdent [ "exc" ; "Message" ])
                                |> SynExpr.applyTo name
                                |> SynExpr.paren
                                |> SynExpr.pipeThroughFunction (SynExpr.createIdent "Some")

                            SynExpr.createMatch
                                (name
                                 |> SynExpr.pipeThroughFunction (SynExpr.createIdent "getEnvironmentVariable"))
                                [
                                    SynMatchClause.create (SynPat.named "None") missingMessage
                                    SynMatchClause.create
                                        (SynPat.nameWithArgs "Some" [ SynPat.named "x" ])
                                        (SynExpr.sequential [ storeDefault parser ; SynExpr.createIdent "None" ]
                                         |> SynExpr.pipeThroughTryWith SynPat.anon envFailureMessage)
                                ]

                    SynMatchClause.create (SynPat.createConst (SynConst.Int32 index)) body |> Some
                )

            let fallthrough =
                SynMatchClause.create (SynPat.anon) (internalError "unknown defaulted argument id")

            SynExpr.createMatch (SynExpr.createIdent "leafId") (branches @ [ fallthrough ])
            |> SynBinding.basic
                [ Ident.create "parser_applyDefault" ]
                [ SynPat.named "leafId" |> SynPat.annotateType SynType.int ]
            |> SynBinding.withReturnAnnotation (SynType.appPostfix "option" SynType.string)

        let callbacksBinding : SynBinding =
            [
                field "StoreOccurrence" (SynExpr.createIdent "parser_storeOccurrence")
                field "StorePositional" (SynExpr.createIdent "parser_storePositional")
                field "HelpText" (SynExpr.createIdent "helpText")
                field "RenderStored" (SynExpr.createIdent "parser_renderStored")
                field "ApplyDefault" (SynExpr.createIdent "parser_applyDefault")
            ]
            |> SynExpr.createRecord None
            |> SynBinding.basic [ Ident.create "parser_callbacks" ] []
            |> SynBinding.withReturnAnnotation (rtType "TypedCallbacks")

        // On success, freeze the typed slots into their final immutable forms and assemble the
        // record. runParse has already guaranteed that every required slot is populated.
        // Slot unwrapping happens inside `instantiate` at each use site (rather than eagerly
        // for every slot), because the slots of a union's unselected cases are legitimately
        // unpopulated on the success path.
        let successExpr : SynExpr =
            { new ParseTreeEval<_> with
                member _.Eval tree = ParseTree.instantiate tree
            }
            |> spec.Apply

        let runOutcome : SynExpr =
            SynExpr.createMatch
                (rt [ "runParse" ]
                 |> SynExpr.applyTo (
                     // The schema is re-checked at runtime because generation-time validation
                     // cannot see argument forms supplied via e.g. [<Literal>] constants.
                     SynExpr.paren (
                         SynExpr.applyFunction
                             (rt [ "WellFormedSchema" ; "checkOrFail" ])
                             (SynExpr.createIdent' schemaVar)
                     )
                 )
                 |> SynExpr.applyTo (SynExpr.createIdent "parser_callbacks")
                 |> SynExpr.applyTo (SynExpr.createIdent "args"))
                [
                    SynMatchClause.create
                        (rtPat [ "ParseOutcome" ; "Success" ] [ SynPat.named "parser_selection" ])
                        successExpr
                    SynMatchClause.create
                        (rtPat [ "ParseOutcome" ; "HelpRequested" ] [])
                        (SynExpr.createIdent "helpText"
                         |> SynExpr.applyTo (SynExpr.CreateConst ())
                         |> SynExpr.pipeThroughFunction (
                             SynExpr.applyFunction
                                 (SynExpr.createIdent "failwithf")
                                 (SynExpr.CreateConst @"Help text requested.\n%s")
                         ))
                    SynMatchClause.create
                        (rtPat [ "ParseOutcome" ; "Fatal" ] [ SynPat.named "message" ])
                        (SynExpr.applyFunction (SynExpr.createIdent "failwith") (SynExpr.createIdent "message"))
                    SynMatchClause.create
                        (rtPat [ "ParseOutcome" ; "Errors" ] [ SynPat.named "errors" ])
                        (SynExpr.createIdent "errors"
                         |> SynExpr.pipeThroughFunction (
                             SynExpr.applyFunction
                                 (SynExpr.createLongIdent [ "String" ; "concat" ])
                                 (SynExpr.CreateConst @"\n")
                         )
                         |> SynExpr.pipeThroughFunction (
                             SynExpr.createIdent "failwithf"
                             |> SynExpr.applyTo (SynExpr.CreateConst @"Errors during parse!\n%s")
                         ))
                ]

        runOutcome
        |> SynExpr.createLet (
            bindings
            @ [
                schemaBinding
                storeOccurrenceBinding
                storePositionalBinding
                renderStoredBinding
                applyDefaultBinding
                callbacksBinding
            ]
        )

    // The type for which we're generating args may refer to any of the supplied records/unions.
    let createModule
        (runtimeModule : Ident)
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

        // Unions whose cases are alternative argument records; flag DUs are argument *leaves*
        // and are excluded.
        let structuralUnions =
            allUnionTypes
            |> List.filter (fun u -> flagDus |> List.forall (fun f -> f.Name.idText <> u.Name.idText))

        let taggedTypeName, typeHelpText, parseSpec =
            let typeHelp (attrs : SynAttributes) =
                attrs
                |> SynAttributes.toAttrs
                |> List.tryPick (fun a ->
                    match (List.last a.TypeName.LongIdent).idText with
                    | "ArgumentHelpTextAttribute"
                    | "ArgumentHelpText" -> Some a.ArgExpr
                    | _ -> None
                )

            match taggedType with
            | SynTypeDefn.SynTypeDefn (SynComponentInfo.SynComponentInfo (attributes = attrs) as sci,
                                       SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.Record (access, fields, _), _),
                                       smd,
                                       _,
                                       _,
                                       _) ->
                let record = RecordType.OfRecord sci smd access fields

                let spec, _ = toParseSpec [] 0 flagDus structuralUnions allRecordTypes record

                record.Name, typeHelp attrs, spec
            | SynTypeDefn.SynTypeDefn (SynComponentInfo.SynComponentInfo (attributes = attrs) as sci,
                                       SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.Union (access, cases, _), _),
                                       smd,
                                       _,
                                       _,
                                       _) ->
                let union = UnionType.OfUnion sci smd access cases

                let spec, _ = unionToParseSpec [] 0 flagDus structuralUnions allRecordTypes union

                union.Name, typeHelp attrs, spec
            | _ ->
                failwith
                    "[<ArgParser>] may only be placed on a record, or on a discriminated union whose cases each hold one record."

        let modAttrs, modName =
            if spec.ExtensionMethods then
                [ SynAttribute.autoOpen ], Ident.create (taggedTypeName.idText + "ArgParse")
            else
                [ SynAttribute.requireQualifiedAccess ; SynAttribute.compilationRepresentation ], taggedTypeName

        let modInfo =
            SynComponentInfo.create modName
            |> SynComponentInfo.withDocString (
                PreXmlDoc.create $"Methods to parse arguments for the type %s{taggedTypeName.idText}"
            )
            |> SynComponentInfo.addAttributes modAttrs

        let taggedMod =
            let argsParam =
                SynPat.named "args"
                |> SynPat.annotateType (SynType.appPostfix "list" SynType.string)

            let parsePrime =
                createRecordParse runtimeModule typeHelpText taggedTypeName parseSpec
                |> SynBinding.basic
                    [ Ident.create "parse'" ]
                    [
                        SynPat.named "getEnvironmentVariable"
                        |> SynPat.annotateType (SynType.funFromDomain SynType.string (SynType.option SynType.string))
                        argsParam
                    ]
                |> SynBinding.withReturnAnnotation (SynType.createLongIdent [ taggedTypeName ])

            let parsePrimeCall =
                if spec.ExtensionMethods then
                    // need to fully qualify
                    [ taggedTypeName ; Ident.create "parse'" ]
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
                |> SynBinding.withReturnAnnotation (SynType.createLongIdent [ taggedTypeName ])

            [

                if spec.ExtensionMethods then
                    let bindingPrime = parsePrime |> SynMemberDefn.staticMember

                    let binding = parse |> SynMemberDefn.staticMember

                    let componentInfo =
                        SynComponentInfo.create taggedTypeName
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

    /// Everything the generator does after parsing: locate the [<ArgParser>] types and build the
    /// generated namespaces (one embedded-runtime module per namespace containing a tagged type,
    /// then one module per tagged type). Split out from IMyriadGenerator.Generate so that tests
    /// can drive the generator over in-memory source.
    let generate (ast : ParsedInput) : SynModuleOrNamespace list =
        let types = Ast.getTypes ast

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

        // The runtime-module prefix is reserved: a type named e.g. ArgParserRuntime_Foo (tagged
        // or not) would collide with the runtime module emitted for a namespace whose first
        // tagged type is Foo. Enforce the reservation over every type the generator can see —
        // the unions and records here are the full recursive group declared alongside each
        // tagged type. Declarations it cannot see (other input files; user modules, which
        // Ast.getTypes does not surface) remain covered by documentation only.
        for _, _, unions, records in namespaceAndTypes do
            let names =
                (unions |> List.map (fun u -> u.Name.idText))
                @ (records |> List.map (fun r -> r.Name.idText))

            for name in names do
                if name.StartsWith ("ArgParserRuntime_", StringComparison.Ordinal) then
                    failwith
                        $"Type names beginning 'ArgParserRuntime_' are reserved: the ArgParser generator emits its runtime module under that prefix alongside the generated parsers. Rename the type '%s{name}'."

        // Each namespace containing a generated parser gets one embedded runtime module,
        // named after the first [<ArgParser>] type in that namespace (see
        // ArgParserRuntimeEmbed.moduleName for why that cannot collide).
        let runtimeModuleByNs =
            namespaceAndTypes
            |> List.groupBy (fun (ns, _, _, _) -> ns |> List.map _.idText)
            |> List.map (fun (nsName, group) ->
                let ns, (firstType, _), _, _ = List.head group

                let ident =
                    SynTypeDefn.getName firstType
                    |> List.last
                    |> _.idText
                    |> ArgParserRuntimeEmbed.moduleName
                    |> Ident.create

                nsName, (ns, ident)
            )
            |> Map.ofList

        let modules =
            namespaceAndTypes
            |> List.map (fun (ns, taggedType, unions, records) ->
                let opens = AstHelper.extractOpensForNamespace ns ast
                let _, runtimeModule = Map.find (ns |> List.map _.idText) runtimeModuleByNs
                createModule runtimeModule opens ns taggedType unions records
            )

        let runtimeModules =
            runtimeModuleByNs
            |> Map.toList
            |> List.map (fun (_, (ns, ident)) ->
                SynModuleOrNamespace.createNamespace ns [ ArgParserRuntimeEmbed.asModule ident.idText ]
            )

        runtimeModules @ modules

open Myriad.Core

/// Myriad generator that provides a catamorphism for an algebraic data type.
[<MyriadGenerator("arg-parser")>]
type ArgParserGenerator () =

    interface IMyriadGenerator with
        member _.ValidInputExtensions = [ ".fs" ]

        member _.Generate (context : GeneratorContext) =
            let ast, _ =
                Ast.fromFilename context.InputFilename |> Async.RunSynchronously |> Array.head

            Output.Ast (ArgParserGenerator.generate ast)
