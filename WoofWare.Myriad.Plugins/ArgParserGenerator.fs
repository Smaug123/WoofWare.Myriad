namespace WoofWare.Myriad.Plugins

open System
open System.Text
open Fantomas.FCS.Syntax
open Fantomas.FCS.Text.Range
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

/// The types defined alongside the [<ArgParser>]-tagged type, classified by the role each plays in
/// an argument schema. A union is an argument *leaf* if it is flag-like ([<ArgumentFlag>] on both
/// cases of a two-case data-free union) or enum-like (no case has any data, so a case name is the
/// argument's value); every other union is a set of alternative argument sets. The three classes
/// are disjoint, and together they exhaust the unions defined alongside the tagged type.
type private AmbientTypes =
    {
        FlagDus : FlagDu list
        EnumDus : UnionType list
        StructuralUnions : UnionType list
        Records : RecordType list
    }

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
    /// From the constant the user wrote in `[<ArgumentDefaultValue 3>]`. We rebuild the literal
    /// rather than reusing the user's expression, so that nothing about where it lands in the
    /// generated file can change what it means.
    | Literal of value : SynExpr
    /// From `[<ArgumentDefaultValue null>]`. This is a literal like any other, but it is kept apart
    /// from `Literal` because it is the one we cannot describe in help text the way we describe the
    /// rest: `(null).ToString ()` does not even compile.
    | NullLiteral

/// How a `Map`-typed field spells its entries on the command line.
///
/// No separator-based encoding is surjective onto `Map<string, string>`: some character must
/// separate a key from its value, and that character may occur in a key. We split each entry at
/// its *first* key-value separator, which confines the restriction to keys — a value may contain
/// anything, including the separator — so the encoding is surjective exactly onto those maps
/// whose keys avoid it. An entry separator buys a terser command line at the price of
/// constraining keys and values alike, so it is opt-in.
type private MapSpec =
    {
        /// Splits a key from its value, at the first occurrence within an entry.
        /// Held as a string rather than a char because char literals do not survive the
        /// parse-and-reprint round trip through which generated code is emitted.
        KeyValueSeparator : string
        /// Splits entries within a single occurrence. When absent, an occurrence is one entry.
        EntrySeparator : string option
        /// A function `string -> %KeyType%`, which is allowed to throw if it fails to parse.
        KeyParser : SynExpr
        KeyType : SynType
    }

type private Accumulation<'choice> =
    | Required
    | Optional
    | Choice of 'choice
    | List of Accumulation<'choice>
    /// Accumulates key-value entries across occurrences. Like `List`, a map is satisfiable with
    /// no arguments (it is then empty), so it is neither optional nor defaultable.
    | Map of MapSpec

/// Turning an argument's spelling into generated source. `ArgForm` holds the semantic spelling,
/// which is what the generation-time name checks must compare; these ready it for emission.
[<RequireQualifiedAccess>]
module private ArgFormEmission =

    /// Rewrite a string so that it can be dropped between the quotes of a generated regular string
    /// literal and read back as itself.
    ///
    /// This is not simply "the F# escaping rules", because Fantomas has its own view: it emits a
    /// `SynConst.String`'s text between quotes having escaped the quotes and nothing else. Anything
    /// else which needs escaping is therefore ours to do (otherwise `C:\temp` would be emitted as
    /// `"C:\temp"`, whose `\t` is a tab), and we escape the quote as `\u0022` rather than `\"` so
    /// that Fantomas finds no quote to escape and hands our text through unaltered.
    let escapeStringConstant (s : string) : string =
        s
        |> String.collect (fun c ->
            match c with
            | '\\' -> @"\\"
            | '"' -> @"\u0022"
            // Everything outside printable ASCII goes out as an escape: `\uXXXX` is a UTF-16 code
            // unit, which is exactly what a char of a .NET string is, so this is faithful even for
            // an unpaired surrogate, and it keeps the generated file free of any dependence on how
            // it is encoded.
            | c when c >= ' ' && c <= '~' -> System.Char.ToString c
            | c -> sprintf @"\u%04x" (int c)
        )

    /// Ready an argument's spelling to be written into the generated file.
    ///
    /// `ArgForm` holds the *semantic* spelling: a `SynConst.String` carries decoded text, and the
    /// generation-time name checks must compare what the scanner will compare, not a rendering of it
    /// (`é` against `É` would miss the collision that `é` and `É` do have). Emission wants
    /// the opposite, so escape here, at the boundary, and emit a regular string whatever the author
    /// wrote -- a verbatim or triple-quoted spelling could not express the escapes we need.
    ///
    /// A form we cannot read (an [<ArgumentLongForm>] naming a [<Literal>]) is an expression the
    /// generated program evaluates for itself, and passes through untouched.
    let emitArgForm (form : SynExpr) : SynExpr =
        // F#'s attribute syntax permits parentheses around the argument, and the generation-time
        // name checks already see through them (`literalForms`), so emission must agree: otherwise
        // `[<ArgumentLongForm ("back\\tab")>]` would be checked as one name and emitted as another.
        match SynExpr.stripOptionalParen form with
        | SynExpr.Const (SynConst.String (s, _, _), _) ->
            SynExpr.Const (SynConst.String (escapeStringConstant s, SynStringKind.Regular, range0), range0)
        | form -> form

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
        /// If this is a data-free union (an enumerated value), the union whose case names are the
        /// values this argument accepts. The help text must list them: the type's name alone tells
        /// the user nothing about how to spell one. It must also render a *default* value through
        /// this union rather than through `ToString`, which under `--reflectionfree` reports the
        /// type's name instead of the case's.
        EnumCases : UnionType option
        Help : SynExpr option
        /// A function string -> %TargetType%, where TargetVariable is probably a `%TargetType% option`.
        /// (Depending on `Accumulation`, we'll remove the `option` at the end of the parse, asserting that the
        /// argument was supplied.)
        /// This is allowed to throw if it fails to parse.
        Parser : SynExpr
        /// If `Accumulation` is `List`, then this is the type of the list *element*; analogously for optionals
        /// and choices and so on. For a `Map` this is the *value* type, so it does not on its own describe
        /// the field: see `DisplayType`.
        TargetType : SynType
        /// How to name this argument's type in help text, when `TargetType` would misdescribe it.
        /// A map's `TargetType` is only half the story, so it supplies the whole `map<K, V>` here.
        DisplayType : string option
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
            let allArgForms =
                (arg.ArgForm @ arg.ArgForm) |> List.map ArgFormEmission.emitArgForm

            (SynExpr.applyFunction (SynExpr.createIdent "sprintf") (SynExpr.CreateConst combinedFormatString),
             allArgForms)
            ||> List.fold SynExpr.applyFunction
            |> SynExpr.paren
        else
            // Standard behavior: just --foo / --bar
            let formatString = List.replicate arg.ArgForm.Length "--%s" |> String.concat " / "

            (SynExpr.applyFunction (SynExpr.createIdent "sprintf") (SynExpr.CreateConst formatString),
             arg.ArgForm |> List.map ArgFormEmission.emitArgForm)
            ||> List.fold SynExpr.applyFunction
            |> SynExpr.paren

[<RequireQualifiedAccess>]
type private ChoicePositional =
    | Normal of includeFlagLike : SynExpr option
    | Choice of includeFlagLike : SynExpr option

type private ParseFunctionPositional = ParseFunction<ChoicePositional>
type private ParseFunctionNonPositional = ParseFunction<Accumulation<ArgumentDefaultSpec>>

/// The parse tree mirroring the schema's shape: named-argument leaves, positional-stream
/// leaves, products (records) and exclusive sums (unions of alternative argument sets).
/// Build Branch nodes only through ParseTree.branch, which enforces the positional-capacity
/// rule (argv holds a single positional stream, so at most one field chain of a record may
/// claim positional args) and keeps the positional-claiming field after its siblings, which
/// is where the positional args have always appeared in help text and in the erased schema.
[<RequireQualifiedAccess>]
type private ParseTree =
    | NonPositionalLeaf of ParseFunctionNonPositional
    | PositionalLeaf of ParseFunctionPositional
    /// `assemble` takes the SynExpr's (e.g. each record field contents) corresponding to each
    /// `Ident` in the branch (e.g. each record field name), and composes them into a `SynExpr`
    /// (e.g. the record-typed object).
    | Branch of fields : (Ident * ParseTree) list * assemble : (Map<string, SynExpr> -> SynExpr)
    /// A discriminated-union arg: at runtime, exactly one case is selected by the arguments
    /// which were supplied. `sumId` ties this node to the erased schema's Sum node; `assemble`
    /// builds the union value from the selected case's name and its assembled payload.
    /// Positional args are not yet permitted inside union cases.
    | Sum of sumId : int * cases : (Ident * ParseTree) list * assemble : (Ident -> SynExpr -> SynExpr)

[<RequireQualifiedAccess>]
module private ParseTree =

    /// Does this tree contain a positional-args leaf anywhere?
    let rec containsPositional (tree : ParseTree) : bool =
        match tree with
        | ParseTree.NonPositionalLeaf _ -> false
        | ParseTree.PositionalLeaf _ -> true
        | ParseTree.Branch (fields, _) -> fields |> List.exists (fun (_, child) -> containsPositional child)
        | ParseTree.Sum (_, cases, _) -> cases |> List.exists (fun (_, case) -> containsPositional case)

    /// The `Ident` here is the field name. Moves the positional-claiming field (at most one
    /// is permitted) after its siblings.
    let branch (assemble : Map<string, SynExpr> -> SynExpr) (subs : (Ident * ParseTree) list) : ParseTree =
        let nonPos, pos =
            subs |> List.partition (fun (_, tree) -> not (containsPositional tree))

        match pos with
        | []
        | [ _ ] -> ParseTree.Branch (nonPos @ pos, assemble)
        | (first, _) :: (second, _) :: _ ->
            failwith $"Multiple entries tried to claim positional args! %s{first.idText} and %s{second.idText}"

    /// Collect all the ParseFunctions which are necessary to define variables, throwing away
    /// all information relevant to composing the resulting variables into records.
    /// Returns the non-positional parsers and the positional parsers, each in tree order.
    let accumulators (tree : ParseTree) : ParseFunctionNonPositional list * ParseFunctionPositional list =
        let rec go (tree : ParseTree) : ParseFunctionNonPositional list * ParseFunctionPositional list =
            match tree with
            | ParseTree.NonPositionalLeaf pf -> [ pf ], []
            | ParseTree.PositionalLeaf pf -> [], [ pf ]
            | ParseTree.Branch (fields, _) ->
                (([], []), fields)
                ||> List.fold (fun (nonPos, pos) (_, child) ->
                    let childNonPos, childPos = go child
                    nonPos @ childNonPos, pos @ childPos
                )
            | ParseTree.Sum (_, cases, _) ->
                (([], []), cases)
                ||> List.fold (fun (nonPos, pos) (_, case) ->
                    let caseNonPos, casePos = go case
                    nonPos @ caseNonPos, pos @ casePos
                )

        let nonPos, pos = go tree

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
            (pos |> List.collect (fun pf -> literalForms pf.ArgForm))
            @ (nonPos |> List.collect (fun pf -> literalForms pf.ArgForm))

        let helpClaims =
            allLiteralForms
            |> List.filter (fun form -> System.String.Equals (form, "help", System.StringComparison.OrdinalIgnoreCase))

        match helpClaims with
        | [] -> ()
        | _ -> failwith "The argument name 'help' is reserved: --help always displays the help text."

        // Every name a `--token` could address, with a description of its claimant, in
        // declaration order. The boolean marks positional-args claimants: sinks in mutually
        // exclusive union cases may share forms with each other (a keyed positional token
        // means the same thing whichever sink is active), but not with anything else.
        let claims : (string * string * bool) list =
            (nonPos
             |> List.collect (fun pf ->
                 let forms = literalForms pf.ArgForm

                 let plain =
                     forms
                     |> List.map (fun form -> form, $"'--%s{form}' (field '%s{pf.FieldName.idText}')", false)

                 let negated =
                     if pf.AcceptsNegation then
                         forms
                         |> List.map (fun form ->
                             $"no-%s{form}",
                             $"the --no- variant of field '%s{pf.FieldName.idText}' (which has [<ArgumentNegateWithPrefix>])",
                             false
                         )
                     else
                         []

                 plain @ negated
             ))
            @ (pos
               |> List.collect (fun pf ->
                   literalForms pf.ArgForm
                   |> List.map (fun form ->
                       form, $"'--%s{form}' (the positional args, field '%s{pf.FieldName.idText}')", true
                   )
               ))

        // Group under the scanner's own equality (OrdinalIgnoreCase), preserving
        // declaration order. This is deliberately not ToUpperInvariant keying, which is a
        // strictly coarser relation: e.g. "s" and "ſ" (long s) uppercase to the same string,
        // but the scanner considers them distinct, so they do not collide.
        let conflicts =
            let indexOf =
                System.Collections.Generic.Dictionary<string, int> (StringComparer.OrdinalIgnoreCase)

            let buckets = ResizeArray<ResizeArray<string * string * bool>> ()

            for form, claimant, isPositional in claims do
                match indexOf.TryGetValue form with
                | true, index -> buckets.[index].Add ((form, claimant, isPositional))
                | false, _ ->
                    indexOf.[form] <- buckets.Count
                    let bucket = ResizeArray ()
                    bucket.Add ((form, claimant, isPositional))
                    buckets.Add bucket

            buckets
            |> Seq.choose (fun bucket ->
                let allPositional = bucket |> Seq.forall (fun (_, _, isPositional) -> isPositional)

                if bucket.Count < 2 || allPositional then
                    None
                else
                    let form, _, _ = bucket.[0]

                    bucket
                    |> Seq.map (fun (_, claimant, _) -> claimant)
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

    /// Does this tree contain any discriminated-union node?
    let rec containsSum (tree : ParseTree) : bool =
        match tree with
        | ParseTree.NonPositionalLeaf _
        | ParseTree.PositionalLeaf _ -> false
        | ParseTree.Sum _ -> true
        | ParseTree.Branch (fields, _) -> fields |> List.exists (fun (_, child) -> containsSum child)

    /// Can this tree be satisfied by supplying no arguments at all? (Defaulted and optional
    /// leaves need nothing, as do positional args; a union needs nothing iff some case needs
    /// nothing.)
    let rec emptySatisfiable (tree : ParseTree) : bool =
        match tree with
        | ParseTree.NonPositionalLeaf pf ->
            match pf.Accumulation with
            | Accumulation.Required -> false
            | Accumulation.Optional
            | Accumulation.Choice _
            | Accumulation.List _
            // An unsupplied map is empty, exactly as an unsupplied list is.
            | Accumulation.Map _ -> true
        | ParseTree.PositionalLeaf _ -> true
        | ParseTree.Branch (fields, _) -> fields |> List.forall (fun (_, child) -> emptySatisfiable child)
        | ParseTree.Sum (_, cases, _) -> cases |> List.exists (fun (_, case) -> emptySatisfiable case)

    /// For every union node in the tree, at most one case may be satisfiable with no arguments:
    /// were two cases so satisfiable, an empty command line could not choose between them.
    let rec checkSumAmbiguity (tree : ParseTree) : unit =
        match tree with
        | ParseTree.NonPositionalLeaf _
        | ParseTree.PositionalLeaf _ -> ()
        | ParseTree.Branch (fields, _) -> fields |> List.iter (fun (_, child) -> checkSumAmbiguity child)
        | ParseTree.Sum (_, cases, _) ->
            cases |> List.iter (fun (_, case) -> checkSumAmbiguity case)

            match cases |> List.filter (fun (_, case) -> emptySatisfiable case) with
            | []
            | [ _ ] -> ()
            | ambiguous ->
                let names =
                    ambiguous |> List.map (fun (name, _) -> name.idText) |> String.concat ", "

                failwith
                    $"Cases %s{names} can all be satisfied without supplying any arguments, so an empty command line cannot choose between them. Make an argument in all but one of them mandatory."

    /// Build the expression for the erased-schema tree mirroring this parse tree. Named-leaf
    /// ids are assigned in `accumulators` order, which is exactly this walk's traversal
    /// order; positional-sink ids likewise, in their own id space. `rt` resolves a path
    /// inside the embedded runtime module; `listOf` builds a list literal (with the empty
    /// list handled).
    let rec toErasedTreeExpr
        (rt : string list -> SynExpr)
        (listOf : SynExpr list -> SynExpr)
        (counter : int ref)
        (posCounter : int ref)
        (tree : ParseTree)
        : SynExpr
        =
        let product (children : SynExpr list) : SynExpr =
            SynExpr.applyFunction (rt [ "ErasedTree" ; "Product" ]) (SynExpr.paren (listOf children))

        match tree with
        | ParseTree.NonPositionalLeaf _ ->
            let index = counter.Value
            counter.Value <- counter.Value + 1

            SynExpr.applyFunction (rt [ "ErasedTree" ; "Leaf" ]) (SynExpr.CreateConst index)
        | ParseTree.PositionalLeaf _ ->
            let index = posCounter.Value
            posCounter.Value <- posCounter.Value + 1

            SynExpr.applyFunction (rt [ "ErasedTree" ; "PositionalLeaf" ]) (SynExpr.CreateConst index)
        | ParseTree.Branch (fields, _) ->
            fields
            |> List.map (fun (_, child) -> toErasedTreeExpr rt listOf counter posCounter child)
            |> product
        | ParseTree.Sum (sumId, cases, _) ->
            let caseExprs =
                cases
                |> List.map (fun (caseName, payload) ->
                    let payloadExpr = toErasedTreeExpr rt listOf counter posCounter payload

                    SynExpr.tuple [ SynExpr.CreateConst caseName.idText ; payloadExpr ]
                )

            SynExpr.applyFunction
                (rt [ "ErasedTree" ; "Sum" ])
                (SynExpr.paren (SynExpr.tuple [ SynExpr.CreateConst sumId ; listOf caseExprs ]))

    /// Build the return value. (References the `parser_selection` binding which the generated
    /// code brings into scope on the success path, to choose among Sum cases.)
    let rec instantiate (tree : ParseTree) : SynExpr =
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
        | ParseTree.NonPositionalLeaf pf ->
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
            | Accumulation.Map _ ->
                // The slot is a ResizeArray of key-value pairs, already checked for duplicate
                // keys as it was filled.
                SynExpr.createIdent' pf.TargetVariable
                |> SynExpr.pipeThroughFunction (SynExpr.createLongIdent [ "Map" ; "ofSeq" ])
                |> SynExpr.paren
        | ParseTree.PositionalLeaf pf ->
            SynExpr.createIdent' pf.TargetVariable
            |> SynExpr.pipeThroughFunction (SynExpr.createLongIdent [ "Seq" ; "toList" ])
            |> SynExpr.paren
        | ParseTree.Sum (sumId, cases, assemble) ->
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
        | ParseTree.Branch (fields, assemble) ->
            fields
            |> List.map (fun (fieldName, contents) ->
                let instantiated = instantiate contents
                fieldName.idText, instantiated
            )
            |> Map.ofList
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

    let private identifyAsEnum (enumDus : UnionType list) (ty : SynType) : UnionType option =
        match localTypeName ty with
        | Some name -> enumDus |> List.tryFind (fun du -> du.Name.idText = name)
        | None -> None

    /// The slot holding the keys a `Map`-typed field has accumulated so far, beside the slot
    /// holding the entries themselves.
    let private seenVariable (target : Ident) : Ident = Ident.create (target.idText + "_seen")

    /// A type as we'd spell it in an error message. `toHumanReadableString` throws on the exotic
    /// shapes (tuples, functions, anonymous records), and an error message is the worst place to
    /// raise a different error from the one being reported, so fall back to the raw form.
    let private describeType (ty : SynType) : string =
        try
            SynType.toHumanReadableString ty
        with _ ->
            string<SynType> ty

    /// The single `char` argument of an attribute whose name is one of `names`, if the field
    /// carries it. Rendered as a string: char literals do not survive the parse-and-reprint round
    /// trip through which generated code is emitted, so every separator is a string from here on.
    let private charAttribute (names : string list) (fieldName : Ident) (attrs : SynAttribute list) : string option =
        attrs
        |> List.tryPick (fun attr ->
            let (SynLongIdent.SynLongIdent (idents, _, _)) = attr.TypeName

            match idents |> List.map _.idText |> List.tryLast with
            | Some name when List.contains name names ->
                match SynExpr.stripOptionalParen attr.ArgExpr with
                | SynExpr.Const (SynConst.Char c, _) -> Some (string<char> c)
                | arg ->
                    failwith
                        $"[<%s{name}>] on field '%s{fieldName.idText}' must be given a literal char, e.g. [<%s{name} ':'>], but got: %O{arg}"
            | _ -> None
        )

    /// The values a data-free union's cases are spelled by on the command line. These are matched
    /// case-insensitively, exactly as the scanner matches argument names, so two case names which
    /// differ only by case would leave the earlier one silently claiming both spellings.
    ///
    /// This is checked here, at the point of *use*, rather than for every data-free union defined
    /// alongside the tagged type: enum-ness is inferred from shape, so a union which happens to sit
    /// in the same namespace without ever being an argument must not fail anyone's build.
    let private checkedEnumCaseNames (union : UnionType) : string list =
        let names = union.Cases |> List.map (fun case -> case.Name.idText)

        let conflicts =
            let indexOf =
                System.Collections.Generic.Dictionary<string, int> (StringComparer.OrdinalIgnoreCase)

            let buckets = ResizeArray<ResizeArray<string>> ()

            for name in names do
                match indexOf.TryGetValue name with
                | true, index -> buckets.[index].Add name
                | false, _ ->
                    indexOf.[name] <- buckets.Count
                    let bucket = ResizeArray ()
                    bucket.Add name
                    buckets.Add bucket

            buckets
            |> Seq.choose (fun bucket ->
                if bucket.Count < 2 then
                    None
                else
                    bucket
                    |> String.concat "; "
                    |> sprintf "The value '%s' is claimed by cases: %s" bucket.[0]
                    |> Some
            )
            |> List.ofSeq

        match conflicts with
        | [] -> names
        | conflicts ->
            let conflictMessages = conflicts |> String.concat "\n"

            failwith
                $"Conflicting case names detected in the data-free union %s{union.Name.idText}, whose cases are argument values (values are matched case-insensitively):\n%s{conflictMessages}"

    /// `fun x -> if System.String.Equals (x, "A", OrdinalIgnoreCase) then FooDto.A elif ... else failwith ...`
    let private createEnumParser (union : UnionType) : SynExpr =
        let names = checkedEnumCaseNames union

        let unrecognised =
            SynExpr.createIdent "sprintf"
            |> SynExpr.applyTo (SynExpr.CreateConst "Unrecognised value '%s' for %s: expected one of %s")
            |> SynExpr.applyTo (SynExpr.createIdent "x")
            // The type and case names are supplied as arguments rather than baked into the format
            // string: a name is an arbitrary F# identifier, and one containing a '%' would
            // otherwise make the generated code's format string invalid.
            |> SynExpr.applyTo (SynExpr.CreateConst union.Name.idText)
            |> SynExpr.applyTo (SynExpr.CreateConst (names |> String.concat ", "))
            |> SynExpr.paren
            |> SynExpr.pipeThroughFunction (SynExpr.createIdent "failwith")

        (union.Cases, unrecognised)
        ||> List.foldBack (fun case ifNoMatch ->
            let isThisCase =
                SynExpr.applyFunction
                    (SynExpr.createLongIdent [ "System" ; "String" ; "Equals" ])
                    (SynExpr.tuple
                        [
                            SynExpr.createIdent "x"
                            SynExpr.CreateConst case.Name.idText
                            SynExpr.createLongIdent [ "System" ; "StringComparison" ; "OrdinalIgnoreCase" ]
                        ])

            // Note the argument order: SynExpr.ifThenElse takes the *false* branch first.
            SynExpr.ifThenElse isThisCase ifNoMatch (SynExpr.createLongIdent' [ union.Name ; case.Name ])
        )
        |> SynExpr.createLambda "x"

    /// `match {value} with | FooDto.A -> "A" | FooDto.B -> "B"`: the spelling the user would have
    /// to type for each case. `ToString` would do under normal compilation, but not under
    /// `--reflectionfree`, which drops the structural override a union would otherwise get.
    let private renderEnumCase (union : UnionType) (value : SynExpr) : SynExpr =
        union.Cases
        |> List.map (fun case ->
            SynMatchClause.create
                (SynPat.identWithArgs [ union.Name ; case.Name ] (SynArgPats.create []))
                (SynExpr.CreateConst case.Name.idText)
        )
        |> SynExpr.createMatch value

    /// Render a stored value as the string the user would have had to type to supply it. That is
    /// not the same as `ToString`: a flag DU is displayed to the user as a bool, and an enumerated
    /// union by case name. `ToString` is also actively wrong for either under `--reflectionfree`,
    /// which drops the structural override and leaves only the type's name.
    let private renderLeafValue
        (boolCases : Choice<FlagDu, unit> option)
        (enumCases : UnionType option)
        (value : SynExpr)
        : SynExpr
        =
        match boolCases, enumCases with
        | Some (Choice1Of2 flagDu), _ ->
            // Care required here: the value is not a bool, but we display it as one.
            [
                SynMatchClause.create
                    (SynPat.identWithArgs [ flagDu.Name ; flagDu.Case1Name ] (SynArgPats.create []))
                    // Note the argument order: SynExpr.ifThenElse takes the *false* branch first.
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
            |> SynExpr.createMatch value
        | None, Some union -> renderEnumCase union value
        // A plain bool, or a type we know nothing special about: `ToString` is all we have, and
        // for the primitives this reaches it agrees with the spelling the user types.
        | Some (Choice2Of2 ()), _
        | None, None -> SynExpr.callMethod "ToString" value

    /// The separator attributes describe how a map entry is spelled, so they mean nothing on a
    /// field which is not a map. Silently ignoring them would leave an author believing they had
    /// configured something. This is checked against the *accumulation* rather than the declared
    /// type, so that e.g. `Map<_, _> option` is reported as an unsupported optional map — the
    /// nearer problem — rather than as a misplaced attribute.
    let private rejectSeparatorAttributes (fieldName : Ident) (fieldType : SynType) (attrs : SynAttribute list) : unit =
        let reject (names : string list) (display : string) (purpose : string) : unit =
            match charAttribute names fieldName attrs with
            | None -> ()
            | Some _ ->
                failwith
                    $"[<%s{display}>] can only be applied to map fields, but was applied to field '%s{fieldName.idText}' of type %s{describeType fieldType}. %s{purpose}"

        reject
            [ "ArgumentKeyValueSeparator" ; "ArgumentKeyValueSeparatorAttribute" ]
            "ArgumentKeyValueSeparator"
            "It controls how one entry of a map is split into a key and a value."

        reject
            [ "ArgumentMapEntrySeparator" ; "ArgumentMapEntrySeparatorAttribute" ]
            "ArgumentMapEntrySeparator"
            "It controls how one occurrence of a map is split into several entries."

    /// A field whose type is another argument record, or a union of alternative argument sets,
    /// contributes that type's whole set of arguments, each named by its own field. There is no
    /// single argument here for an [<ArgumentLongForm>] to rename, so it can only have been a
    /// mistake; say so rather than dropping it, which is what the structural branches did when they
    /// took over before the leaf machinery ran.
    let private rejectLongFormAttribute (fieldName : Ident) (fieldType : SynType) (attrs : SynAttribute list) : unit =
        let present =
            attrs
            |> List.exists (fun attr ->
                match (List.last attr.TypeName.LongIdent).idText with
                | "ArgumentLongForm"
                | "ArgumentLongFormAttribute" -> true
                | _ -> false
            )

        if present then
            let ty = describeType fieldType

            failwith
                $"Field '%s{fieldName.idText}' has an [<ArgumentLongForm>], but its type %s{ty} is an argument record or a discriminated union of alternative argument sets, so it contributes a whole set of arguments rather than one. [<ArgumentLongForm>] renames a single argument, and there is none here to rename: the names come from the fields of %s{ty} itself. Put the attribute on the field you mean to rename."

    let private checkSeparatorAttributesPlacement
        (fieldName : Ident)
        (fieldType : SynType)
        (attrs : SynAttribute list)
        (accumulation : Accumulation<'choice>)
        : unit
        =
        match accumulation with
        | Accumulation.Map _ -> ()
        | Accumulation.Required
        | Accumulation.Optional
        | Accumulation.Choice _
        | Accumulation.List _ -> rejectSeparatorAttributes fieldName fieldType attrs

    /// What we found in the argument of an `[<ArgumentDefaultValue>]`.
    type private DefaultValueExpr =
        /// A constant written out in full. It denotes the same value wherever it appears, so we can
        /// reproduce it in the generated file.
        | Constant of SynConst
        /// The `null` literal, which F# admits as an object-valued attribute argument.
        | Null
        /// One of F#'s context-sensitive constants (`__LINE__`, `__SOURCE_FILE__`,
        /// `__SOURCE_DIRECTORY__`), whose value depends on where it is written.
        | ContextSensitive of name : string
        /// A name standing for a constant, such as a `[<Literal>]` binding or an enum case.
        | Identifier of name : string
        /// Some other shape, which we decline to reproduce sight-unseen.
        | Unrecognised

    /// An `[<ArgumentDefaultValue>]`'s value has to be reproduced in the generated file, so we accept
    /// only expressions which denote the same value there as here. This recognises the shapes rather
    /// than searching for bad ones: an expression we have not anticipated must be refused, not
    /// emitted blind.
    ///
    /// F# has already restricted a custom-attribute argument to a compile-time constant by the time
    /// the whole compilation unit type-checks, so in practice this sees a literal, a name standing
    /// for one, or a context-sensitive constant.
    let rec private classifyDefaultValue (expr : SynExpr) : DefaultValueExpr =
        match expr with
        // F#'s attribute syntax requires parentheses around an argument which is not a bare literal,
        // and the user may add more of their own.
        | SynExpr.Paren (expr = e) -> classifyDefaultValue e
        | SynExpr.Const (constant = c) ->
            let rec ofConst (c : SynConst) : DefaultValueExpr =
                match c with
                | SynConst.SourceIdentifier (constant = name) -> ContextSensitive name
                // A measure annotation such as `3.0<m>` wraps the constant it annotates.
                | SynConst.Measure (constant = inner) -> ofConst inner
                | c -> Constant c

            ofConst c
        | SynExpr.Null _ -> Null
        // A `[<Literal>]` binding or an enum case. We have no type checker, so we cannot tell which
        // binding is meant; and the generated file hoists every `open` in the source above the
        // parser, so the name need not resolve to the same one there anyway.
        | SynExpr.Ident ident -> Identifier ident.idText
        | SynExpr.LongIdent (longDotId = SynLongIdent (id = ids)) ->
            ids |> List.map _.idText |> String.concat "." |> Identifier
        | _ -> Unrecognised

    /// Build the expression for a recognised constant default.
    ///
    /// A `SynConst.Char` needs care: Fantomas renders one without its quotes, so `'a'` reaches the
    /// generated file as a bare `a` and fails to compile (or, worse, picks up an unrelated binding
    /// of that name). Giving the node a synthetic range does not help -- it is the rendering of the
    /// constant itself, not its range, which drops them -- so emit the code point converted instead,
    /// which needs no escaping and cannot be mistaken for an identifier.
    ///
    /// A `SynConst.String` needs care for the opposite reason: it holds the *decoded* text, so
    /// re-emitting it demands the escaping the user's own source supplied. We emit a regular string
    /// whatever the user wrote, since a verbatim or triple-quoted spelling could not express the
    /// escapes we need.
    let private defaultValueExpr (c : SynConst) : SynExpr =
        match c with
        | SynConst.Char c -> SynExpr.CreateConst c
        | SynConst.String (text = s) ->
            SynExpr.Const (
                SynConst.String (ArgFormEmission.escapeStringConstant s, SynStringKind.Regular, range0),
                range0
            )
        | c -> SynExpr.Const (c, range0)

    /// Builds a function or lambda of one string argument, which returns a `ty` (as modified by the `Accumulation`;
    /// for example, maybe it returns a `ty option` or a `ty list`).
    /// The resulting SynType is the type of the *element* being parsed; so if the Accumulation is List, the SynType
    /// is the list element.
    let rec private createParseFunction<'choice>
        (choice : ArgumentDefaultSpec option -> 'choice)
        (ambient : AmbientTypes)
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
                createParseFunction choice ambient owner fieldName attrs eltTy

            match acc with
            | Accumulation.Optional ->
                failwith
                    $"ArgParser does not support optionals containing options at field %s{fieldName.idText}: %O{ty}"
            | Accumulation.Choice _ ->
                failwith
                    $"ArgParser does not support optionals containing choices at field %s{fieldName.idText}: %O{ty}"
            | Accumulation.List _ ->
                failwith $"ArgParser does not support optional lists at field %s{fieldName.idText}: %O{ty}"
            | Accumulation.Map _ ->
                failwith
                    $"ArgParser does not support optional maps at field %s{fieldName.idText}: a map is already satisfiable with no arguments, so it is empty rather than absent."
            | Accumulation.Required -> parseElt, Accumulation.Optional, childTy
        | ChoiceType elts ->
            match elts with
            | [ elt1 ; elt2 ] ->
                if not (SynType.provablyEqual elt1 elt2) then
                    failwith
                        $"ArgParser was unable to prove types %O{elt1} and %O{elt2} to be equal in a Choice. We require them to be equal."

                let parseElt, acc, childTy =
                    createParseFunction choice ambient owner fieldName attrs elt1

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
                | Accumulation.Map _ ->
                    failwith
                        $"ArgParser does not support choices containing maps at field %s{fieldName.idText}: a map is already satisfiable with no arguments, so it is empty rather than defaulted."
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
                        | [ "ArgumentDefaultValue" ]
                        | [ "ArgumentDefaultValueAttribute" ]
                        | [ "Plugins" ; "ArgumentDefaultValue" ]
                        | [ "Plugins" ; "ArgumentDefaultValueAttribute" ]
                        | [ "Myriad" ; "Plugins" ; "ArgumentDefaultValue" ]
                        | [ "Myriad" ; "Plugins" ; "ArgumentDefaultValueAttribute" ]
                        | [ "WoofWare" ; "Myriad" ; "Plugins" ; "ArgumentDefaultValue" ]
                        | [ "WoofWare" ; "Myriad" ; "Plugins" ; "ArgumentDefaultValueAttribute" ] ->
                            let spec =
                                match classifyDefaultValue attr.ArgExpr with
                                | DefaultValueExpr.ContextSensitive name ->
                                    failwith
                                        $"Field '%s{fieldName.idText}' has an [<ArgumentDefaultValue>] whose value uses the context-sensitive constant %s{name}. Its value depends on where it is written, and we reproduce it in the generated file rather than evaluating it at your attribute, so it would not mean there what it means in your source; we also emit it in more than one place, so it need not even be consistent within the generated file. Use [<ArgumentDefaultFunction>] instead: that function is evaluated in your own file."
                                | DefaultValueExpr.Identifier name ->
                                    failwith
                                        $"Field '%s{fieldName.idText}' has an [<ArgumentDefaultValue>] whose value names something (%s{name}) rather than writing out a constant. We reproduce the value in the generated file rather than evaluating it at your attribute, and that file hoists every `open` in your source above the parser, so the name need not resolve to the same binding there as here. Write the constant out literally, or use [<ArgumentDefaultFunction>]: that function is evaluated in your own file."
                                | DefaultValueExpr.Unrecognised ->
                                    failwith
                                        $"Field '%s{fieldName.idText}' has an [<ArgumentDefaultValue>] whose value we do not recognise as a constant. We reproduce the value in the generated file rather than evaluating it at your attribute, so we accept only a literal written out in full (optionally parenthesised). Use [<ArgumentDefaultFunction>] for anything else: that function is evaluated in your own file."
                                | DefaultValueExpr.Null -> ArgumentDefaultSpec.NullLiteral
                                | DefaultValueExpr.Constant c ->
                                    // Parenthesised, because the expression lands in positions such
                                    // as `(3).ToString ()` where a bare literal would lex wrongly.
                                    ArgumentDefaultSpec.Literal (SynExpr.paren (defaultValueExpr c))

                            Some spec
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
                createParseFunction choice ambient owner fieldName attrs eltTy

            match acc with
            | Accumulation.Map _ ->
                failwith
                    $"ArgParser does not support lists of maps at field %s{fieldName.idText}: a map already accumulates across occurrences."
            | _ -> ()

            parseElt, Accumulation.List acc, childTy
        | MapType (keyTy, valueTy) ->
            let keyValueSeparator =
                match
                    charAttribute [ "ArgumentKeyValueSeparator" ; "ArgumentKeyValueSeparatorAttribute" ] fieldName attrs
                with
                | Some sep -> sep
                | None ->
                    failwith
                        $"Field '%s{fieldName.idText}' has type %s{describeType ty}, so it requires an [<ArgumentKeyValueSeparator>] attribute giving the character which separates a key from its value within one entry. There is no default: which separator is safe depends on what your keys can spell."

            let entrySeparator =
                charAttribute [ "ArgumentMapEntrySeparator" ; "ArgumentMapEntrySeparatorAttribute" ] fieldName attrs

            match entrySeparator with
            | Some entry when entry = keyValueSeparator ->
                failwith
                    $"Field '%s{fieldName.idText}' uses '%s{keyValueSeparator}' as both its [<ArgumentKeyValueSeparator>] and its [<ArgumentMapEntrySeparator>]. They must differ, or no entry could be split into a key and a value."
            | _ -> ()

            // A separator is a single UTF-16 code unit, so it could in principle be half of a
            // surrogate pair. Splitting there would cut a character in two, and it would also
            // invalidate the code-unit-by-code-unit reasoning below about which spellings of an
            // enumerated value remain available. Refuse it rather than reason about it.
            for separator, attributeName in
                [
                    keyValueSeparator, "ArgumentKeyValueSeparator"
                    yield!
                        entrySeparator
                        |> Option.map (fun e -> e, "ArgumentMapEntrySeparator")
                        |> Option.toList
                ] do
                if Char.IsSurrogate separator.[0] then
                    failwith
                        $"Field '%s{fieldName.idText}' uses the unpaired surrogate U+%04X{int separator.[0]} as its [<%s{attributeName}>]. A separator must be a whole character: splitting on half of a surrogate pair would cut a character in two."

            // A key and a value each occupy one entry, so each must be a scalar leaf: anything
            // which accumulates (a list), or which is satisfiable by absence (an option, a
            // choice, a map), has no spelling inside a single entry.
            let scalar (role : string) (childTy : SynType) : SynExpr * SynType =
                let parser, acc, parsedTy =
                    createParseFunction choice ambient owner fieldName attrs childTy

                match acc with
                | Accumulation.Required -> parser, parsedTy
                | Accumulation.Optional
                | Accumulation.Choice _
                | Accumulation.List _
                | Accumulation.Map _ ->
                    failwith
                        $"ArgParser does not support map %s{role}s which are themselves lists, options, choices or maps, at field %s{fieldName.idText}: one entry spells one %s{role}."

            let keyParser, keyParsedTy = scalar "key" keyTy
            let valueParser, valueParsedTy = scalar "value" valueTy

            // Case names are arbitrary identifiers: a double-backtick name may contain any
            // character, including a separator. Where the spellings are known at generation time
            // we can reject the unspellable ones instead of silently misparsing.
            //
            // An enumerated value is matched with OrdinalIgnoreCase, so every character which that
            // comparison holds equal to one of the case name's is an alternative spelling, and a
            // position is only fatal when all of its alternatives are separators. (A case named
            // `A` survives the separator 'A', because it may be spelled `a`.)
            //
            // Invariant casing does *not* model that relation, in either direction: 'ſ'
            // uppercases to 'S' although OrdinalIgnoreCase holds them distinct, while the micro
            // sign is held equal to Greek mu despite being neither its upper nor its lower case.
            // So ask the comparison itself rather than reimplementing it.
            //
            // The key-value separator constrains only keys, since entries split at the *first*
            // one and so a value may contain it; the entry separator is stripped before that
            // split, so it constrains keys and values alike.
            let checkEnumSpellings (role : string) (ty : SynType) (separators : string list) : unit =
                match identifyAsEnum ambient.EnumDus ty with
                | None -> ()
                | Some union ->
                    // Working a code unit at a time is sound only because separators are not
                    // surrogates (rejected above): a supplementary character encodes to
                    // surrogates alone, so no separator can occur inside one, and such a
                    // character therefore never needs an alternative spelling. Case mapping for
                    // the pair as a whole — `𐐀` against `𐐨`, whose low surrogates differ — is
                    // consequently never a question we have to answer.
                    /// Every character the generated parser would accept in this one's place.
                    let alternativeSpellings (c : char) : string list =
                        let c = string<char> c

                        [ 0..0xFFFF ]
                        |> List.map (char >> string<char>)
                        |> List.filter (fun d -> String.Equals (d, c, StringComparison.OrdinalIgnoreCase))

                    for case in union.Cases do
                        let unavoidable =
                            case.Name.idText
                            |> Seq.tryPick (fun c ->
                                // A character spells itself, so unless it is a separator this
                                // position already has an escape and the search is over. Only on
                                // the rare path do we pay for the sweep.
                                if not (List.contains (string<char> c) separators) then
                                    None
                                elif alternativeSpellings c |> List.forall (fun d -> List.contains d separators) then
                                    Some c
                                else
                                    None
                            )

                        match unavoidable with
                        | None -> ()
                        | Some c ->
                            // Name the separators in the order they were declared, so the message
                            // points at attributes the author can see.
                            let blocking =
                                let accepted = alternativeSpellings c

                                separators
                                |> List.filter (fun s -> List.contains s accepted)
                                |> List.map (sprintf "'%s'")
                                |> String.concat " or "

                            failwith
                                $"Field '%s{fieldName.idText}' has map %s{role} type %s{union.Name.idText}, whose case '%s{case.Name.idText}' cannot be spelled without using a separator (%s{blocking}). No command line could express that %s{role}, so choose a different separator."

            let entrySeparators = Option.toList entrySeparator

            checkEnumSpellings "key" keyTy (keyValueSeparator :: entrySeparators)

            checkEnumSpellings "value" valueTy entrySeparators

            let spec =
                {
                    KeyValueSeparator = keyValueSeparator
                    EntrySeparator = entrySeparator
                    KeyParser = keyParser
                    KeyType = keyParsedTy
                }

            valueParser, Accumulation.Map spec, valueParsedTy
        | ty ->
            match identifyAsFlag ambient.FlagDus ty with
            | Some flagDu ->
                // Parse as a bool, and then do the `if-then` dance.
                let parser =
                    SynExpr.createIdent "x"
                    |> SynExpr.applyFunction (SynExpr.createLongIdent [ "System" ; "Boolean" ; "Parse" ])
                    |> FlagDu.FromBoolean flagDu
                    |> SynExpr.createLambda "x"

                parser, Accumulation.Required, ty
            | None ->

            // A union with no data in any case is an enumerated value: the user names the case
            // they want, matched case-insensitively.
            match identifyAsEnum ambient.EnumDus ty with
            | Some enumDu -> createEnumParser enumDu, Accumulation.Required, ty
            | None -> failwith $"Could not decide how to parse arguments for field %s{fieldName.idText} of type %O{ty}"

    /// The `[<ArgumentPrefix>]` this field carries, if any.
    let private prefixAttribute (attrs : SynAttribute list) : SynExpr option =
        attrs
        |> List.tryPick (fun attr ->
            match (List.last attr.TypeName.LongIdent).idText with
            | "ArgumentPrefixAttribute"
            | "ArgumentPrefix" -> Some attr.ArgExpr
            | _ -> None
        )

    /// Extend the prefix already in force with the one this field carries. The result carries its
    /// trailing separator, so applying a prefix is concatenation and the empty prefix is the
    /// identity; prefixes therefore compose from the outside in as the recursion descends.
    ///
    /// A prefix is combined into names as the parser is generated, so unlike an
    /// [<ArgumentLongForm>] -- which we can leave to be concatenated when the generated program
    /// runs -- it has to be a literal we can read here.
    let private extendPrefix (fieldName : Ident) (outer : string) (attrExpr : SynExpr) : string =
        let prefix =
            match classifyDefaultValue attrExpr with
            | Constant (SynConst.String (s, _, _)) -> s
            | Identifier name ->
                failwith
                    $"[<ArgumentPrefix>] on field '%s{fieldName.idText}' must be a string literal written out in full, but its value names something (%s{name}) instead. The prefix is combined with every argument name in that field's subtree as the parser is generated, so it has to be known then; the generated file also hoists every `open` in your source above the parser, so a name need not resolve there to what it means here."
            | _ ->
                failwith
                    $"[<ArgumentPrefix>] on field '%s{fieldName.idText}' must be a string literal written out in full, but we do not recognise its value as one. The prefix is combined with every argument name in that field's subtree as the parser is generated, so it has to be known then."

        // A prefix is concatenated into every name beneath it, so a prefix no token could address
        // makes the whole subtree unaddressable. Report it here rather than letting the assembled
        // names fail the ordinary checks, where the name blamed would be the concatenation and the
        // author would have to work backwards to the cause.
        if
            prefix = ""
            || prefix.Contains "="
            || prefix.[0] = '-'
            || prefix.[prefix.Length - 1] = '-'
        then
            failwith
                $"[<ArgumentPrefix>] on field '%s{fieldName.idText}' must be a non-empty string which does not contain '=' and does not start or end with '-' (the generated parser inserts the separating '-' itself), but got '%s{prefix}'. The prefix is used exactly as written, so spell it as you want it to appear on the command line, without the leading '--'."

        outer + prefix + "-"

    /// Namespace an argument's spelling under the prefix in force. A form we can read here becomes
    /// a constant, so it stays visible to the generation-time name checks; one we cannot (an
    /// [<ArgumentLongForm>] naming a [<Literal>]) becomes a concatenation the generated program
    /// performs, which those checks already skip and the runtime schema check already catches.
    ///
    /// The constant holds the argument's *semantic* spelling, decoded, which is what the
    /// generation-time name checks must compare: they match names under the scanner's own
    /// case-insensitive equality, and an escaped rendering would have them comparing source syntax
    /// instead (`é` against `É`, which differ where `é` and `É` collide). Escaping for
    /// emission is a separate concern, and applies equally to the spellings we do not rebuild.
    let private applyPrefix (prefix : string) (form : SynExpr) : SynExpr =
        if prefix = "" then
            form
        else

        match SynExpr.stripOptionalParen form with
        | SynExpr.Const (SynConst.String (s, _, _), _) -> SynExpr.CreateConst (prefix + s)
        | form ->
            // A form we cannot read stays an expression the generated program evaluates, so the
            // prefix is joined to it there. That makes this branch pure emission -- `literalForms`
            // matches only a bare constant, so nothing here reaches the name checks -- and the
            // constant we are about to write out needs escaping like any other.
            SynExpr.plus
                (SynExpr.Const (
                    SynConst.String (ArgFormEmission.escapeStringConstant prefix, SynStringKind.Regular, range0),
                    range0
                ))
                (SynExpr.paren form)

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

    /// `prefix` is the namespace in force for every argument in this record, accumulated from the
    /// [<ArgumentPrefix>]es on the fields traversed to reach it. It carries its trailing separator,
    /// and is "" at the root.
    let rec private toParseSpec
        (ancestors : string list)
        (prefix : string)
        (counter : int)
        (ambient : AmbientTypes)
        (finalRecord : RecordType)
        : ParseTree * int
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
                    // Every consumer of an argument's spelling -- help text, the --no- variant, the
                    // generation-time conflict checks, and the erased schema handed to the runtime
                    // -- reads it from here, so namespacing it here namespaces it everywhere.
                    |> List.map (applyPrefix prefix)

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
                        | "ArgumentDefaultValue"
                        | "ArgumentDefaultValueAttribute"
                        | "ArgumentDefaultEnvironmentVariable"
                        | "ArgumentDefaultEnvironmentVariableAttribute" -> true
                        | _ -> false
                    )

                if hasDefaultAttr then
                    match positionalArgAttr, fieldType with
                    | Some _, _ ->
                        failwith
                            $"Field '%s{ident.idText}' is positional, so it cannot carry a default-value attribute ([<ArgumentDefaultFunction>], [<ArgumentDefaultValue>], or [<ArgumentDefaultEnvironmentVariable>]): positional args are collected, not defaulted."
                    | None, ChoiceType _ -> ()
                    | None, _ ->
                        failwith
                            $"Field '%s{ident.idText}' has a default-value attribute ([<ArgumentDefaultFunction>], [<ArgumentDefaultValue>], or [<ArgumentDefaultEnvironmentVariable>]), but its type is not Choice<'a, 'a>. Defaults are surfaced through Choice<'a, 'a> so a successful parse can report whether a value was user-supplied (Choice1Of2) or defaulted (Choice2Of2); a bare field cannot express this. Change the field's type to Choice<'a, 'a>, or remove the attribute."

                let prefixAttr = prefixAttribute attrs

                // The structural branches below run before any leaf machinery, so this pairing has
                // to be caught ahead of the dispatch: on a record-typed field it would otherwise
                // prefix the subtree and drop the [<PositionalArgs>] without a word.
                match prefixAttr, positionalArgAttr with
                | Some _, Some _ ->
                    failwith
                        $"[<ArgumentPrefix>] was applied to field '%s{ident.idText}', which carries [<PositionalArgs>]. A positional-args field has no subtree of nested arguments to namespace. If you want positional args nested under a prefix, move the [<PositionalArgs>] field into a sub-record and put the [<ArgumentPrefix>] on the record-typed field which holds it."
                | _ -> ()

                let ambientRecordMatch =
                    match localTypeName fieldType with
                    | Some target -> ambient.Records |> List.tryFind (fun r -> r.Name.idText = target)
                    | None -> None

                let ambientUnionMatch =
                    match localTypeName fieldType with
                    | Some target -> ambient.StructuralUnions |> List.tryFind (fun u -> u.Name.idText = target)
                    | None -> None

                match ambientRecordMatch with
                | Some childRecord ->
                    // The structural branches are taken before any leaf machinery runs, so they
                    // must reject the map-only attributes themselves; otherwise an author who
                    // misplaced one would be told nothing at all.
                    rejectSeparatorAttributes ident fieldType attrs
                    rejectLongFormAttribute ident fieldType attrs

                    // This field has a type we need to obtain from parsing another record.
                    let childPrefix =
                        match prefixAttr with
                        | None -> prefix
                        | Some attrExpr -> extendPrefix ident prefix attrExpr

                    let spec, counter = toParseSpec ancestors childPrefix counter ambient childRecord

                    counter, (ident, spec) :: acc
                | None ->

                match ambientUnionMatch with
                | Some union ->
                    rejectSeparatorAttributes ident fieldType attrs
                    rejectLongFormAttribute ident fieldType attrs

                    // A discriminated union of alternative argument sets: exactly one case's
                    // arguments must be supplied. (Flag-like and data-free unions are argument
                    // leaves, not alternatives, and are not in StructuralUnions.)
                    let childPrefix =
                        match prefixAttr with
                        | None -> prefix
                        | Some attrExpr -> extendPrefix ident prefix attrExpr

                    let spec, counter = unionToParseSpec ancestors childPrefix counter ambient union

                    counter, (ident, spec) :: acc
                | None ->

                // The structural branches above have consumed every field which has a subtree, so
                // anything reaching here is a leaf, where [<ArgumentPrefix>] has nothing to
                // namespace and would otherwise be silently dropped. (A prefixed field which is
                // also positional was rejected before the dispatch.)
                match prefixAttr with
                | Some _ ->
                    failwith
                        $"[<ArgumentPrefix>] can only be applied to a field whose type is another [<ArgParser>]-schema record or a discriminated union of alternative argument sets, but was applied to field '%s{ident.idText}' of type %s{describeType fieldType}. It renames every argument contributed by that field's subtree by prepending a namespace (e.g. [<ArgumentPrefix \"foo\">] on a field whose type is a record containing `Blah : string` turns --blah into --foo-blah); a leaf field has no subtree to rename. To change this one argument's name, use [<ArgumentLongForm>] instead."
                | None -> ()

                match positionalArgAttr with
                | Some includeFlagLike ->
                    // Positional fields carrying a default attribute are rejected above, so the
                    // Choice-parsing path only ever reaches this callback with `None`.
                    let getChoice (_ : ArgumentDefaultSpec option) : unit = ()

                    let parser, accumulation, parseTy =
                        createParseFunction<unit> getChoice ambient finalRecord.Name ident attrs fieldType

                    checkSeparatorAttributesPlacement ident fieldType attrs accumulation

                    let isBoolLike =
                        match parseTy with
                        | PrimitiveType ident when ident |> List.map _.idText = [ "System" ; "Boolean" ] ->
                            Some (Choice2Of2 ())
                        | parseTy -> identifyAsFlag ambient.FlagDus parseTy |> Option.map Choice1Of2

                    let enumCases = identifyAsEnum ambient.EnumDus parseTy

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
                            DisplayType = None
                            ArgForm = longForms
                            Help = helpText
                            BoolCases = isBoolLike
                            EnumCases = enumCases
                            AcceptsNegation = false
                        }
                        |> ParseTree.PositionalLeaf
                    | Accumulation.List Accumulation.Required ->
                        {
                            FieldName = ident
                            Parser = parser
                            TargetVariable = Ident.create $"arg_%i{counter}"
                            Accumulation = ChoicePositional.Normal includeFlagLike
                            TargetType = parseTy
                            DisplayType = None
                            ArgForm = longForms
                            Help = helpText
                            BoolCases = isBoolLike
                            EnumCases = enumCases
                            AcceptsNegation = false
                        }
                        |> ParseTree.PositionalLeaf
                    | Accumulation.List (Accumulation.Map _) ->
                        failwith "A list of positional args cannot contain maps."
                    | Accumulation.Choice _
                    | Accumulation.Optional
                    | Accumulation.Required
                    | Accumulation.Map _ ->
                        failwith
                            $"Expected positional arg accumulation type to be List, but it was %s{describeType fieldType}"
                | None ->
                    let getChoice (spec : ArgumentDefaultSpec option) : ArgumentDefaultSpec =
                        match spec with
                        | None ->
                            failwith
                                "Non-positional Choice args must have an `[<ArgumentDefault*>]` attribute on them."
                        | Some spec -> spec

                    let parser, accumulation, parseTy =
                        createParseFunction getChoice ambient finalRecord.Name ident attrs fieldType

                    checkSeparatorAttributesPlacement ident fieldType attrs accumulation

                    // A map's `parseTy` describes its *values*, not the field, so the boolean and
                    // enumerated metadata derived from it would misdescribe the argument. In
                    // particular a bool-valued map must keep arity one: an occurrence always
                    // carries an encoded entry, so `--thing` alone is missing its value rather
                    // than meaning "true", and negation would have nothing to negate.
                    let isMap =
                        match accumulation with
                        | Accumulation.Map _ -> true
                        | Accumulation.Required
                        | Accumulation.Optional
                        | Accumulation.Choice _
                        | Accumulation.List _ -> false

                    let isBoolLike =
                        if isMap then
                            None
                        else

                        match parseTy with
                        | PrimitiveType ident when ident |> List.map _.idText = [ "System" ; "Boolean" ] ->
                            Some (Choice2Of2 ())
                        | parseTy -> identifyAsFlag ambient.FlagDus parseTy |> Option.map Choice1Of2

                    let enumCases =
                        if isMap then
                            None
                        else
                            identifyAsEnum ambient.EnumDus parseTy

                    let displayType =
                        // Each half of an entry is spelled exactly as it would be were it a
                        // scalar leaf, so describe it the same way: a flag DU accepts true/false,
                        // and an enumerated value accepts one of its case names. Naming the bare
                        // type instead would leave the user guessing.
                        let describeComponent (ty : SynType) : string =
                            match identifyAsFlag ambient.FlagDus ty with
                            | Some _ -> "bool"
                            | None ->

                            match identifyAsEnum ambient.EnumDus ty with
                            | Some union ->
                                let values = checkedEnumCaseNames union |> String.concat "|"

                                $"%s{describeType ty} [one of: %s{values}]"
                            | None -> describeType ty

                        match accumulation with
                        | Accumulation.Map spec ->
                            Some $"map<%s{describeComponent spec.KeyType}, %s{describeComponent parseTy}>"
                        | Accumulation.Required
                        | Accumulation.Optional
                        | Accumulation.Choice _
                        | Accumulation.List _ -> None

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
                                    $"[<ArgumentNegateWithPrefix>] can only be applied to boolean or flag DU fields, but was applied to field %s{ident.idText} of type %s{describeType fieldType}"
                        else
                            false

                    {
                        FieldName = ident
                        Parser = parser
                        TargetVariable = Ident.create $"arg_%i{counter}"
                        Accumulation = accumulation
                        TargetType = parseTy
                        DisplayType = displayType
                        ArgForm = longForms
                        Help = helpText
                        BoolCases = isBoolLike
                        EnumCases = enumCases
                        AcceptsNegation = acceptsNegation
                    }
                    |> ParseTree.NonPositionalLeaf
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
        (prefix : string)
        (counter : int)
        (ambient : AmbientTypes)
        (union : UnionType)
        : ParseTree * int
        =
        let ancestors = pushSchemaType ancestors union.Name

        // The cases are alternatives, so their argument names must already be distinct from one
        // another (`accumulators` pools every case's named leaves into one conflict check, with no
        // per-case exemption). A prefix here would therefore disambiguate nothing, and silently
        // dropping it would leave the author with names they did not ask for.
        union.Cases
        |> List.iter (fun case ->
            match prefixAttribute case.Attributes with
            | None -> ()
            | Some _ ->
                failwith
                    $"[<ArgumentPrefix>] was applied to case '%s{case.Name.idText}' of [<ArgParser>] union '%s{union.Name.idText}', but it belongs on a field. A union's cases are alternatives, so their argument names must already be distinct from one another, and a prefix here would buy no disambiguation. To namespace every case's arguments at once, put the [<ArgumentPrefix>] on the field whose type is '%s{union.Name.idText}'."
        )

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
                            | Some target -> ambient.Records |> List.tryFind (fun r -> r.Name.idText = target)
                            | None -> None

                        match payload with
                        | Some payload -> payload
                        | None ->
                            failwith
                                $"Case %s{case.Name.idText} of [<ArgParser>] union %s{union.Name.idText} must have a payload which is a record defined alongside the union."
                    | [] ->
                        // Every case being data-free is the enumerated-value schema, which is a
                        // leaf and so never reaches this function; a mixture of the two shapes has
                        // no meaning yet.
                        failwith
                            $"Case %s{case.Name.idText} of [<ArgParser>] union %s{union.Name.idText} has no data. A union whose cases *all* have no data is parsed as an enumerated value, and a union of alternative argument sets needs a record payload on every case; a mixture of the two is not yet supported."
                    | _ ->
                        failwith
                            $"Case %s{case.Name.idText} of [<ArgParser>] union %s{union.Name.idText} must have exactly one field: a record holding that case's arguments."

                // A case is an alternative, not a nesting level: the prefix in force passes
                // through unchanged, so every case's arguments are namespaced identically.
                let spec, counter = toParseSpec ancestors prefix counter ambient payloadRecord

                counter, (case.Name, spec) :: acc
            )

        let cases = List.rev cases

        let assemble (caseName : Ident) (payload : SynExpr) : SynExpr =
            SynExpr.applyFunction (SynExpr.createLongIdent' [ union.Name ; caseName ]) payload

        ParseTree.Sum (sumId, cases, assemble), counter

    let private helpText (typeHelp : SynExpr option) (tree : ParseTree) : SynBinding =
        let describeNonPositional (arg : ParseFunctionNonPositional) : SynExpr =
            let flagCases = arg.BoolCases

            match arg.Accumulation with
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
                // Display the spelling the user would have to type to supply this value.
                SynExpr.callMethod var.idText (SynExpr.createIdent' owner)
                |> renderLeafValue flagCases arg.EnumCases
                |> SynExpr.pipeThroughFunction (
                    SynExpr.applyFunction (SynExpr.createIdent "sprintf") (SynExpr.CreateConst " (default value: %s)")
                )
                |> SynExpr.paren
            | Accumulation.Choice (ArgumentDefaultSpec.Literal value) ->
                // A literal written in the user's own source is not a secret, so unlike the env-var
                // case we can display it; as for a default function, display the spelling the user
                // would have to type to supply it.
                value
                |> renderLeafValue flagCases arg.EnumCases
                |> SynExpr.pipeThroughFunction (
                    SynExpr.applyFunction (SynExpr.createIdent "sprintf") (SynExpr.CreateConst " (default value: %s)")
                )
                |> SynExpr.paren
            | Accumulation.Choice ArgumentDefaultSpec.NullLiteral ->
                // There is no spelling of `null` the user could type at the command line, and
                // `ToString` on it would throw, so name it rather than rendering it.
                SynExpr.CreateConst " (default value: <null>)"
            | Accumulation.List _ -> SynExpr.CreateConst " (can be repeated)"
            | Accumulation.Map spec ->
                // The type's name says nothing about how to spell an entry, so the help text
                // must show the separators the field was configured with.
                let entry = $"KEY%s{spec.KeyValueSeparator}VALUE"

                let format =
                    match spec.EntrySeparator with
                    | None -> entry
                    | Some entrySep -> $"%s{entry}[%s{entrySep}%s{entry}...]"

                SynExpr.CreateConst $" (%s{format}; can be repeated)"

        let describePositional (_ : ParseFunctionPositional) =
            SynExpr.CreateConst " (positional args) (can be repeated)"

        /// We may sometimes lie about the type name, if e.g. this is a flag DU which we're pretending is a boolean;
        /// and a data-free union's name is augmented with the values it accepts. So the whole `ParseFunction` is in
        /// scope here, not just its `Accumulation`.
        /// `depth` is the nesting depth in union alternatives; each level indents by two spaces.
        let toPrintable (depth : int) (describe : ParseFunction<'a> -> SynExpr) (arg : ParseFunction<'a>) : SynExpr =
            let ty =
                match arg.DisplayType with
                | Some display -> display
                | None ->

                match arg.BoolCases, arg.EnumCases with
                | Some _, _ -> "bool"
                // The type's name alone says nothing about how to spell one of its values.
                | None, Some union ->
                    let values = checkedEnumCaseNames union |> String.concat "|"

                    SynType.toHumanReadableString arg.TargetType + $" [one of: %s{values}]"
                | None, None -> SynType.toHumanReadableString arg.TargetType

            let helpText =
                match arg.Help with
                | None -> SynExpr.CreateConst ""
                | Some helpText ->
                    SynExpr.applyFunction (SynExpr.createIdent "sprintf") (SynExpr.CreateConst " : %s")
                    |> SynExpr.applyTo (SynExpr.paren helpText)
                    |> SynExpr.paren

            let descriptor = describe arg

            let indent = String.replicate depth "  "

            // `ty` is passed as an argument rather than spliced into the format literal: it is
            // derived from user-chosen type and case names, and `%` is legal in a backticked
            // identifier, so splicing it would emit uncompilable format specifiers. (`indent` is
            // whitespace by construction, so it is safe to splice.)
            SynExpr.applyFunction (SynExpr.createIdent "sprintf") (SynExpr.CreateConst $"%s{indent}%%s  %%s%%s%%s")
            |> SynExpr.applyTo arg.HumanReadableArgForm
            |> SynExpr.applyTo (SynExpr.CreateConst ty)
            |> SynExpr.applyTo descriptor
            |> SynExpr.applyTo helpText
            |> SynExpr.paren

        // Walk the tree so that a union's alternatives are *grouped* in the help, not flattened
        // into one undifferentiated list: the user must be able to see which arguments go
        // together. Non-positional lines appear in declaration order; the positional-args
        // line, if any, comes last, as it always has (ParseTree.branch keeps the
        // positional-claiming field after its siblings, and a sink beside a union is shared
        // by every alternative, so it stays outside the case groups).
        let rec fieldHelp (depth : int) (tree : ParseTree) : SynExpr list =
            match tree with
            | ParseTree.NonPositionalLeaf pf -> [ toPrintable depth describeNonPositional pf ]
            | ParseTree.PositionalLeaf pf -> [ toPrintable depth describePositional pf ]
            | ParseTree.Branch (fields, _) -> fields |> List.collect (fun (_, child) -> fieldHelp depth child)
            | ParseTree.Sum (_, cases, _) -> sumHelp depth cases

        and sumHelp (depth : int) (cases : (Ident * ParseTree) list) : SynExpr list =
            let indent = String.replicate depth "  "

            SynExpr.CreateConst (indent + "exactly one of the following sets of arguments:")
            :: (cases
                |> List.collect (fun (caseName, case) ->
                    SynExpr.CreateConst (indent + caseName.idText + ":")
                    :: fieldHelp (depth + 1) case
                ))

        let fieldHelp = fieldHelp 0 tree

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
        (spec : ParseTree)
        : SynExpr
        =
        // For each argument (positional and non-positional), create an accumulator for it; also
        // check the structural constraints which the runtime's selection semantics rely on.
        ParseTree.checkSumAmbiguity spec
        let nonPos, pos = ParseTree.accumulators spec
        let hasSum = ParseTree.containsSum spec

        // Positional args may live beside a union, or inside its cases, only when the
        // scanner's treatment of an unrecognised `--key`-shaped token is provably Reject
        // (fatal). A Collect-mode sink treats such a token as a positional arg, so a typo of a
        // case-selecting argument would be silently collected — with a union in play, silently
        // changing which alternative is chosen. Bare positional tokens are sound: they are
        // routed to a sink only after case selection, and never influence it.
        if hasSum then
            for pf in pos do
                let includeFlagLike =
                    match pf.Accumulation with
                    | ChoicePositional.Normal fl
                    | ChoicePositional.Choice fl -> fl

                match includeFlagLike with
                // The default [<PositionalArgs>] is Reject.
                | None -> ()
                | Some expr ->
                    match SynExpr.stripOptionalParen expr with
                    | SynExpr.Const (SynConst.Bool false, _) -> ()
                    | SynExpr.Const (SynConst.Bool true, _) ->
                        failwith
                            "Positional args which collect unrecognised flag-like tokens ([<PositionalArgs true>]) cannot be combined with a discriminated-union arg: a mistyped case-selecting argument would be collected as a positional arg instead of being reported."
                    | _ ->
                        // E.g. a [<Literal>] constant, which the untyped AST does not resolve.
                        failwith
                            "Positional args combined with a discriminated-union arg must provably reject unrecognised flag-like tokens: use [<PositionalArgs>] or a literal [<PositionalArgs false>]."

        let bindings =
            nonPos
            |> List.collect (fun pf ->
                let slot =
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
                    | Accumulation.List (Accumulation.Choice _)
                    | Accumulation.List (Accumulation.Map _) ->
                        failwith
                            "WoofWare.Myriad invariant violated: expected a list to contain only a Required accumulation. Non-positional lists cannot be optional or Choice, nor can they themselves contain lists or maps."
                    | Accumulation.Map spec ->
                        // Entries accumulate as key-value pairs and become a Map at the end, so that
                        // a duplicate key can be spotted (and reported against the offending
                        // occurrence) rather than silently overwriting.
                        SynExpr.createIdent "ResizeArray"
                        |> SynExpr.applyTo (SynExpr.CreateConst ())
                        |> SynBinding.basic [ pf.TargetVariable ] []
                        |> SynBinding.withReturnAnnotation (
                            SynType.appPostfix
                                "ResizeArray"
                                (SynType.tupleNoParen [ spec.KeyType ; pf.TargetType ]
                                 |> Option.defaultWith (fun () ->
                                     failwith
                                         "WoofWare.Myriad internal error: could not build the key-value pair type for a map field"
                                 )
                                 |> SynType.paren)
                        )
                    | Accumulation.List Accumulation.Required ->
                        SynExpr.createIdent "ResizeArray"
                        |> SynExpr.applyTo (SynExpr.CreateConst ())
                        |> SynBinding.basic [ pf.TargetVariable ] []
                        |> SynBinding.withReturnAnnotation (SynType.appPostfix "ResizeArray" pf.TargetType)

                match pf.Accumulation with
                | Accumulation.Map spec ->
                    // The keys supplied so far, carried between occurrences rather than rebuilt
                    // from the accumulator each time: a map is often filled by many repeated
                    // occurrences, and rebuilding would make that quadratic.
                    let seen =
                        SynExpr.createLongIdent [ "Set" ; "empty" ]
                        |> SynBinding.basic [ seenVariable pf.TargetVariable ] []
                        |> SynBinding.withMutability true
                        |> SynBinding.withReturnAnnotation (SynType.app "Set" [ spec.KeyType ])

                    [ slot ; seen ]
                | Accumulation.Required
                | Accumulation.Optional
                | Accumulation.Choice _
                | Accumulation.List _ -> [ slot ]
            )

        let bindings =
            // One accumulator per positional sink (or the legacy leftover-args accumulator
            // when there is none, kept so that the no-sink shape of the generated code stays
            // stable).
            let sinkBindings =
                match pos with
                | [] ->
                    [
                        SynExpr.createIdent "ResizeArray"
                        |> SynExpr.applyTo (SynExpr.CreateConst ())
                        |> SynBinding.basic [ Ident.create "parser_LeftoverArgs" ] []
                        |> SynBinding.withReturnAnnotation (SynType.appPostfix "ResizeArray" SynType.string)
                    ]
                | pos ->
                    pos
                    |> List.map (fun pf ->
                        let elementType =
                            match pf.Accumulation with
                            | ChoicePositional.Choice _ -> SynType.app "Choice" [ pf.TargetType ; pf.TargetType ]
                            | ChoicePositional.Normal _ -> pf.TargetType

                        SynExpr.createIdent "ResizeArray"
                        |> SynExpr.applyTo (SynExpr.CreateConst ())
                        |> SynBinding.basic [ pf.TargetVariable ] []
                        |> SynBinding.withReturnAnnotation (SynType.appPostfix "ResizeArray" elementType)
                    )

            sinkBindings @ bindings

        let helpText = helpText typeHelpText spec

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
                        | Accumulation.List _
                        | Accumulation.Map _ -> rt [ "ErasedRequirement" ; "Optional" ]

                    let arity =
                        match pf.BoolCases with
                        | Some _ -> rt [ "ErasedArity" ; "BoolLike" ]
                        | None -> rt [ "ErasedArity" ; "One" ]

                    let repeatable =
                        match pf.Accumulation with
                        | Accumulation.List _
                        | Accumulation.Map _ -> SynExpr.CreateConst true
                        | Accumulation.Required
                        | Accumulation.Optional
                        | Accumulation.Choice _ -> SynExpr.CreateConst false

                    [
                        field "Id" (SynExpr.CreateConst index)
                        field "Forms" (listOf (pf.ArgForm |> List.map ArgFormEmission.emitArgForm))
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
                let posCounter = ref 0

                ParseTree.toErasedTreeExpr rt listOf counter posCounter spec |> SynExpr.paren

            let positionals =
                pos
                |> List.mapi (fun index pf ->
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

                    [
                        field "Id" (SynExpr.CreateConst index)
                        field "Forms" (listOf (pf.ArgForm |> List.map ArgFormEmission.emitArgForm))
                        field "FlagLike" flagLike
                        field "TypeDescription" (SynExpr.CreateConst "")
                        field "Help" (SynExpr.createIdent "None")
                    ]
                    |> SynExpr.createRecord None
                )

            [
                field "Leaves" leaves
                field "Tree" tree
                field "Positionals" (listOf positionals)
            ]
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

        /// Store every entry carried by one occurrence of a `Map`-typed leaf.
        ///
        /// The entries are staged and applied together: an occurrence which fails partway through
        /// must leave the slot untouched, or the entries which did land would go on to provoke a
        /// spurious duplicate-key error and bury the real one. Duplicates are detected on the
        /// *parsed* key, so two spellings of one enumerated value collide as they should, but
        /// reported with the key as the user spelled it.
        let storeMapOccurrence (pf : ParseFunctionNonPositional) (spec : MapSpec) : SynExpr =
            let form = pf.HumanReadableArgForm
            let entry = SynExpr.createIdent "entry"
            let pending = SynExpr.createIdent "parser_pending"
            let seen = SynExpr.createIdent "parser_seen"
            let staged = SynExpr.createIdent "parser_entry"

            /// The key of a staged `((key, value), rawKey)`.
            let stagedKey =
                staged
                |> SynExpr.applyFunction (SynExpr.createIdent "fst")
                |> SynExpr.paren
                |> SynExpr.applyFunction (SynExpr.createIdent "fst")
                |> SynExpr.paren

            let occurrenceEntries =
                match spec.EntrySeparator with
                | None -> SynExpr.listLiteral [ SynExpr.createIdent "value" ]
                | Some entrySeparator ->
                    // The `string []` overload, rather than the tidier `Split (string)`: the
                    // latter arrived with .NET Core 2.0, and generated code has to compile
                    // wherever the consumer targets, including netstandard2.0 and .NET Framework.
                    // `StringSplitOptions.None` keeps empty entries, which then fail as entries
                    // with no separator rather than vanishing.
                    SynExpr.createIdent "value"
                    |> SynExpr.callMethodArg
                        "Split"
                        (SynExpr.tuple
                            [
                                SynExpr.arrayLiteral [ SynExpr.CreateConst entrySeparator ]
                                SynExpr.createLongIdent [ "System" ; "StringSplitOptions" ; "None" ]
                            ])
                    |> SynExpr.pipeThroughFunction (SynExpr.createLongIdent [ "Array" ; "toList" ])

            let parseEntry =
                let separatorIndex =
                    entry
                    |> SynExpr.callMethodArg
                        "IndexOf"
                        (SynExpr.tuple
                            [
                                SynExpr.CreateConst spec.KeyValueSeparator
                                SynExpr.createLongIdent [ "System" ; "StringComparison" ; "Ordinal" ]
                            ])

                let malformed =
                    SynExpr.createIdent "sprintf"
                    |> SynExpr.applyTo (SynExpr.CreateConst "Entry '%s' for '%s' does not contain the separator '%s'")
                    |> SynExpr.applyTo entry
                    |> SynExpr.applyTo form
                    |> SynExpr.applyTo (SynExpr.CreateConst spec.KeyValueSeparator)
                    |> SynExpr.paren
                    |> SynExpr.applyFunction (SynExpr.createIdent "failwith")

                // Splitting at the *first* separator is what makes a value unrestricted: whatever
                // follows it is the value, separators and all.
                let split =
                    SynExpr.createLet
                        [
                            entry
                            |> SynExpr.callMethodArg
                                "Substring"
                                (SynExpr.tuple [ SynExpr.CreateConst 0 ; SynExpr.createIdent "parser_sep" ])
                            |> SynBinding.basic [ Ident.create "parser_key" ] []
                        ]
                        (SynExpr.tuple
                            [
                                SynExpr.tuple
                                    [
                                        SynExpr.createIdent "parser_key" |> SynExpr.pipeThroughFunction spec.KeyParser
                                        entry
                                        |> SynExpr.callMethodArg
                                            "Substring"
                                            (SynExpr.paren (
                                                SynExpr.plus (SynExpr.createIdent "parser_sep") (SynExpr.CreateConst 1)
                                            ))
                                        |> SynExpr.paren
                                        |> SynExpr.pipeThroughFunction pf.Parser
                                    ]
                                SynExpr.createIdent "parser_key"
                            ])

                SynExpr.createLet
                    [ separatorIndex |> SynBinding.basic [ Ident.create "parser_sep" ] [] ]
                    (SynExpr.ifThenElse
                        (SynExpr.lessThan (SynExpr.CreateConst 0) (SynExpr.createIdent "parser_sep"))
                        split
                        malformed)
                |> SynExpr.createLambda "entry"

            let duplicateCheck =
                let complain =
                    SynExpr.createIdent "sprintf"
                    |> SynExpr.applyTo (SynExpr.CreateConst "Key '%s' was supplied more than once for '%s'")
                    |> SynExpr.applyTo (SynExpr.paren (SynExpr.applyFunction (SynExpr.createIdent "snd") staged))
                    |> SynExpr.applyTo form
                    |> SynExpr.paren
                    |> SynExpr.applyFunction (SynExpr.createIdent "failwith")

                SynExpr.createForEach
                    (SynPat.named "parser_entry")
                    pending
                    (SynExpr.sequential
                        [
                            SynExpr.ifThenElse
                                (SynExpr.createLongIdent [ "Set" ; "contains" ]
                                 |> SynExpr.applyTo stagedKey
                                 |> SynExpr.applyTo seen)
                                (SynExpr.CreateConst ())
                                complain
                            SynExpr.createLongIdent [ "Set" ; "add" ]
                            |> SynExpr.applyTo stagedKey
                            |> SynExpr.applyTo seen
                            |> SynExpr.assign (SynLongIdent.createS "parser_seen")
                        ])

            SynExpr.createLet
                [
                    occurrenceEntries
                    |> SynExpr.pipeThroughFunction (
                        SynExpr.applyFunction (SynExpr.createLongIdent [ "List" ; "map" ]) (SynExpr.paren parseEntry)
                    )
                    |> SynBinding.basic [ Ident.create "parser_pending" ] []

                    // A persistent set, so starting from the field's accumulated keys is O(1)
                    // rather than a rebuild from every entry stored so far.
                    SynExpr.createIdent' (seenVariable pf.TargetVariable)
                    |> SynBinding.basic [ Ident.create "parser_seen" ] []
                    |> SynBinding.withMutability true
                ]
                (SynExpr.sequential
                    [
                        duplicateCheck
                        // Both mutations happen only once every entry has been parsed and found
                        // fresh, so a failure partway leaves the field exactly as it was.
                        pending
                        |> SynExpr.pipeThroughFunction (
                            SynExpr.applyFunction
                                (SynExpr.createLongIdent [ "List" ; "map" ])
                                (SynExpr.createIdent "fst")
                        )
                        |> SynExpr.paren
                        |> SynExpr.applyFunction (
                            SynExpr.createLongIdent' [ pf.TargetVariable ; Ident.create "AddRange" ]
                        )
                        seen |> SynExpr.assign (SynLongIdent.createI (seenVariable pf.TargetVariable))
                    ])
            |> tryStore (occurrenceField "Source")

        let storeOccurrenceBinding : SynBinding =
            let branches =
                indexed
                |> List.map (fun (index, pf) ->
                    match pf.Accumulation with
                    | Accumulation.Map spec ->
                        // A map leaf has arity one, so the runtime always supplies a value; the
                        // whole occurrence is then split into entries.
                        SynExpr.createMatch
                            (occurrenceField "Value")
                            [
                                SynMatchClause.create
                                    (SynPat.nameWithArgs "Some" [ SynPat.named "value" ])
                                    (storeMapOccurrence pf spec)
                                SynMatchClause.create
                                    (SynPat.named "None")
                                    (internalError "arity-one occurrence with no value")
                            ]
                        |> SynMatchClause.create (SynPat.createConst (SynConst.Int32 index))
                    | Accumulation.Required
                    | Accumulation.Optional
                    | Accumulation.Choice _
                    | Accumulation.List _ ->

                    // The typed value to store, as a function of `value` (the raw string) for
                    // valued occurrences; boolean-like leaves also handle the arity-0 case.
                    let wrapChoice (e : SynExpr) : SynExpr =
                        match pf.Accumulation with
                        | Accumulation.Choice _ ->
                            SynExpr.applyFunction (SynExpr.createIdent "Choice1Of2") (SynExpr.paren e)
                        | Accumulation.Required
                        | Accumulation.Optional
                        | Accumulation.List _
                        | Accumulation.Map _ -> e

                    let store (e : SynExpr) : SynExpr =
                        match pf.Accumulation with
                        | Accumulation.Map _ ->
                            failwith
                                "WoofWare.Myriad invariant violated: a map leaf stores its occurrences through storeMapOccurrence."
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
                                | Accumulation.List _
                                | Accumulation.Map _ -> e

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
                        | Accumulation.List _
                        | Accumulation.Map _ -> body
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
                | [] ->
                    // Never called: the runtime routes positional values only when the schema
                    // has a sink.
                    internalError "no positional sink exists"
                | pos ->
                    // Dispatch on the sink id like storeOccurrence dispatches on the leaf id.
                    let branches =
                        pos
                        |> List.mapi (fun index pf ->
                            let converted =
                                let plain = SynExpr.createIdent "value" |> SynExpr.pipeThroughFunction pf.Parser

                                match pf.Accumulation with
                                | ChoicePositional.Normal _ -> plain
                                | ChoicePositional.Choice _ ->
                                    SynExpr.ifThenElse
                                        (SynExpr.createIdent "afterSeparator")
                                        (SynExpr.applyFunction (SynExpr.createIdent "Choice1Of2") (SynExpr.paren plain))
                                        (SynExpr.applyFunction (SynExpr.createIdent "Choice2Of2") (SynExpr.paren plain))

                            let store =
                                SynExpr.paren converted
                                |> SynExpr.applyFunction (
                                    SynExpr.createLongIdent' [ pf.TargetVariable ; Ident.create "Add" ]
                                )
                                |> tryStore (SynExpr.createIdent "value")

                            SynMatchClause.create (SynPat.createConst (SynConst.Int32 index)) store
                        )

                    SynExpr.createMatch
                        (SynExpr.createIdent "positionalId")
                        (branches
                         @ [
                             SynMatchClause.create SynPat.anon (internalError "unknown positional sink id")
                         ])

            body
            |> SynBinding.basic
                [ Ident.create "parser_storePositional" ]
                [
                    SynPat.named "positionalId" |> SynPat.annotateType SynType.int
                    SynPat.named "value" |> SynPat.annotateType SynType.string
                    SynPat.named "afterSeparator" |> SynPat.annotateType SynType.bool
                ]
            |> SynBinding.withReturnAnnotation (SynType.appPostfix "option" SynType.string)

        let renderStoredBinding : SynBinding =
            let branches =
                indexed
                |> List.choose (fun (index, pf) ->
                    // The duplicate-argument message names the value the user already supplied,
                    // so render it the way they spelled it rather than with `ToString`.
                    let rendered = renderLeafValue pf.BoolCases pf.EnumCases (SynExpr.createIdent "x")

                    match pf.Accumulation with
                    // Repeatable leaves never provoke a duplicate-argument message, so they have
                    // no stored value to render.
                    | Accumulation.List _
                    | Accumulation.Map _ -> None
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
                                    rendered
                                SynMatchClause.create
                                    (SynPat.nameWithArgs
                                        "Some"
                                        [ SynPat.paren (SynPat.nameWithArgs "Choice2Of2" [ SynPat.named "x" ]) ])
                                    rendered
                                SynMatchClause.create (SynPat.named "None") (SynExpr.CreateConst "<no value>")
                            ]
                        |> SynMatchClause.create (SynPat.createConst (SynConst.Int32 index))
                        |> Some
                    | Accumulation.Required
                    | Accumulation.Optional ->
                        SynExpr.createMatch
                            (SynExpr.createIdent' pf.TargetVariable)
                            [
                                SynMatchClause.create (SynPat.nameWithArgs "Some" [ SynPat.named "x" ]) rendered
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
                    | Accumulation.List _
                    // A map is empty rather than defaulted, so there is nothing to fill in.
                    | Accumulation.Map _ -> None
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
                        | ArgumentDefaultSpec.Literal value ->
                            // The literal already has the field's element type, so unlike the
                            // env-var case there is nothing to parse and nothing which can fail.
                            SynExpr.sequential [ storeDefault value ; SynExpr.createIdent "None" ]
                        | ArgumentDefaultSpec.NullLiteral ->
                            SynExpr.sequential [ storeDefault (SynExpr.Null range0) ; SynExpr.createIdent "None" ]
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
        let successExpr : SynExpr = ParseTree.instantiate spec

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
        let argumentFlagAttr (case : UnionCase<Ident option>) : SynExpr option =
            case.Attributes
            |> List.tryPick (fun attr ->
                match attr.TypeName with
                | SynLongIdent.SynLongIdent (id, _, _) ->
                    match id |> List.last |> _.idText with
                    | "ArgumentFlagAttribute"
                    | "ArgumentFlag" -> Some (SynExpr.stripOptionalParen attr.ArgExpr)
                    | _ -> None
            )

        let flagDus =
            allUnionTypes
            |> List.choose (fun ty ->
                match ty.Cases with
                | [ c1 ; c2 ] ->
                    let c1Attr = argumentFlagAttr c1
                    let c2Attr = argumentFlagAttr c2

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
                | cases ->
                    // Without this check the attribute would be silently ignored, and (every case
                    // being data-free) the union would quietly become an enumerated value instead.
                    if cases |> List.exists (fun case -> (argumentFlagAttr case).IsSome) then
                        failwith
                            "[<ArgumentFlag>] must be placed on both cases of a two-case discriminated union, with opposite argument values on each case."

                    None
            )

        let isDataFree (u : UnionType) =
            u.Cases |> List.forall (fun case -> List.isEmpty case.Fields)

        let isFlagDu (u : UnionType) =
            flagDus |> List.exists (fun f -> f.Name.idText = u.Name.idText)

        // A union with no data in any case has no arguments to tell its cases apart, so it cannot
        // be a set of alternative argument sets: it is an argument *value*, spelled by case name.
        let enumDus =
            allUnionTypes |> List.filter (fun u -> not (isFlagDu u) && isDataFree u)

        // Unions whose cases are alternative argument records; flag DUs and data-free unions are
        // argument leaves, and are excluded.
        let structuralUnions =
            allUnionTypes |> List.filter (fun u -> not (isFlagDu u) && not (isDataFree u))

        let ambient =
            {
                FlagDus = flagDus
                EnumDus = enumDus
                StructuralUnions = structuralUnions
                Records = allRecordTypes
            }

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

                let spec, _ = toParseSpec [] "" 0 ambient record

                record.Name, typeHelp attrs, spec
            | SynTypeDefn.SynTypeDefn (SynComponentInfo.SynComponentInfo (attributes = attrs) as sci,
                                       SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.Union (access, cases, _), _),
                                       smd,
                                       _,
                                       _,
                                       _) ->
                let union = UnionType.OfUnion sci smd access cases

                if isDataFree union then
                    // An enumerated value is a *leaf*: it is one argument's value, so it has
                    // nowhere to live at the root, which must consume a whole command line.
                    failwith
                        $"No case of [<ArgParser>] union %s{union.Name.idText} has any data, so it is an enumerated value rather than a set of alternative argument sets: an empty command line could not choose between its cases. Use it as the type of a field of an [<ArgParser>] record instead, where it is supplied as `--field-name=a`."

                let spec, _ = unionToParseSpec [] "" 0 ambient union

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
