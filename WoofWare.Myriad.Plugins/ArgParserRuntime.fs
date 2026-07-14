namespace WoofWare.Myriad.Plugins

open System

/// The type-erased runtime kernel of the generated argument parser.
///
/// This module knows nothing about the target types of a parse: it deals only in the *shape* of a
/// schema (which argument names exist, their arity, whether they may repeat, whether they are
/// required) and in raw string tokens. Scanning argv, routing tokens to arguments, selecting the
/// case of a discriminated-union schema, and detecting structural errors all happen here, as pure
/// functions from data to data. The generated (typed) layer is responsible for converting raw
/// values with real parsers, applying defaults, rendering messages, and assembling records; by the
/// time it runs, the shape of the parse has already been fully determined, so a conversion failure
/// can never change which argument a token was routed to nor which union case was selected.
///
/// This file is compiled into WoofWare.Myriad.Plugins for testing, and is also embedded verbatim
/// into generated parser output, so it must remain self-contained: no dependencies beyond
/// FSharp.Core and System, no reflection, no printf `%A` (generated code may be compiled with
/// `--reflectionfree`). It is embedded by parsing this source and re-printing the AST, and some
/// constructs do not survive that round trip: use no char literals and no `_.member` shorthand
/// lambdas.
module internal ArgParserRuntime =

    /// How many value tokens does one occurrence of a key consume?
    [<RequireQualifiedAccess>]
    type ErasedArity =
        /// Consumes exactly one value token: `--foo bar` or `--foo=bar`.
        | One
        /// Boolean-like (a `bool` field or a two-case flag DU): consumes a following token only if
        /// that token is a boolean literal; otherwise the occurrence alone means "true".
        | BoolLike

    /// What happens if an argument receives no occurrence at all?
    [<RequireQualifiedAccess>]
    type ErasedRequirement =
        /// The parse fails.
        | Required
        /// The argument is absent (an `option`-typed field, or a list which may be empty).
        | Optional
        /// The typed layer falls back to a default source (environment variable or user function).
        | HasDefault

    /// One named (non-positional) argument, erased to its shape.
    type ErasedLeaf =
        {
            /// Index into the typed layer's converter table.
            Id : int
            /// Long forms without the leading `--`, in declaration order; the head is the
            /// canonical form used in messages.
            Forms : string list
            /// Whether `--no-<form>` is also accepted, negating the flag. Only meaningful for
            /// boolean-like arguments.
            AcceptsNegation : bool
            Arity : ErasedArity
            /// May this argument occur more than once (list accumulation)?
            Repeatable : bool
            Requirement : ErasedRequirement
            /// Human-readable description of the target type, e.g. "int32", for help text.
            TypeDescription : string
            /// Help text for this argument, if any.
            Help : string option
        }

    /// How does the positional sink treat an *unrecognised* `--key`-shaped token?
    [<RequireQualifiedAccess>]
    type ErasedFlagLikeBehaviour =
        /// Collect it as a positional arg (`[<PositionalArgs true>]`).
        | Collect
        /// Reject the parse (`[<PositionalArgs false>]` / `[<PositionalArgs>]`).
        | Reject

    /// The positional-argument sink, if the schema has one.
    type ErasedPositional =
        {
            /// Index into the typed layer's converter table.
            Id : int
            /// The forms under which the sink itself can be addressed as `--form value` /
            /// `--form=value` (the field name argified, plus any explicit long forms); the head
            /// is used in help text.
            Forms : string list
            FlagLike : ErasedFlagLikeBehaviour
            TypeDescription : string
            Help : string option
        }

    /// The shape of a parser: a tree of products (records), sums (discriminated unions) and
    /// leaves (actual arguments). Leaves are referred to by id; the flat leaf table plus this
    /// tree fully describe the schema.
    [<RequireQualifiedAccess>]
    type ErasedTree =
        | Leaf of leafId : int
        | Product of children : ErasedTree list
        /// Cases are in declaration order; the string is the case name, for messages.
        | Sum of sumId : int * cases : (string * ErasedTree) list

    type ErasedSchema =
        {
            /// All leaves reachable in the tree, in field declaration order (the order in which
            /// "missing required argument" errors are reported).
            Leaves : ErasedLeaf list
            Tree : ErasedTree
            Positional : ErasedPositional option
        }

    /// One observed occurrence of a named argument.
    type ErasedOccurrence =
        {
            LeafId : int
            /// The raw value token, if one was consumed. None for an arity-0 boolean occurrence
            /// (which means "true", or "false" if negated).
            Value : string option
            /// Whether the occurrence arrived via a `--no-` form.
            Negated : bool
            /// The argv token which introduced this occurrence, e.g. "--foo" or "--foo=3", for
            /// error messages. For a `--key=value` occurrence this is the whole token; for a
            /// `--key value` occurrence it is just the key token.
            Source : string
        }

    /// A structural (pre-conversion) parse error.
    [<RequireQualifiedAccess>]
    type ScanError =
        /// A key which consumes a value was the last token (or the last before `--`).
        | TrailingKeyNoValue of source : string
        /// `--key=value` where the key is not recognised, and the schema does not collect
        /// flag-like positionals. Aborts the parse.
        | UnknownKeyEqualsValue of key : string * value : string
        /// `--key` where the key is not recognised, and the schema does not collect flag-like
        /// positionals. Aborts the parse.
        | UnknownKey of source : string

    /// How a positional value was spelled on the command line.
    [<RequireQualifiedAccess>]
    type PositionalForm =
        /// The value stood alone as its own token.
        | Bare
        /// `--rest=value`, where `rest` is the positional sink's own name; the key text is
        /// recorded as spelled (it matches case-insensitively).
        | KeyEquals of key : string
        /// `--rest value`; the key text is recorded as spelled.
        | KeySpaced of key : string

    /// The scan phase emits an ordered log of these; the typed layer folds over the log in order,
    /// so that e.g. conversion errors interleave with structural errors in argv order.
    [<RequireQualifiedAccess>]
    type ScanEvent =
        | Occurrence of ErasedOccurrence
        /// A value routed to the positional sink (or to the leftover-args accumulator, for a
        /// schema with no sink). `afterSeparator` is true for tokens which appeared after `--`.
        | Positional of value : string * afterSeparator : bool * form : PositionalForm
        | Error of ScanError
        /// A `--help`-shaped token was seen in key position; the token itself is recorded (the
        /// match is case-insensitive, so it may be e.g. "--HELP").
        | Help of source : string
        /// The literal `--` separator token.
        | Separator

    [<RequireQualifiedAccess>]
    module ScanError =
        /// True if this error means "stop the parse immediately" (historically these were
        /// exceptions thrown mid-scan rather than accumulated).
        let isFatal (e : ScanError) : bool =
            match e with
            | ScanError.TrailingKeyNoValue _ -> false
            | ScanError.UnknownKeyEqualsValue _ -> true
            | ScanError.UnknownKey _ -> true

    /// What the scanner is waiting for.
    /// The leaf is None when the pending key was unrecognised: such a key is held back because
    /// its fate (collected as a flag-like positional, or a hard error) is only known once we see
    /// whether any token follows it.
    [<RequireQualifiedAccess>]
    type private ScanState =
        | AwaitingKey
        | AwaitingValue of leaf : (ErasedLeaf * bool) option * source : string
        /// The positional sink's own key (`--rest`) was seen; the next token is its value,
        /// consumed greedily (keyed positionals always take exactly one value).
        | AwaitingPositionalValue of source : string

    /// Match a full `--key` token (value part already split off) against the leaf table.
    /// Returns the leaf and whether the match was via the negated `--no-` form.
    let matchLeaf (leaves : ErasedLeaf list) (key : string) : (ErasedLeaf * bool) option =
        leaves
        |> List.tryPick (fun leaf ->
            leaf.Forms
            |> List.tryPick (fun form ->
                if String.Equals (key, "--" + form, StringComparison.OrdinalIgnoreCase) then
                    Some (leaf, false)
                elif
                    leaf.AcceptsNegation
                    && String.Equals (key, "--no-" + form, StringComparison.OrdinalIgnoreCase)
                then
                    Some (leaf, true)
                else
                    None
            )
        )

    /// Is this token a boolean literal, in the sense the scanner uses to decide whether a
    /// boolean-like key consumes it as a value?
    let isBoolLiteral (token : string) : bool =
        let mutable ignored = false
        Boolean.TryParse (token, &ignored)

    /// Scan argv into an ordered event log. Pure: performs no conversion, throws no exceptions.
    ///
    /// The grammar (deliberately preserving the historical parser's behaviour):
    /// - `--` ends key processing; every subsequent token is positional. A key pending a value at
    ///   the separator is resolved exactly as at end-of-input: boolean-like keys become an
    ///   arity-0 occurrence, anything else is a trailing-key error.
    /// - `--help` (case-insensitive) in key position requests help.
    /// - A recognised key consumes the next token as its value, greedily: `--a --b=false` gives
    ///   `a` the value `--b=false` if `a` is not boolean-like. Boolean-like keys consume the next
    ///   token only if it is a boolean literal.
    /// - An unrecognised `--key` is held pending: if a token follows, the key is collected as a
    ///   flag-like positional (where the schema allows) and the following token is re-processed
    ///   in key position; at end-of-input it is a trailing-key error; where flag-like collection
    ///   is not allowed, it is a fatal unknown-key error.
    /// - The positional sink's own name is a key too: `--rest=value` and `--rest value` route
    ///   `value` to the sink (always consuming exactly one value, greedily).
    /// - Any other token in key position is positional.
    let scan (schema : ErasedSchema) (args : string list) : ScanEvent list =
        let collectFlagLike =
            match schema.Positional with
            | Some p ->
                match p.FlagLike with
                | ErasedFlagLikeBehaviour.Collect -> true
                | ErasedFlagLikeBehaviour.Reject -> false
            | None -> false

        /// Does this full `--key` token (value part already split off) name the positional sink?
        let isPositionalKey (key : string) : bool =
            match schema.Positional with
            | None -> false
            | Some p ->
                p.Forms
                |> List.exists (fun form -> String.Equals (key, "--" + form, StringComparison.OrdinalIgnoreCase))

        /// Resolve a pending key which will receive no value (end of input, or `--` next).
        let resolvePending (state : ScanState) : ScanEvent list =
            match state with
            | ScanState.AwaitingKey -> []
            | ScanState.AwaitingValue (Some (leaf, negated), source) ->
                match leaf.Arity with
                | ErasedArity.BoolLike ->
                    [
                        ScanEvent.Occurrence
                            {
                                LeafId = leaf.Id
                                Value = None
                                Negated = negated
                                Source = source
                            }
                    ]
                | ErasedArity.One -> [ ScanEvent.Error (ScanError.TrailingKeyNoValue source) ]
            | ScanState.AwaitingValue (None, source) -> [ ScanEvent.Error (ScanError.TrailingKeyNoValue source) ]
            | ScanState.AwaitingPositionalValue source -> [ ScanEvent.Error (ScanError.TrailingKeyNoValue source) ]

        let rec go (state : ScanState) (acc : ScanEvent list) (args : string list) : ScanEvent list =
            match args with
            | [] -> List.rev acc @ resolvePending state
            | "--" :: rest ->
                let positionals =
                    rest
                    |> List.map (fun token -> ScanEvent.Positional (token, true, PositionalForm.Bare))

                List.rev acc @ resolvePending state @ (ScanEvent.Separator :: positionals)
            | arg :: rest ->

            match state with
            | ScanState.AwaitingKey ->
                if not (arg.StartsWith ("--", StringComparison.Ordinal)) then
                    go ScanState.AwaitingKey (ScanEvent.Positional (arg, false, PositionalForm.Bare) :: acc) rest
                elif String.Equals (arg, "--help", StringComparison.OrdinalIgnoreCase) then
                    go ScanState.AwaitingKey (ScanEvent.Help arg :: acc) rest
                else
                    // (The string overload: char literals do not survive the parse-and-reprint
                    // round trip through which this file is embedded into generated code.)
                    let equals = arg.IndexOf ("=", StringComparison.Ordinal)

                    if equals >= 0 then
                        let key = arg.Substring (0, equals)
                        let value = arg.Substring (equals + 1)

                        match matchLeaf schema.Leaves key with
                        | Some (leaf, negated) ->
                            let occurrence =
                                {
                                    LeafId = leaf.Id
                                    Value = Some value
                                    Negated = negated
                                    Source = arg
                                }

                            go ScanState.AwaitingKey (ScanEvent.Occurrence occurrence :: acc) rest
                        | None ->
                            if isPositionalKey key then
                                go
                                    ScanState.AwaitingKey
                                    (ScanEvent.Positional (value, false, PositionalForm.KeyEquals key) :: acc)
                                    rest
                            elif collectFlagLike then
                                go
                                    ScanState.AwaitingKey
                                    (ScanEvent.Positional (arg, false, PositionalForm.Bare) :: acc)
                                    rest
                            else
                                go
                                    ScanState.AwaitingKey
                                    (ScanEvent.Error (ScanError.UnknownKeyEqualsValue (key, value)) :: acc)
                                    rest
                    else
                        // No value attached; what the key means depends on the next token.
                        match matchLeaf schema.Leaves arg with
                        | Some matched -> go (ScanState.AwaitingValue (Some matched, arg)) acc rest
                        | None ->
                            if isPositionalKey arg then
                                go (ScanState.AwaitingPositionalValue arg) acc rest
                            else
                                go (ScanState.AwaitingValue (None, arg)) acc rest
            | ScanState.AwaitingValue (Some (leaf, negated), source) ->
                let consume (value : string) : ScanEvent =
                    ScanEvent.Occurrence
                        {
                            LeafId = leaf.Id
                            Value = Some value
                            Negated = negated
                            Source = source
                        }

                match leaf.Arity with
                | ErasedArity.One -> go ScanState.AwaitingKey (consume arg :: acc) rest
                | ErasedArity.BoolLike ->
                    if isBoolLiteral arg then
                        go ScanState.AwaitingKey (consume arg :: acc) rest
                    else
                        // The occurrence stands alone; re-process this token in key position.
                        let occurrence =
                            {
                                LeafId = leaf.Id
                                Value = None
                                Negated = negated
                                Source = source
                            }

                        go ScanState.AwaitingKey (ScanEvent.Occurrence occurrence :: acc) (arg :: rest)
            | ScanState.AwaitingValue (None, source) ->
                // A pending unrecognised key, now known not to be trailing.
                if collectFlagLike then
                    go
                        ScanState.AwaitingKey
                        (ScanEvent.Positional (source, false, PositionalForm.Bare) :: acc)
                        (arg :: rest)
                else
                    go ScanState.AwaitingKey (ScanEvent.Error (ScanError.UnknownKey source) :: acc) (arg :: rest)
            | ScanState.AwaitingPositionalValue source ->
                // The sink's own key consumes the next token greedily, like any arity-one key.
                go
                    ScanState.AwaitingKey
                    (ScanEvent.Positional (arg, false, PositionalForm.KeySpaced source) :: acc)
                    rest

        go ScanState.AwaitingKey [] args

    /// A failure to choose a unique case for a Sum node.
    [<RequireQualifiedAccess>]
    type SelectionError =
        /// Occurrences were seen for leaves belonging to two (or more) different cases of one
        /// union. The witnesses are (case name, example source token routed to that case).
        | ConflictingCases of sumId : int * witnesses : (string * string) list
        /// No case of this union was touched, and no case can be satisfied with no arguments.
        | NoCaseSelected of sumId : int * caseNames : string list
        /// No case of this union was touched, and more than one case can be satisfied with no
        /// arguments, so the choice is ambiguous. (A generation-time check normally rejects
        /// schemas where this can happen; this is the runtime safety net.)
        | AmbiguousEmptyCases of sumId : int * caseNames : string list

    /// The result of case selection: which case index was chosen for each Sum node reachable in
    /// the selected interpretation, and which leaves are *active* (belong to selected cases).
    /// Leaves of unselected cases are inactive: they contribute no missing-required errors and
    /// their defaults must never run.
    type Selection =
        {
            Choices : Map<int, int>
            /// Ids of leaves which are part of the selected interpretation.
            ActiveLeaves : Set<int>
            Errors : SelectionError list
        }

    /// Can this subtree be satisfied with zero occurrences? A leaf can iff it is not required; a
    /// product can iff all its children can; a sum can iff some case can.
    let rec private emptyOk (leaves : Map<int, ErasedLeaf>) (tree : ErasedTree) : bool =
        match tree with
        | ErasedTree.Leaf leafId ->
            match (Map.find leafId leaves).Requirement with
            | ErasedRequirement.Required -> false
            | ErasedRequirement.Optional
            | ErasedRequirement.HasDefault -> true
        | ErasedTree.Product children -> children |> List.forall (emptyOk leaves)
        | ErasedTree.Sum (_, cases) -> cases |> List.exists (fun (_, case) -> emptyOk leaves case)

    /// All leaf ids under a subtree, in declaration order.
    let rec leafIds (tree : ErasedTree) : int list =
        match tree with
        | ErasedTree.Leaf leafId -> [ leafId ]
        | ErasedTree.Product children -> children |> List.collect leafIds
        | ErasedTree.Sum (_, cases) -> cases |> List.collect (fun (_, case) -> leafIds case)

    /// Select a case for every Sum node reachable in the selected interpretation, given a witness
    /// (an example source token) for every leaf which received an occurrence.
    ///
    /// The rule, valid because argument names are globally unique across the schema: a case is
    /// "touched" if any leaf beneath it received an occurrence. Exactly one touched case means
    /// that case is selected; two or more touched cases is a conflict; zero touched cases falls
    /// back to the unique case satisfiable with no arguments.
    let select (schema : ErasedSchema) (observed : Map<int, string>) : Selection =
        let leavesById = schema.Leaves |> List.map (fun leaf -> leaf.Id, leaf) |> Map.ofList

        let rec go (tree : ErasedTree) : Map<int, int> * Set<int> * SelectionError list =
            match tree with
            | ErasedTree.Leaf leafId -> Map.empty, Set.singleton leafId, []
            | ErasedTree.Product children ->
                ((Map.empty, Set.empty, []), children)
                ||> List.fold (fun (choices, active, errors) child ->
                    let childChoices, childActive, childErrors = go child

                    let choices = (choices, childChoices) ||> Map.fold (fun m k v -> Map.add k v m)

                    choices, Set.union active childActive, errors @ childErrors
                )
            | ErasedTree.Sum (sumId, cases) ->
                let touched =
                    cases
                    |> List.indexed
                    |> List.choose (fun (i, (name, case)) ->
                        let witness =
                            leafIds case |> List.tryPick (fun leafId -> Map.tryFind leafId observed)

                        match witness with
                        | Some source -> Some (i, name, source)
                        | None -> None
                    )

                match touched with
                | [ (index, _, _) ] ->
                    let _, case = List.item index cases
                    let childChoices, childActive, childErrors = go case
                    Map.add sumId index childChoices, childActive, childErrors
                | [] ->
                    let satisfiable =
                        cases
                        |> List.indexed
                        |> List.filter (fun (_, (_, case)) -> emptyOk leavesById case)

                    match satisfiable with
                    | [ (index, (_, case)) ] ->
                        let childChoices, childActive, childErrors = go case
                        Map.add sumId index childChoices, childActive, childErrors
                    | [] ->
                        let names = cases |> List.map fst
                        Map.empty, Set.empty, [ SelectionError.NoCaseSelected (sumId, names) ]
                    | multiple ->
                        let names = multiple |> List.map (fun (_, (name, _)) -> name)
                        Map.empty, Set.empty, [ SelectionError.AmbiguousEmptyCases (sumId, names) ]
                | multiple ->
                    let witnesses = multiple |> List.map (fun (_, name, source) -> name, source)
                    Map.empty, Set.empty, [ SelectionError.ConflictingCases (sumId, witnesses) ]

        let choices, active, errors = go schema.Tree

        {
            Choices = choices
            ActiveLeaves = active
            Errors = errors
        }

    /// A structural error found after scanning and selection.
    [<RequireQualifiedAccess>]
    type ValidationError =
        /// A required leaf (active under the selection) received no occurrence.
        | MissingRequired of leafId : int
        /// A non-repeatable leaf received more than one occurrence (through any of its forms,
        /// including negated ones). Carries every occurrence, in argv order.
        | DuplicateOccurrences of leafId : int * occurrences : ErasedOccurrence list
        /// An occurrence was routed to a leaf which is not part of the selected interpretation.
        /// (Selection has necessarily already reported a conflict in this situation; this exists
        /// so that every occurrence is accounted for.)
        | InactiveLeaf of occurrence : ErasedOccurrence
        /// Positional tokens were seen but the schema has no positional sink.
        | NoPositionalSink of tokens : string list

    /// Structural validation of the scanned events against the selected interpretation.
    /// Returns the errors (missing-required in leaf declaration order), together with the ids of
    /// active defaulted leaves which received no occurrence and so must fall back to their
    /// default source.
    let validate
        (schema : ErasedSchema)
        (selection : Selection)
        (events : ScanEvent list)
        : ValidationError list * int list
        =
        let observedIds =
            events
            |> List.choose (fun event ->
                match event with
                | ScanEvent.Occurrence occ -> Some occ.LeafId
                | _ -> None
            )
            |> Set.ofList

        let missing =
            schema.Leaves
            |> List.filter (fun leaf ->
                Set.contains leaf.Id selection.ActiveLeaves
                && not (Set.contains leaf.Id observedIds)
                && (
                    match leaf.Requirement with
                    | ErasedRequirement.Required -> true
                    | ErasedRequirement.Optional
                    | ErasedRequirement.HasDefault -> false
                )
            )
            |> List.map (fun leaf -> ValidationError.MissingRequired leaf.Id)

        let duplicates =
            let occurrencesByLeaf =
                events
                |> List.choose (fun event ->
                    match event with
                    | ScanEvent.Occurrence occ -> Some occ
                    | _ -> None
                )
                |> List.groupBy (fun occurrence -> occurrence.LeafId)

            schema.Leaves
            |> List.choose (fun leaf ->
                if leaf.Repeatable then
                    None
                else
                    match occurrencesByLeaf |> List.tryFind (fun (leafId, _) -> leafId = leaf.Id) with
                    | Some (_, occurrences) when List.length occurrences > 1 ->
                        Some (ValidationError.DuplicateOccurrences (leaf.Id, occurrences))
                    | _ -> None
            )

        let inactive =
            events
            |> List.choose (fun event ->
                match event with
                | ScanEvent.Occurrence occ when not (Set.contains occ.LeafId selection.ActiveLeaves) ->
                    Some (ValidationError.InactiveLeaf occ)
                | _ -> None
            )

        let noSink =
            match schema.Positional with
            | Some _ -> []
            | None ->
                let tokens =
                    events
                    |> List.choose (fun event ->
                        match event with
                        | ScanEvent.Positional (value, _, _) -> Some value
                        | _ -> None
                    )

                match tokens with
                | [] -> []
                | tokens -> [ ValidationError.NoPositionalSink tokens ]

        let needsDefault =
            schema.Leaves
            |> List.filter (fun leaf ->
                Set.contains leaf.Id selection.ActiveLeaves
                && not (Set.contains leaf.Id observedIds)
                && (
                    match leaf.Requirement with
                    | ErasedRequirement.HasDefault -> true
                    | ErasedRequirement.Required
                    | ErasedRequirement.Optional -> false
                )
            )
            |> List.map (fun leaf -> leaf.Id)

        duplicates @ missing @ inactive @ noSink, needsDefault
