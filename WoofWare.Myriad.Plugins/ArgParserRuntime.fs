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

    /// A positional-argument sink: a consumer of the positional-token stream.
    type ErasedPositional =
        {
            /// Index into the typed layer's positional converter table. Ids live in their own
            /// space, independent of ErasedLeaf ids.
            Id : int
            /// The forms under which the sink itself can be addressed as `--form value` /
            /// `--form=value` (the field name argified, plus any explicit long forms); the head
            /// is used in help text. Two sinks in mutually exclusive Sum cases may share a
            /// form: all keyed positional forms have the same arity, and the capacity rule
            /// means the sinks can never both be active.
            Forms : string list
            /// The flag-like policy is lexical (it controls tokenization, which happens before
            /// any case is selected), so every sink in a schema must agree on it;
            /// WellFormedSchema enforces the agreement.
            FlagLike : ErasedFlagLikeBehaviour
            TypeDescription : string
            Help : string option
        }

    /// The shape of a parser: a tree of products (records), sums (discriminated unions) and
    /// leaves (actual arguments, named or positional). Leaves are referred to by id; the flat
    /// leaf tables plus this tree fully describe the schema.
    [<RequireQualifiedAccess>]
    type ErasedTree =
        | Leaf of leafId : int
        /// A positional-stream consumer. At most one may appear in any complete
        /// interpretation of the tree (one case chosen for every reachable Sum): argv holds a
        /// single positional stream, and two unbounded consumers would admit no canonical
        /// partition of it. WellFormedSchema enforces this capacity rule.
        | PositionalLeaf of positionalId : int
        | Product of children : ErasedTree list
        /// Cases are in declaration order; the string is the case name, for messages.
        | Sum of sumId : int * cases : (string * ErasedTree) list

    type ErasedSchema =
        {
            /// All named leaves reachable in the tree, in field declaration order (the order
            /// in which "missing required argument" errors are reported).
            Leaves : ErasedLeaf list
            Tree : ErasedTree
            /// All positional sinks reachable in the tree, in declaration order.
            Positionals : ErasedPositional list
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

    /// One value belonging to the positional stream. Scanning does not decide which sink
    /// receives it: the token's spelling determines only the *candidate* sinks, and the actual
    /// consumer is resolved after case selection. (For today's schemas, with at most one sink,
    /// the candidate set is that sink alone.)
    type PositionalEvent =
        {
            Value : string
            /// True for tokens which appeared after the `--` separator.
            AfterSeparator : bool
            /// How the value was spelled; preserved exactly, for diagnostics and lossless
            /// reconstruction of argv.
            Form : PositionalForm
            /// The sinks which could consume this token: every sink for a bare token
            /// (including everything after `--`, and flag-like tokens collected in Collect
            /// mode), the claimants of the key for a keyed one.
            Candidates : Set<int>
        }

    /// The scan phase emits an ordered log of these; the typed layer folds over the log in order,
    /// so that e.g. conversion errors interleave with structural errors in argv order.
    [<RequireQualifiedAccess>]
    type ScanEvent =
        | Occurrence of ErasedOccurrence
        /// A value belonging to the positional stream (or to the leftover-args accumulator,
        /// for a schema with no sink).
        | Positional of PositionalEvent
        | Error of ScanError
        /// A `--help`-shaped token was seen in key position; the token itself is recorded (the
        /// match is case-insensitive, so it may be e.g. "--HELP").
        | Help of source : string
        /// The literal `--` separator token.
        | Separator

    [<RequireQualifiedAccess>]
    module ScanError =
        /// True if this error means "stop the parse immediately".
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
        /// A positional sink's own key (`--rest`) was seen; the next token is its value,
        /// consumed greedily (keyed positionals always take exactly one value). `candidates`
        /// are the sinks claiming the key.
        | AwaitingPositionalValue of candidates : Set<int> * source : string

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
    /// The grammar:
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
            // The policy is lexical, so all sinks must agree on it (WellFormedSchema enforces
            // this); Collect is honoured only when every sink asks for it.
            match schema.Positionals with
            | [] -> false
            | positionals ->
                positionals
                |> List.forall (fun p ->
                    match p.FlagLike with
                    | ErasedFlagLikeBehaviour.Collect -> true
                    | ErasedFlagLikeBehaviour.Reject -> false
                )

        /// Every sink: the candidate set of a bare positional token.
        let allSinks : Set<int> =
            schema.Positionals |> List.map (fun p -> p.Id) |> Set.ofList

        /// The sinks claiming this full `--key` token (value part already split off); empty
        /// when the token does not name a positional sink at all.
        let positionalClaimants (key : string) : Set<int> =
            schema.Positionals
            |> List.filter (fun p ->
                p.Forms
                |> List.exists (fun form -> String.Equals (key, "--" + form, StringComparison.OrdinalIgnoreCase))
            )
            |> List.map (fun p -> p.Id)
            |> Set.ofList

        let bareToken (value : string) (afterSeparator : bool) : ScanEvent =
            ScanEvent.Positional
                {
                    Value = value
                    AfterSeparator = afterSeparator
                    Form = PositionalForm.Bare
                    Candidates = allSinks
                }

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
            | ScanState.AwaitingPositionalValue (_, source) -> [ ScanEvent.Error (ScanError.TrailingKeyNoValue source) ]

        let rec go (state : ScanState) (acc : ScanEvent list) (args : string list) : ScanEvent list =
            match args with
            | [] -> List.rev acc @ resolvePending state
            | "--" :: rest ->
                let positionals = rest |> List.map (fun token -> bareToken token true)

                List.rev acc @ resolvePending state @ (ScanEvent.Separator :: positionals)
            | arg :: rest ->

            match state with
            | ScanState.AwaitingKey ->
                if not (arg.StartsWith ("--", StringComparison.Ordinal)) then
                    go ScanState.AwaitingKey (bareToken arg false :: acc) rest
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
                            let claimants = positionalClaimants key

                            if not (Set.isEmpty claimants) then
                                let event =
                                    ScanEvent.Positional
                                        {
                                            Value = value
                                            AfterSeparator = false
                                            Form = PositionalForm.KeyEquals key
                                            Candidates = claimants
                                        }

                                go ScanState.AwaitingKey (event :: acc) rest
                            elif collectFlagLike then
                                go ScanState.AwaitingKey (bareToken arg false :: acc) rest
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
                            let claimants = positionalClaimants arg

                            if not (Set.isEmpty claimants) then
                                go (ScanState.AwaitingPositionalValue (claimants, arg)) acc rest
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
                    go ScanState.AwaitingKey (bareToken source false :: acc) (arg :: rest)
                else
                    go ScanState.AwaitingKey (ScanEvent.Error (ScanError.UnknownKey source) :: acc) (arg :: rest)
            | ScanState.AwaitingPositionalValue (candidates, source) ->
                // The sink's own key consumes the next token greedily, like any arity-one key.
                let event =
                    ScanEvent.Positional
                        {
                            Value = arg
                            AfterSeparator = false
                            Form = PositionalForm.KeySpaced source
                            Candidates = candidates
                        }

                go ScanState.AwaitingKey (event :: acc) rest

        go ScanState.AwaitingKey [] args

    /// A failure to choose a unique case for a Sum node, or to route the positional stream.
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
        /// Positional tokens were seen, but no positional sink consistent with the named
        /// arguments can consume them all. `source` is the first positional token, as spelled.
        | UnroutablePositional of source : string
        /// The positional stream could be consumed by more than one sink, and nothing else
        /// distinguishes their alternatives. `source` is the first positional token, as
        /// spelled; `sinkIds` are the competing sinks. (A generation-time check normally
        /// rejects schemas where this can happen with bare tokens; this is the runtime safety
        /// net, and it can also fire for a keyed form shared between otherwise-undistinguished
        /// alternatives.)
        | AmbiguousPositionalRouting of source : string * sinkIds : int list

    /// The result of case selection: which case index was chosen for each Sum node reachable in
    /// the selected interpretation, and which leaves are *active* (belong to selected cases).
    /// Leaves of unselected cases are inactive: they contribute no missing-required errors and
    /// their defaults must never run.
    type Selection =
        {
            Choices : Map<int, int>
            /// Ids of named leaves which are part of the selected interpretation.
            ActiveLeaves : Set<int>
            /// The positional sink which is part of the selected interpretation, if any. The
            /// capacity rule (enforced by WellFormedSchema) guarantees there is at most one.
            ActivePositional : int option
            Errors : SelectionError list
        }

    /// Can this subtree be satisfied with zero occurrences? A named leaf can iff it is not
    /// required; a positional leaf always can (an empty stream is fine); a product can iff all
    /// its children can; a sum can iff some case can.
    let rec private emptyOk (leaves : Map<int, ErasedLeaf>) (tree : ErasedTree) : bool =
        match tree with
        | ErasedTree.Leaf leafId ->
            match (Map.find leafId leaves).Requirement with
            | ErasedRequirement.Required -> false
            | ErasedRequirement.Optional
            | ErasedRequirement.HasDefault -> true
        | ErasedTree.PositionalLeaf _ -> true
        | ErasedTree.Product children -> children |> List.forall (emptyOk leaves)
        | ErasedTree.Sum (_, cases) -> cases |> List.exists (fun (_, case) -> emptyOk leaves case)

    /// All named-leaf ids under a subtree, in declaration order.
    let rec leafIds (tree : ErasedTree) : int list =
        match tree with
        | ErasedTree.Leaf leafId -> [ leafId ]
        | ErasedTree.PositionalLeaf _ -> []
        | ErasedTree.Product children -> children |> List.collect leafIds
        | ErasedTree.Sum (_, cases) -> cases |> List.collect (fun (_, case) -> leafIds case)

    /// All positional-sink ids under a subtree, in declaration order.
    let rec positionalIds (tree : ErasedTree) : int list =
        match tree with
        | ErasedTree.Leaf _ -> []
        | ErasedTree.PositionalLeaf positionalId -> [ positionalId ]
        | ErasedTree.Product children -> children |> List.collect positionalIds
        | ErasedTree.Sum (_, cases) -> cases |> List.collect (fun (_, case) -> positionalIds case)

    /// The maximum number of positional sinks any complete interpretation of the tree can
    /// contain: a product's interpretations combine its children's, a sum's take the maximum
    /// over its cases.
    let rec maxPositionals (tree : ErasedTree) : int =
        match tree with
        | ErasedTree.Leaf _ -> 0
        | ErasedTree.PositionalLeaf _ -> 1
        | ErasedTree.Product children -> children |> List.sumBy maxPositionals
        | ErasedTree.Sum (_, cases) ->
            match cases with
            | [] -> 0
            | cases -> cases |> List.map (fun (_, case) -> maxPositionals case) |> List.max

    /// Select a case for every Sum node reachable in the selected interpretation, given a witness
    /// (an example source token) for every leaf which received an occurrence.
    ///
    /// The rule, valid because argument names are globally unique across the schema: a case is
    /// "touched" if any leaf beneath it received an occurrence. Exactly one touched case means
    /// that case is selected; two or more touched cases is a conflict; zero touched cases falls
    /// back to the unique case satisfiable with no arguments.
    /// Like `select` below, but a positional-stream hypothesis may be supplied: the sink with
    /// the given id is treated as one additional observed (optional) leaf, its witness being
    /// the given source text. This is how the resolver asks "which case would be selected if
    /// sink p consumed the positional stream?".
    let private selectWith
        (schema : ErasedSchema)
        (observed : Map<int, string>)
        (positionalWitness : (int * string) option)
        : Selection
        =
        let leavesById = schema.Leaves |> List.map (fun leaf -> leaf.Id, leaf) |> Map.ofList

        let rec go (tree : ErasedTree) : Map<int, int> * Set<int> * Set<int> * SelectionError list =
            match tree with
            | ErasedTree.Leaf leafId -> Map.empty, Set.singleton leafId, Set.empty, []
            | ErasedTree.PositionalLeaf positionalId -> Map.empty, Set.empty, Set.singleton positionalId, []
            | ErasedTree.Product children ->
                ((Map.empty, Set.empty, Set.empty, []), children)
                ||> List.fold (fun (choices, active, activePos, errors) child ->
                    let childChoices, childActive, childActivePos, childErrors = go child

                    let choices = (choices, childChoices) ||> Map.fold (fun m k v -> Map.add k v m)

                    choices, Set.union active childActive, Set.union activePos childActivePos, errors @ childErrors
                )
            | ErasedTree.Sum (sumId, cases) ->
                let touched =
                    cases
                    |> List.indexed
                    |> List.choose (fun (i, (name, case)) ->
                        let witness =
                            leafIds case |> List.tryPick (fun leafId -> Map.tryFind leafId observed)

                        let witness =
                            match witness with
                            | Some source -> Some source
                            | None ->
                                match positionalWitness with
                                | Some (positionalId, source) when positionalIds case |> List.contains positionalId ->
                                    Some source
                                | _ -> None

                        match witness with
                        | Some source -> Some (i, name, source)
                        | None -> None
                    )

                match touched with
                | [ (index, _, _) ] ->
                    let _, case = List.item index cases
                    let childChoices, childActive, childActivePos, childErrors = go case
                    Map.add sumId index childChoices, childActive, childActivePos, childErrors
                | [] ->
                    let satisfiable =
                        cases
                        |> List.indexed
                        |> List.filter (fun (_, (_, case)) -> emptyOk leavesById case)

                    match satisfiable with
                    | [ (index, (_, case)) ] ->
                        let childChoices, childActive, childActivePos, childErrors = go case
                        Map.add sumId index childChoices, childActive, childActivePos, childErrors
                    | [] ->
                        let names = cases |> List.map fst
                        Map.empty, Set.empty, Set.empty, [ SelectionError.NoCaseSelected (sumId, names) ]
                    | multiple ->
                        let names = multiple |> List.map (fun (_, (name, _)) -> name)
                        Map.empty, Set.empty, Set.empty, [ SelectionError.AmbiguousEmptyCases (sumId, names) ]
                | multiple ->
                    let witnesses = multiple |> List.map (fun (_, name, source) -> name, source)
                    Map.empty, Set.empty, Set.empty, [ SelectionError.ConflictingCases (sumId, witnesses) ]

        let choices, active, activePos, errors = go schema.Tree

        {
            Choices = choices
            ActiveLeaves = active
            ActivePositional =
                // The capacity rule makes two active sinks impossible on a well-formed schema.
                match Set.toList activePos with
                | [] -> None
                | [ p ] -> Some p
                | _ ->
                    failwith
                        "WoofWare.Myriad internal error: two positional sinks were active at once; the schema was not validated"
            Errors = errors
        }

    let select (schema : ErasedSchema) (observed : Map<int, string>) : Selection = selectWith schema observed None

    /// A positional event's original spelling, for error messages.
    let describePositionalEvent (event : PositionalEvent) : string =
        match event.Form with
        | PositionalForm.Bare -> event.Value
        | PositionalForm.KeyEquals key -> key + "=" + event.Value
        | PositionalForm.KeySpaced key -> key + " " + event.Value

    /// Structural resolution: select a case for every Sum *and* route the positional stream,
    /// given the named observations and the scanned positional events. Pure, and independent
    /// of conversion by construction: it sees only which named leaves were observed and how
    /// the positional tokens were spelled.
    ///
    /// The rule (mirroring the exhaustive reference semantics): a complete interpretation
    /// accepts the input iff it contains every observed named leaf, all its required leaves
    /// were observed, and — when positional tokens exist — it contains a sink which is a
    /// candidate of every positional event. The parse succeeds exactly when one
    /// interpretation accepts. Efficiently: intersect the events' candidate sets, and run the
    /// compositional selector once per candidate sink, with that sink treated as one
    /// additional observed leaf.
    ///
    /// Error reporting prefers the friendliest diagnosis: an interpretation which is unique
    /// up to missing required arguments is still selected (the missing arguments are reported
    /// downstream, as for a purely named parse), so positional routing only surfaces in
    /// errors when it is genuinely the obstacle.
    let resolve
        (schema : ErasedSchema)
        (observed : Map<int, string>)
        (positionalEvents : PositionalEvent list)
        : Selection
        =
        match positionalEvents, schema.Positionals with
        | [], _
        | _, [] ->
            // No positional tokens (the sink of the selected interpretation, if any, simply
            // receives an empty stream), or no sinks at all (validation reports the leftover
            // tokens wholesale, as ever).
            select schema observed
        | events, _ ->

        let candidates =
            events |> List.map (fun event -> event.Candidates) |> Set.intersectMany

        /// Are all the required named leaves active under this selection observed?
        let requiredComplete (selection : Selection) : bool =
            schema.Leaves
            |> List.forall (fun leaf ->
                match leaf.Requirement with
                | ErasedRequirement.Required ->
                    not (Set.contains leaf.Id selection.ActiveLeaves)
                    || Map.containsKey leaf.Id observed
                | ErasedRequirement.Optional
                | ErasedRequirement.HasDefault -> true
            )

        let firstSource = describePositionalEvent (List.head events)

        // One hypothesis per sink which could consume every positional token, in sink
        // declaration order. The witness text shown for a positional-driven case selection is
        // the first event this sink could consume.
        let hypotheses =
            schema.Positionals
            |> List.filter (fun p -> Set.contains p.Id candidates)
            |> List.map (fun p ->
                let witness =
                    events
                    |> List.tryFind (fun event -> Set.contains p.Id event.Candidates)
                    |> Option.map describePositionalEvent
                    |> Option.defaultValue firstSource

                p.Id, selectWith schema observed (Some (p.Id, witness))
            )

        // A hypothesis is clean when the selector found no error and the hypothesised sink
        // really is the selected interpretation's sink; it fully accepts when moreover no
        // required named argument is missing.
        let clean =
            hypotheses
            |> List.filter (fun (p, selection) ->
                List.isEmpty selection.Errors
                && (
                    match selection.ActivePositional with
                    | Some active -> active = p
                    | None -> false
                )
            )

        let fullyValid =
            clean |> List.filter (fun (_, selection) -> requiredComplete selection)

        // A hypothesis whose only defect is an untouched sum with several empty-satisfiable
        // cases stands for *several* complete interpretations, every one of which accepts:
        // the input is ambiguous through this sink alone. (Distinct from a conflict, which
        // means no interpretation through this sink accepts at all.)
        let innerAmbiguous =
            hypotheses
            |> List.filter (fun (p, selection) ->
                not (List.isEmpty selection.Errors)
                && (selection.Errors
                    |> List.forall (fun error ->
                        match error with
                        | SelectionError.AmbiguousEmptyCases _ -> true
                        | _ -> false
                    ))
                && (
                    match selection.ActivePositional with
                    | Some active -> active = p
                    | None -> false
                )
                && requiredComplete selection
            )

        let ambiguous (competing : (int * Selection) list) : Selection =
            {
                Choices = Map.empty
                ActiveLeaves = Set.empty
                ActivePositional = None
                Errors =
                    [
                        SelectionError.AmbiguousPositionalRouting (firstSource, competing |> List.map (fun (p, _) -> p))
                    ]
            }

        match fullyValid, innerAmbiguous with
        | [ (_, selection) ], [] -> selection
        // A single sink can consume the stream, but several interpretations through it
        // accept: its own ambiguity errors are the honest story.
        | [], [ (_, selection) ] -> selection
        | [], [] ->
            match clean with
            // Unique up to missing required arguments: select it, and let validation report
            // the missing arguments in the ordinary way.
            | [ (_, selection) ] -> selection
            | clean ->
                // Zero viable hypotheses (no sink can consume the stream consistently with
                // the named arguments), or several, all missing required arguments. Diagnose
                // from the named-only selection: its errors (no case selected, ambiguous
                // empty cases, conflicts) are the friendliest story; the routing
                // contradiction is added when the events share no candidate at all.
                let selection = select schema observed

                let unroutable =
                    { selection with
                        Errors = selection.Errors @ [ SelectionError.UnroutablePositional firstSource ]
                    }

                if Set.isEmpty candidates then
                    unroutable
                elif not (List.isEmpty selection.Errors) then
                    selection
                else
                    match selection.ActivePositional with
                    | Some active when events |> List.forall (fun event -> Set.contains active event.Candidates) ->
                        // Defensive: a routable named-only selection should have appeared as
                        // a clean hypothesis.
                        selection
                    | _ ->
                        match clean with
                        | _ :: _ :: _ ->
                            // Defensive: a clean named-only selection should have made at
                            // most one hypothesis clean.
                            ambiguous clean
                        | _ -> unroutable
        | fullyValid, innerAmbiguous ->
            // Several sinks could each consume the whole stream: genuinely ambiguous
            // routing.
            ambiguous (fullyValid @ innerAmbiguous)

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
            match schema.Positionals with
            | _ :: _ -> []
            | [] ->
                let tokens =
                    events
                    |> List.choose (fun event ->
                        match event with
                        | ScanEvent.Positional positional -> Some positional.Value
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

    /// A structural defect in an ErasedSchema: a violation of an invariant which the scanning
    /// and selection semantics rely on.
    ///
    /// The generator establishes these invariants at generation time where it can see the
    /// argument forms; it cannot when a form is supplied via e.g. a [<Literal>] constant, which
    /// the untyped AST does not resolve. Generated code therefore re-checks them at runtime via
    /// WellFormedSchema.check: a malformed schema must fail loudly up front, not silently route
    /// colliding tokens to whichever leaf was declared first.
    [<RequireQualifiedAccess>]
    type SchemaError =
        /// Two leaves in the table share an id.
        | DuplicateLeafId of leafId : int
        /// The tree refers to this leaf more than once.
        | LeafRepeatedInTree of leafId : int
        /// The tree refers to a leaf id which is not in the table.
        | LeafNotInTable of leafId : int
        /// A leaf in the table is not reachable in the tree.
        | LeafNotInTree of leafId : int
        /// Two Sum nodes in the tree share an id.
        | DuplicateSumId of sumId : int
        /// A leaf has no forms at all, so no token could ever address it.
        | NoForms of leafId : int
        /// A leaf accepts `--no-` negation but is not boolean-like.
        | NegationOnNonBool of leafId : int
        /// A form is the empty string: its token would be `--`, which the scanner always
        /// treats as the positional separator, so the argument could never be addressed.
        | EmptyForm of claimant : string
        /// A form contains an equals sign: the scanner splits a `--key=value` token at its
        /// *first* `=`, so such a form can never match. An argument required under such a form
        /// makes the schema (or a union case of it) permanently unsatisfiable.
        | FormContainsEquals of claimant : string * form : string
        /// Two distinct claimants respond to the same `--token` under the scanner's
        /// case-insensitive matching (so e.g. forms `foo` and `FOO` collide, as do a literal
        /// form `no-foo` and the negated variant of a negatable `foo`). The token is reported
        /// in the first claimant's spelling; the claimant descriptions are human-readable, in
        /// declaration order. Positional sinks *may* share forms with each other (a keyed
        /// positional form always has the same meaning whichever sink wins), so a collision
        /// is only reported when at least one claimant is not a sink.
        | TokenCollision of token : string * claimants : string list
        /// Two positional sinks in the table share an id.
        | DuplicatePositionalId of positionalId : int
        /// The tree refers to this positional sink more than once.
        | PositionalRepeatedInTree of positionalId : int
        /// The tree refers to a positional-sink id which is not in the table.
        | PositionalNotInTable of positionalId : int
        /// A positional sink in the table is not reachable in the tree.
        | PositionalNotInTree of positionalId : int
        /// Some complete interpretation of the tree contains more than one positional sink.
        /// argv holds a single positional stream, and two unbounded consumers would admit no
        /// canonical partition of it: `P × Q` is malformed, while `(A × P) + (B × Q)` is fine.
        | PositionalCapacityExceeded of maximum : int
        /// Positional sinks disagree on the treatment of unrecognised flag-like tokens. The
        /// policy is lexical — it controls tokenization, which happens before any case is
        /// selected — so it must be schema-global.
        | FlagLikePolicyDisagreement

    [<RequireQualifiedAccess>]
    module SchemaError =
        /// Render a schema error for humans. These describe a bug in the parser's *definition*,
        /// not in the arguments a user supplied.
        let describe (error : SchemaError) : string =
            match error with
            | SchemaError.DuplicateLeafId leafId -> sprintf "two arguments share the id %i" leafId
            | SchemaError.LeafRepeatedInTree leafId ->
                sprintf "the schema tree refers to argument id %i more than once" leafId
            | SchemaError.LeafNotInTable leafId ->
                sprintf "the schema tree refers to argument id %i, which does not exist" leafId
            | SchemaError.LeafNotInTree leafId -> sprintf "argument id %i is not reachable in the schema tree" leafId
            | SchemaError.DuplicateSumId sumId -> sprintf "two alternative-groups share the id %i" sumId
            | SchemaError.NoForms leafId -> sprintf "argument id %i has no names" leafId
            | SchemaError.NegationOnNonBool leafId ->
                sprintf "argument id %i accepts --no- negation but is not boolean-like" leafId
            | SchemaError.EmptyForm claimant ->
                sprintf "%s has an empty name: its token would be '--', which is the positional separator" claimant
            | SchemaError.FormContainsEquals (claimant, form) ->
                sprintf
                    "%s has the name '%s', which contains '='; a --key=value token splits at its first '=', so this argument could never be addressed"
                    claimant
                    form
            | SchemaError.TokenCollision (token, claimants) ->
                sprintf
                    "the token '%s' is claimed by: %s (argument names are matched case-insensitively)"
                    token
                    (String.concat "; " claimants)
            | SchemaError.DuplicatePositionalId positionalId ->
                sprintf "two positional-args sinks share the id %i" positionalId
            | SchemaError.PositionalRepeatedInTree positionalId ->
                sprintf "the schema tree refers to positional-args sink id %i more than once" positionalId
            | SchemaError.PositionalNotInTable positionalId ->
                sprintf "the schema tree refers to positional-args sink id %i, which does not exist" positionalId
            | SchemaError.PositionalNotInTree positionalId ->
                sprintf "positional-args sink id %i is not reachable in the schema tree" positionalId
            | SchemaError.PositionalCapacityExceeded maximum ->
                sprintf
                    "some alternative of the schema contains %i positional-args sinks; at most one may be active, because argv holds a single stream of positional args"
                    maximum
            | SchemaError.FlagLikePolicyDisagreement ->
                "positional-args sinks disagree on whether to collect unrecognised flag-like tokens; that choice affects tokenization, which happens before alternatives are chosen between, so all sinks must agree"

    /// An ErasedSchema which has passed the structural checks in WellFormedSchema.check.
    ///
    /// The selection semantics are only correct on schemas of this type: in particular, every
    /// addressable `--token` (a form, a `--no-` variant of a negatable form, or the positional
    /// sink's own name) must name at most one claimant under the scanner's case-insensitive
    /// matching, or matchLeaf would silently route the token to whichever colliding leaf is
    /// declared first — for a Sum schema, silently selecting the wrong case.
    type WellFormedSchema = private | WellFormed of ErasedSchema

    [<RequireQualifiedAccess>]
    module WellFormedSchema =
        /// All the structural defects of this schema (empty exactly when it is well-formed).
        let errors (schema : ErasedSchema) : SchemaError list =
            let duplicateLeafIds =
                schema.Leaves
                |> List.countBy (fun leaf -> leaf.Id)
                |> List.filter (fun (_, count) -> count > 1)
                |> List.map (fun (leafId, _) -> SchemaError.DuplicateLeafId leafId)

            let treeIds = leafIds schema.Tree
            let tableIds = schema.Leaves |> List.map (fun leaf -> leaf.Id)
            let treeIdSet = Set.ofList treeIds
            let tableIdSet = Set.ofList tableIds

            let repeatedInTree =
                treeIds
                |> List.countBy (fun leafId -> leafId)
                |> List.filter (fun (_, count) -> count > 1)
                |> List.map (fun (leafId, _) -> SchemaError.LeafRepeatedInTree leafId)

            let notInTable =
                treeIds
                |> List.distinct
                |> List.filter (fun leafId -> not (Set.contains leafId tableIdSet))
                |> List.map SchemaError.LeafNotInTable

            let notInTree =
                tableIds
                |> List.distinct
                |> List.filter (fun leafId -> not (Set.contains leafId treeIdSet))
                |> List.map SchemaError.LeafNotInTree

            let positionalTreeIds = positionalIds schema.Tree
            let positionalTableIds = schema.Positionals |> List.map (fun p -> p.Id)
            let positionalTreeIdSet = Set.ofList positionalTreeIds
            let positionalTableIdSet = Set.ofList positionalTableIds

            let duplicatePositionalIds =
                positionalTableIds
                |> List.countBy (fun positionalId -> positionalId)
                |> List.filter (fun (_, count) -> count > 1)
                |> List.map (fun (positionalId, _) -> SchemaError.DuplicatePositionalId positionalId)

            let positionalRepeatedInTree =
                positionalTreeIds
                |> List.countBy (fun positionalId -> positionalId)
                |> List.filter (fun (_, count) -> count > 1)
                |> List.map (fun (positionalId, _) -> SchemaError.PositionalRepeatedInTree positionalId)

            let positionalNotInTable =
                positionalTreeIds
                |> List.distinct
                |> List.filter (fun positionalId -> not (Set.contains positionalId positionalTableIdSet))
                |> List.map SchemaError.PositionalNotInTable

            let positionalNotInTree =
                positionalTableIds
                |> List.distinct
                |> List.filter (fun positionalId -> not (Set.contains positionalId positionalTreeIdSet))
                |> List.map SchemaError.PositionalNotInTree

            let capacityExceeded =
                let maximum = maxPositionals schema.Tree

                if maximum > 1 then
                    [ SchemaError.PositionalCapacityExceeded maximum ]
                else
                    []

            let policyDisagreement =
                let policies =
                    schema.Positionals
                    |> List.map (fun p ->
                        match p.FlagLike with
                        | ErasedFlagLikeBehaviour.Collect -> true
                        | ErasedFlagLikeBehaviour.Reject -> false
                    )
                    |> List.distinct

                match policies with
                | []
                | [ _ ] -> []
                | _ -> [ SchemaError.FlagLikePolicyDisagreement ]

            let duplicateSumIds =
                let rec sumIds (tree : ErasedTree) : int list =
                    match tree with
                    | ErasedTree.Leaf _ -> []
                    | ErasedTree.PositionalLeaf _ -> []
                    | ErasedTree.Product children -> children |> List.collect sumIds
                    | ErasedTree.Sum (sumId, cases) -> sumId :: (cases |> List.collect (fun (_, case) -> sumIds case))

                sumIds schema.Tree
                |> List.countBy (fun sumId -> sumId)
                |> List.filter (fun (_, count) -> count > 1)
                |> List.map (fun (sumId, _) -> SchemaError.DuplicateSumId sumId)

            let noForms =
                schema.Leaves
                |> List.filter (fun leaf -> List.isEmpty leaf.Forms)
                |> List.map (fun leaf -> SchemaError.NoForms leaf.Id)

            let negationOnNonBool =
                schema.Leaves
                |> List.filter (fun leaf ->
                    leaf.AcceptsNegation
                    && (
                        match leaf.Arity with
                        | ErasedArity.BoolLike -> false
                        | ErasedArity.One -> true
                    )
                )
                |> List.map (fun leaf -> SchemaError.NegationOnNonBool leaf.Id)

            // Every `--token` the scanner would recognise in key position, with a description
            // of its claimant, in declaration order. matchLeaf, the positional-sink match and
            // the `--help` match are all case-insensitive, so claims are grouped under a
            // case-normalised key (ToUpperInvariant, the equality OrdinalIgnoreCase uses).
            // The boolean records whether the claimant is a positional sink: sinks may share
            // tokens with each other (whichever sink is active, a keyed positional token
            // means the same thing), but not with anything else.
            let claims =
                (schema.Leaves
                 |> List.collect (fun leaf ->
                     let plain =
                         leaf.Forms
                         |> List.map (fun form -> "--" + form, sprintf "argument '--%s'" form, false)

                     let negated =
                         if leaf.AcceptsNegation then
                             leaf.Forms
                             |> List.map (fun form ->
                                 "--no-" + form, sprintf "the --no- form of argument '--%s'" form, false
                             )
                         else
                             []

                     plain @ negated
                 ))
                @ (schema.Positionals
                   |> List.collect (fun positional ->
                       positional.Forms
                       |> List.map (fun form -> "--" + form, sprintf "the positional-args sink '--%s'" form, true)
                   ))
                @ [ "--help", "the built-in help flag", false ]

            // Group under the scanner's own equality (OrdinalIgnoreCase), preserving
            // declaration order. This is deliberately not ToUpperInvariant keying, which is a
            // strictly coarser relation: e.g. "s" and "ſ" (long s) uppercase to the same string,
            // but the scanner considers them distinct, so they do not collide.
            let collisions =
                let indexOf =
                    System.Collections.Generic.Dictionary<string, int> (StringComparer.OrdinalIgnoreCase)

                let buckets = ResizeArray<ResizeArray<string * string * bool>> ()

                for token, claimant, isSink in claims do
                    match indexOf.TryGetValue token with
                    | true, index -> buckets.[index].Add ((token, claimant, isSink))
                    | false, _ ->
                        indexOf.[token] <- buckets.Count
                        let bucket = ResizeArray ()
                        bucket.Add ((token, claimant, isSink))
                        buckets.Add bucket

                buckets
                |> Seq.choose (fun bucket ->
                    let allSinks = bucket |> Seq.forall (fun (_, _, isSink) -> isSink)

                    if bucket.Count < 2 || allSinks then
                        None
                    else
                        let token, _, _ = bucket.[0]

                        Some (
                            SchemaError.TokenCollision (
                                token,
                                bucket |> Seq.map (fun (_, claimant, _) -> claimant) |> List.ofSeq
                            )
                        )
                )
                |> List.ofSeq

            // Forms which no token could ever address, because of how the scanner tokenises.
            let unaddressable =
                (schema.Leaves
                 |> List.collect (fun leaf ->
                     let claimant = sprintf "argument id %i" leaf.Id

                     leaf.Forms
                     |> List.collect (fun form ->
                         if form = "" then
                             [ SchemaError.EmptyForm claimant ]
                         elif form.Contains "=" then
                             [ SchemaError.FormContainsEquals (claimant, form) ]
                         else
                             []
                     )
                 ))
                @ (schema.Positionals
                   |> List.collect (fun positional ->
                       let claimant = sprintf "positional-args sink id %i" positional.Id

                       positional.Forms
                       |> List.collect (fun form ->
                           if form = "" then
                               [ SchemaError.EmptyForm claimant ]
                           elif form.Contains "=" then
                               [ SchemaError.FormContainsEquals (claimant, form) ]
                           else
                               []
                       )
                   ))

            duplicateLeafIds
            @ repeatedInTree
            @ notInTable
            @ notInTree
            @ duplicatePositionalIds
            @ positionalRepeatedInTree
            @ positionalNotInTable
            @ positionalNotInTree
            @ capacityExceeded
            @ policyDisagreement
            @ duplicateSumIds
            @ noForms
            @ negationOnNonBool
            @ unaddressable
            @ collisions

        /// Check the invariants which the scanning and selection semantics rely on.
        let check (schema : ErasedSchema) : Result<WellFormedSchema, SchemaError list> =
            match errors schema with
            | [] -> Ok (WellFormed schema)
            | errors -> Error errors

        /// Check, throwing on failure. For generated code: a malformed schema is a bug in the
        /// parser's definition, which must fail fast rather than misparse.
        let checkOrFail (schema : ErasedSchema) : WellFormedSchema =
            match check schema with
            | Ok wellFormed -> wellFormed
            | Error errors ->
                let messages = errors |> List.map SchemaError.describe |> String.concat "\n"
                failwith ("Invalid argument parser definition:\n" + messages)

        /// Extract the underlying schema.
        let schema (wellFormed : WellFormedSchema) : ErasedSchema =
            match wellFormed with
            | WellFormed schema -> schema

    /// The outcome of a full parse, before the typed layer's final record assembly.
    [<RequireQualifiedAccess>]
    type ParseOutcome =
        /// Every argument was routed, converted and defaulted without error: the typed layer's
        /// slots are fully populated and it may assemble the result. The selection records which
        /// case was chosen for every discriminated union in the schema.
        | Success of selection : Selection
        /// A `--help`-shaped token was seen; the typed layer should render help and stop.
        | HelpRequested
        /// The parse was aborted mid-scan (historically these conditions threw immediately). The
        /// message is fully rendered.
        | Fatal of message : string
        /// The parse completed but there were errors; the messages are fully rendered, in order.
        | Errors of messages : string list

    /// The typed layer's callbacks: everything the erased runtime cannot do itself because it
    /// involves the target types.
    type TypedCallbacks =
        {
            /// Convert an occurrence's value and store it in the leaf's slot, returning a
            /// conversion-error message on failure. Called in argv order. A non-repeatable leaf
            /// which is already populated must be left alone (first occurrence wins); duplicate
            /// errors are reported by the runtime, not by this callback.
            StoreOccurrence : ErasedOccurrence -> string option
            /// Convert a positional token (true = it appeared after the `--` separator) and store
            /// it in the positional sink, returning a conversion-error message on failure. Only
            /// called when the schema has a positional sink.
            StorePositional : string -> bool -> string option
            /// Render the help text (used in fatal unknown-key messages).
            HelpText : unit -> string
            /// Render the already-stored first value of the given leaf, for duplicate-argument
            /// messages (historically the first value is shown via ToString and the rejected
            /// occurrence as its raw token).
            RenderStored : int -> string
            /// Populate the given leaf's slot from its default source (environment variable or
            /// user-supplied function), returning an error message on failure. Only called when
            /// the parse is otherwise error-free.
            ApplyDefault : int -> string option
        }

    /// Every way the argument can be spelled, for messages: e.g. "--foo / --bar / --no-foo / --no-bar".
    let humanReadableForms (leaf : ErasedLeaf) : string =
        let standard = leaf.Forms |> List.map (fun form -> "--" + form)

        let all =
            if leaf.AcceptsNegation then
                standard @ (leaf.Forms |> List.map (fun form -> "--no-" + form))
            else
                standard

        String.concat " / " all

    /// Drive a full parse: scan, route occurrences into the typed layer's slots (in argv order,
    /// so conversion errors interleave faithfully), select union cases, validate, render every
    /// structural error, and apply defaults (only when the parse is otherwise clean).
    let runParse (schema : WellFormedSchema) (callbacks : TypedCallbacks) (args : string list) : ParseOutcome =
        let schema = WellFormedSchema.schema schema
        let leavesById = schema.Leaves |> List.map (fun leaf -> leaf.Id, leaf) |> Map.ofList

        let events = scan schema args
        let errors = ResizeArray<string> ()

        // Leaves for which at least one occurrence was successfully converted and stored. A
        // required leaf whose only occurrences failed conversion has an empty slot, exactly as if
        // it had never been supplied, and is reported as missing (matching the historical parser).
        let stored = System.Collections.Generic.HashSet<int> ()

        let rec consume (events : ScanEvent list) : ParseOutcome option =
            match events with
            | [] -> None
            | event :: rest ->

            match event with
            | ScanEvent.Help _ -> Some ParseOutcome.HelpRequested
            | ScanEvent.Error (ScanError.UnknownKeyEqualsValue (key, value)) ->
                sprintf "Unable to process argument %s=%s as key %s and value %s" key value key value
                |> ParseOutcome.Fatal
                |> Some
            | ScanEvent.Error (ScanError.UnknownKey source) ->
                sprintf "Unable to process supplied arg %s. Help text follows.\n%s" source (callbacks.HelpText ())
                |> ParseOutcome.Fatal
                |> Some
            | ScanEvent.Error (ScanError.TrailingKeyNoValue source) ->
                sprintf
                    "Trailing argument %s had no value. Use a double-dash to separate positional args from key-value args."
                    source
                |> errors.Add

                consume rest
            | ScanEvent.Occurrence occurrence ->
                let leaf = Map.find occurrence.LeafId leavesById

                if stored.Contains occurrence.LeafId && not leaf.Repeatable then
                    // A duplicate of a slot which was already *successfully* populated. (If the
                    // first occurrence failed conversion, a later occurrence legitimately fills
                    // the slot instead, matching the historical parser.) Reported here so that
                    // diagnostics keep argv order.
                    match occurrence.Value with
                    | None ->
                        sprintf "Flag '%s' was supplied multiple times" (humanReadableForms leaf)
                        |> errors.Add
                    | Some value ->
                        sprintf
                            "Argument '%s' was supplied multiple times: %s and %s"
                            (humanReadableForms leaf)
                            (callbacks.RenderStored occurrence.LeafId)
                            value
                        |> errors.Add
                else
                    match callbacks.StoreOccurrence occurrence with
                    | Some error -> errors.Add error
                    | None -> stored.Add occurrence.LeafId |> ignore<bool>

                consume rest
            | ScanEvent.Positional positional ->
                match schema.Positionals with
                | [] ->
                    // No sink: the tokens are reported wholesale by validation below.
                    consume rest
                | _ :: _ ->
                    match callbacks.StorePositional positional.Value positional.AfterSeparator with
                    | Some error -> errors.Add error
                    | None -> ()

                    consume rest
            | ScanEvent.Separator -> consume rest

        match consume events with
        | Some outcome -> outcome
        | None ->

        let observed =
            (Map.empty, events)
            ||> List.fold (fun observed event ->
                match event with
                | ScanEvent.Occurrence occurrence ->
                    if Map.containsKey occurrence.LeafId observed then
                        observed
                    else
                        Map.add occurrence.LeafId occurrence.Source observed
                | _ -> observed
            )

        let selection = select schema observed
        let validationErrors, needsDefault = validate schema selection events

        for selectionError in selection.Errors do
            match selectionError with
            | SelectionError.ConflictingCases (_, witnesses) ->
                let describe =
                    witnesses
                    |> List.map (fun (caseName, source) -> sprintf "%s (via %s)" caseName source)
                    |> String.concat ", "

                sprintf "Arguments select more than one alternative: %s" describe |> errors.Add
            | SelectionError.NoCaseSelected (_, caseNames) ->
                sprintf "No arguments were supplied to select one of: %s" (String.concat ", " caseNames)
                |> errors.Add
            | SelectionError.AmbiguousEmptyCases (_, caseNames) ->
                sprintf
                    "Arguments do not determine which of these alternatives was intended: %s"
                    (String.concat ", " caseNames)
                |> errors.Add
            | SelectionError.UnroutablePositional source ->
                sprintf
                    "Positional argument '%s' does not belong to any single alternative consistent with the other arguments"
                    source
                |> errors.Add
            | SelectionError.AmbiguousPositionalRouting (source, sinkIds) ->
                let describeSink (sinkId : int) : string =
                    let sink = schema.Positionals |> List.tryFind (fun p -> p.Id = sinkId)

                    match sink with
                    | Some sink ->
                        match sink.Forms with
                        | form :: _ -> "--" + form
                        | [] -> sprintf "sink %i" sinkId
                    | None -> sprintf "sink %i" sinkId

                sprintf
                    "Positional args (e.g. '%s') could belong to more than one alternative (%s); supply an argument which distinguishes them"
                    source
                    (sinkIds |> List.map describeSink |> String.concat ", ")
                |> errors.Add

        for validationError in validationErrors do
            match validationError with
            | ValidationError.DuplicateOccurrences _ ->
                // Rendered inline during consumption, so that diagnostics keep argv order and
                // reflect which occurrence actually populated the slot.
                ()
            | ValidationError.MissingRequired _ ->
                // Handled below in leaf order, merged with leaves whose occurrences all failed
                // conversion.
                ()
            | ValidationError.InactiveLeaf _ ->
                // The selection conflict which caused this has already been reported.
                ()
            | ValidationError.NoPositionalSink tokens ->
                sprintf "There were leftover args: %s" (String.concat " " tokens) |> errors.Add

        // A required leaf is missing if nothing was successfully stored for it, whether it was
        // never supplied or every occurrence of it failed conversion.
        for leaf in schema.Leaves do
            let isRequired =
                match leaf.Requirement with
                | ErasedRequirement.Required -> true
                | ErasedRequirement.Optional
                | ErasedRequirement.HasDefault -> false

            if
                isRequired
                && Set.contains leaf.Id selection.ActiveLeaves
                && not (stored.Contains leaf.Id)
            then
                sprintf "Required argument '%s' received no value" (humanReadableForms leaf)
                |> errors.Add

        if errors.Count = 0 then
            for leafId in needsDefault do
                match callbacks.ApplyDefault leafId with
                | Some error -> errors.Add error
                | None -> ()

        if errors.Count = 0 then
            ParseOutcome.Success selection
        else
            ParseOutcome.Errors (List.ofSeq errors)
