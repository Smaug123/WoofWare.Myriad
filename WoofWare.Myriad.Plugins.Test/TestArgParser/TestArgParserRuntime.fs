namespace WoofWare.Myriad.Plugins.Test

open System
open NUnit.Framework
open FsUnitTyped
open FsCheck
open FsCheck.FSharp
open WoofWare.Myriad.Plugins
open WoofWare.Myriad.Plugins.ArgParserRuntime

/// Tests for the type-erased arg-parser runtime kernel: unit tests pinning the token grammar, and
/// property tests establishing that the compositional case-selection algorithm agrees with the
/// exhaustive expand-every-alternative reference semantics.
[<TestFixture>]
module TestArgParserRuntime =

    let private leaf (id : int) (form : string) (arity : ErasedArity) (req : ErasedRequirement) : ErasedLeaf =
        {
            Id = id
            Forms = [ form ]
            AcceptsNegation = false
            Arity = arity
            Repeatable = false
            Requirement = req
            TypeDescription = "string"
            Help = None
        }

    let private productSchema (leaves : ErasedLeaf list) (positionals : ErasedPositional list) : ErasedSchema =
        {
            Leaves = leaves
            Tree =
                ErasedTree.Product (
                    (leaves |> List.map (fun l -> ErasedTree.Leaf l.Id))
                    @ (positionals |> List.map (fun p -> ErasedTree.PositionalLeaf p.Id))
                )
            Positionals = positionals
        }

    /// Shadows the kernel's runParse: every schema in these tests is well-formed, so go through
    /// the checked constructor which generated code also uses.
    let private runParse (schema : ErasedSchema) : TypedCallbacks -> string list -> ParseOutcome =
        ArgParserRuntime.runParse (WellFormedSchema.checkOrFail schema)

    let private occ (leafId : int) (value : string option) (negated : bool) (source : string) : ScanEvent =
        ScanEvent.Occurrence
            {
                LeafId = leafId
                Value = value
                Negated = negated
                Source = source
            }

    let private posEv (value : string) (afterSep : bool) (form : PositionalForm) (candidates : int list) : ScanEvent =
        ScanEvent.Positional
            {
                Value = value
                AfterSeparator = afterSep
                Form = form
                Candidates = Set.ofList candidates
            }

    // ----------------------------------------------------------------------------------------
    // Unit tests: the token grammar.

    [<Test>]
    let ``Equals form and space form produce the same occurrence`` () =
        let schema =
            productSchema [ leaf 0 "foo" ErasedArity.One ErasedRequirement.Required ] []

        scan schema [ "--foo=3" ] |> shouldEqual [ occ 0 (Some "3") false "--foo=3" ]

        scan schema [ "--foo" ; "3" ] |> shouldEqual [ occ 0 (Some "3") false "--foo" ]

    [<Test>]
    let ``Keys match case-insensitively`` () =
        let schema =
            productSchema [ leaf 0 "foo" ErasedArity.One ErasedRequirement.Required ] []

        scan schema [ "--FOO=3" ] |> shouldEqual [ occ 0 (Some "3") false "--FOO=3" ]

    [<Test>]
    let ``A non-boolean-like key consumes the next token greedily`` () =
        let schema =
            productSchema
                [
                    leaf 0 "a" ErasedArity.One ErasedRequirement.Required
                    leaf 1 "b" ErasedArity.One ErasedRequirement.Optional
                ]
                []

        scan schema [ "--a" ; "--b=false" ]
        |> shouldEqual [ occ 0 (Some "--b=false") false "--a" ]

    [<Test>]
    let ``A boolean-like key consumes only boolean literals`` () =
        let schema =
            productSchema
                [
                    leaf 0 "flag" ErasedArity.BoolLike ErasedRequirement.Required
                    leaf 1 "b" ErasedArity.One ErasedRequirement.Optional
                ]
                []

        scan schema [ "--flag" ; "TRUE" ]
        |> shouldEqual [ occ 0 (Some "TRUE") false "--flag" ]

        // Not a boolean literal: the flag stands alone and the token is re-processed.
        scan schema [ "--flag" ; "--b=1" ]
        |> shouldEqual [ occ 0 None false "--flag" ; occ 1 (Some "1") false "--b=1" ]

    [<Test>]
    let ``A pending key is resolved at the separator exactly as at end of input`` () =
        let schema =
            productSchema
                [
                    leaf 0 "flag" ErasedArity.BoolLike ErasedRequirement.Required
                    leaf 1 "b" ErasedArity.One ErasedRequirement.Optional
                ]
                []

        scan schema [ "--flag" ] |> shouldEqual [ occ 0 None false "--flag" ]

        scan schema [ "--flag" ; "--" ]
        |> shouldEqual [ occ 0 None false "--flag" ; ScanEvent.Separator ]

        scan schema [ "--b" ]
        |> shouldEqual [ ScanEvent.Error (ScanError.TrailingKeyNoValue "--b") ]

        scan schema [ "--b" ; "--" ]
        |> shouldEqual [ ScanEvent.Error (ScanError.TrailingKeyNoValue "--b") ; ScanEvent.Separator ]

    [<Test>]
    let ``Help is recognised case-insensitively in key position but not in value position`` () =
        let schema =
            productSchema [ leaf 0 "foo" ErasedArity.One ErasedRequirement.Required ] []

        scan schema [ "--HELP" ] |> shouldEqual [ ScanEvent.Help "--HELP" ]

        scan schema [ "--foo" ; "--help" ]
        |> shouldEqual [ occ 0 (Some "--help") false "--foo" ]

    [<Test>]
    let ``Negated forms are recognised`` () =
        let schema =
            productSchema
                [
                    { leaf 0 "flag" ErasedArity.BoolLike ErasedRequirement.Required with
                        AcceptsNegation = true
                    }
                ]
                []

        scan schema [ "--no-flag" ] |> shouldEqual [ occ 0 None true "--no-flag" ]
        scan schema [ "--NO-FLAG" ] |> shouldEqual [ occ 0 None true "--NO-FLAG" ]

    [<Test>]
    let ``An unknown key is collected as flag-like positional only when a token follows`` () =
        let sink =
            {
                Id = 99
                Forms = [ "rest" ]
                FlagLike = ErasedFlagLikeBehaviour.Collect
                TypeDescription = "string"
                Help = None
            }

        let schema =
            productSchema [ leaf 0 "a" ErasedArity.One ErasedRequirement.Required ] [ sink ]

        scan schema [ "--unknown" ; "foo" ]
        |> shouldEqual
            [
                posEv "--unknown" false PositionalForm.Bare [ 99 ]
                posEv "foo" false PositionalForm.Bare [ 99 ]
            ]

        scan schema [ "--unknown" ]
        |> shouldEqual [ ScanEvent.Error (ScanError.TrailingKeyNoValue "--unknown") ]

        scan schema [ "--unknown=3" ; "foo" ]
        |> shouldEqual
            [
                posEv "--unknown=3" false PositionalForm.Bare [ 99 ]
                posEv "foo" false PositionalForm.Bare [ 99 ]
            ]

    [<Test>]
    let ``The positional sink's own key routes values to the sink in both syntactic forms`` () =
        let sink =
            {
                Id = 99
                Forms = [ "rest" ]
                FlagLike = ErasedFlagLikeBehaviour.Reject
                TypeDescription = "string"
                Help = None
            }

        let schema =
            productSchema [ leaf 0 "a" ErasedArity.One ErasedRequirement.Optional ] [ sink ]

        scan schema [ "--rest=5" ; "--rest" ; "6" ; "plain" ]
        |> shouldEqual
            [
                posEv "5" false (PositionalForm.KeyEquals "--rest") [ 99 ]
                posEv "6" false (PositionalForm.KeySpaced "--rest") [ 99 ]
                posEv "plain" false PositionalForm.Bare [ 99 ]
            ]

        // Case-insensitive, like every other key.
        scan schema [ "--REST=5" ]
        |> shouldEqual [ posEv "5" false (PositionalForm.KeyEquals "--REST") [ 99 ] ]

        // The keyed form always consumes exactly one value, greedily.
        scan schema [ "--rest" ; "--rest" ]
        |> shouldEqual [ posEv "--rest" false (PositionalForm.KeySpaced "--rest") [ 99 ] ]

        // Trailing key with no value: same error as any other arity-one key.
        scan schema [ "--rest" ]
        |> shouldEqual [ ScanEvent.Error (ScanError.TrailingKeyNoValue "--rest") ]

    [<Test>]
    let ``The positional sink is addressable under every one of its forms`` () =
        // A [<PositionalArgs>] field may carry several [<ArgumentLongForm>] aliases; each of
        // them must route to the sink, not just the first.
        let sink =
            {
                Id = 99
                Forms = [ "rest" ; "remainder" ]
                FlagLike = ErasedFlagLikeBehaviour.Reject
                TypeDescription = "string"
                Help = None
            }

        let schema =
            productSchema [ leaf 0 "a" ErasedArity.One ErasedRequirement.Optional ] [ sink ]

        scan schema [ "--remainder=5" ; "--REMAINDER" ; "6" ]
        |> shouldEqual
            [
                posEv "5" false (PositionalForm.KeyEquals "--remainder") [ 99 ]
                posEv "6" false (PositionalForm.KeySpaced "--REMAINDER") [ 99 ]
            ]

        scan schema [ "--rest=7" ]
        |> shouldEqual [ posEv "7" false (PositionalForm.KeyEquals "--rest") [ 99 ] ]

    [<Test>]
    let ``An unknown key is an error where flag-like collection is not allowed`` () =
        let schema =
            productSchema [ leaf 0 "a" ErasedArity.One ErasedRequirement.Required ] []

        scan schema [ "--unknown=3" ]
        |> shouldEqual [ ScanEvent.Error (ScanError.UnknownKeyEqualsValue ("--unknown", "3")) ]

        scan schema [ "--unknown" ; "x" ]
        |> shouldEqual
            [
                ScanEvent.Error (ScanError.UnknownKey "--unknown")
                posEv "x" false PositionalForm.Bare []
            ]

    // ----------------------------------------------------------------------------------------
    // Unit tests: selection semantics on the motivating example.
    //
    //     type Args =
    //         | FooCase of {| Foo : int |}
    //         | BarCase of {| Bar : int ; Baz : int |}

    let private motivating : ErasedSchema =
        {
            Leaves =
                [
                    leaf 0 "foo" ErasedArity.One ErasedRequirement.Required
                    leaf 1 "bar" ErasedArity.One ErasedRequirement.Required
                    leaf 2 "baz" ErasedArity.One ErasedRequirement.Required
                ]
            Tree =
                ErasedTree.Sum (
                    0,
                    [
                        "FooCase", ErasedTree.Product [ ErasedTree.Leaf 0 ]
                        "BarCase", ErasedTree.Product [ ErasedTree.Leaf 1 ; ErasedTree.Leaf 2 ]
                    ]
                )
            Positionals = []
        }

    let private observedOf (events : ScanEvent list) : Map<int, string> =
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

    [<Test>]
    let ``Motivating example: --foo selects FooCase`` () =
        let events = scan motivating [ "--foo=3" ]
        let selection = select motivating (observedOf events)
        selection.Errors |> shouldEqual []
        selection.Choices |> shouldEqual (Map.ofList [ 0, 0 ])
        selection.ActiveLeaves |> shouldEqual (Set.singleton 0)
        let errors, needsDefault = validate motivating selection events
        errors |> shouldEqual []
        needsDefault |> shouldEqual []

    [<Test>]
    let ``Motivating example: --bar --baz selects BarCase`` () =
        let events = scan motivating [ "--bar=8" ; "--baz=9" ]
        let selection = select motivating (observedOf events)
        selection.Errors |> shouldEqual []
        selection.Choices |> shouldEqual (Map.ofList [ 0, 1 ])
        let errors, _ = validate motivating selection events
        errors |> shouldEqual []

    [<Test>]
    let ``Motivating example: --foo --bar is a conflict, not a parse`` () =
        let events = scan motivating [ "--foo=3" ; "--bar=8" ]
        let selection = select motivating (observedOf events)

        selection.Errors
        |> shouldEqual
            [
                SelectionError.ConflictingCases (0, [ "FooCase", "--foo=3" ; "BarCase", "--bar=8" ])
            ]

    [<Test>]
    let ``Motivating example: --bar alone selects BarCase but is incomplete`` () =
        let events = scan motivating [ "--bar=8" ]
        let selection = select motivating (observedOf events)
        selection.Errors |> shouldEqual []
        selection.Choices |> shouldEqual (Map.ofList [ 0, 1 ])
        let errors, _ = validate motivating selection events
        errors |> shouldEqual [ ValidationError.MissingRequired 2 ]

    [<Test>]
    let ``Motivating example: no args selects nothing`` () =
        let selection = select motivating Map.empty

        selection.Errors
        |> shouldEqual [ SelectionError.NoCaseSelected (0, [ "FooCase" ; "BarCase" ]) ]

    // ----------------------------------------------------------------------------------------
    // Unit tests: duplicate detection in validation.

    [<Test>]
    let ``A non-repeatable argument supplied twice is a duplicate, whatever forms were used`` () =
        let schema =
            productSchema
                [
                    { leaf 0 "flag" ErasedArity.BoolLike ErasedRequirement.Required with
                        Forms = [ "flag" ; "alias" ]
                        AcceptsNegation = true
                    }
                ]
                []

        // Twice through different spellings: still a duplicate of the one leaf.
        let events = scan schema [ "--flag=true" ; "--no-alias" ]
        let selection = select schema (observedOf events)
        let errors, _ = validate schema selection events

        match errors with
        | [ ValidationError.DuplicateOccurrences (0, occurrences) ] ->
            occurrences |> List.map _.Source |> shouldEqual [ "--flag=true" ; "--no-alias" ]
        | other -> failwithf "unexpected validation result: %A" other

    [<Test>]
    let ``A repeatable argument supplied twice is not a duplicate`` () =
        let schema =
            productSchema
                [
                    { leaf 0 "rest" ErasedArity.One ErasedRequirement.Optional with
                        Repeatable = true
                    }
                ]
                []

        let events = scan schema [ "--rest=1" ; "--rest=2" ]
        let selection = select schema (observedOf events)
        let errors, _ = validate schema selection events
        errors |> shouldEqual []

    // ----------------------------------------------------------------------------------------
    // Unit tests: the runParse orchestrator, with callbacks which store raw strings into a map
    // (a stand-in for the generated typed layer).

    /// Callbacks which record everything and store first-wins raw values per leaf.
    type private FakeTyped =
        {
            mutable Slots : Map<int, string list>
            mutable Positionals : (string * bool) list
            mutable Defaults : int list
            mutable HelpTextCalls : int
        }

        static member Fresh () : FakeTyped =
            {
                Slots = Map.empty
                Positionals = []
                Defaults = []
                HelpTextCalls = 0
            }

        member this.Callbacks (schema : ErasedSchema) : TypedCallbacks =
            let leafById (id : int) : ErasedLeaf =
                schema.Leaves |> List.find (fun l -> l.Id = id)

            {
                StoreOccurrence =
                    fun occurrence ->
                        let leaf = leafById occurrence.LeafId
                        let existing = Map.tryFind occurrence.LeafId this.Slots |> Option.defaultValue []

                        if not leaf.Repeatable && not (List.isEmpty existing) then
                            // First occurrence wins; the runtime reports the duplicate.
                            None
                        elif occurrence.Value = Some "unconvertible" then
                            Some (sprintf "cannot convert (at arg %s)" occurrence.Source)
                        else
                            let value =
                                match occurrence.Value with
                                | Some v -> (if occurrence.Negated then "negated:" + v else v)
                                | None -> (if occurrence.Negated then "false" else "true")

                            this.Slots <- Map.add occurrence.LeafId (existing @ [ value ]) this.Slots
                            None
                StorePositional =
                    fun value afterSeparator ->
                        this.Positionals <- this.Positionals @ [ value, afterSeparator ]
                        None
                HelpText =
                    fun () ->
                        this.HelpTextCalls <- this.HelpTextCalls + 1
                        "HELP TEXT"
                RenderStored =
                    fun leafId ->
                        match Map.tryFind leafId this.Slots with
                        | Some (first :: _) -> first
                        | _ -> "<nothing stored>"
                ApplyDefault =
                    fun leafId ->
                        this.Defaults <- this.Defaults @ [ leafId ]
                        None
            }

    /// The selection a product-only schema always yields on success.
    let private productSelection (schema : ErasedSchema) : Selection =
        {
            Choices = Map.empty
            ActiveLeaves = schema.Leaves |> List.map (fun l -> l.Id) |> Set.ofList
            ActivePositional = schema.Positionals |> List.tryHead |> Option.map (fun p -> p.Id)
            Errors = []
        }

    [<Test>]
    let ``runParse: clean parse stores values and applies defaults`` () =
        let schema =
            productSchema
                [
                    leaf 0 "foo" ErasedArity.One ErasedRequirement.Required
                    leaf 1 "bar" ErasedArity.One ErasedRequirement.HasDefault
                ]
                []

        let fake = FakeTyped.Fresh ()

        runParse schema (fake.Callbacks schema) [ "--foo=3" ]
        |> shouldEqual (ParseOutcome.Success (productSelection schema))

        fake.Slots |> shouldEqual (Map.ofList [ 0, [ "3" ] ])
        fake.Defaults |> shouldEqual [ 1 ]
        fake.HelpTextCalls |> shouldEqual 0

    [<Test>]
    let ``runParse: defaults do not run when the parse has failed`` () =
        let schema =
            productSchema
                [
                    leaf 0 "foo" ErasedArity.One ErasedRequirement.Required
                    leaf 1 "bar" ErasedArity.One ErasedRequirement.HasDefault
                ]
                []

        let fake = FakeTyped.Fresh ()

        runParse schema (fake.Callbacks schema) [ "--foo=unconvertible" ]
        |> shouldEqual (
            ParseOutcome.Errors
                [
                    "cannot convert (at arg --foo=unconvertible)"
                    "Required argument '--foo' received no value"
                ]
        )

        fake.Defaults |> shouldEqual []

    [<Test>]
    let ``runParse: a conversion error does not stop the scan`` () =
        let schema =
            productSchema
                [
                    leaf 0 "foo" ErasedArity.One ErasedRequirement.Required
                    leaf 1 "bar" ErasedArity.One ErasedRequirement.Required
                ]
                []

        let fake = FakeTyped.Fresh ()

        runParse schema (fake.Callbacks schema) [ "--foo" ; "unconvertible" ; "--bar=ok" ]
        |> shouldEqual (
            ParseOutcome.Errors
                [
                    "cannot convert (at arg --foo)"
                    "Required argument '--foo' received no value"
                ]
        )

        // --bar was still parsed, despite the earlier conversion failure.
        fake.Slots |> shouldEqual (Map.ofList [ 1, [ "ok" ] ])

    [<Test>]
    let ``runParse: help wins, lazily`` () =
        let schema =
            productSchema [ leaf 0 "foo" ErasedArity.One ErasedRequirement.Required ] []

        let fake = FakeTyped.Fresh ()

        runParse schema (fake.Callbacks schema) [ "--foo=unconvertible" ; "--HELP" ]
        |> shouldEqual ParseOutcome.HelpRequested

        // The help text itself is rendered by the caller, not by runParse.
        fake.HelpTextCalls |> shouldEqual 0

    [<Test>]
    let ``runParse: unknown keys are fatal where flag-like collection is not allowed`` () =
        let schema =
            productSchema [ leaf 0 "foo" ErasedArity.One ErasedRequirement.Optional ] []

        let fake = FakeTyped.Fresh ()

        runParse schema (fake.Callbacks schema) [ "--quux=3" ]
        |> shouldEqual (ParseOutcome.Fatal "Unable to process argument --quux=3 as key --quux and value 3")

        let fake = FakeTyped.Fresh ()

        runParse schema (fake.Callbacks schema) [ "--quux" ; "x" ]
        |> shouldEqual (ParseOutcome.Fatal "Unable to process supplied arg --quux. Help text follows.\nHELP TEXT")

        fake.HelpTextCalls |> shouldEqual 1

    [<Test>]
    let ``runParse: duplicate messages distinguish valued and flag occurrences`` () =
        let schema =
            productSchema
                [
                    leaf 0 "foo" ErasedArity.One ErasedRequirement.Required
                    leaf 1 "flag" ErasedArity.BoolLike ErasedRequirement.Optional
                ]
                []

        let fake = FakeTyped.Fresh ()

        runParse schema (fake.Callbacks schema) [ "--foo=3" ; "--foo=4" ; "--flag" ; "--flag" ]
        |> shouldEqual (
            ParseOutcome.Errors
                [
                    "Argument '--foo' was supplied multiple times: 3 and 4"
                    "Flag '--flag' was supplied multiple times"
                ]
        )

        // First occurrence wins.
        fake.Slots |> shouldEqual (Map.ofList [ 0, [ "3" ] ; 1, [ "true" ] ])

    [<Test>]
    let ``runParse: a failed first occurrence is not a duplicate of a successful second`` () =
        let schema =
            productSchema [ leaf 0 "foo" ErasedArity.One ErasedRequirement.Required ] []

        let fake = FakeTyped.Fresh ()

        // The first occurrence never populated the slot, so the second legitimately fills it:
        // no duplicate error, and no false "1 and 1" diagnostic.
        runParse schema (fake.Callbacks schema) [ "--foo=unconvertible" ; "--foo=1" ]
        |> shouldEqual (ParseOutcome.Errors [ "cannot convert (at arg --foo=unconvertible)" ])

        fake.Slots |> shouldEqual (Map.ofList [ 0, [ "1" ] ])

    [<Test>]
    let ``runParse: duplicate errors appear in argv order among other errors`` () =
        let schema =
            productSchema
                [
                    leaf 0 "foo" ErasedArity.One ErasedRequirement.Required
                    leaf 1 "bar" ErasedArity.One ErasedRequirement.Required
                ]
                []

        let fake = FakeTyped.Fresh ()

        runParse schema (fake.Callbacks schema) [ "--foo=1" ; "--foo=2" ; "--bar=unconvertible" ]
        |> shouldEqual (
            ParseOutcome.Errors
                [
                    "Argument '--foo' was supplied multiple times: 1 and 2"
                    "cannot convert (at arg --bar=unconvertible)"
                    "Required argument '--bar' received no value"
                ]
        )

    [<Test>]
    let ``runParse: leftover positionals without a sink are an error`` () =
        let schema =
            productSchema [ leaf 0 "foo" ErasedArity.One ErasedRequirement.Optional ] []

        let fake = FakeTyped.Fresh ()

        runParse schema (fake.Callbacks schema) [ "stray" ; "--" ; "tail" ]
        |> shouldEqual (ParseOutcome.Errors [ "There were leftover args: stray tail" ])

        fake.Positionals |> shouldEqual []

    [<Test>]
    let ``runParse: positionals reach the sink with their separator flag`` () =
        let sink =
            {
                Id = 99
                Forms = [ "rest" ]
                FlagLike = ErasedFlagLikeBehaviour.Reject
                TypeDescription = "string"
                Help = None
            }

        let schema =
            productSchema [ leaf 0 "foo" ErasedArity.One ErasedRequirement.Optional ] [ sink ]

        let fake = FakeTyped.Fresh ()

        runParse schema (fake.Callbacks schema) [ "pre" ; "--foo=1" ; "--" ; "post" ]
        |> shouldEqual (ParseOutcome.Success (productSelection schema))

        fake.Positionals |> shouldEqual [ "pre", false ; "post", true ]

    [<Test>]
    let ``runParse: motivating example end to end`` () =
        let fake = FakeTyped.Fresh ()

        runParse motivating (fake.Callbacks motivating) [ "--bar=8" ]
        |> shouldEqual (ParseOutcome.Errors [ "Required argument '--baz' received no value" ])

        let fake = FakeTyped.Fresh ()

        runParse motivating (fake.Callbacks motivating) [ "--foo=3" ; "--bar=8" ]
        |> shouldEqual (
            ParseOutcome.Errors
                [
                    "Arguments select more than one alternative: FooCase (via --foo=3), BarCase (via --bar=8)"
                ]
        )

    // ----------------------------------------------------------------------------------------
    // The exhaustive reference semantics: expand a schema tree into every complete alternative
    // (one case chosen for every reachable Sum), and accept precisely those alternatives whose
    // leaf set covers everything observed and whose required leaves were all observed. This is
    // the *definition* of the parser's sum semantics; `select` is an optimisation of it which is
    // valid because argument names are globally unique.

    let rec private alternatives (tree : ErasedTree) : (Map<int, int> * Set<int>) list =
        match tree with
        | ErasedTree.Leaf leafId -> [ Map.empty, Set.singleton leafId ]
        // These alternatives track named leaves only; positional resolution has its own
        // reference semantics in TestArgParserPositionalReference.
        | ErasedTree.PositionalLeaf _ -> [ Map.empty, Set.empty ]
        | ErasedTree.Product children ->
            ([ Map.empty, Set.empty ], children)
            ||> List.fold (fun accs child ->
                let childAlts = alternatives child

                accs
                |> List.collect (fun (accChoices, accLeaves) ->
                    childAlts
                    |> List.map (fun (childChoices, childLeaves) ->
                        let choices = (accChoices, childChoices) ||> Map.fold (fun m k v -> Map.add k v m)

                        choices, Set.union accLeaves childLeaves
                    )
                )
            )
        | ErasedTree.Sum (sumId, cases) ->
            cases
            |> List.indexed
            |> List.collect (fun (index, (_, case)) ->
                alternatives case
                |> List.map (fun (choices, leaves) -> Map.add sumId index choices, leaves)
            )

    [<RequireQualifiedAccess>]
    type private ExhaustiveOutcome =
        | Unique of choices : Map<int, int> * activeLeaves : Set<int>
        | NoInterpretation
        | Ambiguous

    let private exhaustiveSelect (schema : ErasedSchema) (observedIds : Set<int>) : ExhaustiveOutcome =
        let requiredIds =
            schema.Leaves
            |> List.filter (fun leaf ->
                match leaf.Requirement with
                | ErasedRequirement.Required -> true
                | ErasedRequirement.Optional
                | ErasedRequirement.HasDefault -> false
            )
            |> List.map (fun leaf -> leaf.Id)
            |> Set.ofList

        let accepted =
            alternatives schema.Tree
            |> List.filter (fun (_, leaves) ->
                Set.isSubset observedIds leaves
                && Set.isSubset (Set.intersect requiredIds leaves) observedIds
            )

        match accepted with
        | [ (choices, leaves) ] -> ExhaustiveOutcome.Unique (choices, leaves)
        | [] -> ExhaustiveOutcome.NoInterpretation
        | _ -> ExhaustiveOutcome.Ambiguous

    // ----------------------------------------------------------------------------------------
    // Generators.

    /// A random leaf. Forms are "argN" (with sometimes an "altN" alias), so they are globally
    /// unique, never clash with "help", and never clash with anything's "no-" form.
    let private genLeaf (id : int) : Gen<ErasedLeaf> =
        gen {
            let! arity = Gen.elements [ ErasedArity.One ; ErasedArity.BoolLike ]

            let! negation =
                match arity with
                | ErasedArity.BoolLike -> Gen.frequency [ (2, Gen.constant true) ; (3, Gen.constant false) ]
                | ErasedArity.One -> Gen.constant false

            let! extraAlias = Gen.frequency [ (1, Gen.constant true) ; (3, Gen.constant false) ]
            let! repeatable = Gen.frequency [ (1, Gen.constant true) ; (3, Gen.constant false) ]

            let! requirement =
                Gen.frequency
                    [
                        (4, Gen.constant ErasedRequirement.Required)
                        (3, Gen.constant ErasedRequirement.Optional)
                        (2, Gen.constant ErasedRequirement.HasDefault)
                    ]

            let forms =
                if extraAlias then
                    [ sprintf "arg%i" id ; sprintf "alt%i" id ]
                else
                    [ sprintf "arg%i" id ]

            return
                {
                    Id = id
                    Forms = forms
                    AcceptsNegation = negation
                    Arity = arity
                    Repeatable = repeatable
                    Requirement = requirement
                    TypeDescription = "string"
                    Help = None
                }
        }

    /// A random tree over exactly the given leaf ids. `sumBias` (out of 100) controls how often
    /// an internal node is a Sum rather than a Product; we fuzz over it so that both sum-heavy
    /// and product-heavy shapes are exercised.
    let rec private genTreeOver (sumBias : int) (ids : int list) : Gen<ErasedTree> =
        match ids with
        | [] -> Gen.constant (ErasedTree.Product [])
        | [ id ] ->
            // Sometimes wrap the single leaf in structure anyway.
            Gen.frequency
                [
                    (4, Gen.constant (ErasedTree.Leaf id))
                    (1, Gen.constant (ErasedTree.Product [ ErasedTree.Leaf id ]))
                ]
        | ids ->
            gen {
                // Split the ids into 2..min(4, length) contiguous nonempty groups.
                let n = List.length ids
                let! groupCount = Gen.choose (2, min 4 n)

                // Choose groupCount-1 distinct cut points in 1..n-1.
                let rec chooseCuts (existing : Set<int>) : Gen<Set<int>> =
                    if Set.count existing = groupCount - 1 then
                        Gen.constant existing
                    else
                        gen {
                            let! cut = Gen.choose (1, n - 1)
                            return! chooseCuts (Set.add cut existing)
                        }

                let! cuts = chooseCuts Set.empty
                let boundaries = [ 0 ] @ Set.toList cuts @ [ n ]

                let groups =
                    List.pairwise boundaries
                    |> List.map (fun (start, finish) -> ids |> List.skip start |> List.take (finish - start))

                let! children = groups |> List.map (genTreeOver sumBias) |> Gen.sequenceToList
                let! isSum = Gen.frequency [ (sumBias, Gen.constant true) ; (100 - sumBias, Gen.constant false) ]

                if isSum then
                    let cases = children |> List.mapi (fun i child -> sprintf "Case%i" i, child)

                    // Sum ids are renumbered globally afterwards; 0 is a placeholder.
                    return ErasedTree.Sum (0, cases)
                else
                    return ErasedTree.Product children
            }

    /// Give every Sum node a distinct id (the generator leaves placeholders).
    let private renumberSums (tree : ErasedTree) : ErasedTree =
        let mutable next = 0

        let rec go (tree : ErasedTree) : ErasedTree =
            match tree with
            | ErasedTree.Leaf id -> ErasedTree.Leaf id
            | ErasedTree.PositionalLeaf id -> ErasedTree.PositionalLeaf id
            | ErasedTree.Product children -> ErasedTree.Product (children |> List.map go)
            | ErasedTree.Sum (_, cases) ->
                let id = next
                next <- next + 1
                ErasedTree.Sum (id, cases |> List.map (fun (name, case) -> name, go case))

        go tree

    let private genSchema (sumBias : int) : Gen<ErasedSchema> =
        gen {
            let! leafCount = Gen.choose (1, 10)
            let! leaves = List.init leafCount genLeaf |> Gen.sequenceToList
            let! tree = genTreeOver sumBias (leaves |> List.map (fun l -> l.Id))

            return
                {
                    Leaves = leaves
                    Tree = renumberSums tree
                    Positionals = []
                }
        }

    /// An observed-leaf set, biased towards nearly-valid interpretations: walk the tree choosing
    /// one case per sum, take that alternative's required leaves, then add each of its optional
    /// leaves with some probability, drop each required one with a small probability, and add
    /// noise leaves from outside the alternative with some probability. The percentages are
    /// fuzzed by the caller.
    let private genObserved
        (dropPct : int)
        (optionalPct : int)
        (noisePct : int)
        (schema : ErasedSchema)
        : Gen<Set<int>>
        =
        gen {
            let rec pickAlternative (tree : ErasedTree) : Gen<Set<int>> =
                match tree with
                | ErasedTree.Leaf id -> Gen.constant (Set.singleton id)
                | ErasedTree.PositionalLeaf _ -> Gen.constant Set.empty
                | ErasedTree.Product children ->
                    gen {
                        let! sets = children |> List.map pickAlternative |> Gen.sequenceToList
                        return Set.unionMany (Set.empty :: sets)
                    }
                | ErasedTree.Sum (_, cases) ->
                    gen {
                        let! _, case = Gen.elements cases
                        return! pickAlternative case
                    }

            let! alternative = pickAlternative schema.Tree

            let requirementOf (id : int) : ErasedRequirement =
                (schema.Leaves |> List.find (fun l -> l.Id = id)).Requirement

            let! kept =
                alternative
                |> Set.toList
                |> List.map (fun id ->
                    gen {
                        let! roll = Gen.choose (1, 100)

                        let keep =
                            match requirementOf id with
                            | ErasedRequirement.Required -> roll > dropPct
                            | ErasedRequirement.Optional
                            | ErasedRequirement.HasDefault -> roll <= optionalPct

                        return (if keep then Some id else None)
                    }
                )
                |> Gen.sequenceToList

            let! noise =
                schema.Leaves
                |> List.map (fun l ->
                    gen {
                        if Set.contains l.Id alternative then
                            return None
                        else
                            let! roll = Gen.choose (1, 100)
                            return (if roll <= noisePct then Some l.Id else None)
                    }
                )
                |> Gen.sequenceToList

            return Set.ofList (List.choose id kept @ List.choose id noise)
        }

    // ----------------------------------------------------------------------------------------
    // Property: compositional selection agrees with the exhaustive reference semantics.

    [<Test>]
    let ``Compositional selection is equivalent to exhaustive alternative expansion`` () =
        let mutable uniqueCount = 0
        let mutable noInterpCount = 0
        let mutable ambiguousCount = 0

        let cases =
            gen {
                let! sumBias = Gen.elements [ 20 ; 50 ; 80 ]
                let! schema = genSchema sumBias
                let! dropPct = Gen.elements [ 0 ; 0 ; 0 ; 20 ]
                let! optionalPct = Gen.elements [ 0 ; 30 ; 70 ; 100 ]
                let! noisePct = Gen.elements [ 0 ; 0 ; 10 ; 40 ]
                let! observed = genObserved dropPct optionalPct noisePct schema
                return schema, observed
            }

        let property (schema : ErasedSchema, observedIds : Set<int>) : unit =
            let observed =
                observedIds
                |> Set.toList
                |> List.map (fun id -> id, sprintf "--arg%i" id)
                |> Map.ofList

            let selection = select schema observed

            // Feed selection through validation to find missing-required errors, using a
            // synthetic event log containing one occurrence per observed leaf.
            let events =
                observedIds
                |> Set.toList
                |> List.map (fun id ->
                    ScanEvent.Occurrence
                        {
                            LeafId = id
                            Value = Some "v"
                            Negated = false
                            Source = sprintf "--arg%i" id
                        }
                )

            let validationErrors, _ = validate schema selection events

            let compositionalAccepts =
                List.isEmpty selection.Errors && List.isEmpty validationErrors

            match exhaustiveSelect schema observedIds with
            | ExhaustiveOutcome.Unique (choices, activeLeaves) ->
                uniqueCount <- uniqueCount + 1
                compositionalAccepts |> shouldEqual true
                selection.Choices |> shouldEqual choices
                selection.ActiveLeaves |> shouldEqual activeLeaves
            | ExhaustiveOutcome.NoInterpretation ->
                noInterpCount <- noInterpCount + 1
                compositionalAccepts |> shouldEqual false
            | ExhaustiveOutcome.Ambiguous ->
                ambiguousCount <- ambiguousCount + 1
                compositionalAccepts |> shouldEqual false

                // Specifically, ambiguity must be reported as such: several cases were
                // satisfiable with nothing observed to distinguish them.
                selection.Errors
                |> List.exists (fun e ->
                    match e with
                    | SelectionError.AmbiguousEmptyCases _ -> true
                    | _ -> false
                )
                |> shouldEqual true

        let config = Config.QuickThrowOnFailure.WithMaxTest 2000
        Check.One (config, Prop.forAll (Arb.fromGen cases) property)

        // The generator must actually explore all three regimes; if it stops doing so, the test
        // is no longer testing anything and should be fixed.
        uniqueCount |> shouldBeGreaterThan 200
        noInterpCount |> shouldBeGreaterThan 100
        ambiguousCount |> shouldBeGreaterThan 20

    // ----------------------------------------------------------------------------------------
    // Property: scanning is lossless. `unscan` reconstructs the exact argv from the event log,
    // for completely arbitrary token lists: every token is accounted for in exactly one event.

    let private unscan (events : ScanEvent list) : string list =
        events
        |> List.collect (fun event ->
            match event with
            | ScanEvent.Occurrence occurrence ->
                match occurrence.Value with
                | None -> [ occurrence.Source ]
                | Some value ->
                    // A `--key=value` occurrence records the whole token as Source; a spaced
                    // occurrence records just the key.
                    if occurrence.Source.Contains '=' then
                        [ occurrence.Source ]
                    else
                        [ occurrence.Source ; value ]
            | ScanEvent.Positional positional ->
                match positional.Form with
                | PositionalForm.Bare -> [ positional.Value ]
                | PositionalForm.KeyEquals key -> [ key + "=" + positional.Value ]
                | PositionalForm.KeySpaced key -> [ key ; positional.Value ]
            | ScanEvent.Error (ScanError.TrailingKeyNoValue source) -> [ source ]
            | ScanEvent.Error (ScanError.UnknownKey source) -> [ source ]
            | ScanEvent.Error (ScanError.UnknownKeyEqualsValue (key, value)) -> [ key + "=" + value ]
            | ScanEvent.Help source -> [ source ]
            | ScanEvent.Separator -> [ "--" ]
        )

    /// Tokens drawn from a mixture of regimes: schema keys (well- and ill-formed), bool literals,
    /// separators, plain words, and pathological strings.
    let private genToken (schema : ErasedSchema) : Gen<string> =
        let forms =
            (schema.Leaves |> List.collect (fun l -> l.Forms))
            @ (schema.Positionals |> List.collect (fun p -> p.Forms))

        let keyish =
            match forms with
            | [] -> Gen.constant "--nothing"
            | forms ->
                gen {
                    let! form = Gen.elements forms
                    let! negate = Gen.elements [ "" ; "no-" ]
                    let! upper = Gen.elements [ true ; false ]
                    let form = (if upper then form.ToUpperInvariant () else form)

                    let! suffix = Gen.elements [ "" ; "=3" ; "=true" ; "=" ; "=a=b" ]
                    return "--" + negate + form + suffix
                }

        Gen.frequency
            [
                (5, keyish)
                (2, Gen.elements [ "true" ; "false" ; "TRUE" ; "False" ])
                (2, Gen.elements [ "--" ; "--help" ; "--HELP" ; "--help=3" ])
                (2, Gen.elements [ "plain" ; "" ; "-x" ; "a=b" ; "---triple" ; "--unknown" ; "--unknown=1" ])
                (1, Gen.elements [ "„unicode“" ; " " ; "--=" ; "--=x" ; "=" ])
            ]

    [<Test>]
    let ``Scanning is lossless: unscan reconstructs argv exactly`` () =
        let cases =
            gen {
                let! sumBias = Gen.elements [ 0 ; 50 ]
                let! schema = genSchema sumBias
                let! hasSink = Gen.elements [ true ; false ]

                let! flagLike = Gen.elements [ ErasedFlagLikeBehaviour.Collect ; ErasedFlagLikeBehaviour.Reject ]

                let schema =
                    if hasSink then
                        { schema with
                            Positionals =
                                [
                                    {
                                        Id = 1000
                                        Forms = [ "rest" ]
                                        FlagLike = flagLike
                                        TypeDescription = "string"
                                        Help = None
                                    }
                                ]
                            Tree = ErasedTree.Product [ schema.Tree ; ErasedTree.PositionalLeaf 1000 ]
                        }
                    else
                        schema

                let! count = Gen.choose (0, 15)
                let! tokens = Gen.listOfLength count (genToken schema)
                return schema, tokens
            }

        let property (schema : ErasedSchema, tokens : string list) : unit =
            unscan (scan schema tokens) |> shouldEqual tokens

        let config = Config.QuickThrowOnFailure.WithMaxTest 2000
        Check.One (config, Prop.forAll (Arb.fromGen cases) property)

    [<Test>]
    let ``Appending a trailing separator only appends a Separator event`` () =
        let cases =
            gen {
                let! schema = genSchema 30
                let! count = Gen.choose (0, 12)
                let! tokens = Gen.listOfLength count (genToken schema)
                // Guarantee no "--" tokens by construction.
                let tokens = tokens |> List.map (fun t -> if t = "--" then "-" else t)
                return schema, tokens
            }

        let property (schema : ErasedSchema, tokens : string list) : unit =
            scan schema (tokens @ [ "--" ])
            |> shouldEqual (scan schema tokens @ [ ScanEvent.Separator ])

        let config = Config.QuickThrowOnFailure.WithMaxTest 1000
        Check.One (config, Prop.forAll (Arb.fromGen cases) property)

    // ----------------------------------------------------------------------------------------
    // Property: round trip. Render a valid command line for a random assignment of values to
    // leaves; scanning it must recover exactly the intended events.

    /// One renderable unit, together with the events it should produce.
    type private RenderUnit =
        {
            Tokens : string list
            Expected : ScanEvent list
        }

    /// Values which are safe as a space-separated value token (anything but the literal
    /// separator).
    let private genValue : Gen<string> =
        Gen.elements [ "3" ; "" ; "a=b" ; "true" ; "--other" ; "plain" ; "-1" ; "„x“" ; "--help" ]

    let private genOccurrenceUnit (leaf : ErasedLeaf) : Gen<RenderUnit> =
        gen {
            let! form = Gen.elements leaf.Forms
            let! upper = Gen.elements [ true ; false ]
            let form = (if upper then form.ToUpperInvariant () else form)

            match leaf.Arity with
            | ErasedArity.One ->
                let! value = genValue
                let! equalsForm = Gen.elements [ true ; false ]

                if equalsForm then
                    let source = "--" + form + "=" + value

                    return
                        {
                            Tokens = [ source ]
                            Expected =
                                [
                                    ScanEvent.Occurrence
                                        {
                                            LeafId = leaf.Id
                                            Value = Some value
                                            Negated = false
                                            Source = source
                                        }
                                ]
                        }
                else
                    return
                        {
                            Tokens = [ "--" + form ; value ]
                            Expected =
                                [
                                    ScanEvent.Occurrence
                                        {
                                            LeafId = leaf.Id
                                            Value = Some value
                                            Negated = false
                                            Source = "--" + form
                                        }
                                ]
                        }
            | ErasedArity.BoolLike ->
                let! negated =
                    if leaf.AcceptsNegation then
                        Gen.elements [ true ; false ]
                    else
                        Gen.constant false

                let key = (if negated then "--no-" + form else "--" + form)
                let! literal = Gen.elements [ "true" ; "false" ; "TRUE" ; "False" ]

                let! style = Gen.elements [ 0 ; 1 ; 2 ]

                match style with
                | 0 ->
                    // --key=literal
                    let source = key + "=" + literal

                    return
                        {
                            Tokens = [ source ]
                            Expected =
                                [
                                    ScanEvent.Occurrence
                                        {
                                            LeafId = leaf.Id
                                            Value = Some literal
                                            Negated = negated
                                            Source = source
                                        }
                                ]
                        }
                | 1 ->
                    // --key literal
                    return
                        {
                            Tokens = [ key ; literal ]
                            Expected =
                                [
                                    ScanEvent.Occurrence
                                        {
                                            LeafId = leaf.Id
                                            Value = Some literal
                                            Negated = negated
                                            Source = key
                                        }
                                ]
                        }
                | _ ->
                    // Bare --key: only valid if the next token is not a boolean literal; the
                    // renderer checks this and falls back to --key=literal if necessary.
                    return
                        {
                            Tokens = [ key ]
                            Expected =
                                [
                                    ScanEvent.Occurrence
                                        {
                                            LeafId = leaf.Id
                                            Value = None
                                            Negated = negated
                                            Source = key
                                        }
                                ]
                        }
        }

    let private genPositionalUnit : Gen<RenderUnit> =
        gen {
            let! value = Gen.elements [ "pos" ; "3" ; "a=b" ; "" ; "x y" ; "true" ]

            return
                {
                    Tokens = [ value ]
                    Expected = [ posEv value false PositionalForm.Bare [ 1000 ] ]
                }
        }

    /// Is this unit a bare boolean-like occurrence (which must not be followed by a boolean
    /// literal)?
    let private isBareBool (u : RenderUnit) : bool =
        match u.Tokens, u.Expected with
        | [ _ ], [ ScanEvent.Occurrence occurrence ] -> Option.isNone occurrence.Value
        | _, _ -> false

    let private startsWithBoolLiteral (u : RenderUnit) : bool =
        match u.Tokens with
        | first :: _ -> isBoolLiteral first
        | [] -> false

    [<Test>]
    let ``Round trip: a rendered command line scans to exactly the intended events`` () =
        let mutable bareBoolCount = 0
        let mutable totalUnits = 0

        let cases =
            gen {
                let! schema = genSchema 40

                let schema =
                    { schema with
                        Positionals =
                            [
                                {
                                    Id = 1000
                                    Forms = [ "rest" ]
                                    FlagLike = ErasedFlagLikeBehaviour.Reject
                                    TypeDescription = "string"
                                    Help = None
                                }
                            ]
                        Tree = ErasedTree.Product [ schema.Tree ; ErasedTree.PositionalLeaf 1000 ]
                    }

                // Any leaf may occur any number of times: the scanner does not enforce
                // requiredness or repetition; that is validation's job.
                let! occurrenceUnits =
                    schema.Leaves
                    |> List.map (fun leaf ->
                        gen {
                            let! count = Gen.frequency [ (2, Gen.constant 0) ; (3, Gen.choose (1, 3)) ]
                            return! Gen.listOfLength count (genOccurrenceUnit leaf)
                        }
                    )
                    |> Gen.sequenceToList

                let! positionalCount = Gen.choose (0, 4)
                let! positionalUnits = Gen.listOfLength positionalCount genPositionalUnit

                // Shuffle all units together.
                let units = List.concat occurrenceUnits @ positionalUnits
                let! keys = Gen.listOfLength (List.length units) (Gen.choose (0, 1_000_000))
                let units = List.zip keys units |> List.sortBy fst |> List.map snd

                // Optionally add post-separator positionals.
                let! withSeparator = Gen.elements [ true ; false ]
                let! postSepCount = (if withSeparator then Gen.choose (0, 3) else Gen.constant 0)

                let! postSep = Gen.listOfLength postSepCount (Gen.elements [ "tail" ; "--foo" ; "--" ; "true" ; "" ])

                return schema, units, withSeparator, postSep
            }

        let property
            (schema : ErasedSchema, units : RenderUnit list, withSeparator : bool, postSep : string list)
            : unit
            =
            // Render with lookahead: a bare boolean-like key followed by a boolean literal would
            // swallow it, so replace such renderings with the explicit form.
            let fixedUnits =
                let rec fix (units : RenderUnit list) : RenderUnit list =
                    match units with
                    | [] -> []
                    | u :: rest ->
                        let rest = fix rest

                        let nextIsBoolLiteral =
                            match rest with
                            | next :: _ -> startsWithBoolLiteral next
                            | [] ->
                                // Followed by the separator or end of input: bare form resolves
                                // as arity-0 in either case, so it is fine.
                                false

                        if isBareBool u && nextIsBoolLiteral then
                            match u.Expected with
                            | [ ScanEvent.Occurrence occurrence ] ->
                                let source = occurrence.Source + "=true"

                                {
                                    Tokens = [ source ]
                                    Expected =
                                        [
                                            ScanEvent.Occurrence
                                                { occurrence with
                                                    Value = Some "true"
                                                    Source = source
                                                }
                                        ]
                                }
                                :: rest
                            | _ -> u :: rest
                        else
                            u :: rest

                fix units

            totalUnits <- totalUnits + List.length fixedUnits

            bareBoolCount <- bareBoolCount + (fixedUnits |> List.filter isBareBool |> List.length)

            let tokens =
                (fixedUnits |> List.collect (fun u -> u.Tokens))
                @ (if withSeparator then "--" :: postSep else [])

            let expected =
                (fixedUnits |> List.collect (fun u -> u.Expected))
                @ (if withSeparator then
                       ScanEvent.Separator
                       :: (postSep |> List.map (fun t -> posEv t true PositionalForm.Bare [ 1000 ]))
                   else
                       [])

            scan schema tokens |> shouldEqual expected

        let config = Config.QuickThrowOnFailure.WithMaxTest 1000
        Check.One (config, Prop.forAll (Arb.fromGen cases) property)

        // The tricky regime is the bare boolean flag; make sure we are actually exercising it.
        bareBoolCount |> shouldBeGreaterThan 100
        totalUnits |> shouldBeGreaterThan 1000

    // ----------------------------------------------------------------------------------------
    // Well-formedness: the checked constructor which generated code must pass its schema
    // through before parsing. The selection semantics assume that every addressable `--token`
    // names at most one leaf *under the scanner's case-insensitive matching*; without that,
    // matchLeaf silently routes colliding tokens to whichever leaf is declared first, and for a
    // Sum schema that silently selects the wrong case.

    [<Test>]
    let ``A well-formed schema passes the checked constructor`` () =
        match WellFormedSchema.check motivating with
        | Ok wellFormed -> WellFormedSchema.schema wellFormed |> shouldEqual motivating
        | Error errors -> failwithf "unexpected schema errors: %O" errors

    [<Test>]
    let ``Forms which differ only by case collide, even across sum cases`` () =
        // The empirical counterexample from review: `foo` in one case and `FOO` in the other
        // passes a case-sensitive uniqueness check, but the scanner matches case-insensitively,
        // so `--FOO=3` routes to the first-declared leaf and silently selects *its* case.
        let schema =
            {
                Leaves =
                    [
                        leaf 0 "foo" ErasedArity.One ErasedRequirement.Required
                        leaf 1 "FOO" ErasedArity.One ErasedRequirement.Required
                    ]
                Tree = ErasedTree.Sum (0, [ "CaseA", ErasedTree.Leaf 0 ; "CaseB", ErasedTree.Leaf 1 ])
                Positionals = []
            }

        WellFormedSchema.errors schema
        |> shouldEqual
            [
                SchemaError.TokenCollision ("--foo", [ "argument '--foo'" ; "argument '--FOO'" ])
            ]

    [<Test>]
    let ``A form colliding with another leaf's negated form is rejected, whatever the casing`` () =
        let negatable =
            { leaf 0 "foo" ErasedArity.BoolLike ErasedRequirement.Required with
                AcceptsNegation = true
            }

        let schema =
            productSchema [ negatable ; leaf 1 "No-Foo" ErasedArity.One ErasedRequirement.Required ] []

        WellFormedSchema.errors schema
        |> shouldEqual
            [
                SchemaError.TokenCollision (
                    "--no-foo",
                    [ "the --no- form of argument '--foo'" ; "argument '--No-Foo'" ]
                )
            ]

    [<Test>]
    let ``No argument may claim the reserved help name, in any casing`` () =
        let schema =
            productSchema [ leaf 0 "HeLp" ErasedArity.One ErasedRequirement.Required ] []

        WellFormedSchema.errors schema
        |> shouldEqual
            [
                SchemaError.TokenCollision ("--HeLp", [ "argument '--HeLp'" ; "the built-in help flag" ])
            ]

    [<Test>]
    let ``The positional sink's forms collide with leaf forms case-insensitively`` () =
        let sink =
            {
                Id = 99
                Forms = [ "REST" ]
                FlagLike = ErasedFlagLikeBehaviour.Reject
                TypeDescription = "string"
                Help = None
            }

        let schema =
            productSchema [ leaf 0 "rest" ErasedArity.One ErasedRequirement.Required ] [ sink ]

        WellFormedSchema.errors schema
        |> shouldEqual
            [
                SchemaError.TokenCollision ("--rest", [ "argument '--rest'" ; "the positional-args sink '--REST'" ])
            ]

    [<Test>]
    let ``Every alias of the positional sink participates in collision checking`` () =
        // Not just the head form: the second alias here collides with a leaf.
        let sink =
            {
                Id = 99
                Forms = [ "rest" ; "TARGET" ]
                FlagLike = ErasedFlagLikeBehaviour.Reject
                TypeDescription = "string"
                Help = None
            }

        let schema =
            productSchema [ leaf 0 "target" ErasedArity.One ErasedRequirement.Required ] [ sink ]

        WellFormedSchema.errors schema
        |> shouldEqual
            [
                SchemaError.TokenCollision (
                    "--target",
                    [ "argument '--target'" ; "the positional-args sink '--TARGET'" ]
                )
            ]

    [<Test>]
    let ``A leaf whose own aliases collide is rejected`` () =
        let doubled =
            { leaf 0 "foo" ErasedArity.One ErasedRequirement.Required with
                Forms = [ "foo" ; "FOO" ]
            }

        WellFormedSchema.errors (productSchema [ doubled ] [])
        |> shouldEqual
            [
                SchemaError.TokenCollision ("--foo", [ "argument '--foo'" ; "argument '--FOO'" ])
            ]

    [<Test>]
    let ``Duplicate leaf ids are rejected`` () =
        let schema =
            {
                Leaves =
                    [
                        leaf 0 "a" ErasedArity.One ErasedRequirement.Required
                        leaf 0 "b" ErasedArity.One ErasedRequirement.Required
                    ]
                Tree = ErasedTree.Product [ ErasedTree.Leaf 0 ]
                Positionals = []
            }

        WellFormedSchema.errors schema |> shouldEqual [ SchemaError.DuplicateLeafId 0 ]

    [<Test>]
    let ``A leaf repeated in the tree is rejected`` () =
        let schema =
            { productSchema [ leaf 0 "a" ErasedArity.One ErasedRequirement.Required ] [] with
                Tree = ErasedTree.Product [ ErasedTree.Leaf 0 ; ErasedTree.Leaf 0 ]
            }

        WellFormedSchema.errors schema
        |> shouldEqual [ SchemaError.LeafRepeatedInTree 0 ]

    [<Test>]
    let ``Tree and leaf table must refer to the same leaves`` () =
        let schema =
            {
                Leaves =
                    [
                        leaf 0 "a" ErasedArity.One ErasedRequirement.Required
                        leaf 1 "b" ErasedArity.One ErasedRequirement.Required
                    ]
                Tree = ErasedTree.Product [ ErasedTree.Leaf 0 ; ErasedTree.Leaf 2 ]
                Positionals = []
            }

        WellFormedSchema.errors schema
        |> shouldEqual [ SchemaError.LeafNotInTable 2 ; SchemaError.LeafNotInTree 1 ]

    [<Test>]
    let ``Duplicate sum ids are rejected`` () =
        let schema =
            {
                Leaves =
                    [
                        leaf 0 "a" ErasedArity.One ErasedRequirement.Required
                        leaf 1 "b" ErasedArity.One ErasedRequirement.Required
                    ]
                Tree =
                    ErasedTree.Product
                        [
                            ErasedTree.Sum (0, [ "A", ErasedTree.Leaf 0 ])
                            ErasedTree.Sum (0, [ "B", ErasedTree.Leaf 1 ])
                        ]
                Positionals = []
            }

        WellFormedSchema.errors schema |> shouldEqual [ SchemaError.DuplicateSumId 0 ]

    [<Test>]
    let ``Negation requires a boolean-like leaf`` () =
        let bad =
            { leaf 0 "a" ErasedArity.One ErasedRequirement.Required with
                AcceptsNegation = true
            }

        WellFormedSchema.errors (productSchema [ bad ] [])
        |> shouldEqual [ SchemaError.NegationOnNonBool 0 ]

    [<Test>]
    let ``A leaf with no forms at all is rejected`` () =
        let bad =
            { leaf 0 "a" ErasedArity.One ErasedRequirement.Required with
                Forms = []
            }

        WellFormedSchema.errors (productSchema [ bad ] [])
        |> shouldEqual [ SchemaError.NoForms 0 ]

    [<Test>]
    let ``checkOrFail renders every defect`` () =
        let schema =
            {
                Leaves =
                    [
                        leaf 0 "foo" ErasedArity.One ErasedRequirement.Required
                        leaf 1 "FOO" ErasedArity.One ErasedRequirement.Required
                    ]
                Tree = ErasedTree.Sum (0, [ "CaseA", ErasedTree.Leaf 0 ; "CaseB", ErasedTree.Leaf 1 ])
                Positionals = []
            }

        let exc =
            Assert.Throws<exn> (fun () -> WellFormedSchema.checkOrFail schema |> ignore<WellFormedSchema>)

        exc.Message
        |> shouldEqual
            "Invalid argument parser definition:\nthe token '--foo' is claimed by: argument '--foo'; argument '--FOO' (argument names are matched case-insensitively)"

    [<Test>]
    let ``Every generated schema is well-formed`` () =
        // The property generators build schemas which satisfy the invariants by construction
        // (forms are "argN"/"altN"); this pins that the checked constructor has no false
        // positives across that whole family.
        let cases =
            gen {
                let! sumBias = Gen.elements [ 20 ; 50 ; 80 ]
                return! genSchema sumBias
            }

        let property (schema : ErasedSchema) : unit =
            WellFormedSchema.errors schema |> shouldEqual []

        let config = Config.QuickThrowOnFailure.WithMaxTest 500
        Check.One (config, Prop.forAll (Arb.fromGen cases) property)

    [<Test>]
    let ``Collision detection uses exactly the scanner's equality`` () =
        // "s" and "ſ" (long s) uppercase to the same string, but OrdinalIgnoreCase — the equality
        // the scanner matches keys with — considers them distinct. A coarser normalisation (e.g.
        // ToUpperInvariant keying) would falsely reject this schema even though the scanner
        // routes its tokens unambiguously.
        let schema =
            productSchema
                [
                    leaf 0 "s" ErasedArity.One ErasedRequirement.Required
                    leaf 1 "ſ" ErasedArity.One ErasedRequirement.Required
                ]
                []

        // The scanner really does distinguish them.
        scan schema [ "--s=1" ] |> shouldEqual [ occ 0 (Some "1") false "--s=1" ]
        scan schema [ "--ſ=1" ] |> shouldEqual [ occ 1 (Some "1") false "--ſ=1" ]

        WellFormedSchema.errors schema |> shouldEqual []

    [<Test>]
    let ``An empty form is rejected: its token would be the separator`` () =
        let bad =
            { leaf 0 "a" ErasedArity.One ErasedRequirement.Required with
                Forms = [ "" ]
            }

        WellFormedSchema.errors (productSchema [ bad ] [])
        |> shouldEqual [ SchemaError.EmptyForm "argument id 0" ]

    [<Test>]
    let ``A form containing an equals sign is rejected: the scanner splits at the first equals`` () =
        // A required argument under such a form would make its schema (or its union case)
        // permanently unsatisfiable, while still passing every name-collision check.
        let bad =
            { leaf 0 "a" ErasedArity.One ErasedRequirement.Required with
                Forms = [ "foo=bar" ]
            }

        WellFormedSchema.errors (productSchema [ bad ] [])
        |> shouldEqual [ SchemaError.FormContainsEquals ("argument id 0", "foo=bar") ]

        let badSink =
            {
                Id = 99
                Forms = [ "rest=stuff" ]
                FlagLike = ErasedFlagLikeBehaviour.Reject
                TypeDescription = "string"
                Help = None
            }

        WellFormedSchema.errors (productSchema [ leaf 0 "a" ErasedArity.One ErasedRequirement.Required ] [ badSink ])
        |> shouldEqual [ SchemaError.FormContainsEquals ("positional-args sink id 99", "rest=stuff") ]

    // ----------------------------------------------------------------------------------------
    // Well-formedness: positional sinks in the tree. The capacity rule (at most one sink per
    // complete interpretation), the global flag-like policy, and the sink table/tree
    // correspondence.

    let private sink' (id : int) (forms : string list) (flagLike : ErasedFlagLikeBehaviour) : ErasedPositional =
        {
            Id = id
            Forms = forms
            FlagLike = flagLike
            TypeDescription = "string"
            Help = None
        }

    [<Test>]
    let ``Two sinks in one product exceed positional capacity`` () =
        let schema =
            {
                Leaves = []
                Tree = ErasedTree.Product [ ErasedTree.PositionalLeaf 0 ; ErasedTree.PositionalLeaf 1 ]
                Positionals =
                    [
                        sink' 0 [ "rest" ] ErasedFlagLikeBehaviour.Reject
                        sink' 1 [ "files" ] ErasedFlagLikeBehaviour.Reject
                    ]
            }

        WellFormedSchema.errors schema
        |> shouldEqual [ SchemaError.PositionalCapacityExceeded 2 ]

    [<Test>]
    let ``Sinks in mutually exclusive cases are within capacity, and may share forms`` () =
        // (A × P) + (B × Q), both sinks addressable as --rest: the sinks can never both be
        // active, and a keyed --rest token has the same arity whichever wins, so this is fine.
        let schema =
            {
                Leaves =
                    [
                        leaf 0 "foo" ErasedArity.One ErasedRequirement.Required
                        leaf 1 "bar" ErasedArity.One ErasedRequirement.Required
                    ]
                Tree =
                    ErasedTree.Sum (
                        0,
                        [
                            "A", ErasedTree.Product [ ErasedTree.Leaf 0 ; ErasedTree.PositionalLeaf 0 ]
                            "B", ErasedTree.Product [ ErasedTree.Leaf 1 ; ErasedTree.PositionalLeaf 1 ]
                        ]
                    )
                Positionals =
                    [
                        sink' 0 [ "rest" ] ErasedFlagLikeBehaviour.Reject
                        sink' 1 [ "rest" ] ErasedFlagLikeBehaviour.Reject
                    ]
            }

        WellFormedSchema.errors schema |> shouldEqual []

    [<Test>]
    let ``A sink form colliding with a named form is still rejected, even across cases`` () =
        let schema =
            {
                Leaves =
                    [
                        leaf 0 "foo" ErasedArity.One ErasedRequirement.Required
                        leaf 1 "rest" ErasedArity.One ErasedRequirement.Required
                    ]
                Tree =
                    ErasedTree.Sum (
                        0,
                        [
                            "A", ErasedTree.Product [ ErasedTree.Leaf 0 ; ErasedTree.PositionalLeaf 0 ]
                            "B", ErasedTree.Leaf 1
                        ]
                    )
                Positionals = [ sink' 0 [ "REST" ] ErasedFlagLikeBehaviour.Reject ]
            }

        WellFormedSchema.errors schema
        |> shouldEqual
            [
                SchemaError.TokenCollision ("--rest", [ "argument '--rest'" ; "the positional-args sink '--REST'" ])
            ]

    [<Test>]
    let ``Sinks disagreeing on flag-like policy are rejected`` () =
        let schema =
            {
                Leaves =
                    [
                        leaf 0 "foo" ErasedArity.One ErasedRequirement.Required
                        leaf 1 "bar" ErasedArity.One ErasedRequirement.Required
                    ]
                Tree =
                    ErasedTree.Sum (
                        0,
                        [
                            "A", ErasedTree.Product [ ErasedTree.Leaf 0 ; ErasedTree.PositionalLeaf 0 ]
                            "B", ErasedTree.Product [ ErasedTree.Leaf 1 ; ErasedTree.PositionalLeaf 1 ]
                        ]
                    )
                Positionals =
                    [
                        sink' 0 [ "rest" ] ErasedFlagLikeBehaviour.Reject
                        sink' 1 [ "files" ] ErasedFlagLikeBehaviour.Collect
                    ]
            }

        WellFormedSchema.errors schema
        |> shouldEqual [ SchemaError.FlagLikePolicyDisagreement ]

    [<Test>]
    let ``Sink table and tree must correspond`` () =
        let schema =
            {
                Leaves = []
                Tree = ErasedTree.Product [ ErasedTree.PositionalLeaf 0 ]
                Positionals = [ sink' 1 [ "rest" ] ErasedFlagLikeBehaviour.Reject ]
            }

        WellFormedSchema.errors schema
        |> shouldEqual [ SchemaError.PositionalNotInTable 0 ; SchemaError.PositionalNotInTree 1 ]

    [<Test>]
    let ``Duplicate sink ids and repeated tree references are rejected`` () =
        let schema =
            {
                Leaves = []
                Tree = ErasedTree.Product [ ErasedTree.PositionalLeaf 0 ; ErasedTree.PositionalLeaf 0 ]
                Positionals =
                    [
                        sink' 0 [ "rest" ] ErasedFlagLikeBehaviour.Reject
                        sink' 0 [ "files" ] ErasedFlagLikeBehaviour.Reject
                    ]
            }

        WellFormedSchema.errors schema
        |> shouldEqual
            [
                SchemaError.DuplicatePositionalId 0
                SchemaError.PositionalRepeatedInTree 0
                SchemaError.PositionalCapacityExceeded 2
            ]

    [<Test>]
    let ``Selection reports the active sink for a sink in the selected interpretation`` () =
        let schema =
            {
                Leaves =
                    [
                        leaf 0 "foo" ErasedArity.One ErasedRequirement.Required
                        leaf 1 "bar" ErasedArity.One ErasedRequirement.Required
                    ]
                Tree =
                    ErasedTree.Product
                        [
                            ErasedTree.Sum (0, [ "A", ErasedTree.Leaf 0 ; "B", ErasedTree.Leaf 1 ])
                            ErasedTree.PositionalLeaf 7
                        ]
                Positionals = [ sink' 7 [ "rest" ] ErasedFlagLikeBehaviour.Reject ]
            }

        let selection = select schema (Map.ofList [ 0, "--foo=1" ])
        selection.Errors |> shouldEqual []
        selection.Choices |> shouldEqual (Map.ofList [ 0, 0 ])
        selection.ActivePositional |> shouldEqual (Some 7)

        // A sink inside an unselected case is not active.
        let schema =
            {
                Leaves =
                    [
                        leaf 0 "foo" ErasedArity.One ErasedRequirement.Required
                        leaf 1 "bar" ErasedArity.One ErasedRequirement.Required
                    ]
                Tree =
                    ErasedTree.Sum (
                        0,
                        [
                            "A", ErasedTree.Leaf 0
                            "B", ErasedTree.Product [ ErasedTree.Leaf 1 ; ErasedTree.PositionalLeaf 7 ]
                        ]
                    )
                Positionals = [ sink' 7 [ "rest" ] ErasedFlagLikeBehaviour.Reject ]
            }

        let selection = select schema (Map.ofList [ 0, "--foo=1" ])
        selection.Errors |> shouldEqual []
        selection.ActivePositional |> shouldEqual None

        let selection = select schema (Map.ofList [ 1, "--bar=1" ])
        selection.Errors |> shouldEqual []
        selection.ActivePositional |> shouldEqual (Some 7)

    // ----------------------------------------------------------------------------------------
    // Property: the runtime's compositional positional-capacity calculation agrees with the
    // exhaustive reference model of TestArgParserPositionalReference.

    [<Test>]
    let ``Runtime positional capacity agrees with the reference model`` () =
        let rec convert (tree : TestArgParserPositionalReference.RefTree) : ErasedTree =
            match tree with
            | TestArgParserPositionalReference.RefTree.Named leaf -> ErasedTree.Leaf leaf.Id
            | TestArgParserPositionalReference.RefTree.Positional p -> ErasedTree.PositionalLeaf p.Id
            | TestArgParserPositionalReference.RefTree.Product children ->
                ErasedTree.Product (children |> List.map convert)
            | TestArgParserPositionalReference.RefTree.Sum (sumId, cases) ->
                ErasedTree.Sum (sumId, cases |> List.map (fun (name, case) -> name, convert case))

        let cases =
            gen {
                let! sumBias = Gen.elements [ 20 ; 50 ; 80 ]
                return! TestArgParserPositionalReference.genTree sumBias
            }

        let property (tree : TestArgParserPositionalReference.RefTree) : unit =
            maxPositionals (convert tree)
            |> shouldEqual (TestArgParserPositionalReference.maxPositionals tree)

        let config = Config.QuickThrowOnFailure.WithMaxTest 1000
        Check.One (config, Prop.forAll (Arb.fromGen cases) property)

    // ----------------------------------------------------------------------------------------
    // Candidate sets: the spelling of a positional token determines which sinks could consume
    // it, and nothing else does — scanning stays independent of the eventual case selection.

    [<Test>]
    let ``Positional events carry the claimant sets of their spelling`` () =
        // Two sinks in mutually exclusive cases, sharing the form "rest"; only sink 11 also
        // answers to "files".
        let schema =
            {
                Leaves =
                    [
                        leaf 0 "foo" ErasedArity.One ErasedRequirement.Required
                        leaf 1 "bar" ErasedArity.One ErasedRequirement.Required
                    ]
                Tree =
                    ErasedTree.Sum (
                        0,
                        [
                            "A", ErasedTree.Product [ ErasedTree.Leaf 0 ; ErasedTree.PositionalLeaf 10 ]
                            "B", ErasedTree.Product [ ErasedTree.Leaf 1 ; ErasedTree.PositionalLeaf 11 ]
                        ]
                    )
                Positionals =
                    [
                        sink' 10 [ "rest" ] ErasedFlagLikeBehaviour.Reject
                        sink' 11 [ "rest" ; "files" ] ErasedFlagLikeBehaviour.Reject
                    ]
            }

        // A shared keyed form yields the union of its claimants.
        scan schema [ "--rest=1" ]
        |> shouldEqual [ posEv "1" false (PositionalForm.KeyEquals "--rest") [ 10 ; 11 ] ]

        // A form claimed by one sink yields that sink alone, case-insensitively.
        scan schema [ "--FILES" ; "2" ]
        |> shouldEqual [ posEv "2" false (PositionalForm.KeySpaced "--FILES") [ 11 ] ]

        // Bare tokens, and everything after the separator, name every sink.
        scan schema [ "bare" ]
        |> shouldEqual [ posEv "bare" false PositionalForm.Bare [ 10 ; 11 ] ]

        scan schema [ "--" ; "tail" ]
        |> shouldEqual [ ScanEvent.Separator ; posEv "tail" true PositionalForm.Bare [ 10 ; 11 ] ]

    [<Test>]
    let ``Every positional event of a one-sink schema names exactly that sink`` () =
        // The legacy-projection property: for the schemas the generator emits today (at most
        // one sink), the candidate metadata is fully determined, so erasing it recovers the
        // historical event log.
        let mutable positionalEvents = 0

        let cases =
            gen {
                let! sumBias = Gen.elements [ 0 ; 50 ]
                let! schema = genSchema sumBias

                let! flagLike = Gen.elements [ ErasedFlagLikeBehaviour.Collect ; ErasedFlagLikeBehaviour.Reject ]

                let schema =
                    { schema with
                        Positionals = [ sink' 1000 [ "rest" ] flagLike ]
                        Tree = ErasedTree.Product [ schema.Tree ; ErasedTree.PositionalLeaf 1000 ]
                    }

                let! count = Gen.choose (0, 15)
                let! tokens = Gen.listOfLength count (genToken schema)
                return schema, tokens
            }

        let property (schema : ErasedSchema, tokens : string list) : unit =
            for event in scan schema tokens do
                match event with
                | ScanEvent.Positional positional ->
                    positionalEvents <- positionalEvents + 1
                    positional.Candidates |> shouldEqual (Set.singleton 1000)
                | _ -> ()

        let config = Config.QuickThrowOnFailure.WithMaxTest 1000
        Check.One (config, Prop.forAll (Arb.fromGen cases) property)

        positionalEvents |> shouldBeGreaterThan 1000
