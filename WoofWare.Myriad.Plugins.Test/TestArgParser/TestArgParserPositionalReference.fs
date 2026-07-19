namespace WoofWare.Myriad.Plugins.Test

open System
open NUnit.Framework
open FsUnitTyped
open FsCheck
open FsCheck.FSharp

/// The exhaustive reference semantics for argument schemas extended with positional leaves.
///
/// This model is the *definition* against which the production resolver will be checked. A
/// schema tree may contain positional-stream consumers ("sinks") as leaves; a complete
/// interpretation of the tree (one case chosen for every reachable Sum) contains at most one
/// of them (argv holds a single positional stream, and two unbounded list consumers would
/// admit no canonical partition of it); and a scanned positional token carries the set of
/// sinks it could belong to, remaining unresolved until structural case selection has
/// finished. Acceptance is deliberately conversion-free: whether a token parses as the
/// sink's element type can never influence which interpretation is chosen.
module TestArgParserPositionalReference =

    [<RequireQualifiedAccess>]
    type RefRequirement =
        | Required
        | Optional

    /// A named argument, erased to exactly what acceptance needs. (The runtime's HasDefault
    /// behaves like Optional for acceptance purposes: an absent defaulted argument does not
    /// fail the parse.)
    type RefNamedLeaf =
        {
            Id : int
            Requirement : RefRequirement
        }

    /// A positional-stream consumer. Forms are the `--key` spellings which address this sink
    /// specifically; two sinks in mutually exclusive cases may share a form.
    type RefPositionalLeaf =
        {
            Id : int
            Forms : string list
        }

    /// The core algebra: named leaves, positional leaves, products and exclusive sums.
    [<RequireQualifiedAccess>]
    type RefTree =
        | Named of RefNamedLeaf
        | Positional of RefPositionalLeaf
        | Product of RefTree list
        | Sum of sumId : int * cases : (string * RefTree) list

    /// One complete interpretation of a tree: a case chosen for every reachable Sum.
    type Interpretation =
        {
            Choices : Map<int, int>
            Named : Set<int>
            RequiredNamed : Set<int>
            /// The active positional leaves. The linearity rule says a well-formed tree
            /// admits at most one per interpretation; expansion records them all so that the
            /// rule itself is testable.
            Positionals : Set<int>
        }

    /// How a positional token was spelled on the command line, which determines its
    /// candidate sinks.
    [<RequireQualifiedAccess>]
    type RefSpelling =
        /// A bare token (including everything after `--`): any sink could consume it.
        | Bare
        /// `--form=value` / `--form value`: only the sinks claiming this form could.
        | Keyed of form : string

    /// A structural input: which named leaves were observed, and the positional events in
    /// order. (Order does not affect acceptance; it is the candidate sets that matter.)
    type RefInput =
        {
            ObservedNamed : Set<int>
            PositionalEvents : RefSpelling list
        }

    [<RequireQualifiedAccess>]
    type RefOutcome =
        | Unique of Interpretation
        | NoInterpretation
        | Ambiguous of Interpretation list

    // ----------------------------------------------------------------------------------------
    // The model itself.

    /// The compositional positional-capacity calculation: the maximum number of positional
    /// leaves any complete interpretation can contain.
    let rec maxPositionals (tree : RefTree) : int =
        match tree with
        | RefTree.Named _ -> 0
        | RefTree.Positional _ -> 1
        | RefTree.Product children -> children |> List.sumBy maxPositionals
        | RefTree.Sum (_, cases) ->
            match cases with
            | [] -> 0
            | cases -> cases |> List.map (fun (_, case) -> maxPositionals case) |> List.max

    /// All positional leaves in the tree, wherever they sit.
    let rec positionalLeaves (tree : RefTree) : RefPositionalLeaf list =
        match tree with
        | RefTree.Named _ -> []
        | RefTree.Positional p -> [ p ]
        | RefTree.Product children -> children |> List.collect positionalLeaves
        | RefTree.Sum (_, cases) -> cases |> List.collect (fun (_, case) -> positionalLeaves case)

    let private emptyInterpretation : Interpretation =
        {
            Choices = Map.empty
            Named = Set.empty
            RequiredNamed = Set.empty
            Positionals = Set.empty
        }

    let private combine (a : Interpretation) (b : Interpretation) : Interpretation =
        {
            Choices = (a.Choices, b.Choices) ||> Map.fold (fun m k v -> Map.add k v m)
            Named = Set.union a.Named b.Named
            RequiredNamed = Set.union a.RequiredNamed b.RequiredNamed
            Positionals = Set.union a.Positionals b.Positionals
        }

    /// Expand a tree into every complete interpretation.
    let rec interpretations (tree : RefTree) : Interpretation list =
        match tree with
        | RefTree.Named leaf ->
            [
                { emptyInterpretation with
                    Named = Set.singleton leaf.Id
                    RequiredNamed =
                        match leaf.Requirement with
                        | RefRequirement.Required -> Set.singleton leaf.Id
                        | RefRequirement.Optional -> Set.empty
                }
            ]
        | RefTree.Positional p ->
            [
                { emptyInterpretation with
                    Positionals = Set.singleton p.Id
                }
            ]
        | RefTree.Product children ->
            ([ emptyInterpretation ], children)
            ||> List.fold (fun accs child ->
                let childInterps = interpretations child
                accs |> List.collect (fun acc -> childInterps |> List.map (combine acc))
            )
        | RefTree.Sum (sumId, cases) ->
            cases
            |> List.indexed
            |> List.collect (fun (index, (_, case)) ->
                interpretations case
                |> List.map (fun interp ->
                    { interp with
                        Choices = Map.add sumId index interp.Choices
                    }
                )
            )

    /// The candidate sinks for one positional event, given the tree's sinks. Keyed forms
    /// match under the scanner's own equality (OrdinalIgnoreCase): `--REST=x` addresses a
    /// sink declared as `rest`.
    let candidates (sinks : RefPositionalLeaf list) (spelling : RefSpelling) : Set<int> =
        match spelling with
        | RefSpelling.Bare -> sinks |> List.map (fun p -> p.Id) |> Set.ofList
        | RefSpelling.Keyed form ->
            sinks
            |> List.filter (fun p ->
                p.Forms
                |> List.exists (fun f -> String.Equals (f, form, StringComparison.OrdinalIgnoreCase))
            )
            |> List.map (fun p -> p.Id)
            |> Set.ofList

    /// The acceptance predicate: does this interpretation structurally accept this input?
    ///
    /// - every observed named leaf belongs to the interpretation;
    /// - every required named leaf of the interpretation was observed;
    /// - if any positional events exist, the interpretation has a positional leaf and it is
    ///   a candidate of every event.
    ///
    /// Conversion is deliberately absent: whether values parse never features.
    let accepts (sinks : RefPositionalLeaf list) (interp : Interpretation) (input : RefInput) : bool =
        Set.isSubset input.ObservedNamed interp.Named
        && Set.isSubset interp.RequiredNamed input.ObservedNamed
        && (List.isEmpty input.PositionalEvents
            || (
                match Set.toList interp.Positionals with
                | [ p ] ->
                    input.PositionalEvents
                    |> List.forall (fun spelling -> Set.contains p (candidates sinks spelling))
                | [] -> false
                | _ -> failwith "linearity violated: an interpretation held two positional leaves"
            ))

    /// The reference semantics: the parse is structurally successful exactly when one
    /// interpretation accepts.
    let exhaustiveSelect (tree : RefTree) (input : RefInput) : RefOutcome =
        let sinks = positionalLeaves tree

        match interpretations tree |> List.filter (fun interp -> accepts sinks interp input) with
        | [ interp ] -> RefOutcome.Unique interp
        | [] -> RefOutcome.NoInterpretation
        | several -> RefOutcome.Ambiguous several

    /// Can this subtree be satisfied with no *named* observations? (Positional leaves need
    /// nothing: an empty stream is fine.) This is the per-sum condition the generator
    /// enforces so that an empty command line, and by the lemma below any bare-only input,
    /// selects at most one case.
    let rec emptySatisfiable (tree : RefTree) : bool =
        match tree with
        | RefTree.Named leaf ->
            match leaf.Requirement with
            | RefRequirement.Required -> false
            | RefRequirement.Optional -> true
        | RefTree.Positional _ -> true
        | RefTree.Product children -> children |> List.forall emptySatisfiable
        | RefTree.Sum (_, cases) -> cases |> List.exists (fun (_, case) -> emptySatisfiable case)

    /// Does every Sum node have at most one empty-satisfiable case?
    let rec sumsAreUnambiguous (tree : RefTree) : bool =
        match tree with
        | RefTree.Named _
        | RefTree.Positional _ -> true
        | RefTree.Product children -> children |> List.forall sumsAreUnambiguous
        | RefTree.Sum (_, cases) ->
            (cases |> List.forall (fun (_, case) -> sumsAreUnambiguous case))
            && (cases |> List.filter (fun (_, case) -> emptySatisfiable case) |> List.length)
               <= 1

    // ----------------------------------------------------------------------------------------
    // Generators. Trees are generated valid by construction (positional budget 0 or 1,
    // threaded so that a Product gives its budget to one child and a Sum to every case), and
    // the capacity property below cross-checks the construction against the compositional
    // calculation.

    let private formPool = [ "rest" ; "files" ; "extra" ]

    let private genForms : Gen<string list> =
        gen {
            let! count = Gen.frequency [ (4, Gen.constant 1) ; (1, Gen.constant 2) ]
            let! forms = Gen.elements formPool |> Gen.listOfLength count
            return List.distinct forms
        }

    /// A tree over exactly the given named-leaf ids, with a positional leaf permitted (not
    /// required) exactly when `budget` is true. Positional ids are placeholders, renumbered
    /// by genTree.
    let rec private genTreeOver (sumBias : int) (budget : bool) (ids : int list) : Gen<RefTree> =
        match ids, budget with
        | [], false -> Gen.constant (RefTree.Product [])
        | [], true ->
            gen {
                let! forms = genForms
                let! makeSink = Gen.frequency [ (3, Gen.constant true) ; (1, Gen.constant false) ]

                if makeSink then
                    return
                        RefTree.Positional
                            {
                                Id = 0
                                Forms = forms
                            }
                else
                    return RefTree.Product []
            }
        | [ id ], budget ->
            gen {
                let! requirement = Gen.elements [ RefRequirement.Required ; RefRequirement.Optional ]

                let named =
                    RefTree.Named
                        {
                            Id = id
                            Requirement = requirement
                        }

                if budget then
                    let! sink = genTreeOver sumBias true []

                    match sink with
                    | RefTree.Product [] -> return named
                    | sink -> return RefTree.Product [ named ; sink ]
                else
                    return named
            }
        | ids, budget ->
            gen {
                let n = List.length ids
                let! groupCount = Gen.choose (2, min 4 n)

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

                let! isSum = Gen.frequency [ (sumBias, Gen.constant true) ; (100 - sumBias, Gen.constant false) ]

                if isSum then
                    // Every case of a sum inherits the whole budget: at most one case is live.
                    let! children = groups |> List.map (genTreeOver sumBias budget) |> Gen.sequenceToList
                    let cases = children |> List.mapi (fun i child -> sprintf "Case%i" i, child)
                    return RefTree.Sum (0, cases)
                else
                    // A product's budget goes to exactly one child.
                    let! luckyChild = Gen.choose (0, List.length groups - 1)

                    let! children =
                        groups
                        |> List.mapi (fun i group -> genTreeOver sumBias (budget && i = luckyChild) group)
                        |> Gen.sequenceToList

                    return RefTree.Product children
            }

    /// Give every Sum a distinct id and every positional leaf a distinct id (ids at 1000+ so
    /// they never collide with named ids).
    let private renumber (tree : RefTree) : RefTree =
        let mutable nextSum = 0
        let mutable nextPos = 1000

        let rec go (tree : RefTree) : RefTree =
            match tree with
            | RefTree.Named _ -> tree
            | RefTree.Positional p ->
                let id = nextPos
                nextPos <- nextPos + 1

                RefTree.Positional
                    { p with
                        Id = id
                    }
            | RefTree.Product children -> RefTree.Product (children |> List.map go)
            | RefTree.Sum (_, cases) ->
                let id = nextSum
                nextSum <- nextSum + 1
                RefTree.Sum (id, cases |> List.map (fun (name, case) -> name, go case))

        go tree

    /// Like renumber, but leaves positional ids alone: for trees whose sinks were placed by
    /// hand. The numbering is DFS order, so two trees sharing a sub-structure prefix agree
    /// on the ids of the sums within it.
    let private renumberSumsOnly (tree : RefTree) : RefTree =
        let mutable next = 0

        let rec go (tree : RefTree) : RefTree =
            match tree with
            | RefTree.Named _
            | RefTree.Positional _ -> tree
            | RefTree.Product children -> RefTree.Product (children |> List.map go)
            | RefTree.Sum (_, cases) ->
                let id = next
                next <- next + 1
                RefTree.Sum (id, cases |> List.map (fun (name, case) -> name, go case))

        go tree

    let genTree (sumBias : int) : Gen<RefTree> =
        gen {
            let! namedCount = Gen.choose (1, 8)
            let! budget = Gen.frequency [ (3, Gen.constant true) ; (1, Gen.constant false) ]
            let! tree = genTreeOver sumBias budget (List.init namedCount id)
            return renumber tree
        }

    /// An input biased towards nearly-valid: walk the tree choosing one case per sum, keep
    /// that alternative's required named leaves (dropping each with a small probability),
    /// add its optional leaves with some probability, add noise leaves from outside, and
    /// spell positional events either bare or keyed (keyed forms biased towards the chosen
    /// alternative's sink where it has one).
    let genInput (dropPct : int) (optionalPct : int) (noisePct : int) (tree : RefTree) : Gen<RefInput> =
        gen {
            let sinks = positionalLeaves tree

            let rec pickAlternative (tree : RefTree) : Gen<Interpretation> =
                match tree with
                | RefTree.Named leaf -> Gen.constant (List.exactlyOne (interpretations (RefTree.Named leaf)))
                | RefTree.Positional p -> Gen.constant (List.exactlyOne (interpretations (RefTree.Positional p)))
                | RefTree.Product children ->
                    gen {
                        let! parts = children |> List.map pickAlternative |> Gen.sequenceToList
                        return List.fold combine emptyInterpretation parts
                    }
                | RefTree.Sum (sumId, cases) ->
                    gen {
                        let! index = Gen.choose (0, List.length cases - 1)
                        let! interp = pickAlternative (snd (List.item index cases))

                        return
                            { interp with
                                Choices = Map.add sumId index interp.Choices
                            }
                    }

            let! alternative = pickAlternative tree

            let! kept =
                alternative.Named
                |> Set.toList
                |> List.map (fun id ->
                    gen {
                        let! roll = Gen.choose (1, 100)

                        let keep =
                            if Set.contains id alternative.RequiredNamed then
                                roll > dropPct
                            else
                                roll <= optionalPct

                        return (if keep then Some id else None)
                    }
                )
                |> Gen.sequenceToList

            let allNamedIds =
                let rec go (tree : RefTree) : int list =
                    match tree with
                    | RefTree.Named leaf -> [ leaf.Id ]
                    | RefTree.Positional _ -> []
                    | RefTree.Product children -> children |> List.collect go
                    | RefTree.Sum (_, cases) -> cases |> List.collect (fun (_, case) -> go case)

                go tree

            let! noise =
                allNamedIds
                |> List.map (fun id ->
                    gen {
                        if Set.contains id alternative.Named then
                            return None
                        else
                            let! roll = Gen.choose (1, 100)
                            return (if roll <= noisePct then Some id else None)
                    }
                )
                |> Gen.sequenceToList

            let! eventCount = Gen.frequency [ (2, Gen.constant 0) ; (3, Gen.choose (1, 4)) ]

            let ownForms =
                alternative.Positionals
                |> Set.toList
                |> List.collect (fun p -> (sinks |> List.find (fun s -> s.Id = p)).Forms)

            let! events =
                gen {
                    let! spellKeyed = Gen.frequency [ (1, Gen.constant true) ; (2, Gen.constant false) ]

                    if spellKeyed then
                        let! form =
                            match ownForms with
                            | [] -> Gen.elements formPool
                            | ownForms -> Gen.frequency [ (3, Gen.elements ownForms) ; (1, Gen.elements formPool) ]

                        // The scanner matches keys case-insensitively; the model must too.
                        let! upper = Gen.frequency [ (1, Gen.constant true) ; (3, Gen.constant false) ]
                        return RefSpelling.Keyed (if upper then form.ToUpperInvariant () else form)
                    else
                        return RefSpelling.Bare
                }
                |> Gen.listOfLength eventCount

            return
                {
                    ObservedNamed = Set.ofList (List.choose id kept @ List.choose id noise)
                    PositionalEvents = events
                }
        }

    // ----------------------------------------------------------------------------------------
    // Properties: the algebra.

    [<Test>]
    let ``Compositional positional capacity agrees with exhaustive expansion`` () =
        // On generated (budget ≤ 1) trees the agreement pins the linearity rule; gluing two
        // generated trees into a product also exercises capacities above one, where the
        // compositional sum must still agree with the expansion.
        let mutable overCapacityCount = 0

        let cases =
            gen {
                let! sumBias = Gen.elements [ 20 ; 50 ; 80 ]
                let! t1 = genTree sumBias
                let! glue = Gen.elements [ true ; false ]

                if glue then
                    let! t2 = genTree sumBias

                    // Rename so that ids stay unique across the two halves.
                    let rec bump (tree : RefTree) : RefTree =
                        match tree with
                        | RefTree.Named leaf ->
                            RefTree.Named
                                { leaf with
                                    Id = leaf.Id + 100
                                }
                        | RefTree.Positional p ->
                            RefTree.Positional
                                { p with
                                    Id = p.Id + 100
                                }
                        | RefTree.Product children -> RefTree.Product (children |> List.map bump)
                        | RefTree.Sum (sumId, cases) ->
                            RefTree.Sum (sumId + 100, cases |> List.map (fun (name, case) -> name, bump case))

                    return RefTree.Product [ t1 ; bump t2 ]
                else
                    return t1
            }

        let property (tree : RefTree) : unit =
            let expansionMax =
                match interpretations tree with
                | [] -> failwith "a tree always has at least one interpretation"
                | interps -> interps |> List.map (fun i -> Set.count i.Positionals) |> List.max

            maxPositionals tree |> shouldEqual expansionMax

            if expansionMax > 1 then
                overCapacityCount <- overCapacityCount + 1

        let config = Config.QuickThrowOnFailure.WithMaxTest 1000
        Check.One (config, Prop.forAll (Arb.fromGen cases) property)

        // The glued regime must actually produce over-capacity trees, or the agreement is
        // only being tested at 0 and 1.
        overCapacityCount |> shouldBeGreaterThan 50

    [<Test>]
    let ``Every interpretation of a generated tree contains at most one positional leaf`` () =
        let cases =
            gen {
                let! sumBias = Gen.elements [ 20 ; 50 ; 80 ]
                return! genTree sumBias
            }

        let property (tree : RefTree) : unit =
            maxPositionals tree |> shouldBeSmallerThan 2

            for interp in interpretations tree do
                Set.count interp.Positionals |> shouldBeSmallerThan 2

        let config = Config.QuickThrowOnFailure.WithMaxTest 1000
        Check.One (config, Prop.forAll (Arb.fromGen cases) property)

    [<Test>]
    let ``Acceptance is invariant under product identity and flattening`` () =
        // Product [t] means the same as t, and Product [a; Product [b; c]] the same as
        // Product [a; b; c]: the interpretations are equal as data, so outcomes coincide on
        // every input.
        let cases =
            gen {
                let! sumBias = Gen.elements [ 20 ; 50 ]
                let! tree = genTree sumBias
                let! input = genInput 20 50 20 tree
                return tree, input
            }

        let property (tree : RefTree, input : RefInput) : unit =
            exhaustiveSelect (RefTree.Product [ tree ]) input
            |> shouldEqual (exhaustiveSelect tree input)

            match tree with
            | RefTree.Product (first :: rest) when not (List.isEmpty rest) ->
                exhaustiveSelect (RefTree.Product [ first ; RefTree.Product rest ]) input
                |> shouldEqual (exhaustiveSelect tree input)
            | _ -> ()

        let config = Config.QuickThrowOnFailure.WithMaxTest 1000
        Check.One (config, Prop.forAll (Arb.fromGen cases) property)

    [<Test>]
    let ``Factorization: a common sink beside a sum accepts the same inputs as per-case sinks`` () =
        // (A + B) × P and (A × P₁) + (B × P₂), with P₁ and P₂ spelt like P, accept exactly
        // the same structural inputs, choosing the same case; only the identity of the
        // active sink differs.
        let mutable acceptCount = 0

        let cases =
            gen {
                let! sumBias = Gen.elements [ 0 ; 40 ]
                let! aCount = Gen.choose (1, 4)
                let! bCount = Gen.choose (1, 4)
                let! caseA = genTreeOver sumBias false (List.init aCount id)
                let! caseB = genTreeOver sumBias false (List.init bCount (fun i -> 100 + i))
                let! forms = genForms

                let sink (id : int) : RefTree =
                    RefTree.Positional
                        {
                            Id = id
                            Forms = forms
                        }

                let common =
                    RefTree.Product [ RefTree.Sum (0, [ "A", caseA ; "B", caseB ]) ; sink 1000 ]
                    |> renumberSumsOnly

                let factored =
                    RefTree.Sum (
                        0,
                        [
                            "A", RefTree.Product [ caseA ; sink 1000 ]
                            "B", RefTree.Product [ caseB ; sink 1001 ]
                        ]
                    )
                    |> renumberSumsOnly

                let! input = genInput 20 50 20 common
                return common, factored, input
            }

        let property (common : RefTree, factored : RefTree, input : RefInput) : unit =
            let observable (outcome : RefOutcome) : Choice<Map<int, int> * Set<int>, unit, int> =
                match outcome with
                | RefOutcome.Unique interp -> Choice1Of3 (interp.Choices, interp.Named)
                | RefOutcome.NoInterpretation -> Choice2Of3 ()
                | RefOutcome.Ambiguous interps -> Choice3Of3 (List.length interps)

            let commonOutcome = exhaustiveSelect common input
            let factoredOutcome = exhaustiveSelect factored input

            observable factoredOutcome |> shouldEqual (observable commonOutcome)

            match commonOutcome with
            | RefOutcome.Unique _ -> acceptCount <- acceptCount + 1
            | _ -> ()

        let config = Config.QuickThrowOnFailure.WithMaxTest 2000
        Check.One (config, Prop.forAll (Arb.fromGen cases) property)

        // The law is only interesting if some inputs are actually accepted.
        acceptCount |> shouldBeGreaterThan 200

    [<Test>]
    let ``Bare-only inputs are never ambiguous when every sum passes the empty-ambiguity check`` () =
        // The lemma which makes the generation-time check sufficient: bare tokens name every
        // sink, so they cannot discriminate between cases; if two interpretations both
        // accepted a bare-only input, both their cases (at the outermost sum where they
        // differ) would have to be satisfiable with no named observations — exactly what the
        // per-sum check forbids. Keyed events are excluded: a keyed form genuinely can
        // discriminate, which is by design.
        let mutable checkedTrees = 0
        let mutable inputsWithEvents = 0

        let cases =
            gen {
                let! sumBias = Gen.elements [ 30 ; 60 ; 90 ]
                let! tree = genTree sumBias
                let! rawInput = genInput 30 40 20 tree
                let bareOnly = rawInput.PositionalEvents |> List.map (fun _ -> RefSpelling.Bare)

                return
                    tree,
                    { rawInput with
                        PositionalEvents = bareOnly
                    }
            }

        let property (tree : RefTree, input : RefInput) : unit =
            if sumsAreUnambiguous tree then
                checkedTrees <- checkedTrees + 1

                if not (List.isEmpty input.PositionalEvents) then
                    inputsWithEvents <- inputsWithEvents + 1

                match exhaustiveSelect tree input with
                | RefOutcome.Ambiguous _ -> failwithf "ambiguous outcome for %A on %A" tree input
                | RefOutcome.Unique _
                | RefOutcome.NoInterpretation -> ()

        let config = Config.QuickThrowOnFailure.WithMaxTest 2000
        Check.One (config, Prop.forAll (Arb.fromGen cases) property)

        checkedTrees |> shouldBeGreaterThan 500
        inputsWithEvents |> shouldBeGreaterThan 200

    [<Test>]
    let ``The generator explores every outcome regime`` () =
        let mutable unique = 0
        let mutable noInterp = 0
        let mutable ambiguous = 0
        let mutable sharedSinkAccepts = 0
        let mutable positionalFree = 0

        let cases =
            gen {
                let! sumBias = Gen.elements [ 20 ; 50 ; 80 ]
                let! tree = genTree sumBias
                let! dropPct = Gen.elements [ 0 ; 0 ; 20 ]
                let! optionalPct = Gen.elements [ 0 ; 50 ; 100 ]
                let! noisePct = Gen.elements [ 0 ; 10 ; 40 ]
                let! input = genInput dropPct optionalPct noisePct tree
                return tree, input
            }

        let property (tree : RefTree, input : RefInput) : unit =
            let sinks = positionalLeaves tree

            let sharesForm =
                sinks
                |> List.exists (fun a ->
                    sinks
                    |> List.exists (fun b ->
                        a.Id <> b.Id && (a.Forms |> List.exists (fun f -> b.Forms |> List.contains f))
                    )
                )

            match exhaustiveSelect tree input with
            | RefOutcome.Unique interp ->
                unique <- unique + 1

                if List.isEmpty sinks then
                    positionalFree <- positionalFree + 1

                if sharesForm && not (Set.isEmpty interp.Positionals) then
                    sharedSinkAccepts <- sharedSinkAccepts + 1
            | RefOutcome.NoInterpretation -> noInterp <- noInterp + 1
            | RefOutcome.Ambiguous _ -> ambiguous <- ambiguous + 1

        let config = Config.QuickThrowOnFailure.WithMaxTest 3000
        Check.One (config, Prop.forAll (Arb.fromGen cases) property)

        unique |> shouldBeGreaterThan 300
        noInterp |> shouldBeGreaterThan 200
        ambiguous |> shouldBeGreaterThan 20
        sharedSinkAccepts |> shouldBeGreaterThan 20
        positionalFree |> shouldBeGreaterThan 50

    // ----------------------------------------------------------------------------------------
    // Hand examples: the motivating shapes, pinned concretely.

    let private req (id : int) : RefTree =
        RefTree.Named
            {
                Id = id
                Requirement = RefRequirement.Required
            }

    let private opt (id : int) : RefTree =
        RefTree.Named
            {
                Id = id
                Requirement = RefRequirement.Optional
            }

    let private sink (id : int) (forms : string list) : RefTree =
        RefTree.Positional
            {
                Id = id
                Forms = forms
            }

    /// (Foo × P₁) + (Bar × P₂), both sinks spelt --rest: the plan §8 example.
    let private perCaseSinks : RefTree =
        RefTree.Sum (
            0,
            [
                "Foo", RefTree.Product [ req 0 ; sink 1000 [ "rest" ] ]
                "Bar", RefTree.Product [ req 1 ; sink 1001 [ "rest" ] ]
            ]
        )

    let private input (named : int list) (events : RefSpelling list) : RefInput =
        {
            ObservedNamed = Set.ofList named
            PositionalEvents = events
        }

    [<Test>]
    let ``Per-case sinks: a named discriminator selects the case and its sink`` () =
        match exhaustiveSelect perCaseSinks (input [ 0 ] [ RefSpelling.Bare ; RefSpelling.Bare ]) with
        | RefOutcome.Unique interp ->
            interp.Choices |> shouldEqual (Map.ofList [ 0, 0 ])
            interp.Positionals |> shouldEqual (Set.singleton 1000)
        | other -> failwithf "unexpected outcome: %A" other

        match exhaustiveSelect perCaseSinks (input [ 1 ] [ RefSpelling.Bare ]) with
        | RefOutcome.Unique interp ->
            interp.Choices |> shouldEqual (Map.ofList [ 0, 1 ])
            interp.Positionals |> shouldEqual (Set.singleton 1001)
        | other -> failwithf "unexpected outcome: %A" other

    [<Test>]
    let ``Per-case sinks: the shared --rest form stays neutral`` () =
        // --rest names both sinks, so it does not select; the named discriminator does.
        match exhaustiveSelect perCaseSinks (input [ 0 ] [ RefSpelling.Keyed "rest" ]) with
        | RefOutcome.Unique interp -> interp.Choices |> shouldEqual (Map.ofList [ 0, 0 ])
        | other -> failwithf "unexpected outcome: %A" other

        // Keys match case-insensitively, like everything the scanner recognises.
        match exhaustiveSelect perCaseSinks (input [ 1 ] [ RefSpelling.Keyed "REST" ]) with
        | RefOutcome.Unique interp -> interp.Choices |> shouldEqual (Map.ofList [ 0, 1 ])
        | other -> failwithf "unexpected outcome: %A" other

    [<Test>]
    let ``Per-case sinks: positional tokens alone select nothing`` () =
        // Both cases have a required named leaf, so no interpretation accepts. Conversion is
        // not part of the model at all: parseability could not have broken this tie.
        exhaustiveSelect perCaseSinks (input [] [ RefSpelling.Bare ; RefSpelling.Bare ])
        |> shouldEqual RefOutcome.NoInterpretation

    [<Test>]
    let ``Per-case sinks: a distinctive keyed form is itself a structural discriminator`` () =
        let tree =
            RefTree.Sum (
                0,
                [
                    "Foo", RefTree.Product [ opt 0 ; sink 1000 [ "files" ] ]
                    "Bar", RefTree.Product [ req 1 ; sink 1001 [ "nums" ] ]
                ]
            )

        // Nothing but --files=x: only Foo's interpretation can consume the event.
        match exhaustiveSelect tree (input [] [ RefSpelling.Keyed "files" ]) with
        | RefOutcome.Unique interp ->
            interp.Choices |> shouldEqual (Map.ofList [ 0, 0 ])
            interp.Positionals |> shouldEqual (Set.singleton 1000)
        | other -> failwithf "unexpected outcome: %A" other

        // A keyed form belonging to the case the named observations exclude: no acceptance.
        exhaustiveSelect tree (input [ 1 ] [ RefSpelling.Keyed "files" ])
        |> shouldEqual RefOutcome.NoInterpretation

    [<Test>]
    let ``A common sink beside the sum accepts what either case accepts`` () =
        let tree =
            RefTree.Product [ RefTree.Sum (0, [ "Foo", req 0 ; "Bar", req 1 ]) ; sink 1000 [ "rest" ] ]

        match exhaustiveSelect tree (input [ 0 ] [ RefSpelling.Bare ]) with
        | RefOutcome.Unique interp ->
            interp.Choices |> shouldEqual (Map.ofList [ 0, 0 ])
            interp.Positionals |> shouldEqual (Set.singleton 1000)
        | other -> failwithf "unexpected outcome: %A" other

        exhaustiveSelect tree (input [] [ RefSpelling.Bare ])
        |> shouldEqual RefOutcome.NoInterpretation

    [<Test>]
    let ``A product of two sinks is over capacity`` () =
        // The linearity rule: argv has one positional stream, so P × Q is rejected by
        // capacity while (A × P) + (B × Q) is fine. This is what schema validation will
        // enforce; the model just states the arithmetic.
        maxPositionals (RefTree.Product [ sink 1000 [ "rest" ] ; sink 1001 [ "files" ] ])
        |> shouldEqual 2

        maxPositionals perCaseSinks |> shouldEqual 1
