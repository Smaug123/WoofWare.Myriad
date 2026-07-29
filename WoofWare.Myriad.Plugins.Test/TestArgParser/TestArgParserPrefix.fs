namespace WoofWare.Myriad.Plugins.Test

open System
open NUnit.Framework
open FsUnitTyped
open FsCheck
open FsCheck.FSharp
open ConsumePlugin

/// [<ArgumentPrefix>] namespaces every argument contributed by a field's subtree. The generator
/// applies it in exactly one place -- where an argument's spellings are assembled -- so these tests
/// are mostly about confirming that the consumers of those spellings (the scanner, the help text,
/// the `--no-` variant) all see the prefixed name, and that nothing else about the parse changes.
[<TestFixture>]
module TestArgParserPrefix =

    let private noEnvVar (_ : string) : string option = failwith "should not call"

    [<Test>]
    let ``A prefixed sub-record's arguments are namespaced`` () =
        PrefixedParent.parse' noEnvVar [ "--foo-thing1=9" ; "--foo-thing2=hi" ; "--and-another=true" ]
        |> shouldEqual
            {
                Child =
                    {
                        Thing1 = 9
                        Thing2 = "hi"
                    }
                AndAnother = true
            }

    /// The prefix replaces the bare name rather than adding an alias: an author who prefixes a
    /// subtree has renamed it, and leaving the old spelling working would defeat the point of
    /// embedding the same sub-record twice.
    [<Test>]
    let ``The unprefixed spelling is no longer accepted`` () =
        let exc =
            Assert.Throws<exn> (fun () ->
                PrefixedParent.parse' noEnvVar [ "--thing1=9" ; "--thing2=hi" ; "--and-another=true" ]
                |> ignore<PrefixedParent>
            )

        exc.Message |> shouldContainText "--thing1"

    /// A sibling of the prefixed field keeps its own spelling: the prefix descends into one
    /// subtree, it does not apply to the record which declares the field.
    [<Test>]
    let ``A prefix does not leak to the prefixed field's siblings`` () =
        let parsed =
            PrefixedParent.parse' noEnvVar [ "--foo-thing1=1" ; "--foo-thing2=x" ; "--and-another=false" ]

        parsed.AndAnother |> shouldEqual false

    /// The motivating case, and the thing which is impossible without the feature: two copies of
    /// one sub-record, told apart only by their prefixes.
    [<Test>]
    let ``The same sub-record can be embedded twice under distinct prefixes`` () =
        Transfer.parse'
            noEnvVar
            [
                "--src-host=a.example.com"
                "--src-port=1"
                "--dst-host=b.example.com"
                "--dst-port=2"
            ]
        |> shouldEqual
            {
                Source =
                    {
                        Host = "a.example.com"
                        Port = 1
                    }
                Dest =
                    {
                        Host = "b.example.com"
                        Port = 2
                    }
            }

    [<Test>]
    let ``Prefixes compose from the outside in`` () =
        PrefixedNested.parse' noEnvVar [ "--outer-inner-leaf=3" ; "--outer-sibling=4" ]
        |> shouldEqual
            {
                Middle =
                    {
                        Grandchild =
                            {
                                Leaf = 3
                            }
                        Sibling = 4
                    }
            }

    /// An unprefixed record beneath a prefixed one is not a barrier: the prefix applies to the
    /// whole subtree, however deep, so `Leaf` is `--outer-leaf` and not `--leaf`.
    [<Test>]
    let ``An outer prefix reaches through an unprefixed sub-record`` () =
        PrefixedThroughUnprefixed.parse' noEnvVar [ "--outer-leaf=5" ; "--outer-sibling=6" ]
        |> shouldEqual
            {
                Middle =
                    {
                        Grandchild =
                            {
                                Leaf = 5
                            }
                        Sibling = 6
                    }
            }

    /// An explicit [<ArgumentLongForm>] is a spelling like any other, so the prefix applies to it
    /// too. If it escaped, two parents embedding this child under different prefixes would still
    /// collide on it.
    [<Test>]
    let ``The prefix applies to explicit long forms`` () =
        PrefixedLongForms.parse' noEnvVar [ "--pre-renamed=7" ]
        |> shouldEqual
            {
                Child =
                    {
                        Original = 7
                    }
            }

        PrefixedLongForms.parse' noEnvVar [ "--pre-r=8" ]
        |> shouldEqual
            {
                Child =
                    {
                        Original = 8
                    }
            }

    [<Test>]
    let ``The unprefixed long form is no longer accepted`` () =
        let exc =
            Assert.Throws<exn> (fun () ->
                PrefixedLongForms.parse' noEnvVar [ "--renamed=7" ] |> ignore<PrefixedLongForms>
            )

        exc.Message |> shouldContainText "--renamed"

    /// The runtime builds the negated token as `"--no-" + form`, and `form` is the whole prefixed
    /// name, so negation wraps the prefix from outside.
    [<Test>]
    let ``Negation composes outside the prefix`` () =
        PrefixedNegation.parse' noEnvVar [ "--flags-enable-feature" ]
        |> shouldEqual
            {
                Child =
                    {
                        EnableFeature = true
                    }
            }

        PrefixedNegation.parse' noEnvVar [ "--no-flags-enable-feature" ]
        |> shouldEqual
            {
                Child =
                    {
                        EnableFeature = false
                    }
            }

    [<Test>]
    let ``The inner negated form is not accepted`` () =
        let exc =
            Assert.Throws<exn> (fun () ->
                PrefixedNegation.parse' noEnvVar [ "--flags-no-enable-feature" ]
                |> ignore<PrefixedNegation>
            )

        exc.Message |> shouldContainText "--flags-no-enable-feature"

    /// Prefixing rebuilds a spelling as a fresh string constant, and a `SynConst.String` holds
    /// *decoded* text, so the combined name needs the escaping the author's own source supplied.
    /// Without it the emitted `\n` and `\t` are read back as a newline and a tab, and the argument
    /// silently answers to a name other than the one declared.
    [<Test>]
    let ``A prefixed spelling containing backslashes survives re-emission`` () =
        PrefixedAwkwardSpelling.parse' noEnvVar [ "--aw\\newline-back\\tab=3" ]
        |> shouldEqual
            {
                Child =
                    {
                        Awkward = 3
                    }
            }

    /// A long form we cannot read at generation time stays an expression the generated program
    /// evaluates, so the prefix is concatenated to it at *runtime* -- but the prefix half is still a
    /// constant we invent, and still needs escaping on the way out.
    [<Test>]
    let ``A prefix joined to an unreadable long form is escaped`` () =
        PrefixedViaLiteral.parse' noEnvVar [ "--viaesc\\tab-via-literal=5" ]
        |> shouldEqual
            {
                Child =
                    {
                        X = 5
                    }
            }

    /// A positional sink inside a prefixed sub-record still collects bare tokens, and its keyed
    /// alias -- the `--form=value` spelling which addresses the sink explicitly -- is prefixed like
    /// any other name.
    [<Test>]
    let ``Positional args inside a prefixed sub-record still collect bare tokens`` () =
        PrefixedPositionals.parse' noEnvVar [ "--pos-thing1=1" ; "a" ; "b" ]
        |> shouldEqual
            {
                Child =
                    {
                        Thing1 = 1
                        Rest = [ "a" ; "b" ]
                    }
            }

    [<Test>]
    let ``The keyed alias of a prefixed positional sink is prefixed`` () =
        PrefixedPositionals.parse' noEnvVar [ "--pos-thing1=1" ; "--pos-rest=a" ; "--pos-rest=b" ]
        |> shouldEqual
            {
                Child =
                    {
                        Thing1 = 1
                        Rest = [ "a" ; "b" ]
                    }
            }

    /// The key/value and entry separators govern how one occurrence's payload is split, which is
    /// orthogonal to the name the occurrence arrives under.
    [<Test>]
    let ``A prefix does not disturb Map separators`` () =
        PrefixedMap.parse' noEnvVar [ "--m-entries=a:b,c:d" ]
        |> shouldEqual
            {
                Child =
                    {
                        Entries = Map.ofList [ "a", "b" ; "c", "d" ]
                    }
            }

    /// A union's cases are alternatives, so the prefix passes through unchanged into each of them:
    /// every case's arguments are namespaced identically.
    [<Test>]
    let ``A prefix applies to every case of a union-typed field`` () =
        PrefixedUnion.parse' noEnvVar [ "--mode-level=3" ]
        |> shouldEqual
            {
                Mode =
                    PrefixedMode.Manual
                        {
                            Level = 3
                        }
            }

        PrefixedUnion.parse' noEnvVar [ "--mode-quiet=true" ]
        |> shouldEqual
            {
                Mode =
                    PrefixedMode.Auto
                        {
                            Quiet = Some true
                        }
            }

    [<Test>]
    let ``Help text shows the prefixed names`` () =
        let exc =
            Assert.Throws<exn> (fun () -> PrefixedParent.parse' noEnvVar [ "--help" ] |> ignore<PrefixedParent>)

        exc.Message
        |> shouldEqual
            """Help text requested.
Child:
  --foo-thing1  int32
  --foo-thing2  string
--and-another  bool"""

    [<Test>]
    let ``Help text shows the prefixed negated form`` () =
        let exc =
            Assert.Throws<exn> (fun () -> PrefixedNegation.parse' noEnvVar [ "--help" ] |> ignore<PrefixedNegation>)

        exc.Message |> shouldContainText "--no-flags-enable-feature"

    // ------------------------------------------------------------------------------------------
    // Properties.
    // ------------------------------------------------------------------------------------------

    /// The unprefixed parser is the reference implementation for the prefixed one: prefixing
    /// renames arguments and changes nothing else. `PrefixedParent`/`PrefixedChild` and
    /// `ParentRecord`/`ChildRecord` are the same shape, so any command line accepted by one, with
    /// the names rewritten, must be accepted by the other and yield the same values.
    [<Test>]
    let ``Prefixing is a renaming: the unprefixed parser is the oracle`` () =
        let inputs =
            gen {
                let! thing1 = ArbMap.defaults |> ArbMap.generate<int>
                // The scanner splits a --key=value token at its first '=', so a value containing
                // '=' is fine but one containing a leading '-' would be read as a new key.
                let! thing2 =
                    Gen.listOf (Gen.elements [ 'a' .. 'e' ])
                    |> Gen.map (fun cs -> System.String (Array.ofList cs))

                let! andAnother = ArbMap.defaults |> ArbMap.generate<bool>
                return thing1, thing2, andAnother
            }
            |> Arb.fromGen

        Prop.forAll
            inputs
            (fun (thing1, thing2, andAnother) ->
                let prefixed =
                    PrefixedParent.parse'
                        noEnvVar
                        [
                            $"--foo-thing1=%i{thing1}"
                            $"--foo-thing2=%s{thing2}"
                            $"--and-another=%b{andAnother}"
                        ]

                let unprefixed =
                    ParentRecord.parse'
                        noEnvVar
                        [
                            $"--thing1=%i{thing1}"
                            $"--thing2=%s{thing2}"
                            $"--and-another=%b{andAnother}"
                        ]

                prefixed.Child.Thing1 = unprefixed.Child.Thing1
                && prefixed.Child.Thing2 = unprefixed.Child.Thing2
                && prefixed.AndAnother = unprefixed.AndAnother
            )
        |> Check.QuickThrowOnFailure

    /// Composition is associative with respect to spelling: nesting prefix "inner" inside prefix
    /// "outer" gives exactly the names that the single prefix "outer-inner" gives. This is the
    /// property which pins down *how* prefixes compose, as opposed to merely that they do.
    [<Test>]
    let ``Nested prefixes agree with the equivalent flattened prefix`` () =
        let leaves = ArbMap.defaults |> ArbMap.arbitrary<int>

        Prop.forAll
            leaves
            (fun leaf ->
                let nested =
                    PrefixedNested.parse' noEnvVar [ $"--outer-inner-leaf=%i{leaf}" ; "--outer-sibling=0" ]

                let flattened = PrefixedFlattened.parse' noEnvVar [ $"--outer-inner-leaf=%i{leaf}" ]

                nested.Middle.Grandchild.Leaf = flattened.Grandchild.Leaf
            )
        |> Check.QuickThrowOnFailure

    /// The spellings the help text advertises and the spellings the scanner accepts both come from
    /// one place in the generator. This is the property which catches a prefix applied to one
    /// consumer of that value but not another -- the most likely way to get this feature wrong.
    [<Test>]
    let ``Every argument the help text advertises is accepted`` () =
        let helpTextOf (parse : string list -> unit) : string =
            let exc = Assert.Throws<exn> (fun () -> parse [ "--help" ])
            exc.Message

        let advertisedForms (help : string) : string list =
            help.Split '\n'
            |> Array.toList
            |> List.collect (fun line ->
                line.Split ' '
                |> Array.toList
                |> List.filter (fun tok -> tok.StartsWith ("--", StringComparison.Ordinal) && tok.Length > 2)
            )
            |> List.distinct

        // Each parser is paired with a value which is well-typed for every one of its arguments, so
        // that supplying `--form=value` can only fail because the *form* was not recognised.
        let parsers : (string * (string list -> unit) * string) list =
            [
                "PrefixedParent", (fun args -> PrefixedParent.parse' noEnvVar args |> ignore<PrefixedParent>), "true"
                "Transfer", (fun args -> Transfer.parse' noEnvVar args |> ignore<Transfer>), "1"
                "PrefixedNested", (fun args -> PrefixedNested.parse' noEnvVar args |> ignore<PrefixedNested>), "1"
                "PrefixedLongForms",
                (fun args -> PrefixedLongForms.parse' noEnvVar args |> ignore<PrefixedLongForms>),
                "1"
                "PrefixedNegation",
                (fun args -> PrefixedNegation.parse' noEnvVar args |> ignore<PrefixedNegation>),
                "true"
                "PrefixedUnion", (fun args -> PrefixedUnion.parse' noEnvVar args |> ignore<PrefixedUnion>), "true"
            ]

        for name, parse, value in parsers do
            let forms = helpTextOf parse |> advertisedForms

            forms |> shouldNotEqual []

            for form in forms do
                // Supplying one argument need not satisfy the whole schema, so the parse may well
                // fail; what matters is *why*. The runtime reports a name it does not know with
                // "Unable to process", and every other failure (a missing required argument
                // elsewhere, a value of the wrong type) means the form itself was understood,
                // which is all this property asserts.
                let failure =
                    try
                        parse [ $"%s{form}=%s{value}" ]
                        None
                    with e ->
                        Some e.Message

                match failure with
                | None -> ()
                | Some message ->
                    if message.Contains "Unable to process" then
                        failwithf "Parser %s advertised %s in its help text but did not accept it: %s" name form message
