namespace WoofWare.Myriad.Plugins.Test

open System
open NUnit.Framework
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open ConsumePlugin

/// `Map`-typed fields accumulate across occurrences like lists do, but each occurrence carries
/// key-value entries rather than bare values.
///
/// No separator-based encoding is surjective onto `Map<string, string>`: some character must
/// separate a key from its value, and that character may occur in a key. Splitting at the *first*
/// separator confines the damage to keys — a value may contain anything — so the encoding is
/// surjective exactly onto those maps whose keys avoid the separator (and, when an entry
/// separator is configured, whose keys and values both avoid *that*). For key types which cannot
/// spell the separator at all (an int, or a data-free union whose case names are alphanumeric)
/// the restriction is vacuous and every map is expressible. These tests pin down that boundary.
[<TestFixture>]
module TestArgParserMap =

    let private noEnv (_ : string) : string option = None

    // ------------------------------------------------------------------ helpers

    /// Render one entry per occurrence of `--labels`, which needs no entry separator.
    let private renderLabels (entries : (string * string) list) : string list =
        entries |> List.map (fun (k, v) -> $"--labels=%s{k}:%s{v}")

    let private parseLabels (entries : (string * string) list) : Map<string, string> =
        (MapArgs.parse' noEnv (renderLabels entries)).Labels

    let private notNull (s : string) : bool = not (isNull s)

    let private avoids (chars : char list) (s : string) : bool =
        notNull s && chars |> List.forall (s.Contains >> not)

    /// Strings which stress the encoding: separators in both positions, argument-shaped tokens,
    /// and the empty string (a legal `string` key and a legal `string` value).
    let private adversarial =
        [
            ""
            ":"
            ","
            "="
            "a:b"
            "a,b"
            "a=b"
            ":leading"
            "trailing:"
            "--labels"
            "--labels=x:y"
            "--"
            " "
            "a b"
            "\n"
            "\u0000"
            "ß"
            "İ"
        ]

    let private strings : Gen<string> =
        Gen.oneof
            [
                Gen.elements adversarial
                ArbMap.defaults |> ArbMap.generate<string> |> Gen.filter notNull
            ]

    /// Entry lists with distinct keys, so that the duplicate-key rule never fires.
    let private distinctEntries (keys : Gen<string>) (values : Gen<string>) : Gen<(string * string) list> =
        Gen.zip keys values |> Gen.listOf |> Gen.map (List.distinctBy fst)

    let private check (property : 'a -> bool) (gen : Gen<'a>) : unit =
        Check.One (Config.QuickThrowOnFailure.WithMaxTest 500, Prop.forAll (Arb.fromGen gen) property)

    // ------------------------------------------------------- the round-trip property

    [<Test>]
    let ``Every map whose keys avoid the separator round-trips`` () =
        // Values are deliberately unrestricted: splitting at the first separator means everything
        // after it is the value, verbatim.
        distinctEntries (strings |> Gen.filter (avoids [ ':' ])) strings
        |> check (fun entries -> parseLabels entries = Map.ofList entries)

    [<Test>]
    let ``A value may contain the key-value separator`` () =
        parseLabels [ "url", "https://example.com:8080" ]
        |> shouldEqual (Map.ofList [ "url", "https://example.com:8080" ])

    [<Test>]
    let ``A value may contain the entry separator when none is configured`` () =
        parseLabels [ "csv", "a,b,c" ] |> shouldEqual (Map.ofList [ "csv", "a,b,c" ])

    [<Test>]
    let ``An enumerated key needs no restriction at all`` () =
        // No Severity case name contains ':', so every Map<Severity, int> is expressible and the
        // property holds over the whole domain rather than a filtered subset.
        let severities = Gen.elements [ Severity.Low ; Severity.High ]

        Gen.zip severities (ArbMap.defaults |> ArbMap.generate<int>)
        |> Gen.listOf
        |> Gen.map (List.distinctBy fst)
        |> check (fun entries ->
            let args =
                entries
                |> List.map (fun (k, v) ->
                    let key =
                        match k with
                        | Severity.Low -> "Low"
                        | Severity.High -> "High"

                    $"--thresholds=%s{key}:%i{v}"
                )

            (MapArgs.parse' noEnv args).Thresholds = Map.ofList entries
        )

    [<Test>]
    let ``Keys are parsed, so an enumerated key is matched case-insensitively`` () =
        let args = MapArgs.parse' noEnv [ "--thresholds=low:1" ; "--thresholds=HIGH:2" ]

        args.Thresholds
        |> shouldEqual (Map.ofList [ Severity.Low, 1 ; Severity.High, 2 ])

    [<Test>]
    let ``String keys are matched case-sensitively`` () =
        // Argument *names* are matched case-insensitively, but a string key is a value, and
        // values compare ordinally.
        parseLabels [ "A", "1" ; "a", "2" ]
        |> shouldEqual (Map.ofList [ "A", "1" ; "a", "2" ])

    // ------------------------------------------------------- accumulation semantics

    [<Test>]
    let ``An absent map field is empty`` () =
        let args = MapArgs.parse' noEnv []

        args.Labels |> shouldEqual Map.empty
        args.Env |> shouldEqual Map.empty
        args.Thresholds |> shouldEqual Map.empty

    [<Test>]
    let ``Both the equals and the space form are accepted`` () =
        let args = MapArgs.parse' noEnv [ "--labels=a:1" ; "--labels" ; "b:2" ]

        args.Labels |> shouldEqual (Map.ofList [ "a", "1" ; "b", "2" ])

    [<Test>]
    let ``How entries are partitioned across occurrences does not matter`` () =
        // With an entry separator, the same entries may be written as one occurrence, as several,
        // or any mixture; the resulting map is the same. The wire encoding is plumbing, not
        // semantics.
        let entryChars = [ ',' ; '=' ]

        let partitions (entries : (string * string) list) : Gen<string list> =
            gen {
                let! splits = Gen.listOfLength entries.Length (Gen.elements [ true ; false ])

                let groups =
                    (List.zip entries splits, [])
                    ||> List.foldBack (fun (entry, startsNew) groups ->
                        match groups with
                        | g :: rest when not startsNew -> (entry :: g) :: rest
                        | _ -> [ entry ] :: groups
                    )

                return
                    groups
                    |> List.map (fun group ->
                        group
                        |> List.map (fun (k, v) -> $"%s{k}=%s{v}")
                        |> String.concat ","
                        |> sprintf "--env=%s"
                    )
            }

        let gen =
            gen {
                let! entries =
                    distinctEntries
                        (strings |> Gen.filter (avoids entryChars))
                        (strings |> Gen.filter (avoids entryChars))

                let! args = partitions entries
                return entries, args
            }

        gen
        |> check (fun (entries, args) -> (MapArgs.parse' noEnv args).Env = Map.ofList entries)

    [<Test>]
    let ``An entry separator is honoured within one occurrence`` () =
        let args = MapArgs.parse' noEnv [ "--env=a=1,b=2" ; "--env=c=3" ]

        args.Env |> shouldEqual (Map.ofList [ "a", "1" ; "b", "2" ; "c", "3" ])

    [<Test>]
    let ``Without an entry separator the whole occurrence is one entry`` () =
        // `--labels` has no entry separator, so a comma is just an ordinary character.
        parseLabels [ "a", "1,b:2" ] |> shouldEqual (Map.ofList [ "a", "1,b:2" ])

    // ------------------------------------------------------- boolean-valued maps

    [<Test>]
    let ``A map of bools is not itself boolean-like`` () =
        // A bool-valued map must keep arity one. Were the field classified boolean-like from its
        // value type, the scanner would stop consuming the following token.
        let args = MapArgs.parse' noEnv [ "--switches" ; "a:true" ; "--switches=b:false" ]

        args.Switches |> shouldEqual (Map.ofList [ "a", true ; "b", false ])

    [<Test>]
    let ``A map of bools supplied with no value is a parse error`` () =
        // Not an internal error: the field is missing its entry, which is an ordinary user
        // mistake.
        let exc =
            Assert.Throws<exn> (fun () -> MapArgs.parse' noEnv [ "--switches" ] |> ignore<MapArgs>)

        exc.Message |> shouldNotContainText "internal error"

    [<Test>]
    let ``Help text describes a bool-valued map as a map`` () =
        let exc =
            Assert.Throws<exn> (fun () -> MapArgs.parse' noEnv [ "--help" ] |> ignore<MapArgs>)

        exc.Message |> shouldContainText "map<string, bool>"

    // ------------------------------------------------------- component syntax in help

    [<Test>]
    let ``Help text describes each side of a map in the syntax that side accepts`` () =
        let exc =
            Assert.Throws<exn> (fun () -> MapDisplayArgs.parse' noEnv [ "--help" ] |> ignore<MapDisplayArgs>)

        // A flag DU is spelled true/false, and an enumerated value by case name, exactly as they
        // would be if they were scalar leaves rather than halves of a map entry.
        exc.Message |> shouldContainText "map<Severity [one of: Low|High], bool>"

    [<Test>]
    let ``A cased separator does not make an enumerated case unspellable`` () =
        // 'A' is the separator and the case is named `Apple`, but values are matched
        // case-insensitively, so `apple` names the case while avoiding the separator.
        let args = MapDisplayArgs.parse' noEnv [ "--casing=appleAvalue" ]

        args.Casing |> shouldEqual (Map.ofList [ Alpha.Apple, "value" ])

    [<Test>]
    let ``A flag-valued map parses its values as booleans`` () =
        let args =
            MapDisplayArgs.parse' noEnv [ "--features=Low:true" ; "--features=High:false" ]

        args.Features
        |> shouldEqual (Map.ofList [ Severity.Low, Enabled.Yes ; Severity.High, Enabled.No ])

    // ------------------------------------------------------- accumulating many occurrences

    [<Test>]
    let ``Many occurrences accumulate, and a late duplicate is still caught`` () =
        // The seen-key set is carried between occurrences rather than rebuilt from the
        // accumulator each time; this exercises that it is actually kept up to date.
        let entries = List.init 500 (fun i -> $"k%i{i}", $"v%i{i}")

        parseLabels entries |> shouldEqual (Map.ofList entries)

        let exc =
            Assert.Throws<exn> (fun () ->
                MapArgs.parse' noEnv (renderLabels entries @ [ "--labels=k0:again" ])
                |> ignore<MapArgs>
            )

        exc.Message |> shouldContainText "Key 'k0' was supplied more than once"

    // ------------------------------------------------------- reference implementation

    [<Test>]
    let ``The parser agrees with a naive reference decoder`` () =
        // An independent implementation of the documented semantics: split the occurrence on the
        // entry separator, then split each entry at its first key-value separator.
        let reference (occurrences : string list) : Map<string, string> =
            occurrences
            |> List.collect (fun occurrence -> occurrence.Split ',' |> List.ofArray)
            |> List.map (fun entry ->
                let i = entry.IndexOf '='
                entry.Substring (0, i), entry.Substring (i + 1)
            )
            |> Map.ofList

        let occurrences =
            distinctEntries
                (strings |> Gen.filter (avoids [ ',' ; '=' ]))
                (strings |> Gen.filter (avoids [ ',' ; '=' ]))
            |> Gen.map (List.map (fun (k, v) -> $"%s{k}=%s{v}"))
            |> Gen.map (fun entries ->
                if entries.IsEmpty then
                    []
                else
                    [ String.concat "," entries ]
            )

        occurrences
        |> check (fun occs ->
            let actual = (MapArgs.parse' noEnv (occs |> List.map (sprintf "--env=%s"))).Env
            actual = reference occs
        )

    // ------------------------------------------------------- error behaviour

    [<Test>]
    let ``A duplicated key is an error`` () =
        let exc =
            Assert.Throws<exn> (fun () -> MapArgs.parse' noEnv [ "--labels=a:1" ; "--labels=a:2" ] |> ignore<MapArgs>)

        exc.Message
        |> shouldEqual
            """Errors during parse!
Key 'a' was supplied more than once for '--labels' (at arg --labels=a:2)"""

    [<Test>]
    let ``A key duplicated only after parsing is an error`` () =
        // `low` and `LOW` are distinct strings but the same Severity, so the duplicate is only
        // visible once the key has been parsed.
        let exc =
            Assert.Throws<exn> (fun () ->
                MapArgs.parse' noEnv [ "--thresholds=low:1" ; "--thresholds=LOW:2" ]
                |> ignore<MapArgs>
            )

        exc.Message
        |> shouldEqual
            """Errors during parse!
Key 'LOW' was supplied more than once for '--thresholds' (at arg --thresholds=LOW:2)"""

    [<Test>]
    let ``A duplicated key within a single occurrence is an error`` () =
        let exc =
            Assert.Throws<exn> (fun () -> MapArgs.parse' noEnv [ "--env=a=1,a=2" ] |> ignore<MapArgs>)

        exc.Message
        |> shouldEqual
            """Errors during parse!
Key 'a' was supplied more than once for '--env' (at arg --env=a=1,a=2)"""

    [<Test>]
    let ``An entry with no separator is an error`` () =
        let exc =
            Assert.Throws<exn> (fun () -> MapArgs.parse' noEnv [ "--labels=oops" ] |> ignore<MapArgs>)

        exc.Message
        |> shouldEqual
            """Errors during parse!
Entry 'oops' for '--labels' does not contain the separator ':' (at arg --labels=oops)"""

    [<Test>]
    let ``Every entry of an occurrence must be well-formed`` () =
        let exc =
            Assert.Throws<exn> (fun () -> MapArgs.parse' noEnv [ "--env=a=1,oops" ] |> ignore<MapArgs>)

        exc.Message
        |> shouldEqual
            """Errors during parse!
Entry 'oops' for '--env' does not contain the separator '=' (at arg --env=a=1,oops)"""

    [<Test>]
    let ``An unparseable value reports the underlying failure`` () =
        let exc =
            Assert.Throws<exn> (fun () -> MapArgs.parse' noEnv [ "--thresholds=Low:banana" ] |> ignore<MapArgs>)

        exc.Message |> shouldContainText "banana"

    [<Test>]
    let ``An unparseable key reports the recognised values`` () =
        let exc =
            Assert.Throws<exn> (fun () -> MapArgs.parse' noEnv [ "--thresholds=Medium:1" ] |> ignore<MapArgs>)

        exc.Message |> shouldContainText "expected one of Low, High"

    [<Test>]
    let ``An occurrence which fails partway stores nothing from that occurrence`` () =
        // All-or-nothing per occurrence: a later entry's failure must not leave earlier entries
        // of the same occurrence half-applied, or the error message would be followed by a
        // confusing duplicate-key error on retry.
        let exc =
            Assert.Throws<exn> (fun () -> MapArgs.parse' noEnv [ "--env=a=1,oops" ; "--env=a=2" ] |> ignore<MapArgs>)

        exc.Message |> shouldContainText "does not contain the separator"
        exc.Message |> shouldNotContainText "more than once"

    // ------------------------------------------------------- composition

    [<Test>]
    let ``A map inside a union case payload accumulates normally`` () =
        let args = MapInUnion.parse' noEnv [ "--tags=env:prod" ; "--tags=tier:web" ]

        args
        |> shouldEqual (
            MapInUnion.Deploy
                {
                    Tags = Map.ofList [ "env", "prod" ; "tier", "web" ]
                }
        )

    [<Test>]
    let ``A map is satisfiable with no arguments, so it does not force its case`` () =
        let args = MapInUnion.parse' noEnv [ "--to=abc123" ]

        args
        |> shouldEqual (
            MapInUnion.Rollback
                {
                    Target = "abc123"
                }
        )

    [<Test>]
    let ``An empty command line selects the case whose map may be empty`` () =
        MapInUnion.parse' noEnv []
        |> shouldEqual (
            MapInUnion.Deploy
                {
                    Tags = Map.empty
                }
        )

    [<Test>]
    let ``Help text describes the entry format and repeatability`` () =
        let exc =
            Assert.Throws<exn> (fun () -> MapArgs.parse' noEnv [ "--help" ] |> ignore<MapArgs>)

        exc.Message |> shouldContainText "--labels"
        exc.Message |> shouldContainText "Labels to attach"
        exc.Message |> shouldContainText "can be repeated"
