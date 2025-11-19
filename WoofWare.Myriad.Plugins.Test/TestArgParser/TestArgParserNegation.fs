namespace WoofWare.Myriad.Plugins.Test

open System.Threading
open NUnit.Framework
open FsUnitTyped
open ConsumePlugin

[<TestFixture>]
module TestArgParserNegation =

    [<Test>]
    let ``Boolean field with ArgumentNegateWithPrefix: --foo sets to true`` () =
        let getEnvVar (_ : string) = failwith "should not call"

        let result = BoolNegation.parse' getEnvVar [ "--enable-feature" ]

        result.EnableFeature |> shouldEqual true

    [<Test>]
    let ``Boolean field with ArgumentNegateWithPrefix: --foo=true sets to true`` () =
        let getEnvVar (_ : string) = failwith "should not call"

        let result = BoolNegation.parse' getEnvVar [ "--enable-feature=true" ]

        result.EnableFeature |> shouldEqual true

    [<Test>]
    let ``Boolean field with ArgumentNegateWithPrefix: --foo true sets to true`` () =
        let getEnvVar (_ : string) = failwith "should not call"

        let result = BoolNegation.parse' getEnvVar [ "--enable-feature" ; "true" ]

        result.EnableFeature |> shouldEqual true

    [<Test>]
    let ``Boolean field with ArgumentNegateWithPrefix: --foo=false sets to false`` () =
        let getEnvVar (_ : string) = failwith "should not call"

        let result = BoolNegation.parse' getEnvVar [ "--enable-feature=false" ]

        result.EnableFeature |> shouldEqual false

    [<Test>]
    let ``Boolean field with ArgumentNegateWithPrefix: --foo false sets to false`` () =
        let getEnvVar (_ : string) = failwith "should not call"

        let result = BoolNegation.parse' getEnvVar [ "--enable-feature" ; "false" ]

        result.EnableFeature |> shouldEqual false

    [<Test>]
    let ``Boolean field with ArgumentNegateWithPrefix: --no-foo sets to false`` () =
        let getEnvVar (_ : string) = failwith "should not call"

        let result = BoolNegation.parse' getEnvVar [ "--no-enable-feature" ]

        result.EnableFeature |> shouldEqual false

    [<Test>]
    let ``Boolean field with ArgumentNegateWithPrefix: --no-foo=true sets to false`` () =
        let getEnvVar (_ : string) = failwith "should not call"

        let result = BoolNegation.parse' getEnvVar [ "--no-enable-feature=true" ]

        result.EnableFeature |> shouldEqual false

    [<Test>]
    let ``Boolean field with ArgumentNegateWithPrefix: --no-foo true sets to false`` () =
        let getEnvVar (_ : string) = failwith "should not call"

        let result = BoolNegation.parse' getEnvVar [ "--no-enable-feature" ; "true" ]

        result.EnableFeature |> shouldEqual false

    [<Test>]
    let ``Boolean field with ArgumentNegateWithPrefix: --no-foo=false sets to true`` () =
        let getEnvVar (_ : string) = failwith "should not call"

        let result = BoolNegation.parse' getEnvVar [ "--no-enable-feature=false" ]

        result.EnableFeature |> shouldEqual true

    [<Test>]
    let ``Boolean field with ArgumentNegateWithPrefix: --no-foo false sets to true`` () =
        let getEnvVar (_ : string) = failwith "should not call"

        let result = BoolNegation.parse' getEnvVar [ "--no-enable-feature" ; "false" ]

        result.EnableFeature |> shouldEqual true

    [<Test>]
    let ``Flag DU with ArgumentNegateWithPrefix: --dry-run sets to Dry`` () =
        let getEnvVar (_ : string) = failwith "should not call"

        let result = FlagNegation.parse' getEnvVar [ "--dry-run" ]

        result.DryRun |> shouldEqual TestDryRunMode.Dry

    [<Test>]
    let ``Flag DU with ArgumentNegateWithPrefix: --dry-run=true sets to Dry`` () =
        let getEnvVar (_ : string) = failwith "should not call"

        let result = FlagNegation.parse' getEnvVar [ "--dry-run=true" ]

        result.DryRun |> shouldEqual TestDryRunMode.Dry

    [<Test>]
    let ``Flag DU with ArgumentNegateWithPrefix: --dry-run true sets to Dry`` () =
        let getEnvVar (_ : string) = failwith "should not call"

        let result = FlagNegation.parse' getEnvVar [ "--dry-run" ; "true" ]

        result.DryRun |> shouldEqual TestDryRunMode.Dry

    [<Test>]
    let ``Flag DU with ArgumentNegateWithPrefix: --dry-run=false sets to Wet`` () =
        let getEnvVar (_ : string) = failwith "should not call"

        let result = FlagNegation.parse' getEnvVar [ "--dry-run=false" ]

        result.DryRun |> shouldEqual TestDryRunMode.Wet

    [<Test>]
    let ``Flag DU with ArgumentNegateWithPrefix: --dry-run false sets to Wet`` () =
        let getEnvVar (_ : string) = failwith "should not call"

        let result = FlagNegation.parse' getEnvVar [ "--dry-run" ; "false" ]

        result.DryRun |> shouldEqual TestDryRunMode.Wet

    [<Test>]
    let ``Flag DU with ArgumentNegateWithPrefix: --no-dry-run sets to Wet`` () =
        let getEnvVar (_ : string) = failwith "should not call"

        let result = FlagNegation.parse' getEnvVar [ "--no-dry-run" ]

        result.DryRun |> shouldEqual TestDryRunMode.Wet

    [<Test>]
    let ``Flag DU with ArgumentNegateWithPrefix: --no-dry-run=true sets to Wet`` () =
        let getEnvVar (_ : string) = failwith "should not call"

        let result = FlagNegation.parse' getEnvVar [ "--no-dry-run=true" ]

        result.DryRun |> shouldEqual TestDryRunMode.Wet

    [<Test>]
    let ``Flag DU with ArgumentNegateWithPrefix: --no-dry-run true sets to Wet`` () =
        let getEnvVar (_ : string) = failwith "should not call"

        let result = FlagNegation.parse' getEnvVar [ "--no-dry-run" ; "true" ]

        result.DryRun |> shouldEqual TestDryRunMode.Wet

    [<Test>]
    let ``Flag DU with ArgumentNegateWithPrefix: --no-dry-run=false sets to Dry`` () =
        let getEnvVar (_ : string) = failwith "should not call"

        let result = FlagNegation.parse' getEnvVar [ "--no-dry-run=false" ]

        result.DryRun |> shouldEqual TestDryRunMode.Dry

    [<Test>]
    let ``Flag DU with ArgumentNegateWithPrefix: --no-dry-run false sets to Dry`` () =
        let getEnvVar (_ : string) = failwith "should not call"

        let result = FlagNegation.parse' getEnvVar [ "--no-dry-run" ; "false" ]

        result.DryRun |> shouldEqual TestDryRunMode.Dry

    [<Test>]
    let ``ArgumentNegateWithPrefix works with multiple ArgumentLongForm: --verbose`` () =
        let getEnvVar (_ : string) = failwith "should not call"

        let result = MultipleFormsNegation.parse' getEnvVar [ "--verbose" ]

        result.VerboseMode |> shouldEqual true

    [<Test>]
    let ``ArgumentNegateWithPrefix works with multiple ArgumentLongForm: --v`` () =
        let getEnvVar (_ : string) = failwith "should not call"

        let result = MultipleFormsNegation.parse' getEnvVar [ "--v" ]

        result.VerboseMode |> shouldEqual true

    [<Test>]
    let ``ArgumentNegateWithPrefix works with multiple ArgumentLongForm: --no-verbose`` () =
        let getEnvVar (_ : string) = failwith "should not call"

        let result = MultipleFormsNegation.parse' getEnvVar [ "--no-verbose" ]

        result.VerboseMode |> shouldEqual false

    [<Test>]
    let ``ArgumentNegateWithPrefix works with multiple ArgumentLongForm: --no-v`` () =
        let getEnvVar (_ : string) = failwith "should not call"

        let result = MultipleFormsNegation.parse' getEnvVar [ "--no-v" ]

        result.VerboseMode |> shouldEqual false

    [<Test>]
    let ``ArgumentNegateWithPrefix works with multiple ArgumentLongForm: --verbose=false`` () =
        let getEnvVar (_ : string) = failwith "should not call"

        let result = MultipleFormsNegation.parse' getEnvVar [ "--verbose=false" ]

        result.VerboseMode |> shouldEqual false

    [<Test>]
    let ``ArgumentNegateWithPrefix works with multiple ArgumentLongForm: --no-v=false`` () =
        let getEnvVar (_ : string) = failwith "should not call"

        let result = MultipleFormsNegation.parse' getEnvVar [ "--no-v=false" ]

        result.VerboseMode |> shouldEqual true

    [<Test>]
    let ``Help text shows both standard and negated forms for boolean`` () =
        let getEnvVar (_ : string) = failwith "do not call"

        let exc =
            Assert.Throws<exn> (fun () -> BoolNegation.parse' getEnvVar [ "--help" ] |> ignore<BoolNegation>)

        exc.Message
        |> shouldEqual
            """Help text requested.
--enable-feature / --no-enable-feature  bool"""

    [<Test>]
    let ``Help text shows both standard and negated forms for flag DU`` () =
        let getEnvVar (_ : string) = failwith "do not call"

        let exc =
            Assert.Throws<exn> (fun () -> FlagNegation.parse' getEnvVar [ "--help" ] |> ignore<FlagNegation>)

        exc.Message
        |> shouldEqual
            """Help text requested.
--dry-run / --no-dry-run  bool"""

    [<Test>]
    let ``Help text with multiple long forms shows all variants`` () =
        let getEnvVar (_ : string) = failwith "do not call"

        let exc =
            Assert.Throws<exn> (fun () ->
                MultipleFormsNegation.parse' getEnvVar [ "--help" ]
                |> ignore<MultipleFormsNegation>
            )

        exc.Message
        |> shouldEqual
            """Help text requested.
--verbose / --v / --no-verbose / --no-v  bool"""

    [<Test>]
    let ``Multiple occurrences error: --foo and --no-foo both supplied`` () =
        let getEnvVar (_ : string) = failwith "should not call"

        let exc =
            Assert.Throws<exn> (fun () ->
                BoolNegation.parse' getEnvVar [ "--enable-feature" ; "--no-enable-feature" ]
                |> ignore<BoolNegation>
            )

        // Should report as duplicate argument
        exc.Message |> shouldContainText "supplied multiple times"

    [<Test>]
    let ``Multiple occurrences error: --no-foo and --foo both supplied`` () =
        let getEnvVar (_ : string) = failwith "should not call"

        let exc =
            Assert.Throws<exn> (fun () ->
                BoolNegation.parse' getEnvVar [ "--no-enable-feature" ; "--enable-feature" ]
                |> ignore<BoolNegation>
            )

        // Should report as duplicate argument
        exc.Message |> shouldContainText "supplied multiple times"

    [<Test>]
    let ``Multiple occurrences error: --foo=true and --no-foo=false both supplied`` () =
        let getEnvVar (_ : string) = failwith "should not call"

        let exc =
            Assert.Throws<exn> (fun () ->
                BoolNegation.parse' getEnvVar [ "--enable-feature=true" ; "--no-enable-feature=false" ]
                |> ignore<BoolNegation>
            )

        // Should report as duplicate argument
        exc.Message |> shouldContainText "supplied multiple times"

    [<Test>]
    let ``CombinedFeatures: verbose with default value works with negation`` () =
        let getEnvVar (_ : string) = failwith "should not call"

        let result = CombinedFeatures.parse' getEnvVar [ "--debug" ; "--normal-bool" ]

        result.Verbose |> shouldEqual (Choice2Of2 false)
        result.Debug |> shouldEqual true
        result.NormalBool |> shouldEqual true

    [<Test>]
    let ``CombinedFeatures: can override default with --verbose`` () =
        let getEnvVar (_ : string) = failwith "should not call"

        let result =
            CombinedFeatures.parse' getEnvVar [ "--verbose" ; "--debug" ; "--normal-bool" ]

        result.Verbose |> shouldEqual (Choice1Of2 true)
        result.Debug |> shouldEqual true
        result.NormalBool |> shouldEqual true

    [<Test>]
    let ``CombinedFeatures: can override default with --no-verbose`` () =
        let getEnvVar (_ : string) = failwith "should not call"

        let result =
            CombinedFeatures.parse' getEnvVar [ "--no-verbose" ; "--debug" ; "--normal-bool" ]

        result.Verbose |> shouldEqual (Choice1Of2 false)
        result.Debug |> shouldEqual true
        result.NormalBool |> shouldEqual true

    [<Test>]
    let ``CombinedFeatures: --no-debug sets debug to false`` () =
        let getEnvVar (_ : string) = failwith "should not call"

        let result = CombinedFeatures.parse' getEnvVar [ "--no-debug" ; "--normal-bool" ]

        result.Debug |> shouldEqual false

    [<Test>]
    let ``CombinedFeatures: help text shows negation for fields with attribute`` () =
        let getEnvVar (_ : string) = failwith "do not call"

        let exc =
            Assert.Throws<exn> (fun () -> CombinedFeatures.parse' getEnvVar [ "--help" ] |> ignore<CombinedFeatures>)

        // Verbose and Debug should have --no- forms, NormalBool should not
        exc.Message |> shouldContainText "--verbose / --no-verbose"
        exc.Message |> shouldContainText "--debug / --no-debug"
        exc.Message |> shouldContainText "--normal-bool  bool"
        exc.Message |> shouldNotContainText "--no-normal-bool"

    [<Test>]
    let ``Case insensitivity: --NO-ENABLE-FEATURE works`` () =
        let getEnvVar (_ : string) = failwith "should not call"

        let result = BoolNegation.parse' getEnvVar [ "--NO-ENABLE-FEATURE" ]

        result.EnableFeature |> shouldEqual false

    [<Test>]
    let ``Case insensitivity: --No-Enable-Feature works`` () =
        let getEnvVar (_ : string) = failwith "should not call"

        let result = BoolNegation.parse' getEnvVar [ "--No-Enable-Feature" ]

        result.EnableFeature |> shouldEqual false

    [<Test>]
    let ``Case insensitivity: --NO-DRY-RUN works`` () =
        let getEnvVar (_ : string) = failwith "should not call"

        let result = FlagNegation.parse' getEnvVar [ "--NO-DRY-RUN" ]

        result.DryRun |> shouldEqual TestDryRunMode.Wet
