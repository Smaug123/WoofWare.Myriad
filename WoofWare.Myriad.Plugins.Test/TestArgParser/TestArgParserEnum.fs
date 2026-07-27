namespace WoofWare.Myriad.Plugins.Test

open System
open NUnit.Framework
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open ConsumePlugin

/// Unions with no data in any case are argument *values*, spelled by case name and matched
/// case-insensitively. They are ordinary argument leaves, so they compose with optionality,
/// repetition, defaults and the positional stream.
[<TestFixture>]
module TestArgParserEnum =

    let private noEnv (_ : string) : string option = None

    /// Everything `EnumArgs` requires, other than the enumerated field under test.
    let private otherArgs = [ "--env-colour=Red" ]

    [<Test>]
    let ``A case name selects that case`` () =
        let args = EnumArgs.parse' noEnv ("--verbosity=Quiet" :: otherArgs)

        args.Verbosity |> shouldEqual Verbosity.Quiet

    [<Test>]
    let ``Case names are matched case-insensitively`` () =
        for spelling in [ "quiet" ; "Quiet" ; "QUIET" ; "qUiEt" ] do
            let args = EnumArgs.parse' noEnv ($"--verbosity=%s{spelling}" :: otherArgs)

            args.Verbosity |> shouldEqual Verbosity.Quiet

    [<Test>]
    let ``An unrecognised value reports the values which are recognised`` () =
        let exc =
            Assert.Throws<exn> (fun () -> EnumArgs.parse' noEnv ("--verbosity=loud" :: otherArgs) |> ignore<EnumArgs>)

        exc.Message
        |> shouldEqual
            """Errors during parse!
Unrecognised value 'loud' for Verbosity: expected one of Quiet, Normal, ExtremelyLoud (at arg --verbosity=loud)
Required argument '--verbosity' received no value"""

    [<Test>]
    let ``An enumerated argument can be required`` () =
        let exc =
            Assert.Throws<exn> (fun () -> EnumArgs.parse' noEnv otherArgs |> ignore<EnumArgs>)

        exc.Message
        |> shouldEqual
            """Errors during parse!
Required argument '--verbosity' received no value"""

    [<Test>]
    let ``An enumerated argument can be optional`` () =
        let absent = EnumArgs.parse' noEnv ("--verbosity=Normal" :: otherArgs)
        absent.Colour |> shouldEqual None

        let present =
            EnumArgs.parse' noEnv ("--verbosity=Normal" :: "--colour=green" :: otherArgs)

        present.Colour |> shouldEqual (Some Colour.Green)

    [<Test>]
    let ``An enumerated argument can be repeated`` () =
        let args =
            EnumArgs.parse' noEnv ("--verbosity=Normal" :: "--palette=red" :: "--palette=Green" :: otherArgs)

        args.Palette |> shouldEqual [ Colour.Red ; Colour.Green ]

    [<Test>]
    let ``An enumerated argument can have a default function`` () =
        let defaulted = EnumArgs.parse' noEnv ("--verbosity=Quiet" :: otherArgs)
        defaulted.Fallback |> shouldEqual (Choice2Of2 Verbosity.Normal)

        let supplied =
            EnumArgs.parse' noEnv ("--verbosity=Quiet" :: "--fallback=extremelyloud" :: otherArgs)

        supplied.Fallback |> shouldEqual (Choice1Of2 Verbosity.ExtremelyLoud)

    [<Test>]
    let ``An enumerated argument can default from an environment variable`` () =
        let getEnvVar (s : string) =
            s |> shouldEqual "CONSUMEPLUGIN_ENUM_COLOUR"
            Some "green"

        let defaulted = EnumArgs.parse' getEnvVar [ "--verbosity=Quiet" ]
        defaulted.EnvColour |> shouldEqual (Choice2Of2 Colour.Green)

        let supplied =
            EnumArgs.parse' getEnvVar [ "--verbosity=Quiet" ; "--env-colour=Red" ]

        supplied.EnvColour |> shouldEqual (Choice1Of2 Colour.Red)

    [<Test>]
    let ``An unrecognised value in an environment variable is reported as such`` () =
        let getEnvVar (_ : string) = Some "purple"

        let exc =
            Assert.Throws<exn> (fun () -> EnumArgs.parse' getEnvVar [ "--verbosity=Quiet" ] |> ignore<EnumArgs>)

        exc.Message
        |> shouldEqual
            """Errors during parse!
Unrecognised value 'purple' for Colour: expected one of Red, Green (from environment variable CONSUMEPLUGIN_ENUM_COLOUR)"""

    [<Test>]
    let ``An enumerated argument can collect the positional stream`` () =
        let args =
            EnumArgs.parse' noEnv ("--verbosity=Normal" :: "red" :: "GREEN" :: otherArgs)

        args.Rest |> shouldEqual [ Colour.Red ; Colour.Green ]

    [<Test>]
    let ``Help text lists the values each enumerated argument accepts`` () =
        let exc =
            Assert.Throws<exn> (fun () -> EnumArgs.parse' noEnv [ "--help" ] |> ignore<EnumArgs>)

        exc.Message
        |> shouldEqual
            """Help text requested.
--verbosity  Verbosity [one of: Quiet|Normal|ExtremelyLoud]
--colour  Colour [one of: Red|Green] (optional) : Which colour to paint it
--palette  Colour [one of: Red|Green] (can be repeated)
--fallback  Verbosity [one of: Quiet|Normal|ExtremelyLoud] (default value: Normal)
--env-colour  Colour [one of: Red|Green] (default value populated from env var CONSUMEPLUGIN_ENUM_COLOUR)
--rest  Colour [one of: Red|Green] (positional args) (can be repeated)"""

    // An enumerated value inside a union case. Case selection is by which arguments were
    // supplied, and happens before any value is converted, so the value can never influence it.

    [<Test>]
    let ``An enumerated value inside a union case`` () =
        EnumInUnion.parse' noEnv [ "--verbosity=quiet" ]
        |> shouldEqual (
            Build
                {
                    Verbosity = Verbosity.Quiet
                }
        )

        EnumInUnion.parse' noEnv []
        |> shouldEqual (
            Clean
                {
                    Force = None
                }
        )

    [<Test>]
    let ``An unrecognised value does not change which case is selected`` () =
        // Build is selected because --verbosity was supplied; only then is 'nonsense' converted.
        // Were selection to depend on convertibility, this would instead fall back to Clean.
        let exc =
            Assert.Throws<exn> (fun () -> EnumInUnion.parse' noEnv [ "--verbosity=nonsense" ] |> ignore<EnumInUnion>)

        exc.Message
        |> shouldEqual
            """Errors during parse!
Unrecognised value 'nonsense' for Verbosity: expected one of Quiet, Normal, ExtremelyLoud (at arg --verbosity=nonsense)
Required argument '--verbosity' received no value"""

    // The generated parser accepts exactly the case names, under OrdinalIgnoreCase. That is a
    // relation the .NET runtime defines, so we check ours agrees with it rather than reasoning
    // about which pairs of strings fold together.

    let private verbosityCases =
        [
            "Quiet", Verbosity.Quiet
            "Normal", Verbosity.Normal
            "ExtremelyLoud", Verbosity.ExtremelyLoud
        ]

    /// The parsed value, or the message we failed with.
    let private parseVerbosity (s : string) : Choice<Verbosity, string> =
        try
            Choice1Of2 (EnumArgs.parse' noEnv ($"--verbosity=%s{s}" :: otherArgs)).Verbosity
        with exc ->
            Choice2Of2 exc.Message

    [<Test>]
    let ``Exactly the case names are accepted, under OrdinalIgnoreCase`` () =
        let adversarial =
            [
                // Values which fold onto a case name, and near misses which do not: "İ" is not
                // OrdinalIgnoreCase-equal to "i", and "ſ" (long s) is not equal to "s", though
                // both pairs collide under ToUpperInvariant.
                "Quiet"
                "quiet"
                "QUIET"
                "quİet"
                "QUİET"
                "ſ"
                "s"
                ""
                " Quiet"
                "Quiet "
                "Quiet=Normal"
                "--normal"
                "Quie"
                "Quiett"
            ]

        let values =
            Gen.oneof
                [
                    Gen.elements adversarial
                    ArbMap.defaults |> ArbMap.generate<string> |> Gen.filter (isNull >> not)
                ]

        let property (s : string) : bool =
            let expected =
                verbosityCases
                |> List.tryPick (fun (name, case) ->
                    if String.Equals (s, name, StringComparison.OrdinalIgnoreCase) then
                        Some case
                    else
                        None
                )

            match parseVerbosity s, expected with
            | Choice1Of2 actual, Some expected -> actual = expected
            | Choice2Of2 message, None ->
                // Fail for the right reason: any other error would make this vacuous.
                message.Contains "Unrecognised value" && message.Contains "expected one of"
            | Choice1Of2 _, None
            | Choice2Of2 _, Some _ -> false

        let config = Config.QuickThrowOnFailure.WithMaxTest 1000
        Check.One (config, Prop.forAll (Arb.fromGen values) property)
