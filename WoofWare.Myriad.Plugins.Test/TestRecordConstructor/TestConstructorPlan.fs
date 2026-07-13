namespace WoofWare.Myriad.Plugins.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.Myriad.Plugins

[<RequireQualifiedAccess>]
type private LayoutRegime =
    | AllRequired
    | AllOptional
    | Mixed

type private ConstructorLayout =
    {
        Regime : LayoutRegime
        Fields : ConstructorField<string, string> list
    }

[<TestFixture>]
module TestConstructorPlan =

    let private requiredKinds (count : int) : ConstructorFieldKind<string> list =
        List.init count (fun index -> Required $"required_type_{index}")

    let private fields (kinds : ConstructorFieldKind<string> list) : ConstructorField<string, string> list =
        kinds
        |> List.mapi (fun index kind ->
            {
                Name = $"Field_{index}"
                Kind = kind
            }
        )

    let private allRequiredGen : Gen<ConstructorLayout> =
        gen {
            let! count = Gen.choose (1, 24)
            let! shuffled = Gen.shuffle (requiredKinds count)

            return
                {
                    Regime = LayoutRegime.AllRequired
                    Fields = shuffled |> Array.toList |> fields
                }
        }

    let private allOptionalGen : Gen<ConstructorLayout> =
        gen {
            let! count = Gen.choose (1, 24)

            return
                {
                    Regime = LayoutRegime.AllOptional
                    Fields = List.replicate count Optional |> fields
                }
        }

    let private mixedGen : Gen<ConstructorLayout> =
        gen {
            let! requiredCount = Gen.choose (1, 12)
            let! optionalCount = Gen.choose (1, 12)

            let kinds = requiredKinds requiredCount @ List.replicate optionalCount Optional
            let! shuffled = Gen.shuffle kinds

            return
                {
                    Regime = LayoutRegime.Mixed
                    Fields = shuffled |> Array.toList |> fields
                }
        }

    let private layoutGen : Gen<ConstructorLayout> =
        Gen.oneof [ allRequiredGen ; allOptionalGen ; mixedGen ]

    let private layouts = Arb.fromGen layoutGen

    let private expectedParameters (input : ConstructorField<string, string> list) : ConstructorParameter<string> list =
        input
        |> List.choose (fun field ->
            match field.Kind with
            | Required fieldType -> Some fieldType
            | Optional -> None
        )
        |> List.mapi (fun index fieldType ->
            {
                Name = $"arg_{index}"
                Type = fieldType
            }
        )

    let private expectedAssignments
        (input : ConstructorField<string, string> list)
        : ConstructorAssignment<string> list
        =
        let folder requiredIndex field =
            match field.Kind with
            | Optional ->
                {
                    FieldName = field.Name
                    Initializer = UseNone
                },
                requiredIndex
            | Required _ ->
                {
                    FieldName = field.Name
                    Initializer = FromParameter $"arg_{requiredIndex}"
                },
                requiredIndex + 1

        input |> List.mapFold folder 0 |> fst

    let private checkProperty (property : ConstructorLayout -> unit) : unit =
        Check.One (Config.QuickThrowOnFailure.WithMaxTest 500, property |> Prop.forAll layouts)

    [<Test>]
    let ``assignments contain every field exactly once and in declaration order`` () =
        let property layout =
            let actual = ConstructorPlan.create layout.Fields

            actual.Assignments
            |> List.map _.FieldName
            |> shouldEqual (layout.Fields |> List.map _.Name)

            actual.Assignments.Length |> shouldEqual layout.Fields.Length

        checkProperty property

    [<Test>]
    let ``parameters are precisely the required field types in declaration order`` () =
        let property layout =
            let actual = ConstructorPlan.create layout.Fields
            actual.Parameters |> shouldEqual (expectedParameters layout.Fields)

        checkProperty property

    [<Test>]
    let ``optional fields use None and required fields use their unique parameter`` () =
        let property layout =
            let actual = ConstructorPlan.create layout.Fields
            actual.Assignments |> shouldEqual (expectedAssignments layout.Fields)

            actual.Parameters
            |> List.map _.Name
            |> shouldEqual (List.init actual.Parameters.Length (fun index -> $"arg_{index}"))

            actual.Parameters
            |> List.map _.Name
            |> List.distinct
            |> shouldEqual (actual.Parameters |> List.map _.Name)

        checkProperty property

    [<Test>]
    let ``planning is deterministic and field-name independent`` () =
        let property layout =
            let actual = ConstructorPlan.create layout.Fields
            ConstructorPlan.create layout.Fields |> shouldEqual actual

            let renamedFields =
                layout.Fields
                |> List.map (fun field ->
                    { field with
                        Name = $"Renamed_{field.Name}"
                    }
                )

            let renamed = ConstructorPlan.create renamedFields
            renamed.Parameters |> shouldEqual actual.Parameters

            renamed.Assignments
            |> shouldEqual (
                actual.Assignments
                |> List.map (fun assignment ->
                    { assignment with
                        FieldName = $"Renamed_{assignment.FieldName}"
                    }
                )
            )

        checkProperty property

    [<Test>]
    let ``layout generator covers required optional and mixed regimes`` () =
        let regimeCounts = Array.zeroCreate<int> 3
        let parameterCounts = Array.zeroCreate<int> 3
        let mutable optionalBeforeRequired = 0
        let mutable optionalBetweenRequired = 0
        let mutable optionalAfterRequired = 0

        let property layout =
            let regimeIndex =
                match layout.Regime with
                | LayoutRegime.AllRequired -> 0
                | LayoutRegime.AllOptional -> 1
                | LayoutRegime.Mixed -> 2

            regimeCounts.[regimeIndex] <- regimeCounts.[regimeIndex] + 1

            let requiredIndices =
                layout.Fields
                |> List.indexed
                |> List.choose (fun (index, field) ->
                    match field.Kind with
                    | Required _ -> Some index
                    | Optional -> None
                )

            let optionalCount = layout.Fields.Length - requiredIndices.Length

            layout.Fields
            |> List.map _.Name
            |> List.distinct
            |> List.length
            |> shouldEqual layout.Fields.Length

            match layout.Regime with
            | LayoutRegime.AllRequired ->
                requiredIndices.Length |> shouldEqual layout.Fields.Length
                optionalCount |> shouldEqual 0
            | LayoutRegime.AllOptional ->
                requiredIndices.Length |> shouldEqual 0
                optionalCount |> shouldEqual layout.Fields.Length
            | LayoutRegime.Mixed ->
                requiredIndices.Length |> shouldBeGreaterThan 0
                optionalCount |> shouldBeGreaterThan 0

            match requiredIndices.Length with
            | 0 -> parameterCounts.[0] <- parameterCounts.[0] + 1
            | 1 -> parameterCounts.[1] <- parameterCounts.[1] + 1
            | _ -> parameterCounts.[2] <- parameterCounts.[2] + 1

            match requiredIndices with
            | [] -> ()
            | firstRequired :: _ ->
                let lastRequired = List.last requiredIndices

                let optionalIndices =
                    layout.Fields
                    |> List.indexed
                    |> List.choose (fun (index, field) ->
                        match field.Kind with
                        | Optional -> Some index
                        | Required _ -> None
                    )

                if optionalIndices |> List.exists (fun index -> index < firstRequired) then
                    optionalBeforeRequired <- optionalBeforeRequired + 1

                if
                    optionalIndices
                    |> List.exists (fun index -> firstRequired < index && index < lastRequired)
                then
                    optionalBetweenRequired <- optionalBetweenRequired + 1

                if optionalIndices |> List.exists (fun index -> lastRequired < index) then
                    optionalAfterRequired <- optionalAfterRequired + 1

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 1000, property |> Prop.forAll layouts)

        regimeCounts |> Array.sum |> shouldEqual 1000

        for count in regimeCounts do
            count |> shouldBeGreaterThan 200

        parameterCounts.[0] |> shouldEqual regimeCounts.[1]
        parameterCounts.[1] |> shouldBeGreaterThan 0
        parameterCounts.[2] |> shouldBeGreaterThan 200
        optionalBeforeRequired |> shouldBeGreaterThan 20
        optionalBetweenRequired |> shouldBeGreaterThan 20
        optionalAfterRequired |> shouldBeGreaterThan 20
