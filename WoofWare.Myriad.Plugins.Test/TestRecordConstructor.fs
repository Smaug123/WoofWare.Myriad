namespace WoofWare.Myriad.Plugins.Test

open System
open ConsumePlugin
open FsCheck
open FsUnitTyped
open NUnit.Framework

[<TestFixture>]
module TestRecordConstructor =

    [<Test>]
    let ``create takes required fields in declaration order and defaults options`` () =
        let property (firstValue : string) (secondValue : string) (thirdValue : string) : unit =
            let first = $"first:%s{firstValue}"
            let second = $"second:%s{secondValue}"
            let third = $"third:%s{thirdValue}"
            let actual = MixedConstructorRecord.create first second third

            actual
            |> shouldEqual
                {
                    FirstRequired = first
                    FirstOptional = None
                    SecondRequired = second
                    SecondOptional = None
                    ThirdRequired = third
                }

        Check.QuickThrowOnFailure property

    [<Test>]
    let ``create preserves every all-required argument`` () =
        let actual = AllRequiredConstructorRecord.create "first" "second" "third"

        actual
        |> shouldEqual
            {
                First = "first"
                Second = "second"
                Third = "third"
            }

    [<Test>]
    let ``all-optional create takes unit`` () =
        let actual = AllOptionalConstructorRecord.create ()

        actual
        |> shouldEqual
            {
                Number = None
                Text = None
            }

    [<Test>]
    let ``nested and prefix options default at the outermost level`` () =
        let id = Guid.Parse "4d36e967-e325-11ce-bfc1-08002be10318"
        let actual = NestedOptionConstructorRecord.create id

        actual
        |> shouldEqual
            {
                Id = id
                PostfixNested = None
                PrefixOption = None
            }

    [<Test>]
    let ``generic create retains required and optional type parameters`` () =
        let actual : GenericConstructorRecord<int, string> =
            GenericConstructorRecord.create 42 [ 1 ; 2 ; 3 ]

        actual
        |> shouldEqual
            {
                Required = 42
                Optional = None
                RequiredList = [ 1 ; 2 ; 3 ]
            }

        let allOptional : GenericAllOptionalConstructorRecord<Guid> =
            GenericAllOptionalConstructorRecord.create ()

        allOptional
        |> shouldEqual
            {
                Value = None
            }

    [<Test>]
    let ``internal records receive internal constructors`` () =
        let actual = InternalConstructorRecord.create "required"

        actual
        |> shouldEqual
            {
                Required = "required"
                Optional = None
            }

    [<Test>]
    let ``tupled and function fields remain single curried arguments`` () =
        let transform value = $"value: %i{value}"
        let actual = TupleAndFunctionConstructorRecord.create (42, "answer") transform

        actual.Pair |> shouldEqual (42, "answer")
        actual.Transform 7 |> shouldEqual "value: 7"
        actual.OptionalTransform |> Option.isNone |> shouldEqual true

    [<Test>]
    let ``only syntactic option fields are omitted`` () =
        let actual =
            NonSyntacticOptionConstructorRecord.create (Some 42) (ValueSome "value") (Nullable 7)

        actual
        |> shouldEqual
            {
                Alias = Some 42
                ValueOption = ValueSome "value"
                Nullable = Nullable 7
                Optional = None
            }

    [<Test>]
    let ``generated parameter names do not depend on record labels`` () =
        let actual = NameHazardConstructorRecord.create "keyword" "lower" "upper" "spaces"

        actual.``type`` |> shouldEqual "keyword"
        actual.arg_0 |> shouldEqual "lower"
        actual.Arg_0 |> shouldEqual "upper"
        actual.``two words`` |> shouldEqual "spaces"
        actual.``two-words`` |> shouldEqual None

    [<Test>]
    let ``generated parameter types retain namespace-scoped opens`` () =
        let firstToken = RecordConstructorFirstInput.Token.FirstToken 42
        let first = ConsumePlugin.FirstScopedOpen.ScopedOpenRecord.create firstToken
        first.Token |> shouldEqual firstToken
        first.Optional |> shouldEqual None

        let secondToken = RecordConstructorSecondInput.Token.SecondToken "value"
        let second = ConsumePlugin.SecondScopedOpen.ScopedOpenRecord.create secondToken
        second.Token |> shouldEqual secondToken
        second.Optional |> shouldEqual None
