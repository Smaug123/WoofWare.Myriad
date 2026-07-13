namespace WoofWare.Myriad.Plugins.Test

open System
open Fantomas.FCS.Syntax
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.Myriad.Plugins
open WoofWare.Whippet.Fantomas

[<TestFixture>]
module TestRecordConstructorGenerator =

    let private annotatedRecordSource (fieldTypes : string list) : string =
        [
            "namespace GeneratedProperty"
            ""
            "[<WoofWare.Myriad.Plugins.GenerateRecordConstructor>]"
            "type GeneratedRecord ="
            "    {"

            for index, fieldType in List.indexed fieldTypes do
                $"        Field_{index} : %s{fieldType}"

            "    }"
        ]
        |> String.concat "\n"

    let private selectedRecordAndPlan (source : string) =
        let ast = Ast.parse source
        let selected = RecordConstructorGenerator.select ast

        RecordConstructorGenerator.generate ast
        |> List.length
        |> shouldEqual selected.Length

        let record = selected |> List.exactlyOne |> _.Record
        record, RecordConstructorGenerator.plan record

    let private selectedPlan (source : string) = source |> selectedRecordAndPlan |> snd

    let private parameterNames (plan : ConstructorPlan<_, _>) : string list = plan.Parameters |> List.map _.Name

    [<Test>]
    let ``parsed record plans preserve every initializer`` () =
        let fieldLayoutGen =
            gen {
                let! count = Gen.choose (1, 24)
                let! optionalFields = Gen.listOfLength count (ArbMap.generate<bool> ArbMap.defaults)
                return optionalFields
            }

        let property (optionalFields : bool list) : unit =
            let fieldTypes =
                optionalFields
                |> List.mapi (fun index isOptional ->
                    let fieldType = "int" + String.replicate index " list"

                    if isOptional then $"({fieldType}) option" else fieldType
                )

            let record, plan = annotatedRecordSource fieldTypes |> selectedRecordAndPlan

            let expectedParameters =
                optionalFields |> List.filter not |> List.mapi (fun index _ -> $"arg_{index}")

            parameterNames plan |> shouldEqual expectedParameters

            let expectedParameterTypes =
                (optionalFields, record.Fields)
                ||> List.map2 (fun isOptional field ->
                    if isOptional then
                        None
                    else
                        field |> SynField.extractWithIdent |> _.Type |> Some
                )
                |> List.choose id

            plan.Parameters
            |> List.map (fun parameter -> SynType.toHumanReadableString parameter.Type)
            |> shouldEqual (expectedParameterTypes |> List.map SynType.toHumanReadableString)

            let expectedAssignments =
                let folder requiredIndex (fieldIndex, isOptional) =
                    if isOptional then
                        (fieldIndex, "None"), requiredIndex
                    else
                        (fieldIndex, $"arg_{requiredIndex}"), requiredIndex + 1

                optionalFields |> List.indexed |> List.mapFold folder 0 |> fst

            let actualAssignments =
                plan.Assignments
                |> List.map (fun assignment ->
                    let initializer =
                        match assignment.Initializer with
                        | FromParameter name -> name
                        | UseNone -> "None"

                    let fieldIndex =
                        assignment.FieldName.idText.Substring ("Field_".Length) |> Int32.Parse

                    fieldIndex, initializer
                )

            actualAssignments |> shouldEqual expectedAssignments

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 500, property |> Prop.forAll (Arb.fromGen fieldLayoutGen))

    [<TestCase("int option", 0)>]
    [<TestCase("option<int>", 0)>]
    [<TestCase("Option<int>", 0)>]
    [<TestCase("Microsoft.FSharp.Core.option<int>", 0)>]
    [<TestCase("int option option", 0)>]
    [<TestCase("int option list", 1)>]
    [<TestCase("int voption", 1)>]
    [<TestCase("System.Nullable<int>", 1)>]
    let ``only an outer syntactic option removes a parameter`` (fieldType : string, expectedParameterCount : int) =
        let actual =
            annotatedRecordSource [ fieldType ]
            |> selectedPlan
            |> parameterNames
            |> List.length

        actual |> shouldEqual expectedParameterCount

    [<Test>]
    let ``an option type alias remains a required parameter`` () =
        let source =
            [
                "namespace GeneratedProperty"
                ""
                "type Alias<'value> = 'value option"
                ""
                "[<WoofWare.Myriad.Plugins.GenerateRecordConstructor>]"
                "type GeneratedRecord = { Field_0 : Alias<int> }"
            ]
            |> String.concat "\n"

        source |> selectedPlan |> parameterNames |> shouldEqual [ "arg_0" ]

    [<Test>]
    let ``selection emits exactly annotated records in their namespaces`` () =
        let source =
            [
                "namespace FirstNamespace"
                ""
                "[<WoofWare.Myriad.Plugins.GenerateRecordConstructor>]"
                "type FirstRecord = { Required : int }"
                ""
                "type UnannotatedRecord = { Required : int }"
                ""
                "namespace SecondNamespace"
                ""
                "[<WoofWare.Myriad.Plugins.GenerateRecordConstructor>]"
                "type SecondRecord = { Optional : int option }"
            ]
            |> String.concat "\n"

        let ast = Ast.parse source
        RecordConstructorGenerator.generate ast |> List.length |> shouldEqual 2

        RecordConstructorGenerator.select ast
        |> List.map (fun selection ->
            selection.NamespaceId |> List.map _.idText |> String.concat ".", selection.Record.Name.idText
        )
        |> shouldEqual [ "FirstNamespace", "FirstRecord" ; "SecondNamespace", "SecondRecord" ]

    [<Test>]
    let ``opens remain scoped to the namespace containing each record`` () =
        let source =
            [
                "namespace FirstInput"
                "type Token = FirstToken"
                ""
                "namespace FirstConsumer"
                "open FirstInput"
                "[<WoofWare.Myriad.Plugins.GenerateRecordConstructor>]"
                "type FirstRecord = { Token : Token }"
                ""
                "namespace SecondInput"
                "type Token = SecondToken"
                ""
                "namespace SecondConsumer"
                "open SecondInput"
                "[<WoofWare.Myriad.Plugins.GenerateRecordConstructor>]"
                "type SecondRecord = { Token : Token }"
            ]
            |> String.concat "\n"

        let openNames =
            source
            |> Ast.parse
            |> RecordConstructorGenerator.select
            |> List.map (fun selection ->
                selection.Opens
                |> List.map (fun target ->
                    match target with
                    | SynOpenDeclTarget.ModuleOrNamespace (name, _) -> SynLongIdent.toString name
                    | SynOpenDeclTarget.Type _ -> failwith "Expected a namespace open."
                )
            )

        openNames |> shouldEqual [ [ "FirstInput" ] ; [ "SecondInput" ] ]

    [<Test>]
    let ``attribute on a non-record is rejected`` () =
        let source =
            [
                "namespace GeneratedProperty"
                ""
                "[<WoofWare.Myriad.Plugins.GenerateRecordConstructor>]"
                "type NotARecord = First | Second"
            ]
            |> String.concat "\n"

        let thrown =
            Assert.Throws<Exception> (fun () -> source |> Ast.parse |> RecordConstructorGenerator.generate |> ignore)

        thrown.Message
        |> shouldEqual "[<GenerateRecordConstructor>] may only be applied to record types. Type: NotARecord"

    [<Test>]
    let ``private record representation is rejected explicitly`` () =
        let source =
            [
                "namespace GeneratedProperty"
                ""
                "[<WoofWare.Myriad.Plugins.GenerateRecordConstructor>]"
                "type PrivateRecord = private { Required : int }"
            ]
            |> String.concat "\n"

        let thrown =
            Assert.Throws<Exception> (fun () -> source |> Ast.parse |> RecordConstructorGenerator.generate |> ignore)

        thrown.Message
        |> shouldEqual (
            "[<GenerateRecordConstructor>] cannot be applied to a record with a private representation. Type: PrivateRecord"
        )

    [<Test>]
    let ``annotation inside a nested module is rejected explicitly`` () =
        let source =
            [
                "namespace GeneratedProperty"
                ""
                "module Nested ="
                "    [<WoofWare.Myriad.Plugins.GenerateRecordConstructor>]"
                "    type NestedRecord = { Required : int }"
            ]
            |> String.concat "\n"

        let thrown =
            Assert.Throws<Exception> (fun () -> source |> Ast.parse |> RecordConstructorGenerator.generate |> ignore)

        thrown.Message
        |> shouldEqual (
            "[<GenerateRecordConstructor>] may only be applied to namespace-level records. Nested type: GeneratedProperty.Nested.NestedRecord"
        )
