namespace WoofWare.Myriad.Plugins.Test

open System.Collections.Generic
open Fantomas.FCS.Syntax
open NUnit.Framework
open FsUnitTyped
open WoofWare.Myriad.Plugins
open WoofWare.Myriad.Plugins.SwaggerV2

[<TestFixture>]
module TestSwaggerTypeRender =

    let private nameOfSynType (ty : SynType) : string =
        match ty with
        | SynType.LongIdent (SynLongIdent (idents, _, _)) -> idents |> List.map _.idText |> String.concat "."
        | ty -> failwith $"unexpectedly got a non-LongIdent SynType: %+A{ty}"

    let integerCases =
        [ None, "int" ; Some "int32", "int" ; Some "int64", "int64" ]
        |> List.map TestCaseData

    [<TestCaseSource(nameof integerCases)>]
    let ``defnToType respects integer formats`` (format : string option, expected : string) : unit =
        let result =
            SwaggerClientGenerator.defnToType
                (fun () -> failwith "no anonymous types expected")
                (Dictionary ())
                (Dictionary ())
                "definitions"
                None
                (Definition.Integer format)

        match result with
        | None -> failwith "expected a type"
        | Some entry -> nameOfSynType entry.Signature |> shouldEqual expected

    [<TestCaseSource(nameof integerCases)>]
    let ``renderType respects integer formats`` (format : string option, expected : string) : unit =
        let types : Types =
            {
                ByHandle = Dictionary ()
                ByDefinition = Dictionary ()
            }

        match SwaggerClientGenerator.renderType types (Definition.Integer format) with
        | None -> failwith "expected a type"
        | Some ty -> nameOfSynType ty |> shouldEqual expected
