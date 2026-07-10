namespace WoofWare.Myriad.Plugins.Test

open FsCheck
open FsCheck.FSharp
open NUnit.Framework
open FsUnitTyped
open WoofWare.Myriad.Plugins
open WoofWare.Myriad.Plugins.SwaggerV2

[<TestFixture>]
module TestSuccessResponse =

    /// Distinct definitions, so that we can tell which one came back.
    let private defnGen : Gen<Definition> =
        [
            Definition.String
            Definition.Boolean
            Definition.Unspecified
            Definition.File
            Definition.Handle "#/definitions/Error"
            Definition.Integer (Some "int64")
        ]
        |> Gen.elements

    let private successCodeGen : Gen<ResponseKey> =
        Gen.choose (200, 299) |> Gen.map ResponseKey.Code

    /// Any key which is neither an explicit 2xx code nor `default`.
    let private errorCodeGen : Gen<ResponseKey> =
        Gen.oneof [ Gen.choose (100, 199) ; Gen.choose (300, 599) ]
        |> Gen.map ResponseKey.Code

    let private mapGen (keys : Gen<ResponseKey>) : Gen<Map<ResponseKey, Definition>> =
        gen {
            let! keys = Gen.listOf keys
            let keys = List.distinct keys
            let! values = Gen.listOfLength keys.Length defnGen
            return List.zip keys values |> Map.ofList
        }

    /// An arbitrary response map: some 2xx codes, some error codes, maybe a `default`.
    let private responsesGen : Gen<Map<ResponseKey, Definition>> =
        gen {
            let! successes = mapGen successCodeGen
            let! errors = mapGen errorCodeGen
            let! deflt = Gen.optionOf defnGen

            let deflt =
                match deflt with
                | None -> Map.empty
                | Some d -> Map.ofList [ ResponseKey.Default, d ]

            return
                Seq.concat [ Map.toSeq successes ; Map.toSeq errors ; Map.toSeq deflt ]
                |> Map.ofSeq
        }

    [<Test>]
    let ``an Exactly result is always one of the declared schemas`` () : unit =
        let property (responses : Map<ResponseKey, Definition>) : bool =
            match SwaggerClientGenerator.successResponse responses with
            | SuccessResponse.Exactly defn -> responses |> Map.exists (fun _ v -> v = defn)
            | SuccessResponse.Ambiguous
            | SuccessResponse.Missing -> true

        Prop.forAll (Arb.fromGen responsesGen) property |> Check.QuickThrowOnFailure

    [<Test>]
    let ``responses for non-2xx codes never affect the result`` () : unit =
        let property (responses : Map<ResponseKey, Definition>, errors : Map<ResponseKey, Definition>) : bool =
            let withoutErrors =
                responses
                |> Map.filter (fun k _ ->
                    match k with
                    | ResponseKey.Code code -> 200 <= code && code < 300
                    | ResponseKey.Default -> true
                )

            let withErrors =
                (withoutErrors, Map.toSeq errors)
                ||> Seq.fold (fun acc (k, v) -> Map.add k v acc)

            SwaggerClientGenerator.successResponse withErrors = SwaggerClientGenerator.successResponse withoutErrors

        Prop.forAll (Arb.fromGen (Gen.zip responsesGen (mapGen errorCodeGen))) property
        |> Check.QuickThrowOnFailure

    [<Test>]
    let ``the default response is ignored whenever an explicit 2xx response exists`` () : unit =
        let property (responses : Map<ResponseKey, Definition>, deflt : Definition) : bool =
            let hasSuccess =
                responses
                |> Map.exists (fun k _ ->
                    match k with
                    | ResponseKey.Code code -> 200 <= code && code < 300
                    | ResponseKey.Default -> false
                )

            if not hasSuccess then
                true
            else

            let withoutDefault = Map.remove ResponseKey.Default responses
            let withDefault = Map.add ResponseKey.Default deflt responses

            SwaggerClientGenerator.successResponse withDefault = SwaggerClientGenerator.successResponse withoutDefault

        Prop.forAll (Arb.fromGen (Gen.zip responsesGen defnGen)) property
        |> Check.QuickThrowOnFailure

    [<Test>]
    let ``the default response is the return type whenever no explicit 2xx response exists`` () : unit =
        let property (errors : Map<ResponseKey, Definition>, deflt : Definition) : bool =
            let responses = Map.add ResponseKey.Default deflt errors

            SwaggerClientGenerator.successResponse responses = SuccessResponse.Exactly deflt

        Prop.forAll (Arb.fromGen (Gen.zip (mapGen errorCodeGen) defnGen)) property
        |> Check.QuickThrowOnFailure

    [<Test>]
    let ``exactly one 2xx response is that response`` () : unit =
        [
            ResponseKey.Code 200, Definition.String
            ResponseKey.Default, Definition.Handle "#/definitions/Error"
        ]
        |> Map.ofList
        |> SwaggerClientGenerator.successResponse
        |> shouldEqual (SuccessResponse.Exactly Definition.String)

    [<Test>]
    let ``a default-only response map falls back to the default`` () : unit =
        [ ResponseKey.Default, Definition.Handle "#/definitions/Error" ]
        |> Map.ofList
        |> SwaggerClientGenerator.successResponse
        |> shouldEqual (SuccessResponse.Exactly (Definition.Handle "#/definitions/Error"))

    [<Test>]
    let ``errors plus a default falls back to the default`` () : unit =
        [
            ResponseKey.Code 404, Definition.Unspecified
            ResponseKey.Default, Definition.Handle "#/definitions/Error"
        ]
        |> Map.ofList
        |> SwaggerClientGenerator.successResponse
        |> shouldEqual (SuccessResponse.Exactly (Definition.Handle "#/definitions/Error"))

    [<Test>]
    let ``no success response and no default is Missing`` () : unit =
        [ ResponseKey.Code 404, Definition.Unspecified ]
        |> Map.ofList
        |> SwaggerClientGenerator.successResponse
        |> shouldEqual SuccessResponse.Missing

    [<Test>]
    let ``an empty response map is Missing`` () : unit =
        Map.empty
        |> SwaggerClientGenerator.successResponse
        |> shouldEqual SuccessResponse.Missing

    [<Test>]
    let ``multiple 2xx responses are Ambiguous`` () : unit =
        [
            ResponseKey.Code 200, Definition.String
            ResponseKey.Code 201, Definition.Boolean
            ResponseKey.Default, Definition.Handle "#/definitions/Error"
        ]
        |> Map.ofList
        |> SwaggerClientGenerator.successResponse
        |> shouldEqual SuccessResponse.Ambiguous
