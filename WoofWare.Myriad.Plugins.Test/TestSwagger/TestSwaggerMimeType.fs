namespace WoofWare.Myriad.Plugins.Test

open FsCheck
open FsCheck.FSharp
open NUnit.Framework
open FsUnitTyped
open WoofWare.Myriad.Plugins
open WoofWare.Myriad.Plugins.SwaggerV2

[<TestFixture>]
module TestSwaggerMimeType =

    [<TestCase("application/json", true)>]
    [<TestCase("application/json; charset=utf-8", true)>]
    [<TestCase("APPLICATION/JSON", true)>]
    [<TestCase("  application/json  ", true)>]
    [<TestCase("application/vnd.api+json", true)>]
    [<TestCase("application/problem+json; charset=utf-8", true)>]
    [<TestCase("text/plain", false)>]
    [<TestCase("text/html", false)>]
    [<TestCase("application/xml", false)>]
    [<TestCase("application/octet-stream", false)>]
    [<TestCase("application/jsonish", false)>]
    let ``isJsonMimeType recognises JSON media types`` (mime : string, expected : bool) : unit =
        SwaggerClientGenerator.isJsonMimeType (MimeType mime) |> shouldEqual expected

    /// No handle can be resolved: exercises the raw-vs-JSON classification of the leaf types.
    let private noHandles (_ : string) : Definition option = None

    [<Test>]
    let ``a string payload is raw text and a file payload is opaque`` () : unit =
        SwaggerClientGenerator.classifyPayload noHandles Definition.String
        |> shouldEqual (Some PayloadShape.RawText)

        SwaggerClientGenerator.classifyPayload noHandles Definition.File
        |> shouldEqual (Some PayloadShape.Opaque)

    [<Test>]
    let ``a schema-less definition is no payload at all`` () : unit =
        // E.g. a 204 No Content response: nothing travels, so there is nothing for a MIME
        // type to describe.
        SwaggerClientGenerator.classifyPayload noHandles Definition.Unspecified
        |> shouldEqual None

        // ... even behind a `$ref`.
        let resolve =
            function
            | "#/responses/empty" -> Some Definition.Unspecified
            | _ -> None

        SwaggerClientGenerator.classifyPayload resolve (Definition.Handle "#/responses/empty")
        |> shouldEqual None

    [<Test>]
    let ``object, array, and scalar payloads require JSON serialisation`` () : unit =
        let objDefn =
            Definition.Object
                {
                    Properties = None
                    Required = None
                    AdditionalProperties = None
                    Description = None
                    Extras = Map.empty
                    Example = None
                }

        let arrDefn =
            Definition.Array
                {
                    Items = Definition.String
                }

        for defn in
            [
                objDefn
                arrDefn
                Definition.Boolean
                Definition.Integer None
                Definition.Integer (Some "int64")
            ] do
            SwaggerClientGenerator.classifyPayload noHandles defn
            |> shouldEqual (Some PayloadShape.Json)

    [<Test>]
    let ``a handle is classified by what it resolves to`` () : unit =
        let resolve =
            function
            | "#/definitions/RawText" -> Some Definition.String
            | "#/definitions/Thing" ->
                Definition.Object
                    {
                        Properties = None
                        Required = None
                        AdditionalProperties = None
                        Description = None
                        Extras = Map.empty
                        Example = None
                    }
                |> Some
            | _ -> None

        SwaggerClientGenerator.classifyPayload resolve (Definition.Handle "#/definitions/RawText")
        |> shouldEqual (Some PayloadShape.RawText)

        SwaggerClientGenerator.classifyPayload resolve (Definition.Handle "#/definitions/Thing")
        |> shouldEqual (Some PayloadShape.Json)

    [<Test>]
    let ``an unresolvable handle is conservatively JSON`` () : unit =
        SwaggerClientGenerator.classifyPayload noHandles (Definition.Handle "#/definitions/Missing")
        |> shouldEqual (Some PayloadShape.Json)

    [<Test>]
    let ``a circular handle chain terminates and is JSON`` () : unit =
        // A points at B points back at A: we can't prove the payload is raw, so we call it JSON
        // rather than looping forever.
        let resolve =
            function
            | "#/definitions/A" -> Some (Definition.Handle "#/definitions/B")
            | "#/definitions/B" -> Some (Definition.Handle "#/definitions/A")
            | _ -> None

        SwaggerClientGenerator.classifyPayload resolve (Definition.Handle "#/definitions/A")
        |> shouldEqual (Some PayloadShape.Json)

    // ---- checkMimeCompatibility: the exact wiring used by the generator ----

    let private objectDefn : Definition =
        Definition.Object
            {
                Properties = None
                Required = None
                AdditionalProperties = None
                Description = None
                Extras = Map.empty
                Example = None
            }

    let private check
        (consumes : string option)
        (produces : string option)
        (bodyType : Definition option)
        (returnType : Definition)
        : unit
        =
        SwaggerClientGenerator.checkMimeCompatibility
            noHandles
            "/thing"
            HttpMethod.Get
            (consumes |> Option.map MimeType)
            (produces |> Option.map MimeType)
            bodyType
            returnType

    [<Test>]
    let ``producing a non-JSON MIME with an object response is rejected`` () : unit =
        let e =
            Assert.Throws<exn> (fun () -> check None (Some "application/xml") None objectDefn)

        e.Message.Contains "application/xml" |> shouldEqual true

    [<Test>]
    let ``producing a non-JSON MIME with a raw string response is allowed`` () : unit =
        // text/plain plus a string response is exactly Gitea's diff/patch endpoints: the client
        // reads the body verbatim, so any MIME type is fine.
        check None (Some "text/plain") None Definition.String

    [<Test>]
    let ``producing a non-JSON MIME with a stream response is allowed`` () : unit =
        check None (Some "application/octet-stream") None Definition.File

    [<Test>]
    let ``producing a JSON MIME with an object response is allowed`` () : unit =
        check None (Some "application/json") None objectDefn

    [<Test>]
    let ``producing a +json MIME with an object response is allowed`` () : unit =
        check None (Some "application/vnd.api+json") None objectDefn

    [<Test>]
    let ``an absent produces MIME is allowed for any response`` () : unit = check None None None objectDefn

    [<Test>]
    let ``consuming a non-JSON MIME with an object body is rejected`` () : unit =
        let e =
            Assert.Throws<exn> (fun () -> check (Some "application/xml") None (Some objectDefn) Definition.String)

        e.Message.Contains "application/xml" |> shouldEqual true

    [<Test>]
    let ``consuming a non-JSON MIME with a raw string body is allowed`` () : unit =
        // text/plain plus a string body is Gitea's renderMarkdownRaw: the string is sent verbatim.
        check (Some "text/plain") None (Some Definition.String) Definition.String

    [<Test>]
    let ``consuming a non-JSON MIME with no body param is allowed`` () : unit =
        // No request body means there's nothing for the Content-Type to mislabel.
        check (Some "application/xml") None None Definition.String

    [<Test>]
    let ``producing a JSON MIME with a raw string response is rejected`` () : unit =
        // The generated client returns the response body verbatim, but a JSON string
        // payload arrives quoted and escaped; returning it raw would hand the caller
        // the quotes.
        let e =
            Assert.Throws<exn> (fun () -> check None (Some "application/json") None Definition.String)

        e.Message.Contains "application/json" |> shouldEqual true

    [<Test>]
    let ``producing a parameterised JSON MIME with a raw string response is rejected`` () : unit =
        Assert.Throws<exn> (fun () -> check None (Some "application/json; charset=utf-8") None Definition.String)
        |> ignore<exn>

    [<Test>]
    let ``consuming a JSON MIME with a raw string body is rejected`` () : unit =
        // The generated client sends a string body verbatim, but Content-Type:
        // application/json promises a quoted, escaped JSON string.
        let e =
            Assert.Throws<exn> (fun () ->
                check (Some "application/json") None (Some Definition.String) Definition.String
            )

        e.Message.Contains "application/json" |> shouldEqual true

    [<Test>]
    let ``producing a JSON MIME with a stream response is allowed`` () : unit =
        // A `file` response is raw bytes handed straight to the caller, which is faithful
        // whatever the MIME type claims the bytes are.
        check None (Some "application/json") None Definition.File

    [<Test>]
    let ``consuming a JSON MIME with a file body is allowed`` () : unit =
        // Likewise a `file` body: the caller supplies the exact wire bytes.
        check (Some "application/json") None (Some Definition.File) Definition.String

    [<Test>]
    let ``a schema-less response is compatible with any produces MIME`` () : unit =
        // A 204-style response carries nothing, so no MIME type can mislabel it.
        check None (Some "application/xml") None Definition.Unspecified
        check None (Some "application/json") None Definition.Unspecified

    // ---- selectMimeType: boiling a Consumes/Produces list down to one MIME type ----

    let private selectFor
        (payload : PayloadShape option)
        (globalLevel : string list)
        (endpointLevel : string list option)
        : MimeType option
        =
        SwaggerClientGenerator.selectMimeType
            MimeUsage.Consumes
            "/thing"
            HttpMethod.Get
            payload
            (globalLevel |> List.map MimeType)
            (endpointLevel |> Option.map (List.map MimeType))

    /// Most selection behaviour is payload-independent; exercise it with a JSON payload.
    let private select = selectFor (Some PayloadShape.Json)

    [<Test>]
    let ``an endpoint-level MIME type overrides the global list entirely`` () : unit =
        select [ "application/json" ; "text/html" ] (Some [ "text/plain" ])
        |> shouldEqual (Some (MimeType "text/plain"))

    [<Test>]
    let ``an empty endpoint-level list is rejected`` () : unit =
        Assert.Throws<exn> (fun () -> select [ "application/json" ] (Some []) |> ignore)
        |> ignore<exn>

    [<Test>]
    let ``multiple endpoint-level MIME types are rejected`` () : unit =
        Assert.Throws<exn> (fun () -> select [] (Some [ "application/json" ; "text/plain" ]) |> ignore)
        |> ignore<exn>

    [<Test>]
    let ``no preference anywhere means no MIME type`` () : unit = select [] None |> shouldEqual None

    [<Test>]
    let ``a sole global MIME type is used even when it is not JSON`` () : unit =
        select [ "text/plain" ] None |> shouldEqual (Some (MimeType "text/plain"))

    [<Test>]
    let ``an ambiguous global list is resolved by the sole JSON-capable entry`` () : unit =
        // `isJsonMimeType` accepts the +json structured-suffix family, so this list has
        // exactly one entry the generated client can honour for a JSON payload.
        select [ "application/vnd.api+json" ; "application/xml" ] None
        |> shouldEqual (Some (MimeType "application/vnd.api+json"))

    [<Test>]
    let ``an ambiguous global list is resolved by canonical application/json`` () : unit =
        select [ "text/html" ; "application/json" ] None
        |> shouldEqual (Some (MimeType "application/json"))

    [<Test>]
    let ``canonical application/json is recognised despite case and parameters`` () : unit =
        // The selected value is passed through verbatim; only the comparison is normalised.
        select [ "APPLICATION/JSON; charset=utf-8" ; "application/xml" ] None
        |> shouldEqual (Some (MimeType "APPLICATION/JSON; charset=utf-8"))

    [<Test>]
    let ``canonical application/json beats other JSON flavours`` () : unit =
        select [ "application/vnd.api+json" ; "application/json" ] None
        |> shouldEqual (Some (MimeType "application/json"))

    [<Test>]
    let ``an ambiguous global list with no entry fitting a JSON payload is rejected`` () : unit =
        Assert.Throws<exn> (fun () -> select [ "text/html" ; "application/xml" ] None |> ignore)
        |> ignore<exn>

    [<Test>]
    let ``an ambiguous global list with several non-canonical JSON entries is rejected`` () : unit =
        Assert.Throws<exn> (fun () ->
            select [ "application/vnd.api+json" ; "application/problem+json" ] None
            |> ignore
        )
        |> ignore<exn>

    [<Test>]
    let ``no payload means no MIME type, whatever the spec offers`` () : unit =
        // A bodyless endpoint has no Consumes negotiation to resolve, so ambiguity in the
        // global list is irrelevant to it — even an otherwise-unresolvable one.
        selectFor None [ "text/plain" ; "application/xml" ] None |> shouldEqual None

        selectFor None [ "text/plain" ; "application/xml" ] (Some [ "text/plain" ])
        |> shouldEqual None

    [<Test>]
    let ``a raw string payload selects the sole non-JSON option`` () : unit =
        // The generated client sends/receives strings verbatim, so application/json (which
        // would demand JSON quoting) is not an option for it.
        selectFor (Some PayloadShape.RawText) [ "application/json" ; "text/plain" ] None
        |> shouldEqual (Some (MimeType "text/plain"))

    [<Test>]
    let ``a raw string payload with several non-JSON options is rejected`` () : unit =
        Assert.Throws<exn> (fun () ->
            selectFor (Some PayloadShape.RawText) [ "application/json" ; "text/plain" ; "text/html" ] None
            |> ignore
        )
        |> ignore<exn>

    [<Test>]
    let ``an opaque payload accepts anything and prefers canonical application/json`` () : unit =
        selectFor (Some PayloadShape.Opaque) [ "text/plain" ; "application/json" ] None
        |> shouldEqual (Some (MimeType "application/json"))

    [<Test>]
    let ``an opaque payload with no canonical JSON entry is rejected when ambiguous`` () : unit =
        Assert.Throws<exn> (fun () ->
            selectFor (Some PayloadShape.Opaque) [ "text/plain" ; "text/html" ] None
            |> ignore
        )
        |> ignore<exn>

    [<Test>]
    let ``a selected MIME value must be a parseable media type`` () : unit =
        // Every selected value is emitted into a Content-Type or Accept header; a malformed
        // one would make the generated client throw FormatException on every request, so it
        // must fail generation instead — wherever it was declared.
        Assert.Throws<exn> (fun () -> select [ "not-a-media-type" ] None |> ignore)
        |> ignore<exn>

        Assert.Throws<exn> (fun () -> select [] (Some [ "utter garbage" ]) |> ignore)
        |> ignore<exn>

    [<Test>]
    let ``whatever selection makes of an ambiguous global list, it never invents or mislabels`` () : unit =
        let mimeGen : Gen<string> =
            Gen.elements
                [
                    "application/json"
                    "APPLICATION/JSON"
                    "application/json; charset=utf-8"
                    "application/vnd.api+json"
                    "application/problem+json"
                    "application/xml"
                    "application/octet-stream"
                    "text/plain"
                    "text/html"
                ]

        let payloadGen : Gen<PayloadShape option> =
            Gen.elements
                [
                    None
                    Some PayloadShape.Json
                    Some PayloadShape.RawText
                    Some PayloadShape.Opaque
                ]

        let inputGen : Gen<PayloadShape option * string list> =
            Gen.zip payloadGen (Gen.listOf mimeGen)

        /// Could the generated client honour this MIME type for this payload?
        let compatible (payload : PayloadShape) (m : MimeType) : bool =
            match payload with
            | PayloadShape.Json -> SwaggerClientGenerator.isJsonMimeType m
            | PayloadShape.RawText -> not (SwaggerClientGenerator.isJsonMimeType m)
            | PayloadShape.Opaque -> true

        let property (payload : PayloadShape option, globalLevel : string list) : bool =
            match
                (try
                    selectFor payload globalLevel None |> Ok
                 with e ->
                     Error e)
            with
            | Error _ ->
                // Refusing to choose is always sound (the caller fails generation loudly).
                true
            | Ok None ->
                // Only "no payload" or "no preference" produce no MIME type.
                payload.IsNone || List.isEmpty globalLevel
            | Ok (Some m) ->
                // Anything we choose must have been on offer, for an actual payload; and a
                // choice among several must be one the generated client can honour.
                match payload with
                | None -> false
                | Some payload ->
                    List.contains m (List.map MimeType globalLevel)
                    && (globalLevel.Length = 1 || compatible payload m)

        Prop.forAll (Arb.fromGen inputGen) property |> Check.QuickThrowOnFailure
