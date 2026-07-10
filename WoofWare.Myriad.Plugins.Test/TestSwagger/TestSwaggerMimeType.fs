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
    let ``a string payload is raw text and file or absent payloads are opaque`` () : unit =
        SwaggerClientGenerator.classifyPayload noHandles Definition.String
        |> shouldEqual PayloadShape.RawText

        for defn in [ Definition.File ; Definition.Unspecified ] do
            SwaggerClientGenerator.classifyPayload noHandles defn
            |> shouldEqual PayloadShape.Opaque

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
            |> shouldEqual PayloadShape.Json

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
        |> shouldEqual PayloadShape.RawText

        SwaggerClientGenerator.classifyPayload resolve (Definition.Handle "#/definitions/Thing")
        |> shouldEqual PayloadShape.Json

    [<Test>]
    let ``an unresolvable handle is conservatively JSON`` () : unit =
        SwaggerClientGenerator.classifyPayload noHandles (Definition.Handle "#/definitions/Missing")
        |> shouldEqual PayloadShape.Json

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
        |> shouldEqual PayloadShape.Json

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

    // ---- selectMimeType: boiling a Consumes/Produces list down to one MIME type ----

    let private select (globalLevel : string list) (endpointLevel : string list option) : MimeType option =
        SwaggerClientGenerator.selectMimeType
            MimeUsage.Consumes
            "/thing"
            HttpMethod.Get
            (globalLevel |> List.map MimeType)
            (endpointLevel |> Option.map (List.map MimeType))

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
        // exactly one entry the generated client can honour.
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
    let ``an ambiguous global list with no JSON entry is rejected`` () : unit =
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

        let globalGen : Gen<string list> = Gen.listOf mimeGen

        let property (globalLevel : string list) : bool =
            match
                (try
                    select globalLevel None |> Ok
                 with e ->
                     Error e)
            with
            | Error _ ->
                // Refusing to choose is always sound.
                true
            | Ok None ->
                // Only an empty list expresses no preference.
                List.isEmpty globalLevel
            | Ok (Some m) ->
                // Anything we choose must have been on offer, and a choice among several
                // must be one the generated client's JSON serialiser can honour.
                List.contains m (List.map MimeType globalLevel)
                && (globalLevel.Length = 1 || SwaggerClientGenerator.isJsonMimeType m)

        Prop.forAll (Arb.fromGen globalGen) property |> Check.QuickThrowOnFailure
