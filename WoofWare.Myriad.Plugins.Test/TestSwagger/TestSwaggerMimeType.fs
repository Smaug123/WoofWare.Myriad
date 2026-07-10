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
    let ``raw and absent payloads never require JSON serialisation`` () : unit =
        for defn in [ Definition.String ; Definition.File ; Definition.Unspecified ] do
            SwaggerClientGenerator.requiresJsonSerialisation noHandles defn
            |> shouldEqual false

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
            SwaggerClientGenerator.requiresJsonSerialisation noHandles defn
            |> shouldEqual true

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

        SwaggerClientGenerator.requiresJsonSerialisation resolve (Definition.Handle "#/definitions/RawText")
        |> shouldEqual false

        SwaggerClientGenerator.requiresJsonSerialisation resolve (Definition.Handle "#/definitions/Thing")
        |> shouldEqual true

    [<Test>]
    let ``an unresolvable handle conservatively requires JSON`` () : unit =
        SwaggerClientGenerator.requiresJsonSerialisation noHandles (Definition.Handle "#/definitions/Missing")
        |> shouldEqual true

    [<Test>]
    let ``a circular handle chain terminates and requires JSON`` () : unit =
        // A points at B points back at A: we can't prove the payload is raw, so we require JSON
        // rather than looping forever.
        let resolve =
            function
            | "#/definitions/A" -> Some (Definition.Handle "#/definitions/B")
            | "#/definitions/B" -> Some (Definition.Handle "#/definitions/A")
            | _ -> None

        SwaggerClientGenerator.requiresJsonSerialisation resolve (Definition.Handle "#/definitions/A")
        |> shouldEqual true

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
