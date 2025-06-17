module WoofWare.Myriad.Plugins.SwaggerV2

open System
open System.Text.Json.Nodes

/// A MIME type, like "application/json"
type MimeType =
    /// A MIME type, like "application/json"
    | MimeType of string

/// A URL scheme, like "https"
type Scheme =
    /// A URL scheme, like "https"
    | Scheme of string

/// "Licence information for the exposed API", whatever that means.
type SwaggerLicense =
    {
        /// "The license name used for the API", whatever that means.
        Name : string
        /// Link to the license used. Mutually exclusive with `Identifier`.
        Url : Uri option
        /// SPDX license identifier. Mutually exclusive with `Url`.
        Identifier : string option
    }

    /// Render a JsonObject into the strongly-typed version, performing sanity
    /// checks and throwing on input that can't be parsed.
    static member Parse (node : JsonObject) : SwaggerLicense =
        let name = asString node "name"
        let url = asOpt<string> node "url" |> Option.map Uri
        let identifier = asOpt<string> node "identifier"

        match url, identifier with
        | Some _, Some _ -> failwith "Invalid license spec: cannot supply both URL and identifier"
        | _, _ -> ()

        {
            Name = name
            Url = url
            Identifier = identifier
        }

/// Overall information about the API described by this Swagger spec.
type SwaggerInfo =
    {
        /// Human-readable description of what this Swagger API is for.
        /// Supports GitHub-flavoured markdown, apparently.
        Description : string
        /// Human-readable title of the service to which this is an API.
        Title : string
        /// The license applying to this schema. It's very unclear what this means.
        /// The spec just says:
        /// "Licence information for the exposed API"
        License : SwaggerLicense
        /// The version of this API (not the version of Swagger or the file defining the API!).
        /// Strictly speaking this can be anything, but I am assuming it's roughly
        /// SemVer.
        Version : Version
    }

    /// Render a JsonObject into the strongly-typed version, performing sanity
    /// checks and throwing on input that can't be parsed.
    static member Parse (node : JsonObject) : SwaggerInfo =
        let description = asString node "description"
        let title = asString node "title"
        let version = asString node "version" |> Version.Parse
        let license = asObj node "license" |> SwaggerLicense.Parse

        {
            Description = description
            Title = title
            License = license
            Version = version
        }

/// An "optional unique string used to describe an operation".
/// If present, these are assumed to be unique among all operations described
/// in the API.
type OperationId =
    /// An "optional unique string used to describe an operation".
    /// If present, these are assumed to be unique among all operations described
    /// in the API.
    | OperationId of string

    /// Round-trip string representation.
    override this.ToString () =
        match this with
        | OperationId.OperationId s -> s

/// Constraints on the `additionalProperties` (in the JSON schema sense).
/// "Additional properties" are properties of a JSON object which were not
/// listed in the schema.
type AdditionalProperties =
    /// No additional properties are allowed: all properties must have been
    /// mentioned in the schema.
    | Never
    /// Additional properties are permitted, but if they exist, they must
    /// match this schema definition.
    | Constrained of Definition

/// The Swagger schema lets you define types. An ObjectTypeDefinition
/// is specifically the information about types defined as `"type": "object"`.
and ObjectTypeDefinition =
    {
        /// Human-readable description of the purpose of this type.
        Description : string option
        /// Fields which any object must have to satisfy this type.
        Properties : Map<string, Definition> option
        /// Extra properties in the type description. In Gitea, these are
        /// (for example) "x-go-package":"code.gitea.io/gitea/modules/structs".
        Extras : Map<string, JsonNode>
        /// List of fields which are required; all other fields are optional.
        Required : string list option
        /// Constraints, if any, placed on fields which are not mentioned in
        /// the schema. If absent, there are no constraints.
        AdditionalProperties : AdditionalProperties option
        /// Example of an object which satisfies this schema.
        Example : JsonObject option
    }

    /// Render a JsonObject into the strongly-typed version, performing sanity
    /// checks and throwing on input that can't be parsed.
    static member Parse (node : JsonObject) : ObjectTypeDefinition =
        let description =
            match asOpt<string> node "description", asOpt<string> node "title" with
            | None, None -> None
            | Some v, None
            | None, Some v -> Some v
            | Some v1, Some v2 -> failwith "both description and title were given"

        let additionalProperties =
            match node.["additionalProperties"] with
            | null -> None
            | :? JsonValue as p ->
                if not (p.GetValue<bool> ()) then
                    Some AdditionalProperties.Never
                else
                    failwith $"additionalProperties should be 'false' or an object, but was: %s{p.ToJsonString ()}"
            | p ->
                let p = p.AsObject ()
                Definition.Parse p |> AdditionalProperties.Constrained |> Some

        let properties =
            match node.["properties"] with
            | null -> None
            | p ->
                p.AsObject ()
                |> Seq.map (fun (KeyValue (key, value)) ->
                    let value = value.AsObject ()
                    key, Definition.Parse value
                )
                |> Map.ofSeq
                |> Some

        let example =
            match node.["example"] with
            | null -> None
            | :? JsonObject as o -> Some o
            | _ ->
                // Gitea returns a stringified and malformed JSON object here.
                // Don't throw; just omit.
                None

        let required = asArrOpt'<string> node "required"

        let extras =
            node.AsObject ()
            |> Seq.choose (fun (KeyValue (key, value)) ->
                match key with
                | "type"
                | "description"
                | "title"
                | "additionalProperties"
                | "example"
                | "required"
                | "properties" -> None
                | _ -> Some (key, value)
            )
            |> Map.ofSeq

        {
            Description = description
            Properties = properties
            AdditionalProperties = additionalProperties
            Required = required
            Extras = extras
            Example = example
        }

/// The Swagger schema lets you define types. An ArrayTypeDefinition
/// is specifically the information about types defined as `"type": "array"`.
and ArrayTypeDefinition =
    {
        /// The type is `'a array`; this field describes `'a`.
        Items : Definition
    }

    /// Render a JsonNode into the strongly-typed version, performing sanity
    /// checks and throwing on input that can't be parsed.
    static member Parse (n : JsonNode) : ArrayTypeDefinition =
        let items = asObj n "items" |> Definition.Parse

        {
            Items = items
        }

/// Any definition of a type in the Swagger document. This is basically any
/// information associated with the `"type": "blah"` field.
and Definition =
    /// For example, if `"$ref": "#/responses/Blah", then this is "#/responses/Blah".
    | Handle of string
    /// A type definition with "type": "object".
    | Object of ObjectTypeDefinition
    /// A type definition with "type": "array".
    | Array of ArrayTypeDefinition
    /// A type definition with "type": "string".
    | String
    /// A type definition with "type": "boolean".
    | Boolean
    /// A response without a body has no "schema" specified.
    | Unspecified
    /// A type definition with "type": "integer".
    /// The format is an optional hint which could be e.g. "int64" or "int32".
    /// https://swagger.io/docs/specification/data-models/data-types/#numbers
    | Integer of format : string option
    /// Not a JSON schema type, but a Swagger 2.0 type.
    | File

    /// Render a JsonObject into this strongly-typed specification.
    static member Parse (n : JsonObject) : Definition =
        match n.["$ref"] |> Option.ofObj with
        | Some ref -> Definition.Handle (ref.GetValue<string> ())
        | None ->

        let ty = asOpt<string> n "type"

        match ty with
        | None -> Definition.Unspecified
        | Some "object" -> ObjectTypeDefinition.Parse n |> Definition.Object
        | Some "array" -> ArrayTypeDefinition.Parse n |> Definition.Array
        | Some "string" -> Definition.String
        | Some "boolean" -> Definition.Boolean
        | Some "file" -> Definition.File
        | Some "integer" ->
            let format = asOpt<string> n "format"
            Definition.Integer format
        | Some ty -> failwith $"Unrecognised type: %s{ty}"

/// REST APIs allow their parameters to be passed in various ways. This describes
/// how one single parameter is passed.
type ParameterIn =
    /// The parameter is interpolated into the path, e.g. "/foo/{blah}".
    /// The "name" is what we replace in the path: e.g. "/foo/{person}" would
    /// have a name of "person".
    | Path of name : string
    /// The parameter is appended to the URL's query params, e.g. "?<name>=blah"
    | Query of name : string
    /// The parameter is passed in the body of the HTTP request.
    | Body
    /// Some spec that WoofWare.Myriad doesn't support.
    | Unrecognised of op : string * name : string

/// Description of a single input parameter to an endpoint.
type SwaggerParameter =
    {
        /// The type schema to which this parameter must conform.
        Type : Definition
        /// Optional human-readable description of this parameter.
        Description : string option
        /// How this parameter is passed.
        In : ParameterIn
        /// Name of this parameter. For most `In` values, this name is the
        /// name of the parameter as supplied to the API at runtime, and in WoofWare's
        /// strongly-typed domain types this information is also contained in the `In` field.
        /// For `Body` parameters, this is purely for dev-time information.
        Name : string
        /// Whether this parameter is required for validation to succeed.
        /// I think this defaults to "no".
        Required : bool option
    }

    /// Render a JsonObject into this strongly-typed specification.
    static member Parse (node : JsonObject) : SwaggerParameter =
        let ty =
            match asObjOpt node "schema" with
            | None -> Definition.Parse node
            | Some node -> Definition.Parse node

        let description = asOpt<string> node "description"
        let name = asString node "name"

        let paramIn =
            match asString node "in" with
            | "path" -> ParameterIn.Path name
            | "query" -> ParameterIn.Query name
            | "body" -> ParameterIn.Body
            | f -> ParameterIn.Unrecognised (f, name)

        let required = asOpt<bool> node "required"

        {
            Type = ty
            Description = description
            In = paramIn
            Name = name
            Required = required
        }

/// An "endpoint" is basically a single HTTP verb, applied to some path.
type SwaggerEndpoint =
    {
        /// The MIME types we should send our request body in.
        /// This overrides (does not extend) any global definitions on the spec itself.
        Consumes : MimeType list option
        /// The MIME types we should expect to receive in response to this request.
        /// This overrides (does not extend) any global definitions on the spec itself.
        Produces : MimeType list option
        /// Arbitrary list of [tags](https://swagger.io/docs/specification/2-0/grouping-operations-with-tags/).
        Tags : string list
        /// Human-readable description of the endpoint.
        Summary : string
        /// Arbitrary identifier of this endpoint; this must be unique across *all* endpoints
        /// in this entire spec.
        OperationId : OperationId
        /// Parameters that must be supplied at HTTP-request-time to the endpoint.
        /// (Each parameter knows how it needs to be supplied: e.g. if it's a query parameter or
        /// if it's interpolated into the path.)
        Parameters : SwaggerParameter list option
        /// Map of HTTP response code to the type that we expect to receive in the body if we
        /// get that response code back.
        Responses : Map<int, Definition>
    }

    /// Render a JsonObject into this strongly-typed specification.
    static member Parse (r : JsonObject) : SwaggerEndpoint =
        let produces = asArrOpt'<string> r "produces" |> Option.map (List.map MimeType)
        let consumes = asArrOpt'<string> r "consumes" |> Option.map (List.map MimeType)
        let tags = asArr'<string> r "tags"
        let summary = asString r "summary"
        let operationId = asString r "operationId" |> OperationId

        let responses =
            asObj r "responses"
            |> Seq.map (fun (KeyValue (key, value)) ->
                let value = value.AsObject ()
                Int32.Parse key, Definition.Parse value
            )
            |> Map.ofSeq

        let parameters =
            asArrOpt r "parameters"
            |> Option.map (fun pars ->
                pars
                |> Seq.map (fun par -> par.AsObject () |> SwaggerParameter.Parse)
                |> Seq.toList
            )

        {
            Produces = produces
            Consumes = consumes
            Tags = tags
            Summary = summary
            OperationId = operationId
            Parameters = parameters
            Responses = responses
        }

/// Specifies the form a response to an endpoint will take if it's complying with this spec.
type Response =
    {
        /// Human-readable description.
        Description : string
        /// Specification of the type to which responses will conform under this spec.
        Schema : Definition
    }

    /// Render a JsonObject into this strongly-typed specification.
    static member Parse (r : JsonObject) : Response =
        let desc = asString r "description"

        let schema =
            match asObjOpt r "schema" with
            | None -> Definition.Unspecified
            | Some s -> Definition.Parse s

        {
            Description = desc
            Schema = schema
        }

/// A Swagger API specification.
type SwaggerV2 =
    {
        /// Global collection of MIME types which any endpoint expects to consume its inputs in.
        /// This may be overridden on any individual endpoint by that endpoint.
        Consumes : MimeType list
        /// Global collection of MIME types which any endpoint will produce.
        /// This may be overridden on any individual endpoint by that endpoint.
        Produces : MimeType list
        /// HTTP or HTTPS, for example. Indicates which scheme to access the API on.
        Schemes : Scheme list
        /// The version of OpenAPI this specification is written against.
        /// (As of this writing, we only support 2.0.)
        Swagger : Version
        /// General information about this API.
        Info : SwaggerInfo
        /// Path under the URI host, which should be prefixed (with trailing slash if necessary)
        /// to all requests.
        BasePath : string
        /// Map from relative path to "what is served at that path".
        Paths : Map<string, Map<HttpMethod, SwaggerEndpoint>>
        /// Types defined in the schema. Requests may use these definitions just like in any other JSON schema.
        /// Key is a domain type name, e.g. "APIError".
        Definitions : Map<string, Definition>
        /// Types of each response.
        /// Key is a domain type name, e.g. "AccessToken".
        Responses : Map<string, Response>
    }

/// Parse a JSON-schema-based specification of a Swagger 2.0 API and
/// build the strongly-typed version. Throws on invalid inputs; returns Error if the input is JSON but has a Swagger
/// version that is not in the 2.0 series.
let parse (s : string) : Result<SwaggerV2, JsonNode> =
    let node = JsonNode.Parse s
    let swagger = asString node "swagger" |> Version.Parse

    if swagger.Major <> 2 then
        Error node
    else

    let consumes = asArr'<string> node "consumes" |> List.map MimeType
    let produces = asArr'<string> node "produces" |> List.map MimeType
    let schemes = asArr'<string> node "schemes" |> List.map Scheme
    let info = asObj node "info" |> SwaggerInfo.Parse
    let basePath = asString node "basePath"

    let definitions =
        asObj node "definitions"
        |> Seq.map (fun (KeyValue (key, value)) ->
            let value = value.AsObject ()
            key, Definition.Parse value
        )
        |> Map.ofSeq

    let paths =
        asObj node "paths"
        |> Seq.map (fun (KeyValue (key, value)) ->
            let contents =
                value.AsObject ()
                |> Seq.map (fun (KeyValue (endpoint, contents)) ->
                    let contents = contents.AsObject ()
                    HttpMethod.Parse endpoint, SwaggerEndpoint.Parse contents
                )
                |> Map.ofSeq

            key, contents
        )
        |> Map.ofSeq

    let responses =
        asObj node "responses"
        |> Seq.map (fun (KeyValue (key, value)) ->
            let value = value.AsObject ()
            key, Response.Parse value
        )
        |> Map.ofSeq

    {
        Consumes = consumes
        Produces = produces
        Schemes = schemes
        Swagger = swagger
        Info = info
        BasePath = basePath
        Paths = paths
        Definitions = definitions
        Responses = responses
    }
    |> Ok
