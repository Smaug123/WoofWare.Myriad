module internal WoofWare.Myriad.Plugins.OpenApi3

open System
open System.Text.Json.Nodes

type ExternalDocumentation =
    {
        /// A short description of the target documentation, possibly in CommonMark.
        Description : string option
        /// The URL for the target documentation.
        Url : Uri
    }

    static member Parse (node : JsonObject) : ExternalDocumentation =
        let description = asOpt<string> node "description"
        let url = asString node "url" |> Uri

        {
            Description = description
            Url = url
        }

type Schema =
    | Schema of unit

    static member Parse (_ : JsonObject) : Schema = Schema ()

type Example =
    {
        /// Short description for the example.
        Summary : string option
        /// Long description for the example, possibly CommonMark.
        Description : string option
        Value : Choice<JsonNode, Uri> option
    }

    static member Parse (node : JsonObject) : Example =
        let description = asOpt<string> node "description"
        let summary = asOpt<string> node "summary"
        let externalValue = asOpt<string> node "externalValue" |> Option.map Uri

        let value =
            match externalValue with
            | Some u -> Choice2Of2 u |> Some
            | None ->
                match node.TryGetPropertyValue "value" with
                | true, v -> Choice1Of2 v |> Some
                | false, _ -> None

        {
            Summary = summary
            Description = description
            Value = value
        }

type Reference =
    {
        /// The reference string.
        Ref : string
    }

    static member Parse (node : JsonObject) : Reference =
        let ref = asString node "$ref"

        {
            Ref = ref
        }

type Tag =
    {
        /// The name of the tag.
        Name : string
        /// A short description for the tag, possibly CommonMark.
        Description : string option
        /// Additional external documentation for this tag.
        ExternalDocs : ExternalDocumentation option
    }

    static member Parse (node : JsonObject) : Tag =
        let name = asString node "name"
        let description = asOpt<string> node "description"
        let docs = asObjOpt node "externalDocs" |> Option.map ExternalDocumentation.Parse

        {
            Name = name
            Description = description
            ExternalDocs = docs
        }

type ServerVariable =
    {
        /// An enumeration of string values to be used if the substitution options are from a limited set.
        Enum : string list option
        /// The default value to use for substitution, and to send, if an alternate value is not supplied.
        /// Unlike the Schema Object’s default, this value MUST be provided by the consumer.
        Default : string
        /// An optional description for the server variable, possibly in CommonMark.
        Description : string option
    }

    static member Parse (node : JsonObject) : ServerVariable =
        let enum = asArrOpt'<string> node "enum"
        let default' = asString node "default"
        let description = asOpt<string> node "description"

        {
            Enum = enum
            Default = default'
            Description = description
        }

type Server =
    {
        /// A URL to the target host.
        /// This URL supports Server Variables and MAY be relative, to indicate that the host location is relative to the location where the OpenAPI document is being served.
        /// Variable substitutions will be made when a variable is named in {brackets}.
        Url : string
        /// Describes the host designated by the URL, possibly with CommonMark.
        Description : string option
        /// Used for substituting in the Url.
        Variables : Map<string, ServerVariable> option
    }

    static member Parse (node : JsonObject) : Server =
        let url = asString node "url"
        let description = asOpt<string> node "description"

        let variables =
            match node.TryGetPropertyValue "variables" with
            | false, _ -> None
            | true, o ->
                o.AsObject ()
                |> Seq.map (fun (KeyValue (k, v)) -> k, ServerVariable.Parse (v.AsObject ()))
                |> Map.ofSeq
                |> Some

        {
            Url = url
            Description = description
            Variables = variables
        }

type StringFormat =
    /// base64-encoded characters
    | Byte
    /// any sequence of octets
    | Binary
    /// As defined by full-date - RFC3339 Section 5.6
    | Date
    /// As defined by date-time - RFC3339 Section 5.6
    | DateTime
    /// A hint to UIs to obscure input
    | Password
    | Verbatim of string

type IntegerFormat =
    /// Signed 32 bits
    | Int32
    /// Signed 64 bits
    | Int64
    | Verbatim of string

type NumberFormat =
    | Float
    | Double
    | Verbatim of string

type DataFormat =
    | Integer of IntegerFormat option
    | Number of NumberFormat option
    | String of StringFormat option
    | Boolean of format : string option

type Contact =
    {
        /// The identifying name of the contact person/organization.
        Name : string option
        /// The URL pointing to the contact information.
        Url : Uri option
        /// This MUST be in email address format.
        Email : string option
    }

    static member Parse (node : JsonObject) : Contact =
        let name = asOpt<string> node "name"
        let url = asOpt<string> node "url" |> Option.map Uri
        let email = asOpt<string> node "email"

        {
            Email = email
            Url = url
            Name = name
        }

type License =
    {
        /// The license name used for the API.
        Name : string
        /// A URL to the license used for the API.
        Url : Uri option
    }

    static member Parse (node : JsonObject) : License =
        let url = asOpt<string> node "url" |> Option.map Uri
        let name = asString node "name"

        {
            Name = name
            Url = url
        }

type OpenApiInfo =
    {
        /// Title of the application
        Title : string
        /// Short description of the application, might be in CommonMark
        Description : string option
        /// Link to the ToS of the application
        TermsOfService : Uri option
        /// The contact information for the exposed API.
        Contact : Contact option
        /// The license information for the exposed API.
        License : License option
        /// The version of the OpenAPI document (which is distinct from the OpenAPI Specification version or the API implementation version).
        Version : string
    }

    static member Parse (node : JsonObject) : OpenApiInfo =
        let title = asString node "title"
        let version = asString node "version"
        let desc = asOpt<string> node "description"
        let termsOfService = asOpt<string> node "termsOfService" |> Option.map Uri
        let contact = asObjOpt node "contact" |> Option.map Contact.Parse
        let license = asObjOpt node "license" |> Option.map License.Parse

        {
            Title = title
            Description = desc
            TermsOfService = termsOfService
            Contact = contact
            License = license
            Version = version
        }

type Encoding =
    {
        /// The Content-Type for encoding a specific property.
        /// Default value depends on the property type:
        /// for string with format being binary – application/octet-stream;
        /// for other primitive types – text/plain;
        /// for object - application/json;
        /// for array – the default is defined based on the inner type.
        /// The value can be a specific media type (e.g. application/json), a wildcard media type (e.g. image/*), or a comma-separated list of the two types.
        ContentType : string option
        /// A map allowing additional information to be provided as headers, for example Content-Disposition.
        /// Content-Type is described separately and SHALL be ignored in this section.
        /// This property SHALL be ignored if the request body media type is not a multipart.
        Headers : Map<string, Choice<Header, Reference>> option
        /// Describes how a specific property value will be serialized depending on its type.
        /// See Parameter Object for details on the style property.
        /// The behavior follows the same values as query parameters, including default values.
        /// This property SHALL be ignored if the request body media type is not application/x-www-form-urlencoded.
        Style : string option
        /// When this is true, property values of type array or object generate separate parameters for each value of the array, or key-value-pair of the map.
        /// For other types of properties this property has no effect.
        /// When style is form, the default value is true.
        /// For all other styles, the default value is false.
        /// This property SHALL be ignored if the request body media type is not application/x-www-form-urlencoded.
        Explode : bool option
        /// Determines whether the parameter value SHOULD allow reserved characters, as defined by [RFC3986] Section 2.2 :/?#[]@!$&'()*+,;=
        /// to be included without percent-encoding.
        /// The default value is false.
        /// This property SHALL be ignored if the request body media type is not application/x-www-form-urlencoded.
        AllowReserved : bool option
    }

    static member Parse (node : JsonObject) : Encoding =
        let contentType = asOpt<string> node "contentType"
        let style = asOpt<string> node "style"
        let explode = asOpt<bool> node "explode"
        let allowReserved = asOpt<bool> node "allowReserved"

        let headers =
            match node.TryGetPropertyValue "headers" with
            | false, _ -> None
            | true, o ->
                o.AsObject ()
                |> Seq.map (fun (KeyValue (k, v)) ->
                    let obj = v.AsObject ()

                    let parsed =
                        if obj.ContainsKey "$ref" then
                            Choice2Of2 (Reference.Parse obj)
                        else
                            Choice1Of2 (Header.Parse obj)

                    k, parsed
                )
                |> Map.ofSeq
                |> Some

        {
            ContentType = contentType
            Headers = headers
            Style = style
            Explode = explode
            AllowReserved = allowReserved
        }

and MediaType =
    {
        /// The schema defining the type used for the request body.
        Schema : Choice<Schema, Reference> option
        Example : Choice<JsonNode, Map<string, Choice<Example, Reference>>> option
        /// A map between a property name and its encoding information.
        /// The key, being the property name, MUST exist in the schema as a property.
        /// The encoding object SHALL only apply to requestBody objects when the media type is multipart or application/x-www-form-urlencoded.
        Encoding : Map<string, Encoding> option
    }

    static member Parse (node : JsonObject) : MediaType =
        let schema =
            match node.TryGetPropertyValue "schema" with
            | false, _ -> None
            | true, s ->
                let obj = s.AsObject ()

                if obj.ContainsKey "$ref" then
                    Choice2Of2 (Reference.Parse obj) |> Some
                else
                    Choice1Of2 (Schema.Parse obj) |> Some

        let example =
            match node.TryGetPropertyValue "example" with
            | true, e -> Choice1Of2 e |> Some
            | false, _ ->
                match node.TryGetPropertyValue "examples" with
                | false, _ -> None
                | true, e ->
                    e.AsObject ()
                    |> Seq.map (fun (KeyValue (k, v)) ->
                        let obj = v.AsObject ()

                        let parsed =
                            if obj.ContainsKey "$ref" then
                                Choice2Of2 (Reference.Parse obj)
                            else
                                Choice1Of2 (Example.Parse obj)

                        k, parsed
                    )
                    |> Map.ofSeq
                    |> Choice2Of2
                    |> Some

        let encoding =
            match node.TryGetPropertyValue "encoding" with
            | false, _ -> None
            | true, e ->
                e.AsObject ()
                |> Seq.map (fun (KeyValue (k, v)) -> k, Encoding.Parse (v.AsObject ()))
                |> Map.ofSeq
                |> Some

        {
            Schema = schema
            Example = example
            Encoding = encoding
        }

/// The Header Object basically follows the structure of the Parameter Object.
/// All traits that are affected by the location MUST be applicable to a location of header (for example, style).
and Header =
    {
        /// A brief description of the header, possibly CommonMark.
        Description : string option
        /// Determines whether this header is mandatory.
        /// If the header location is “path”, this property is REQUIRED and its value MUST be true.
        /// Otherwise, the property MAY be included and its default value is false.
        Required : bool option
        /// Specifies that a header is deprecated and SHOULD be transitioned out of usage.
        Deprecated : bool option
        /// Sets the ability to pass empty-valued headers.
        /// This is valid only for query headers and allows sending a header with an empty value.
        /// Default value is false.
        /// If style is used, and if behavior is n/a (cannot be serialized), the value of allowEmptyValue SHALL be ignored.
        AllowEmptyValue : bool option
        /// Describes how the header value will be serialized depending on the type of the header value.
        /// Default values (based on value of in): for query - form; for path - simple; for header - simple; for cookie - form.
        Style : string option
        /// When this is true, header values of type array or object generate separate headers for each value of the array or key-value pair of the map.
        /// For other types of headers this property has no effect.
        /// When style is form, the default value is true.
        /// For all other styles, the default value is false.
        Explode : bool option
        /// Determines whether the header value SHOULD allow reserved characters, as defined by [RFC3986] Section 2.2 :/?#[]@!$&'()*+,;=
        /// to be included without percent-encoding.
        /// This property only applies to headers with an in value of query.
        /// The default value is false.
        AllowReserved : bool option
        /// The schema defining the type used for the header.
        Schema : Choice<Schema, Reference> option
        Example : Choice<JsonNode, Map<string, Choice<Example, Reference>>> option
        /// A map containing the representations for the header.
        /// The key is the media type and the value describes it.
        /// The map MUST only contain one entry.
        Content : Map<string, MediaType> option
    }

    static member Parse (node : JsonObject) : Header =
        let description = asOpt<string> node "description"
        let required = asOpt<bool> node "required"
        let deprecated = asOpt<bool> node "deprecated"
        let allowEmptyValue = asOpt<bool> node "allowEmptyValue"
        let style = asOpt<string> node "style"
        let explode = asOpt<bool> node "explode"
        let allowReserved = asOpt<bool> node "allowReserved"

        let schema =
            match node.TryGetPropertyValue "schema" with
            | false, _ -> None
            | true, s ->
                let obj = s.AsObject ()

                if obj.ContainsKey "$ref" then
                    Choice2Of2 (Reference.Parse obj) |> Some
                else
                    Choice1Of2 (Schema.Parse obj) |> Some

        let example =
            match node.TryGetPropertyValue "example" with
            | true, e -> Choice1Of2 e |> Some
            | false, _ ->
                match node.TryGetPropertyValue "examples" with
                | false, _ -> None
                | true, e ->
                    e.AsObject ()
                    |> Seq.map (fun (KeyValue (k, v)) ->
                        let obj = v.AsObject ()

                        let parsed =
                            if obj.ContainsKey "$ref" then
                                Choice2Of2 (Reference.Parse obj)
                            else
                                Choice1Of2 (Example.Parse obj)

                        k, parsed
                    )
                    |> Map.ofSeq
                    |> Choice2Of2
                    |> Some

        let content =
            match node.TryGetPropertyValue "content" with
            | false, _ -> None
            | true, c ->
                c.AsObject ()
                |> Seq.map (fun (KeyValue (k, v)) -> k, MediaType.Parse (v.AsObject ()))
                |> Map.ofSeq
                |> Some

        {
            Description = description
            Required = required
            Deprecated = deprecated
            AllowEmptyValue = allowEmptyValue
            Style = style
            Explode = explode
            AllowReserved = allowReserved
            Schema = schema
            Example = example
            Content = content
        }

type LinkOperation =
    /// A relative or absolute reference to an OAS operation.
    /// This field is mutually exclusive of the operationId field, and MUST point to an Operation Object.
    /// Relative operationRef values MAY be used to locate an existing Operation Object in the OpenAPI definition.
    | Ref of string
    /// The name of an existing, resolvable OAS operation, as defined with a unique operationId.
    /// This field is mutually exclusive of the operationRef field.
    | Id of string

type RuntimeExpression = | RuntimeExpression of unit

type Link =
    {
        /// A relative or absolute reference to an OAS operation.
        /// This field is mutually exclusive of the operationId field, and MUST point to an Operation Object. Relative operationRef values MAY be used to locate an existing Operation Object in the OpenAPI definition.
        Operation : LinkOperation option
        /// A map representing parameters to pass to an operation as specified with operationId or identified via operationRef.
        /// The key is the parameter name to be used, whereas the value can be a constant or an expression to be evaluated and passed to the linked operation.
        /// The parameter name can be qualified using the parameter location [{in}.]{name} for operations that use the same parameter name in different locations (e.g. path.id).
        Parameters : Map<string, Choice<JsonNode, RuntimeExpression>> option
        /// A literal value or {expression} to use as a request body when calling the target operation.
        RequestBody : Choice<JsonNode, RuntimeExpression> option
        /// A description of the link, possibly CommonMark.
        Description : string option
        /// A server object to be used by the target operation.
        Server : Server option
    }

    static member Parse (node : JsonObject) : Link =
        let operation =
            match node.TryGetPropertyValue "operationRef" with
            | true, ref -> LinkOperation.Ref (ref.GetValue<string> ()) |> Some
            | false, _ ->
                match node.TryGetPropertyValue "operationId" with
                | true, id -> LinkOperation.Id (id.GetValue<string> ()) |> Some
                | false, _ -> None

        let parameters =
            match node.TryGetPropertyValue "parameters" with
            | false, _ -> None
            | true, p ->
                p.AsObject ()
                |> Seq.map (fun (KeyValue (k, v)) ->
                    // In OpenAPI spec, this can be any value or {expression}
                    // For simplicity, treating non-string values as JsonNode
                    k, Choice1Of2 v
                )
                |> Map.ofSeq
                |> Some

        let requestBody =
            match node.TryGetPropertyValue "requestBody" with
            | false, _ -> None
            | true, rb -> Choice1Of2 rb |> Some

        let description = asOpt<string> node "description"
        let server = asObjOpt node "server" |> Option.map Server.Parse

        {
            Operation = operation
            Parameters = parameters
            RequestBody = requestBody
            Description = description
            Server = server
        }

type Response =
    {
        /// A short description of the response, possibly CommonMark.
        Description : string
        /// Maps a header name to its definition.
        /// [RFC7230] Page 22 states header names are case insensitive.
        /// If a response header is defined with the name "Content-Type", it SHALL be ignored.
        Headers : Map<string, Choice<Header, Reference>> option
        /// A map containing descriptions of potential response payloads.
        /// The key is a media type or media type range, see [RFC7231] Appendix D, and the value describes it.
        /// For responses that match multiple keys, only the most specific key is applicable. e.g. text/plain overrides text/*
        Content : Map<string, MediaType> option
        /// A map of operations links that can be followed from the response.
        /// The key of the map is a short name for the link, following the naming constraints of the names for Component Objects.
        Links : Map<string, Choice<Link, Reference>> option
    }

    static member Parse (node : JsonObject) : Response =
        let description = asString node "description"

        let headers =
            match node.TryGetPropertyValue "headers" with
            | false, _ -> None
            | true, h ->
                h.AsObject ()
                |> Seq.map (fun (KeyValue (k, v)) ->
                    let obj = v.AsObject ()

                    let parsed =
                        if obj.ContainsKey "$ref" then
                            Choice2Of2 (Reference.Parse obj)
                        else
                            Choice1Of2 (Header.Parse obj)

                    k, parsed
                )
                |> Map.ofSeq
                |> Some

        let content =
            match node.TryGetPropertyValue "content" with
            | false, _ -> None
            | true, c ->
                c.AsObject ()
                |> Seq.map (fun (KeyValue (k, v)) -> k, MediaType.Parse (v.AsObject ()))
                |> Map.ofSeq
                |> Some

        let links =
            match node.TryGetPropertyValue "links" with
            | false, _ -> None
            | true, l ->
                l.AsObject ()
                |> Seq.map (fun (KeyValue (k, v)) ->
                    let obj = v.AsObject ()

                    let parsed =
                        if obj.ContainsKey "$ref" then
                            Choice2Of2 (Reference.Parse obj)
                        else
                            Choice1Of2 (Link.Parse obj)

                    k, parsed
                )
                |> Map.ofSeq
                |> Some

        {
            Description = description
            Headers = headers
            Content = content
            Links = links
        }

type Responses =
    {
        /// The documentation of responses other than the ones declared for specific HTTP response codes.
        /// Use this field to cover undeclared responses.
        Default : Choice<Response, Reference> option
        /// Map from HTTP status code to expected response.
        /// The keys are allowed to be "2XX" for example, hence being strings and not ints.
        Patterns : Map<string, Choice<Response, Reference>> option
    }

    static member Parse (node : JsonObject) : Responses =
        let default' =
            match node.TryGetPropertyValue "default" with
            | false, _ -> None
            | true, d ->
                let obj = d.AsObject ()

                if obj.ContainsKey "$ref" then
                    Choice2Of2 (Reference.Parse obj) |> Some
                else
                    Choice1Of2 (Response.Parse obj) |> Some

        // All other properties are HTTP status codes
        let patterns =
            node
            |> Seq.choose (fun (KeyValue (k, v)) ->
                if k = "default" then
                    None
                else
                    let obj = v.AsObject ()

                    let parsed =
                        if obj.ContainsKey "$ref" then
                            Choice2Of2 (Reference.Parse obj)
                        else
                            Choice1Of2 (Response.Parse obj)

                    Some (k, parsed)
            )
            |> Map.ofSeq
            |> function
                | m when m.IsEmpty -> None
                | m -> Some m

        {
            Default = default'
            Patterns = patterns
        }

type SecuritySchemeIn =
    | Query
    | Header
    | Cookie

type OauthFlow =
    {
        /// The authorization URL to be used for this flow.
        /// Required for "implicit" and "authorizationCode".
        AuthorizationUrl : Uri option
        /// The token URL to be used for this flow.
        /// Required for "password", "clientCredentials", "authorizationCode".
        TokenUrl : Uri option
        /// The URL to be used for obtaining refresh tokens.
        RefreshUrl : Uri option
        /// The available scopes for the OAuth2 security scheme. A map between the scope name and a short description for it.
        /// Required for "oauth2".
        Scopes : Map<string, string> option
    }

    static member Parse (node : JsonObject) : OauthFlow =
        let authorizationUrl = asOpt<string> node "authorizationUrl" |> Option.map Uri
        let tokenUrl = asOpt<string> node "tokenUrl" |> Option.map Uri
        let refreshUrl = asOpt<string> node "refreshUrl" |> Option.map Uri

        let scopes =
            asObjOpt node "scopes"
            |> Option.map (fun s -> s |> Seq.map (fun (KeyValue (k, v)) -> k, v.GetValue<string> ()) |> Map.ofSeq)

        {
            AuthorizationUrl = authorizationUrl
            TokenUrl = tokenUrl
            RefreshUrl = refreshUrl
            Scopes = scopes
        }

type SecurityRequirement =
    {
        /// Each name MUST correspond to a security scheme which is declared in the Security Schemes under the Components Object.
        /// If the security scheme is of type "oauth2" or "openIdConnect", then the value is a list of scope names required for the execution.
        /// For other security scheme types, the array MUST be empty.
        Fields : Map<string, string list> option
    }

    static member Parse (node : JsonObject) : SecurityRequirement =
        // The entire object is the security requirement
        let fields =
            node
            |> Seq.map (fun (KeyValue (k, v)) ->
                let scopes = v.AsArray () |> Seq.map (fun s -> s.GetValue<string> ()) |> Seq.toList
                k, scopes
            )
            |> Map.ofSeq
            |> function
                | m when m.IsEmpty -> None
                | m -> Some m

        {
            Fields = fields
        }

type OauthFlows =
    {
        /// Configuration for the OAuth Implicit flow
        Implicit : OauthFlow option
        /// Configuration for the OAuth Resource Owner Password flow
        Password : OauthFlow option
        /// Configuration for the OAuth Client Credentials flow.
        ClientCredentials : OauthFlow option
        /// Configuration for the OAuth Authorization Code flow.
        AuthorizationCode : OauthFlow option
    }

    static member Parse (node : JsonObject) : OauthFlows =
        let implicit = asObjOpt node "implicit" |> Option.map OauthFlow.Parse
        let password = asObjOpt node "password" |> Option.map OauthFlow.Parse

        let clientCredentials =
            asObjOpt node "clientCredentials" |> Option.map OauthFlow.Parse

        let authorizationCode =
            asObjOpt node "authorizationCode" |> Option.map OauthFlow.Parse

        {
            Implicit = implicit
            Password = password
            ClientCredentials = clientCredentials
            AuthorizationCode = authorizationCode
        }

type SecurityScheme =
    | ApiKey of description : string option * name : string * inValue : SecuritySchemeIn
    | Http of description : string option * scheme : string * bearerFormat : string option
    | Oauth2 of description : string option * OauthFlows
    | OpenIdConnect of description : string option * url : Uri

    static member Parse (node : JsonObject) : SecurityScheme =
        let type' = asString node "type"
        let description = asOpt<string> node "description"

        match type' with
        | "apiKey" ->
            let name = asString node "name"

            let inValue =
                match asString node "in" with
                | "query" -> SecuritySchemeIn.Query
                | "header" -> SecuritySchemeIn.Header
                | "cookie" -> SecuritySchemeIn.Cookie
                | other -> failwithf "Unknown 'in' value for apiKey: %s" other

            SecurityScheme.ApiKey (description, name, inValue)
        | "http" ->
            let scheme = asString node "scheme"
            let bearerFormat = asOpt<string> node "bearerFormat"
            SecurityScheme.Http (description, scheme, bearerFormat)
        | "oauth2" ->
            let flows = asObj node "flows" |> OauthFlows.Parse
            SecurityScheme.Oauth2 (description, flows)
        | "openIdConnect" ->
            let url = asString node "openIdConnectUrl" |> Uri
            SecurityScheme.OpenIdConnect (description, url)
        | other -> failwithf "Unknown security scheme type: %s" other

type ParameterIn =
    /// Used together with Path Templating, where the parameter value is actually part of the operation’s URL.
    /// This does not include the host or base path of the API.
    /// For example, in /items/{itemId}, the path parameter is itemId.
    | Path
    /// Custom headers that are expected as part of the request.
    /// Note that [RFC7230] Page 22 states header names are case insensitive.
    | Header
    /// Parameters that are appended to the URL. For example, in /items?id=###, the query parameter is id.
    | Query
    /// Used to pass a specific cookie value to the API.
    | Cookie

/// A unique parameter is defined by a combination of a name and location.
type Parameter =
    {
        /// Name of the parameter, case sensitive.
        /// If in is "path", the name field MUST correspond to the associated path segment from the path field in the Paths Object.
        /// See Path Templating for further information.
        /// If in is "header" and the name field is "Accept", "Content-Type" or "Authorization", the parameter definition SHALL be ignored.
        /// For all other cases, the name corresponds to the parameter name used by the in property.
        Name : string
        /// The location of the parameter.
        In : ParameterIn
        /// A brief description of the parameter, possibly CommonMark.
        Description : string option
        /// Determines whether this parameter is mandatory.
        /// If the parameter location is “path”, this property is REQUIRED and its value MUST be true.
        /// Otherwise, the property MAY be included and its default value is false.
        Required : bool option
        /// Specifies that a parameter is deprecated and SHOULD be transitioned out of usage.
        Deprecated : bool option
        /// Sets the ability to pass empty-valued parameters.
        /// This is valid only for query parameters and allows sending a parameter with an empty value.
        /// Default value is false.
        /// If style is used, and if behavior is n/a (cannot be serialized), the value of allowEmptyValue SHALL be ignored.
        AllowEmptyValue : bool option
        /// Describes how the parameter value will be serialized depending on the type of the parameter value.
        /// Default values (based on value of in): for query - form; for path - simple; for header - simple; for cookie - form.
        Style : string option
        /// When this is true, parameter values of type array or object generate separate parameters for each value of the array or key-value pair of the map.
        /// For other types of parameters this property has no effect.
        /// When style is form, the default value is true.
        /// For all other styles, the default value is false.
        Explode : bool option
        /// Determines whether the parameter value SHOULD allow reserved characters, as defined by [RFC3986] Section 2.2 :/?#[]@!$&'()*+,;=
        /// to be included without percent-encoding.
        /// This property only applies to parameters with an in value of query.
        /// The default value is false.
        AllowReserved : bool option
        /// The schema defining the type used for the parameter.
        Schema : Choice<Schema, Reference> option
        Example : Choice<JsonNode, Map<string, Choice<Example, Reference>>> option
        /// A map containing the representations for the parameter.
        /// The key is the media type and the value describes it.
        /// The map MUST only contain one entry.
        Content : Map<string, MediaType> option
    }

    static member Parse (node : JsonObject) : Parameter =
        let name = asString node "name"

        let in' =
            match asString node "in" with
            | "path" -> ParameterIn.Path
            | "header" -> ParameterIn.Header
            | "query" -> ParameterIn.Query
            | "cookie" -> ParameterIn.Cookie
            | other -> failwithf "Unknown 'in' value for parameter: %s" other

        let description = asOpt<string> node "description"
        let required = asOpt<bool> node "required"
        let deprecated = asOpt<bool> node "deprecated"
        let allowEmptyValue = asOpt<bool> node "allowEmptyValue"
        let style = asOpt<string> node "style"
        let explode = asOpt<bool> node "explode"
        let allowReserved = asOpt<bool> node "allowReserved"

        let schema =
            match node.TryGetPropertyValue "schema" with
            | false, _ -> None
            | true, s ->
                let obj = s.AsObject ()

                if obj.ContainsKey "$ref" then
                    Choice2Of2 (Reference.Parse obj) |> Some
                else
                    Choice1Of2 (Schema.Parse obj) |> Some

        let example =
            match node.TryGetPropertyValue "example" with
            | true, e -> Choice1Of2 e |> Some
            | false, _ ->
                match node.TryGetPropertyValue "examples" with
                | false, _ -> None
                | true, e ->
                    e.AsObject ()
                    |> Seq.map (fun (KeyValue (k, v)) ->
                        let obj = v.AsObject ()

                        let parsed =
                            if obj.ContainsKey "$ref" then
                                Choice2Of2 (Reference.Parse obj)
                            else
                                Choice1Of2 (Example.Parse obj)

                        k, parsed
                    )
                    |> Map.ofSeq
                    |> Choice2Of2
                    |> Some

        let content =
            match node.TryGetPropertyValue "content" with
            | false, _ -> None
            | true, c ->
                c.AsObject ()
                |> Seq.map (fun (KeyValue (k, v)) -> k, MediaType.Parse (v.AsObject ()))
                |> Map.ofSeq
                |> Some

        {
            Name = name
            In = in'
            Description = description
            Required = required
            Deprecated = deprecated
            AllowEmptyValue = allowEmptyValue
            Style = style
            Explode = explode
            AllowReserved = allowReserved
            Schema = schema
            Example = example
            Content = content
        }

type RequestBody =
    {
        /// A brief description of the request body. This could contain examples of use.
        /// Possibly CommonMark.
        Description : string option
        /// The content of the request body.
        /// The key is a media type or media type range, see [RFC7231] Appendix D, and the value describes it.
        /// For requests that match multiple keys, only the most specific key is applicable. e.g. text/plain overrides text/*
        Content : Map<string, MediaType>
        /// Determines if the request body is required in the request. Defaults to false.
        Required : bool option
    }

    static member Parse (node : JsonObject) : RequestBody =
        let description = asOpt<string> node "description"

        let content =
            asObj node "content"
            |> Seq.map (fun (KeyValue (k, v)) -> k, MediaType.Parse (v.AsObject ()))
            |> Map.ofSeq

        let required = asOpt<bool> node "required"

        {
            Description = description
            Content = content
            Required = required
        }

type Callback =
    {
        /// For the semantics of the keys, see https://spec.openapis.org/oas/v3.0.0#key-expression
        Patterns : Map<string, PathItem> option
    }

    static member Parse (node : JsonObject) : Callback =
        let patterns =
            node
            |> Seq.map (fun (KeyValue (k, v)) -> k, PathItem.Parse (v.AsObject ()))
            |> Map.ofSeq
            |> function
                | m when m.IsEmpty -> None
                | m -> Some m

        {
            Patterns = patterns
        }

and Operation =
    {
        /// A list of tags for API documentation control.
        /// Tags can be used for logical grouping of operations by resources or any other qualifier.
        Tags : string list option
        /// A short summary of what the operation does.
        Summary : string option
        /// A verbose explanation of the operation behavior, possibly in CommonMark.
        Description : string option
        /// Additional external documentation for this operation.
        ExternalDocs : ExternalDocumentation option
        /// Unique string used to identify the operation.
        /// The id MUST be unique among all operations described in the API.
        /// Tools and libraries MAY use the operationId to uniquely identify an operation, therefore,
        /// it is RECOMMENDED to follow common programming naming conventions.
        OperationId : string option
        /// A list of parameters that are applicable for this operation.
        /// If a parameter is already defined at the Path Item, the new definition will override it but can never remove it.
        /// The list MUST NOT include duplicated parameters.
        /// A unique parameter is defined by a combination of a name and location.
        /// The list can use the Reference Object to link to parameters that are defined at the OpenAPI Object’s components/parameters.
        Parameters : Choice<Parameter, Reference> list option
        /// The request body applicable for this operation.
        /// The requestBody is only supported in HTTP methods where the HTTP 1.1 specification [RFC7231] Section 4.3.1 has explicitly defined semantics for request bodies.
        /// In other cases where the HTTP spec is vague, requestBody SHALL be ignored by consumers.
        RequestBody : Choice<RequestBody, Reference> option
        /// The list of possible responses as they are returned from executing this operation.
        /// Per the spec these are required, but one of the official examples lacks them.
        Responses : Responses option
        /// A map of possible out-of band callbacks related to the parent operation.
        /// The key is a unique identifier for the Callback Object.
        /// Each value in the map is a Callback Object that describes a request that may be initiated by the API provider and the expected responses.
        /// The key value used to identify the callback object is an expression, evaluated at runtime, that identifies a URL to use for the callback operation.
        Callbacks : Map<string, Choice<Callback, Reference>> option
        /// Default value is "false".
        Deprecated : bool option
        /// A declaration of which security mechanisms can be used for this operation.
        /// The list of values includes alternative security requirement objects that can be used.
        /// Only one of the security requirement objects need to be satisfied to authorize a request.
        /// This definition overrides any declared top-level security.
        /// To remove a top-level security declaration, an empty array can be used.
        Security : SecurityRequirement list option
        /// An alternative server array to service this operation.
        /// If an alternative server object is specified at the Path Item Object or Root level, it will be overridden by this value.
        Servers : Server list option
    }

    static member Parse (node : JsonObject) : Operation =
        let tags = asArrOpt'<string> node "tags"
        let summary = asOpt<string> node "summary"
        let description = asOpt<string> node "description"

        let externalDocs =
            asObjOpt node "externalDocs" |> Option.map ExternalDocumentation.Parse

        let operationId = asOpt<string> node "operationId"

        let parameters =
            match node.TryGetPropertyValue "parameters" with
            | false, _ -> None
            | true, p ->
                p.AsArray ()
                |> Seq.map (fun v ->
                    let obj = v.AsObject ()

                    if obj.ContainsKey "$ref" then
                        Choice2Of2 (Reference.Parse obj)
                    else
                        Choice1Of2 (Parameter.Parse obj)
                )
                |> Seq.toList
                |> Some

        let requestBody =
            match node.TryGetPropertyValue "requestBody" with
            | false, _ -> None
            | true, rb ->
                let obj = rb.AsObject ()

                if obj.ContainsKey "$ref" then
                    Choice2Of2 (Reference.Parse obj) |> Some
                else
                    Choice1Of2 (RequestBody.Parse obj) |> Some

        let responses = asObjOpt node "responses" |> Option.map Responses.Parse

        let callbacks =
            match node.TryGetPropertyValue "callbacks" with
            | false, _ -> None
            | true, c ->
                c.AsObject ()
                |> Seq.map (fun (KeyValue (k, v)) ->
                    let obj = v.AsObject ()

                    let parsed =
                        if obj.ContainsKey "$ref" then
                            Choice2Of2 (Reference.Parse obj)
                        else
                            Choice1Of2 (Callback.Parse obj)

                    k, parsed
                )
                |> Map.ofSeq
                |> Some

        let deprecated = asOpt<bool> node "deprecated"

        let security =
            match node.TryGetPropertyValue "security" with
            | false, _ -> None
            | true, s ->
                s.AsArray ()
                |> Seq.map (fun v -> SecurityRequirement.Parse (v.AsObject ()))
                |> Seq.toList
                |> Some

        let servers =
            match node.TryGetPropertyValue "servers" with
            | false, _ -> None
            | true, s ->
                s.AsArray ()
                |> Seq.map (fun v -> Server.Parse (v.AsObject ()))
                |> Seq.toList
                |> Some

        {
            Tags = tags
            Summary = summary
            Description = description
            ExternalDocs = externalDocs
            OperationId = operationId
            Parameters = parameters
            RequestBody = requestBody
            Responses = responses
            Callbacks = callbacks
            Deprecated = deprecated
            Security = security
            Servers = servers
        }

and PathItem =
    {
        /// Allows for an external definition of this path item.
        /// The referenced structure MUST be in the format of a Path Item Object.
        /// If there are conflicts between the referenced definition and this Path Item’s definition, the behavior is undefined.
        Ref : string option
        /// A string summary, intended to apply to all operations in this path.
        Summary : string option
        /// A string description, intended to apply to all operations in this path, possibly in CommonMark
        Description : string option
        /// A definition of a GET operation on this path.
        Get : Operation option
        /// A definition of a PUT operation on this path.
        Put : Operation option
        /// A definition of a POST operation on this path.
        Post : Operation option
        /// A definition of a DELETE operation on this path.
        Delete : Operation option
        /// A definition of an OPTIONS operation on this path.
        Options : Operation option
        /// A definition of a HEAD operation on this path.
        Head : Operation option
        /// A definition of a PATCH operation on this path.
        Patch : Operation option
        /// A definition of a TRACE operation on this path.
        Trace : Operation option
        /// An alternative server array to service all operations in this path.
        Servers : Server list option
        /// A list of parameters that are applicable for all the operations described under this path.
        /// These parameters can be overridden at the operation level, but cannot be removed there.
        /// The list MUST NOT include duplicated parameters.
        /// A unique parameter is defined by a combination of a name and location.
        /// The list can use the Reference Object to link to parameters that are defined at the OpenAPI Object’s components/parameters.
        Parameters : Choice<Parameter, Reference> list option
    }

    static member Parse (node : JsonObject) : PathItem =
        let ref = asOpt<string> node "$ref"
        let summary = asOpt<string> node "summary"
        let description = asOpt<string> node "description"
        let get = asObjOpt node "get" |> Option.map Operation.Parse
        let put = asObjOpt node "put" |> Option.map Operation.Parse
        let post = asObjOpt node "post" |> Option.map Operation.Parse
        let delete = asObjOpt node "delete" |> Option.map Operation.Parse
        let options = asObjOpt node "options" |> Option.map Operation.Parse
        let head = asObjOpt node "head" |> Option.map Operation.Parse
        let patch = asObjOpt node "patch" |> Option.map Operation.Parse
        let trace = asObjOpt node "trace" |> Option.map Operation.Parse

        let servers =
            match node.TryGetPropertyValue "servers" with
            | false, _ -> None
            | true, s ->
                s.AsArray ()
                |> Seq.map (fun v -> Server.Parse (v.AsObject ()))
                |> Seq.toList
                |> Some

        let parameters =
            match node.TryGetPropertyValue "parameters" with
            | false, _ -> None
            | true, p ->
                p.AsArray ()
                |> Seq.map (fun v ->
                    let obj = v.AsObject ()

                    if obj.ContainsKey "$ref" then
                        Choice2Of2 (Reference.Parse obj)
                    else
                        Choice1Of2 (Parameter.Parse obj)
                )
                |> Seq.toList
                |> Some

        {
            Ref = ref
            Summary = summary
            Description = description
            Get = get
            Put = put
            Post = post
            Delete = delete
            Options = options
            Head = head
            Patch = patch
            Trace = trace
            Servers = servers
            Parameters = parameters
        }

type Paths =
    {
        /// A relative path to an individual endpoint.
        /// The field name MUST begin with a slash.
        /// The path is appended (no relative URL resolution) to the expanded URL from the Server Object’s url field in order to construct the full URL.
        /// Path templating is allowed.
        /// When matching URLs, concrete (non-templated) paths would be matched before their templated counterparts.
        /// Templated paths with the same hierarchy but different templated names MUST NOT exist as they are identical.
        /// In case of ambiguous matching, it’s up to the tooling to decide which one to use.
        Fields : Map<string, PathItem> option
    }

    static member Parse (node : JsonObject) : Paths =
        let fields =
            node
            |> Seq.map (fun (KeyValue (k, v)) -> k, PathItem.Parse (v.AsObject ()))
            |> Map.ofSeq
            |> function
                | m when m.IsEmpty -> None
                | m -> Some m

        {
            Fields = fields
        }

type Components =
    {
        Schemas : Map<string, Choice<Schema, Reference>> option
        Responses : Map<string, Choice<Response, Reference>> option
        Parameters : Map<string, Choice<Parameter, Reference>> option
        Examples : Map<string, Choice<Example, Reference>> option
        RequestBodies : Map<string, Choice<RequestBody, Reference>> option
        Headers : Map<string, Choice<Header, Reference>> option
        SecuritySchemes : Map<string, Choice<SecurityScheme, Reference>> option
        Links : Map<string, Choice<Link, Reference>> option
        Callbacks : Map<string, Choice<Callback, Reference>> option
    }

    static member Parse (node : JsonObject) : Components =
        let parseMap (key : string) (parser : JsonObject -> 'T) =
            match node.TryGetPropertyValue key with
            | false, _ -> None
            | true, o ->
                o.AsObject ()
                |> Seq.map (fun (KeyValue (k, v)) ->
                    let obj = v.AsObject ()

                    let parsed =
                        if obj.ContainsKey "$ref" then
                            Choice2Of2 (Reference.Parse obj)
                        else
                            Choice1Of2 (parser obj)

                    k, parsed
                )
                |> Map.ofSeq
                |> Some

        {
            Schemas = parseMap "schemas" Schema.Parse
            Responses = parseMap "responses" Response.Parse
            Parameters = parseMap "parameters" Parameter.Parse
            Examples = parseMap "examples" Example.Parse
            RequestBodies = parseMap "requestBodies" RequestBody.Parse
            Headers = parseMap "headers" Header.Parse
            SecuritySchemes = parseMap "securitySchemes" SecurityScheme.Parse
            Links = parseMap "links" Link.Parse
            Callbacks = parseMap "callbacks" Callback.Parse
        }

type OpenApiSpec =
    {
        OpenApi : Version
        Info : OpenApiInfo
        Servers : Server list option
        /// According to the spec, this is required, but then one of their own examples
        /// does not contain a Paths.
        Paths : Paths option
        Components : Components option
        Security : SecurityRequirement list option
        Tags : Tag list option
        ExternalDocs : ExternalDocumentation option
    }

    static member Parse (node : JsonObject) : OpenApiSpec =
        let openapi = asString node "openapi" |> Version
        let info = asObj node "info" |> OpenApiInfo.Parse

        let servers =
            match node.TryGetPropertyValue "servers" with
            | false, _ -> None
            | true, s ->
                s.AsArray ()
                |> Seq.map (fun v -> Server.Parse (v.AsObject ()))
                |> Seq.toList
                |> Some

        let paths = asObjOpt node "paths" |> Option.map Paths.Parse
        let components = asObjOpt node "components" |> Option.map Components.Parse

        let security =
            match node.TryGetPropertyValue "security" with
            | false, _ -> None
            | true, s ->
                s.AsArray ()
                |> Seq.map (fun v -> SecurityRequirement.Parse (v.AsObject ()))
                |> Seq.toList
                |> Some

        let tags =
            match node.TryGetPropertyValue "tags" with
            | false, _ -> None
            | true, t ->
                t.AsArray ()
                |> Seq.map (fun v -> Tag.Parse (v.AsObject ()))
                |> Seq.toList
                |> Some

        let externalDocs =
            asObjOpt node "externalDocs" |> Option.map ExternalDocumentation.Parse

        {
            OpenApi = openapi
            Info = info
            Servers = servers
            Paths = paths
            Components = components
            Security = security
            Tags = tags
            ExternalDocs = externalDocs
        }
