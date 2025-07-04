namespace WoofWare.Myriad.Plugins

open System.Collections.Generic
open System.Threading
open Fantomas.FCS.Syntax
open Fantomas.FCS.Xml
open Fantomas.FCS.Text.Range
open WoofWare.Whippet.Fantomas

type internal SwaggerClientConfig =
    {
        /// Additionally create a mock with `InterfaceMockGenerator`, with the given boolean arg.
        /// (`None` means "no mock".)
        CreateMock : bool option
        ClassName : string
    }

type internal Produces =
    // TODO: this will cope with decoding JSON, plain text, etc
    | Produces of string
    | OctetStream

type internal Endpoint =
    {
        DocString : PreXmlDoc
        Produces : Produces
        ReturnType : SwaggerV2.Definition
        Method : WoofWare.Myriad.Plugins.HttpMethod
        Operation : SwaggerV2.OperationId
        Parameters : SwaggerV2.SwaggerParameter list
        Endpoint : string
    }

type internal TypeEntry =
    {
        /// If we had to define a type for this, here it is.
        FSharpDefinition : SynTypeDefn option
        /// SynType you use in e.g. a type annotation to refer to this type in F# code.
        Signature : SynType
    }

type internal Types =
    {
        ByHandle : IReadOnlyDictionary<string, TypeEntry>
        ByDefinition : IReadOnlyDictionary<SwaggerV2.Definition, TypeEntry>
    }

[<RequireQualifiedAccess>]
module internal SwaggerClientGenerator =

    let internal log (_ : string) = ()

    let renderType (types : Types) (defn : SwaggerV2.Definition) : SynType option =
        match types.ByDefinition.TryGetValue defn with
        | true, v -> Some v.Signature
        | false, _ ->

        match defn with
        | SwaggerV2.Definition.Handle h ->
            match types.ByHandle.TryGetValue h with
            | false, _ -> None
            | true, v -> Some v.Signature
        | SwaggerV2.Definition.Object _ -> failwith "should not hit"
        | SwaggerV2.Definition.Array _ -> failwith "should not hit"
        | SwaggerV2.Definition.Unspecified -> failwith "should not hit"
        | SwaggerV2.Definition.String -> SynType.string |> Some
        | SwaggerV2.Definition.Boolean -> SynType.bool |> Some
        | SwaggerV2.Definition.Integer _ -> SynType.int |> Some
        | SwaggerV2.Definition.File -> SynType.createLongIdent' [ "System" ; "IO" ; "Stream" ] |> Some

    /// Returns None if we lacked the information required to do this.
    /// bigCache is a map of e.g. {"securityDefinition": {Defn : F# type}}.
    let rec defnToType
        (anonymousTypeCount : int ref)
        (handlesMap : Dictionary<string, TypeEntry>)
        (bigCache : Dictionary<string, Dictionary<SwaggerV2.Definition, TypeEntry>>)
        (thisKey : string)
        (typeName : string option)
        (d : SwaggerV2.Definition)
        : TypeEntry option
        =
        let cache =
            match bigCache.TryGetValue thisKey with
            | false, _ ->
                let d = Dictionary ()
                bigCache.Add (thisKey, d)
                d
            | true, d -> d

        let handleKey =
            match typeName with
            | None -> None
            | Some typeName -> $"#/%s{thisKey}/%s{typeName}" |> Some

        match handleKey with
        | Some hk when handlesMap.ContainsKey hk ->
            let result = handlesMap.[hk]
            cache.[d] <- result
            Some result

        | _ ->

        match cache.TryGetValue d with
        | true, v ->
            match handleKey with
            | None -> ()
            | Some key -> handlesMap.Add (key, v)

            Some v
        | false, _ ->

        let result =
            match d with
            | SwaggerV2.Definition.Object obj ->
                let requiredFields = obj.Required |> Option.defaultValue [] |> Set.ofList

                let namedProperties =
                    obj.Properties
                    |> Option.map Seq.cast
                    |> Option.defaultValue Seq.empty
                    |> Seq.map (fun (KeyValue (fieldName, defn)) ->
                        // TODO this is a horrible hack and is incomplete, e.g. if we contain an array of ourself
                        // Special case for when this is a reference to this very type
                        let isOurself =
                            match defn with
                            | SwaggerV2.Definition.Handle h ->
                                match h.Split '/' with
                                | [| "#" ; location ; ty |] when location = thisKey && Some ty = typeName ->
                                    SynType.named ty |> Some
                                | _ -> None
                            | _ -> None

                        let jsonPropertyName =
                            SynExpr.CreateConst (fieldName : string)
                            |> SynAttribute.create (
                                SynLongIdent.createS'
                                    [ "System" ; "Text" ; "Json" ; "Serialization" ; "JsonPropertyName" ]
                            )

                        match isOurself with
                        | Some alreadyDone ->
                            let ty =
                                if Set.contains fieldName requiredFields then
                                    alreadyDone
                                else
                                    SynType.option alreadyDone

                            {
                                Attrs = [ jsonPropertyName ]
                                Type = ty
                                Ident = Some (Ident.createSanitisedTypeName fieldName)
                            }
                            |> SynField.make
                            |> Some
                        | None ->

                        let defn' = defnToType anonymousTypeCount handlesMap bigCache thisKey None defn

                        match defn' with
                        | None -> None
                        | Some defn' ->
                            let ty =
                                if Set.contains fieldName requiredFields then
                                    defn'.Signature
                                else
                                    defn'.Signature |> SynType.option

                            {
                                Attrs = [ jsonPropertyName ]
                                Ident = Ident.createSanitisedTypeName fieldName |> Some
                                Type = ty
                            }
                            |> SynField.make
                            |> Some
                    )
                    |> Seq.toList

                let additionalProperties =
                    match obj.AdditionalProperties with
                    | None ->
                        {
                            Attrs =
                                [
                                    SynAttribute.create
                                        (SynLongIdent.createS'
                                            [ "System" ; "Text" ; "Json" ; "Serialization" ; "JsonExtensionData" ])
                                        (SynExpr.CreateConst ())
                                ]
                            Ident = Ident.create "AdditionalProperties" |> Some
                            Type =
                                SynType.app'
                                    (SynType.createLongIdent' [ "System" ; "Collections" ; "Generic" ; "Dictionary" ])
                                    [
                                        SynType.string
                                        SynType.createLongIdent' [ "System" ; "Text" ; "Json" ; "Nodes" ; "JsonNode" ]
                                    ]
                        }
                        |> SynField.make
                        |> List.singleton
                        |> Some
                    | Some SwaggerV2.AdditionalProperties.Never -> Some []
                    | Some (SwaggerV2.AdditionalProperties.Constrained defn) ->
                        let defn' = defnToType anonymousTypeCount handlesMap bigCache thisKey None defn

                        match defn' with
                        | None -> None
                        | Some defn' ->
                            {
                                Attrs =
                                    [
                                        SynAttribute.create
                                            (SynLongIdent.createS'
                                                [ "System" ; "Text" ; "Json" ; "Serialization" ; "JsonExtensionData" ])
                                            (SynExpr.CreateConst ())
                                    ]
                                Ident = Ident.create "AdditionalProperties" |> Some
                                Type =
                                    SynType.app'
                                        (SynType.createLongIdent'
                                            [ "System" ; "Collections" ; "Generic" ; "Dictionary" ])
                                        [ SynType.string ; defn'.Signature ]
                            }
                            |> SynField.make
                            |> List.singleton
                            |> Some

                match additionalProperties with
                | None -> None
                | Some additionalProperties ->

                match List.allSome namedProperties with
                | None -> None
                | Some namedProperties ->

                let fSharpTypeName =
                    match typeName with
                    | None -> $"Type%i{Interlocked.Increment anonymousTypeCount}"
                    | Some typeName -> typeName

                let properties = additionalProperties @ namedProperties

                let properties =
                    if properties.IsEmpty then
                        // sigh, they didn't give us any properties at all; let's make one up
                        {
                            Attrs = []
                            Ident = Some (Ident.create "_SchemaUnspecified")
                            Type = SynType.obj
                        }
                        |> SynField.make
                        |> List.singleton
                    else
                        properties

                let defn =
                    let sci =
                        SynComponentInfo.create (Ident.createSanitisedTypeName fSharpTypeName)
                        |> SynComponentInfo.addAttributes
                            [
                                SynAttribute.create (SynLongIdent.createS' [ "JsonParse" ]) (SynExpr.CreateConst true)
                                SynAttribute.create
                                    (SynLongIdent.createS' [ "JsonSerialize" ])
                                    (SynExpr.CreateConst true)
                            ]
                        |> fun sci ->
                            match obj.Description with
                            | None -> sci
                            | Some doc -> sci |> SynComponentInfo.withDocString (PreXmlDoc.create doc)

                    properties |> SynTypeDefnRepr.record |> SynTypeDefn.create sci

                let defn =
                    {
                        Signature = SynType.named fSharpTypeName
                        FSharpDefinition = Some defn
                    }

                defn |> Some

            | SwaggerV2.Definition.Array elt ->
                let child = defnToType anonymousTypeCount handlesMap bigCache thisKey None elt.Items

                match child with
                | None -> None
                | Some child ->
                    let defn =
                        {
                            Signature = SynType.list child.Signature
                            FSharpDefinition = None
                        }

                    Some defn
            | SwaggerV2.Definition.String ->
                {
                    Signature = SynType.string
                    FSharpDefinition = None
                }
                |> Some
            | SwaggerV2.Definition.Boolean ->
                {
                    Signature = SynType.bool
                    FSharpDefinition = None
                }
                |> Some
            | SwaggerV2.Definition.Unspecified ->
                {
                    Signature = SynType.unit
                    FSharpDefinition = None
                }
                |> Some
            | SwaggerV2.Definition.Integer _ ->
                {
                    Signature = SynType.createLongIdent' [ "int" ]
                    FSharpDefinition = None
                }
                |> Some
            | SwaggerV2.Definition.File ->
                {
                    Signature = SynType.createLongIdent' [ "System" ; "IO" ; "Stream" ]
                    FSharpDefinition = None
                }
                |> Some
            | SwaggerV2.Definition.Handle s ->
                let split = s.Split '/' |> List.ofArray

                match split with
                | [ "#" ; _location ; _handle ] ->
                    match handlesMap.TryGetValue s with
                    | false, _ -> None
                    | true, computed ->
                        let defn =
                            {
                                FSharpDefinition = None
                                Signature = computed.Signature
                            }

                        defn |> Some
                | _ -> failwith $"we don't know how to deal with object handle %s{s}"

        match result with
        | None -> None
        | Some result ->

        match handleKey with
        | None -> ()
        | Some handleKey -> handlesMap.Add (handleKey, result)

        cache.Add (d, result)
        Some result

    let instantiateRequiredTypes (types : Types) : SynModuleDecl =
        types.ByDefinition
        |> Seq.choose (fun (KeyValue (_defn, typeEntry)) -> typeEntry.FSharpDefinition)
        |> Seq.toList
        |> SynModuleDecl.createTypes

    type private IsIn =
        | Path of str : string
        | Query of str : string
        | Body

    let computeType
        (options : SwaggerClientConfig)
        (basePath : string)
        (types : Types)
        (clientDocString : PreXmlDoc)
        (endpoints : Endpoint list)
        : SynModuleDecl list
        =
        endpoints
        |> List.choose (fun ep ->
            let name = (Ident.createSanitisedTypeName (ep.Operation.ToString ())).idText

            match renderType types ep.ReturnType with
            | None ->
                log $"Skipping %O{ep.Operation}: Couldn't render return type: %O{ep.ReturnType}"
                None
            | Some returnType ->

            let pars =
                ep.Parameters
                |> List.map (fun par ->
                    let inParam =
                        match par.In with
                        | SwaggerV2.ParameterIn.Unrecognised (f, name) ->
                            log
                                $"Skipping %O{ep.Operation} at %s{ep.Endpoint}: unrecognised In parameter %s{f} with name %s{name}"

                            None
                        | SwaggerV2.ParameterIn.Body -> Some IsIn.Body
                        | SwaggerV2.ParameterIn.Query name -> Some (IsIn.Query name)
                        | SwaggerV2.ParameterIn.Path name -> Some (IsIn.Path name)

                    match inParam with
                    | None -> None
                    | Some inParam ->

                    match renderType types par.Type with
                    | None ->
                        // Couldn't render the return type
                        // failwith "Did not have a type here"
                        log $"Skipping %O{ep.Operation}: Couldn't render parameter: %O{par.Type}"
                        None
                    | Some v -> Some (Ident.createSanitisedParamName par.Name, inParam, v)
                )
                |> List.allSome

            match pars with
            | None -> None
            | Some pars ->

            let arity =
                SynValInfo.SynValInfo (
                    [
                        ep.Parameters
                        |> List.map (fun par ->
                            let name = par.Name |> Ident.create |> Some
                            SynArgInfo.SynArgInfo ([], false, name)
                        )
                        |> fun l -> l @ [ SynArgInfo.SynArgInfo ([], true, Some (Ident.create "ct")) ]
                    ],
                    SynArgInfo.SynArgInfo ([], false, None)
                )

            let domain =
                let ctParam =
                    SynType.signatureParamOfType
                        []
                        (SynType.createLongIdent' [ "System" ; "Threading" ; "CancellationToken" ])
                        true
                        (Some (Ident.create "ct"))

                let argParams =
                    pars
                    |> List.map (fun (ident, isIn, t) ->
                        let attr : SynAttribute list =
                            match isIn with
                            | IsIn.Path name ->
                                SynAttribute.create
                                    (SynLongIdent.createS' [ "RestEase" ; "Path" ])
                                    (SynExpr.CreateConst name)
                                |> List.singleton
                            | IsIn.Query name ->
                                SynAttribute.create
                                    (SynLongIdent.createS' [ "RestEase" ; "Query" ])
                                    (SynExpr.CreateConst name)
                                |> List.singleton
                            | IsIn.Body ->
                                SynAttribute.create
                                    (SynLongIdent.createS' [ "RestEase" ; "Body" ])
                                    (SynExpr.CreateConst ())
                                |> List.singleton

                        SynType.signatureParamOfType attr t false (Some ident)
                    )

                SynType.tupleNoParen (argParams @ [ ctParam ]) |> Option.get

            let attrs =
                [
                    SynAttribute.create
                        (SynLongIdent.createS' [ "RestEase" ; ep.Method.ToString () ])
                        // Gitea, at least, starts with a `/`, which `Uri` then takes to indicate an absolute path.
                        (SynExpr.CreateConst (ep.Endpoint.TrimStart '/'))

                    match ep.Produces with
                    | Produces.Produces contentType ->
                        SynAttribute.create
                            (SynLongIdent.createS' [ "RestEase" ; "Header" ])
                            // Gitea, at least, starts with a `/`, which `Uri` then takes to indicate an absolute path.
                            (SynExpr.tuple [ SynExpr.CreateConst "Content-Type" ; SynExpr.CreateConst contentType ])
                    | Produces.OctetStream ->
                        SynAttribute.create
                            (SynLongIdent.createS' [ "RestEase" ; "Header" ])
                            // Gitea, at least, starts with a `/`, which `Uri` then takes to indicate an absolute path.
                            (SynExpr.tuple
                                [
                                    SynExpr.CreateConst "Content-Type"
                                    SynExpr.CreateConst "application/octet-stream"
                                ])
                ]

            returnType
            |> SynType.task
            |> SynType.toFun [ domain ]
            |> SynMemberDefn.abstractMember attrs (SynIdent.createS name) None arity ep.DocString
            |> Some
        )
        |> SynTypeDefnRepr.interfaceType
        |> SynTypeDefn.create (
            let attrs =
                [
                    yield SynAttribute.create (SynLongIdent.createS' [ "HttpClient" ]) (SynExpr.CreateConst false)
                    yield
                        SynAttribute.create
                            (SynLongIdent.createS' [ "RestEase" ; "BasePath" ])
                            (SynExpr.CreateConst basePath)
                    match options.CreateMock with
                    | None -> ()
                    | Some createMockValue ->
                        yield
                            SynAttribute.create
                                (SynLongIdent.createS' [ "GenerateMock" ])
                                (SynExpr.CreateConst createMockValue)
                ]

            SynComponentInfo.create (Ident.create ("I" + options.ClassName))
            |> SynComponentInfo.withDocString clientDocString
            |> SynComponentInfo.addAttributes attrs
        )
        |> List.singleton
        |> SynModuleDecl.createTypes
        |> List.singleton

open Myriad.Core
open System.IO

[<RequireQualifiedAccess>]
module internal SwaggerV2Generator =
    let generate (pars : Map<string, string>) (contents : SwaggerV2.SwaggerV2) : Output =
        let scheme =
            let preferred = SwaggerV2.Scheme "https"

            if List.isEmpty contents.Schemes then
                failwith "no schemes specified in API spec!"

            if List.contains preferred contents.Schemes then
                preferred
            else
                List.head contents.Schemes

        let clientDocstring = contents.Info.Description |> PreXmlDoc.create

        let basePath = contents.BasePath

        let typeDefs =
            let bigCache = Dictionary<_, Dictionary<_, _>> ()

            let countAll () =
                (0, bigCache) ||> Seq.fold (fun count (KeyValue (_, v)) -> count + v.Count)

            let byHandle = Dictionary ()
            let anonymousTypeCount = ref 0

            let rec go (contents : ((string * SwaggerV2.Definition) * string) list) =
                let lastRound = countAll ()

                contents
                |> List.filter (fun ((name, defn), defnClass) ->
                    let doIt =
                        SwaggerClientGenerator.defnToType
                            anonymousTypeCount
                            byHandle
                            bigCache
                            defnClass
                            (Some name)
                            defn

                    match doIt with
                    | None -> true
                    | Some _ -> false
                )
                |> fun remaining ->
                    if not remaining.IsEmpty then
                        let currentCount = countAll ()

                        if currentCount = lastRound then
                            for (name, remaining), kind in remaining do
                                SwaggerClientGenerator.log $"Remaining: %s{name} (%s{kind})"

                            SwaggerClientGenerator.log "--------"

                            for KeyValue (handle, defn) in byHandle do
                                SwaggerClientGenerator.log $"Known: %s{handle} %O{defn}"

                            // TODO: ohh noooooo the Gitea spec is genuinely circular,
                            // it's impossible to construct a Repository type
                            // we're going to have to somehow detect this case and break the cycle
                            // by artificially making a property optional
                            // :sob: Gitea why are you like this
                            // failwith "Made no further progress rendering types"
                            ()
                        else
                            go remaining

            seq {
                for defnClass in [ "definitions" ; "responses" ] do
                    match defnClass with
                    | "definitions" ->
                        for KeyValue (k, v) in contents.Definitions do
                            yield (k, v), defnClass
                    | "responses" ->
                        for KeyValue (k, v) in contents.Responses do
                            yield (k, v.Schema), defnClass
                    | _ -> failwith "oh no"
            }
            |> Seq.toList
            |> go

            let result = Dictionary ()

            for KeyValue (_container, types) in bigCache do
                for KeyValue (defn, rendered) in types do
                    result.TryAdd (defn, rendered) |> ignore<bool>

            {
                ByHandle = byHandle
                ByDefinition = result :> IReadOnlyDictionary<_, _>
            }

        let summary =
            contents.Paths
            |> Seq.collect (fun (KeyValue (path, endpoints)) ->
                endpoints
                |> Seq.choose (fun (KeyValue (method, endpoint)) ->
                    let docstring = endpoint.Summary |> PreXmlDoc.create

                    let produces =
                        match endpoint.Produces with
                        | None -> Produces.Produces "json"
                        | Some [] -> failwith $"API specified empty Produces: %s{path} (%O{method})"
                        | Some [ SwaggerV2.MimeType "application/octet-stream" ] -> Produces.OctetStream
                        | Some [ SwaggerV2.MimeType "application/json" ] -> Produces.Produces "json"
                        | Some [ SwaggerV2.MimeType (StartsWith "text/" t) ] -> Produces.Produces t
                        | Some [ SwaggerV2.MimeType s ] ->
                            failwithf
                                $"we don't support non-JSON Produces right now, got: %s{s} (%s{path} %O{method})"
                        | Some (_ :: _) ->
                            failwith $"we don't support multiple Produces right now, at %s{path} (%O{method})"

                    let returnType =
                        endpoint.Responses
                        |> Seq.choose (fun (KeyValue (response, defn)) ->
                            if 200 <= response && response < 300 then
                                Some defn
                            else
                                None
                        )
                        |> Seq.toList

                    let returnType =
                        match returnType with
                        | [ t ] -> Some t
                        | [] -> failwith $"got no successful response results, %s{path} %O{method}"
                        | _ ->
                            SwaggerClientGenerator.log
                                $"Ignoring %s{path} %O{method} due to multiple success responses"
                            // can't be bothered to work out how to deal with multiple success
                            // results right now
                            None

                    match returnType with
                    | None -> None
                    | Some returnType ->

                    {
                        Method = method
                        Produces = produces
                        DocString = docstring
                        ReturnType = returnType
                        Operation = endpoint.OperationId
                        Parameters = endpoint.Parameters |> Option.defaultValue []
                        Endpoint = path
                    }
                    |> Some
                )
                |> Seq.toList
            )
            |> Seq.toList

        let config =
            let createMock =
                match Map.tryFind "GENERATEMOCKVISIBILITY" pars with
                | None -> None
                | Some v ->
                    match v.ToLowerInvariant () with
                    | "internal" -> Some true
                    | "public" -> Some false
                    | _ ->
                        failwith
                            $"Expected GenerateMockVisibility parameter to be 'internal' or 'public', but was: '%s{v.ToLowerInvariant ()}'"

            let className =
                match Map.tryFind "CLASSNAME" pars with
                | None -> failwith "You must supply the <ClassName /> parameter in <MyriadParams />."
                | Some v -> v

            {
                CreateMock = createMock
                ClassName = className
            }

        let ty =
            SwaggerClientGenerator.computeType config basePath typeDefs clientDocstring summary

        [
            yield
                SynModuleDecl.Open (
                    SynOpenDeclTarget.ModuleOrNamespace (
                        SynLongIdent.createS' [ "WoofWare" ; "Myriad" ; "Plugins" ],
                        range0
                    ),
                    range0
                )
            yield SwaggerClientGenerator.instantiateRequiredTypes typeDefs
            yield! ty
        ]
        |> SynModuleOrNamespace.createNamespace [ Ident.create config.ClassName ]
        |> List.singleton
        |> Output.Ast

/// Myriad generator that stamps out an interface and class to access a Swagger-specified API.
[<MyriadGenerator("swagger-client")>]
type SwaggerClientGenerator () =

    interface IMyriadGenerator with
        member _.ValidInputExtensions = [ ".json" ]

        member _.Generate (context : GeneratorContext) =
            let pars = MyriadParamParser.render context.AdditionalParameters

            let pars =
                pars
                |> Map.toSeq
                |> Seq.map (fun (k, v) -> k.ToUpperInvariant (), v)
                |> Map.ofSeq

            if pars.IsEmpty then
                failwith "No parameters given. You must supply the <ClassName /> parameter in <MyriadParams />."

            let contents = File.ReadAllText context.InputFilename |> SwaggerV2.parse

            match contents with
            | Ok contents -> SwaggerV2Generator.generate pars contents
            | Error node -> failwith "Input was not a Swagger 2 spec"
