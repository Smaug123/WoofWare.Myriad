namespace WoofWare.Myriad.Plugins

open System
open System.Net.Http
open System.Text
open Fantomas.FCS.Syntax
open Fantomas.FCS.SyntaxTrivia
open Fantomas.FCS.Xml
open Myriad.Core

/// Attribute indicating a record type to which the "create HTTP client" Myriad
/// generator should apply during build.
type HttpClientAttribute () =
    inherit Attribute ()

[<RequireQualifiedAccess>]
module internal HttpClientGenerator =
    open Fantomas.FCS.Text.Range
    open Myriad.Core.Ast

    /// E.g. converts `[<Get "blah">]` to (HttpMethod.Get, "blah")
    let extractHttpInformation (attrs : SynAttribute list) : HttpMethod * string =
        let matchingAttrs =
            attrs
            |> List.choose (fun attr ->
                match attr.TypeName.AsString with
                | "Get"
                | "GetAttribute"
                | "RestEase.Get"
                | "RestEase.GetAttribute" -> Some (HttpMethod.Get, attr.ArgExpr)
                | "Post"
                | "PostAttribute"
                | "RestEase.Post"
                | "RestEase.PostAttribute" -> Some (HttpMethod.Post, attr.ArgExpr)
                | "Put"
                | "PutAttribute"
                | "RestEase.Put"
                | "RestEase.PutAttribute" -> Some (HttpMethod.Put, attr.ArgExpr)
                | "Delete"
                | "DeleteAttribute"
                | "RestEase.Delete"
                | "RestEase.DeleteAttribute" -> Some (HttpMethod.Delete, attr.ArgExpr)
                | "Head"
                | "HeadAttribute"
                | "RestEase.Head"
                | "RestEase.HeadAttribute" -> Some (HttpMethod.Head, attr.ArgExpr)
                | "Options"
                | "OptionsAttribute"
                | "RestEase.Options"
                | "RestEase.OptionsAttribute" -> Some (HttpMethod.Options, attr.ArgExpr)
                | "Patch"
                | "PatchAttribute"
                | "RestEase.Patch"
                | "RestEase.PatchAttribute" -> Some (HttpMethod.Patch, attr.ArgExpr)
                | "Trace"
                | "TraceAttribute"
                | "RestEase.Trace"
                | "RestEase.TraceAttribute" -> Some (HttpMethod.Trace, attr.ArgExpr)
                | _ -> None
            )

        match matchingAttrs with
        | [ (meth, arg) ] ->
            match arg with
            | SynExpr.Const (SynConst.String (text, SynStringKind.Regular, _), _) -> meth, text
            | arg ->
                failwithf "Unrecognised AST member in attribute argument. Only regular strings are supported: %+A" arg
        | [] -> failwith "Required exactly one recognised RestEase attribute on member, but got none"
        | matchingAttrs ->
            failwithf "Required exactly one recognised RestEase attribute on member, but got %i" matchingAttrs.Length

    let createModule (ns : LongIdent) (interfaceType : SynTypeDefn) : SynModuleOrNamespace =
        let (SynTypeDefn (synComponentInfo, synTypeDefnRepr, members, _, _, _)) =
            interfaceType

        let members =
            match synTypeDefnRepr with
            | SynTypeDefnRepr.ObjectModel (_kind, members, _) ->
                members
                |> List.map (fun defn ->
                    match defn with
                    | SynMemberDefn.AbstractSlot (slotSig, flags, _, _) ->
                        match flags.MemberKind with
                        | SynMemberKind.Member -> ()
                        | kind -> failwithf "Unrecognised member kind: %+A" kind

                        if not flags.IsInstance then
                            failwith "member was not an instance member"

                        match slotSig with
                        | SynValSig (attrs,
                                     ident,
                                     _typeParams,
                                     synType,
                                     arity,
                                     isInline,
                                     isMutable,
                                     _xmlDoc,
                                     accessibility,
                                     synExpr,
                                     _,
                                     _) ->
                            if isInline then
                                failwith "inline members not supported"

                            if isMutable then
                                failwith "mutable members not supported"

                            match accessibility with
                            | Some (SynAccess.Internal _)
                            | Some (SynAccess.Private _) -> failwith "only public members are supported"
                            | _ -> ()

                            match synExpr with
                            | Some _ -> failwith "literal members are not supported"
                            | None -> ()

                            let attrs = attrs |> List.collect (fun a -> a.Attributes)

                            let arity =
                                match arity with
                                | SynValInfo ([ curriedArgs ], SynArgInfo ([], false, _)) -> curriedArgs
                                | SynValInfo (curriedArgs, SynArgInfo ([], false, _)) ->
                                    failwithf "only tupled arguments are supported, but got: %+A" curriedArgs
                                | SynValInfo (_, info) ->
                                    failwithf
                                        "only bare return values like `Task<foo>` are supported, but got: %+A"
                                        info

                            let args, ret =
                                match synType with
                                | SynType.Fun (argType, Task returnType, _, _) -> argType, returnType
                                | _ ->
                                    failwithf
                                        "Expected a return type of a generic Task; bad signature was: %+A"
                                        synType

                            extractHttpInformation attrs, ident, args, ret, arity
                    | _ -> failwithf "Unrecognised member definition: %+A" defn
                )
            | _ -> failwithf "Unrecognised SynTypeDefnRepr: %+A" synTypeDefnRepr

        failwithf "TODO: %+A" members

/// Myriad generator that provides an HTTP client for an interface type using RestEase annotations.
[<MyriadGenerator("http-client")>]
type HttpClientGenerator () =

    interface IMyriadGenerator with
        member _.ValidInputExtensions = [ ".fs" ]

        member _.Generate (context : GeneratorContext) =
            let ast, _ =
                Ast.fromFilename context.InputFilename |> Async.RunSynchronously |> Array.head

            let types = Ast.extractTypeDefn ast

            let namespaceAndTypes =
                types
                |> List.choose (fun (ns, types) ->
                    match types |> List.filter Ast.hasAttribute<HttpClientAttribute> with
                    | [] -> None
                    | types -> Some (ns, types)
                )

            let modules =
                namespaceAndTypes
                |> List.collect (fun (ns, types) ->
                    types
                    |> List.map (fun interfaceType ->
                        let clientModule = HttpClientGenerator.createModule ns interfaceType
                        clientModule
                    )
                )

            Output.Ast modules
