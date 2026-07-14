namespace WoofWare.Myriad.Plugins

open System.IO
open System.Reflection
open Fantomas.FCS.Syntax
open Fantomas.FCS.Text.Range
open WoofWare.Whippet.Fantomas

/// Splices the ArgParserRuntime source (carried as an embedded resource, and compiled into this
/// assembly for testing) into generated output as a private module, so that generated parsers are
/// self-contained: the exact code which was property-tested in this repository ships inside the
/// generated file, and consumers need no runtime dependency.
[<RequireQualifiedAccess>]
module internal ArgParserRuntimeEmbed =

    let private runtimeSource : Lazy<string> =
        lazy
            use stream =
                Assembly.GetExecutingAssembly().GetManifestResourceStream "ArgParserRuntime.fs"

            use reader = new StreamReader (stream)
            reader.ReadToEnd ()

    /// The declarations of the runtime: the namespace-level opens of ArgParserRuntime.fs followed
    /// by the contents of its `module internal ArgParserRuntime`.
    let private runtimeDecls : Lazy<SynModuleDecl list> =
        lazy
            match Ast.parse runtimeSource.Value with
            | ParsedInput.ImplFile (ParsedImplFileInput (contents = [ SynModuleOrNamespace (decls = decls) ])) ->
                decls
                |> List.collect (fun decl ->
                    match decl with
                    | SynModuleDecl.Open _ -> [ decl ]
                    | SynModuleDecl.NestedModule (decls = moduleDecls) -> moduleDecls
                    | _ -> []
                )
            | _ -> failwith "could not parse the embedded ArgParserRuntime.fs; this is a bug in WoofWare.Myriad"

    /// A private module named `moduleName` containing the runtime, to be emitted into a generated
    /// namespace fragment. The name must be unique within the namespace across all generated
    /// files, so derive it from the input file name.
    let asModule (moduleName : string) : SynModuleDecl =
        let info =
            SynComponentInfo.create (Ident.create moduleName)
            |> SynComponentInfo.setAccessibility (Some (SynAccess.Private range0))
            |> SynComponentInfo.withDocString (
                PreXmlDoc.create
                    "The WoofWare.Myriad argument-parser runtime, embedded verbatim into this generated file."
            )

        SynModuleDecl.nestedModule info runtimeDecls.Value

    /// The name of the embedded runtime module for a namespace, derived from a type for which a
    /// parser is being generated there. That type's name is necessarily unique across the
    /// project's input files (two files defining the same type in one namespace would fail to
    /// compile anyway), unlike e.g. the input file's base name, so runtime modules emitted into
    /// the same namespace from different generated files cannot collide.
    let moduleName (taggedTypeName : string) : string = "ArgParserRuntime_" + taggedTypeName
