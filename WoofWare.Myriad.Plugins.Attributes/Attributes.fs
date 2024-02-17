namespace WoofWare.Myriad.Plugins

open System

/// Attribute indicating a record type to which the "Remove Options" Myriad
/// generator should apply during build.
/// The purpose of this generator is to strip the `option` modifier from types.
type RemoveOptionsAttribute () =
    inherit Attribute ()

/// Attribute indicating an interface type for which the "Generate Mock" Myriad
/// generator should apply during build.
/// This generator creates a record which implements the interface,
/// but where each method is represented as a record field, so you can use
/// record update syntax to easily specify partially-implemented mock objects.
/// You may optionally specify `isInternal = false` to get a mock with the public visibility modifier.
type GenerateMockAttribute (isInternal : bool) =
    inherit Attribute ()
    /// The default value of `isInternal`, the optional argument to the GenerateMockAttribute constructor.
    static member DefaultIsInternal = true

    /// Shorthand for the "isExtensionMethod = false" constructor; see documentation there for details.
    new () = GenerateMockAttribute GenerateMockAttribute.DefaultIsInternal

/// Attribute indicating a record type to which the "Add JSON serializer" Myriad
/// generator should apply during build.
/// The purpose of this generator is to create methods (possibly extension methods) of the form
/// `{TypeName}.toJsonNode : {TypeName} -> System.Text.Json.Nodes.JsonNode`.
///
/// If you supply isExtensionMethod = true, you will get extension methods.
/// These can only be consumed from F#, but the benefit is that they don't use up the module name
/// (since by default we create a module called "{TypeName}").
type JsonSerializeAttribute (isExtensionMethod : bool) =
    inherit Attribute ()

    /// The default value of `isExtensionMethod`, the optional argument to the JsonSerializeAttribute constructor.
    static member DefaultIsExtensionMethod = false

    /// Shorthand for the "isExtensionMethod = false" constructor; see documentation there for details.
    new () = JsonSerializeAttribute JsonSerializeAttribute.DefaultIsExtensionMethod

/// Attribute indicating a record type to which the "Add JSON parse" Myriad
/// generator should apply during build.
/// The purpose of this generator is to create methods (possibly extension methods) of the form
/// `{TypeName}.jsonParse : System.Text.Json.Nodes.JsonNode -> {TypeName}`.
///
/// If you supply isExtensionMethod = true, you will get extension methods.
/// These can only be consumed from F#, but the benefit is that they don't use up the module name
/// (since by default we create a module called "{TypeName}").
type JsonParseAttribute (isExtensionMethod : bool) =
    inherit Attribute ()

    /// The default value of `isExtensionMethod`, the optional argument to the JsonParseAttribute constructor.
    static member DefaultIsExtensionMethod = false

    /// Shorthand for the "isExtensionMethod = false" constructor; see documentation there for details.
    new () = JsonParseAttribute JsonParseAttribute.DefaultIsExtensionMethod

/// Attribute indicating a record type to which the "create HTTP client" Myriad
/// generator should apply during build.
/// This generator is intended to replicate much of the functionality of RestEase,
/// i.e. to stamp out HTTP REST clients from interfaces defining the API.
type HttpClientAttribute () =
    inherit Attribute ()

/// Attribute indicating a DU type to which the "create catamorphism" Myriad
/// generator should apply during build.
/// Supply the `typeName` for the name of the record type we will generate, which contains
/// all the catas required; for example, "MyThing" would generate:
/// type MyThing<'a, 'b> = { Du1 : Du1Cata<'a, 'b> ; Du2 : Du2Cata<'a, 'b> }.
type CreateCatamorphismAttribute (typeName : string) =
    inherit Attribute ()
