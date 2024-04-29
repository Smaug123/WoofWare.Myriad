namespace WoofWare.Myriad.Plugins

open System

/// Module containing duplicates of the supported RestEase attributes, in case you don't want
/// to take a dependency on RestEase.
[<RequireQualifiedAccess>]
module RestEase =
    /// Indicates that a method represents an HTTP Get query to the specified endpoint.
    type GetAttribute (path : string) =
        inherit Attribute ()

    /// Indicates that a method represents an HTTP Post query to the specified endpoint.
    type PostAttribute (path : string) =
        inherit Attribute ()

    /// Indicates that a method represents an HTTP Delete query to the specified endpoint.
    type DeleteAttribute (path : string) =
        inherit Attribute ()

    /// Indicates that a method represents an HTTP Head query to the specified endpoint.
    type HeadAttribute (path : string) =
        inherit Attribute ()

    /// Indicates that a method represents an HTTP Options query to the specified endpoint.
    type OptionsAttribute (path : string) =
        inherit Attribute ()

    /// Indicates that a method represents an HTTP Put query to the specified endpoint.
    type PutAttribute (path : string) =
        inherit Attribute ()

    /// Indicates that a method represents an HTTP Patch query to the specified endpoint.
    type PatchAttribute (path : string) =
        inherit Attribute ()

    /// Indicates that a method represents an HTTP Trace query to the specified endpoint.
    type TraceAttribute (path : string) =
        inherit Attribute ()

    /// Indicates that this argument to a method is interpolated into the HTTP request at runtime
    /// by setting a query parameter (with the given name) to the value of the annotated argument.
    type QueryAttribute (paramName : string) =
        inherit Attribute ()

    /// Indicates that this interface represents a REST client which accesses an API whose paths are
    /// all relative to the given address.
    type BaseAddressAttribute (addr : string) =
        inherit Attribute ()

    /// Indicates that this interface member causes the interface to set a header with the given name,
    /// whose value is obtained whenever required by a fresh call to the interface member.
    type HeaderAttribute (header : string, value : string option) =
        inherit Attribute ()
        new (header : string) = HeaderAttribute (header, None)
        new (header : string, value : string) = HeaderAttribute (header, Some value)

    /// Indicates that this argument to a method is interpolated into the request path at runtime
    /// by writing it into the templated string that specifies the HTTP query e.g. in the `[<Get "/foo/{template}">]`.
    type PathAttribute (path : string option) =
        inherit Attribute ()
        new (path : string) = PathAttribute (Some path)
        new () = PathAttribute None
