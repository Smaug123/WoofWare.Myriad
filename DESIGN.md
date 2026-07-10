# Generated record constructors

## Problem

Constructing an F# record with many `option` fields is needlessly repetitive when the usual initial state for every optional field is `None`. The generator should make the required part of construction explicit while retaining ordinary record-update syntax for later optional values.

## Public API

Annotating a record with `GenerateRecordConstructorAttribute`:

```fsharp
[<WoofWare.Myriad.Plugins.GenerateRecordConstructor>]
type Request =
    {
        Name : string
        RetryCount : int option
        Enabled : bool
    }
```

generates a companion module and a curried `create` function:

```fsharp
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Request =
    /// Create a Request with every optional field set to None.
    let create (arg_0 : string) (arg_1 : bool) : Request =
        {
            Name = arg_0
            RetryCount = None
            Enabled = arg_1
        }
```

Required arguments appear in record declaration order. If every field is optional, `create` takes `unit`, so the generated API remains a function and generic records do not encounter the value restriction. The generated module has the source type's accessibility.

Parameters are named `arg_0`, `arg_1`, and so on in required-field order. These names are deterministic, legal for every record-label spelling, and cannot collide when labels differ only by casing or identifier sanitisation.

The Myriad generator is exposed as `RecordConstructorGenerator` with the identifier `record-constructor`.

## Meaning of optional

A field is optional exactly when its outermost syntactic type constructor is F# `option`, including the ordinary postfix and prefix spellings understood by the parsed syntax. Such a field is initialized with `None`, even when its element type is itself optional.

Type aliases cannot be resolved from the untyped syntax tree. An alias of `option`, `voption`, `System.Nullable<_>`, and all other types are therefore required arguments. This boundary keeps generation deterministic and reflection-free.

## Functional core and generator shell

The implementation is split into three parts:

1. A syntax-independent constructor planner accepts an ordered list of fields classified as required or optional. It returns inert data describing ordered parameters and every record-field initializer (`FromParameter` or `UseNone`).
2. A thin Fantomas/FCS adapter classifies parsed record fields and renders that plan as an AST. It also constructs the applied return type for generic records.
3. The Myriad shell parses the input file, finds annotated types, rejects a use on a non-record, and emits the rendered modules.

The planner is the semantic core. It has no IO, compiler service, formatting, or Myriad dependencies, and its output can be interpreted directly in property tests. The syntax adapter is checked by parsing generated record shapes and observing their plans, while a checked-in consumer project supplies the renderer and compilation boundary.

## Guarantees

- Every source field appears exactly once in the generated record expression and remains in declaration order.
- Every direct `option` field is initialized with `None` and creates no parameter.
- Every other field has exactly one typed parameter, and its initializer references that parameter.
- Parameter order is the declaration order of required fields.
- Parameter names are unique and are the consecutive sequence `arg_0` through `arg_(n-1)`.
- Generic parameters used only by optional fields remain represented in the result type.
- Generated source is deterministic for a given input syntax tree.

## Deliberate limits

- The attribute applies only to records; placing it on another type is an error.
- The record must be declared directly in a namespace. F# modules cannot be reopened from another file, so an annotation nested inside a module is rejected explicitly.
- There is no attribute-free `MyriadParams` spelling or extension-member mode in this first version.
- A record with private type or representation accessibility cannot be constructed from a separate generated file and is rejected with an explicit error.
- A pre-existing companion module can collide with the generated module, as it can for the non-extension JsonParse generator.
