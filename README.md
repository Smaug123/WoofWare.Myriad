# WoofWare.Myriad.Plugins

[![NuGet version](https://img.shields.io/nuget/v/WoofWare.Myriad.Plugins.svg?style=flat-square)](https://www.nuget.org/packages/WoofWare.Myriad.Plugins)
[![GitHub Actions status](https://github.com/Smaug123/WoofWare.Myriad/actions/workflows/dotnet.yaml/badge.svg)](https://github.com/Smaug123/WoofWare.Myriad/actions?query=branch%3Amain)
[![License file](https://img.shields.io/github/license/Smaug123/WoofWare.Myriad)](./LICENSE)

![Project logo: the face of a cartoon Shiba Inu, staring with powerful cyborg eyes directly at the viewer, with a background of stylised plugs.](./WoofWare.Myriad.Plugins/logo.png)

Some helpers in [Myriad](https://github.com/MoiraeSoftware/myriad/) which might be useful.

Currently implemented:

* `JsonParse` (to stamp out `jsonParse : JsonNode -> 'T` methods).
* `JsonSerialize` (to stamp out `toJsonNode : 'T -> JsonNode` methods).
* `HttpClient` (to stamp out a [RestEase](https://github.com/canton7/RestEase)-style HTTP client).
* `GenerateMock` and `GenerateCapturingMock` (to stamp out a record type corresponding to an interface, like a compile-time [Foq](https://github.com/fsprojects/Foq)).
* `ArgParser` (to stamp out a basic argument parser).
* `SwaggerClient` (to stamp out an HTTP client for a Swagger API).
* `CreateCatamorphism` (to stamp out a non-stack-overflowing [catamorphism](https://fsharpforfunandprofit.com/posts/recursive-types-and-folds/) for a discriminated union).
* `RemoveOptions` (to strip `option` modifiers from a type) - this one is particularly half-baked!

If you would like to ensure that your particular use-case remains unbroken, please do contribute tests to this repository.
The `ConsumePlugin` assembly contains a number of invocations of these source generators,
so you just need to add copies of your types to that assembly to ensure that I will at least notice if I break the build;
and if you add tests to `WoofWare.Myriad.Plugins.Test` then I will also notice if I break the runtime semantics of the generated code.

## `JsonParse`

Takes records like this:

```fsharp
[<WoofWare.Myriad.Plugins.JsonParse>]
type InnerType =
    {
        [<JsonPropertyName "something">]
        Thing : string
    }

/// My whatnot
[<WoofWare.Myriad.Plugins.JsonParse>]
type JsonRecordType =
    {
        /// A thing!
        A : int
        /// Another thing!
        B : string
        [<System.Text.Json.Serialization.JsonPropertyName "hi">]
        C : int list
        D : InnerType
    }

```

and stamps out parsing methods like this:

```fsharp
/// Module containing JSON parsing methods for the InnerType type
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module InnerType =
    /// Parse from a JSON node.
    let jsonParse (node: System.Text.Json.Nodes.JsonNode) : InnerType =
        let Thing = node.["something"].AsValue().GetValue<string>()
        { Thing = Thing }
namespace UsePlugin

/// Module containing JSON parsing methods for the JsonRecordType type
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module JsonRecordType =
    /// Parse from a JSON node.
    let jsonParse (node: System.Text.Json.Nodes.JsonNode) : JsonRecordType =
        let D = InnerType.jsonParse node.["d"]

        let C =
            node.["hi"].AsArray() |> Seq.map (fun elt -> elt.GetValue<int>()) |> List.ofSeq

        let B = node.["b"].AsValue().GetValue<string>()
        let A = node.["a"].AsValue().GetValue<int>()
        { A = A; B = B; C = C; D = D }
```

You can optionally supply the boolean `true` to the attribute,
which will cause Myriad to stamp out an extension method rather than a module with the same name as the type.
This is useful if you want to reuse the type name as a module name yourself,
or if you want to apply multiple source generators which each want to use the module name.

### What's the point?

`System.Text.Json`, in a `PublishAot` context, relies on C# source generators.
The default reflection-heavy implementations have the necessary code trimmed away, and result in a runtime exception.
But C# source generators [are entirely unsupported in F#](https://github.com/dotnet/fsharp/issues/14300).

This Myriad generator expects you to use `System.Text.Json` to construct a `JsonNode`,
and then the generator takes over to construct a strongly-typed object.

### Limitations

This source generator is enough for what I first wanted to use it for.
However, there is *far* more that could be done.

* Make it possible to give an exact format and cultural info in date and time parsing.
* Make it possible to reject parsing if extra fields are present.
* Generally support all the `System.Text.Json` attributes.

For an example of using both `JsonParse` and `JsonSerialize` together with complex types, see [the type definitions](./ConsumePlugin/SerializationAndDeserialization.fs) and [tests](./WoofWare.Myriad.Plugins.Test/TestJsonSerialize/TestJsonSerde.fs).

## `JsonSerialize`

Takes records like this:
```fsharp
[<WoofWare.Myriad.Plugins.JsonSerialize true>]
type InnerTypeWithBoth =
    {
        [<JsonPropertyName("it's-a-me")>]
        Thing : string
        ReadOnlyDict : IReadOnlyDictionary<string, Uri list>
    }
```

and stamps out modules like this:
```fsharp
module InnerTypeWithBoth =
    let toJsonNode (input : InnerTypeWithBoth) : System.Text.Json.Nodes.JsonNode =
        let node = System.Text.Json.Nodes.JsonObject ()

        do
            node.Add (("it's-a-me"), System.Text.Json.Nodes.JsonValue.Create<string> input.Thing)

            node.Add (
                "ReadOnlyDict",
                (fun field ->
                    let ret = System.Text.Json.Nodes.JsonObject ()

                    for (KeyValue (key, value)) in field do
                        ret.Add (key.ToString (), System.Text.Json.Nodes.JsonValue.Create<Uri> value)

                    ret
                ) input.ReadOnlyDict
            )

        node
```

Also includes an *opinionated* serializer for discriminated unions.
(Any such serializer must be opinionated, because JSON does not natively model DUs.)

As in `JsonParse`, you can optionally supply the boolean `true` to the attribute,
which will cause Myriad to stamp out an extension method rather than a module with the same name as the type.

The same limitations generally apply to `JsonSerialize` as do to `JsonParse`.

For an example of using both `JsonParse` and `JsonSerialize` together with complex types, see [the type definitions](./ConsumePlugin/SerializationAndDeserialization.fs) and [tests](./WoofWare.Myriad.Plugins.Test/TestJsonSerialize/TestJsonSerde.fs).

## `ArgParser`

Takes a record like this:

```fsharp
type DryRunMode =
    | [<ArgumentFlag true> Dry
    | [<ArgumentFlag false> Wet

[<ArgParser>]
type Foo =
    {
        [<ArgumentHelpText "Enable the frobnicator">]
        SomeFlag : bool
        A : int option
        [<ArgumentDefaultFunction>]
        B : Choice<int, int>
        [<ArgumentDefaultEnvironmentVariable "MY_ENV_VAR">]
        BWithEnv : Choice<int, int>
        [<ArgumentDefaultFunction>]
        DryRun : DryRunMode
        [<ArgumentLongForm "longer-form-replaces-c">]
        C : float list
        // optionally:
        [<PositionalArgs>]
        Rest : string list // or e.g. `int list` if you want them parsed into a type too
    }
    static member DefaultB () = 4
    static member DefaultDryRun () = DryRunMode.Wet
```

and stamps out a basic `parse` method of this signature:

```fsharp
[<RequireQualifiedAccess>]
module Foo =
    // in case you want to test it
    let parse' (getEnvVar : string -> string) (args : string list) : Foo = ...
    // the one we expect you actually want to use
    let parse (args : string list) : Foo = ...
```

Default arguments are handled as `Choice<'a, 'a>`:
you get a `Choice1Of2` if the user provided the input, or a `Choice2Of2` if the parser filled in your specified default value.

You can control `TimeSpan` and friends with the `[<InvariantCulture>]` and `[<ParseExact @"hh\:mm\:ss">]` attributes.

You can generate extension methods for the type, instead of a module with the type's name, using `[<ArgParser (* isExtensionMethod = *) true>]`.

If `--help` appears in a position where the parser is expecting a key (e.g. in the first position, or after a `--foo=bar`), the parser fails with help text.
The parser also makes a limited effort to supply help text when encountering an invalid parse.

### What's the point?

I got fed up of waiting for us to find time to rewrite the in-house one at work.
That one has a bunch of nice compositional properties, which my version lacks:
I can basically only deal with primitive types, and e.g. you can't stack records and discriminated unions inside each other.

But I *do* want an F#-native argument parser suitable for AOT-compilation.

Why not [Argu](https://fsprojects.github.io/Argu/)?
Answer: I got annoyed with having to construct my records by hand even after Argu returned and said the parsing was all "done".

### Limitations

This is very bare-bones, but do raise GitHub issues if you like (or if you find cases where the parser does the wrong thing).

* Help is signalled by throwing an exception, so you'll get an unsightly stack trace and a nonzero exit code.
* Help doesn't take into account any arguments the user has entered. Ideally you'd get contextual information like an identification of which args the user has supplied at the point where the parse failed or help was requested.
* I don't handle very many types, and in particular a real arg parser would handle DUs and records with nesting.
* I don't try very hard to find a valid parse. It may well be possible to find a case where I fail to parse despite there existing a valid parse.
* There's no subcommand support (you'll have to do that yourself).

It should work fine if you just want to compose a few primitive types, though.

## `SwaggerClient`

Takes a JSON-schema definition of a [Swagger API](https://swagger.io/), and stamps out a client like this:

```fsharp
/// A type which was defined in the Swagger spec
[<JsonParse true ; JsonSerialize true>]
type SwaggerType1 =
    {
        [<System.Text.Json.Serialization.JsonExtensionData>]
        AdditionalProperties : System.Collections.Generic.Dictionary<string, System.Text.Json.Nodes.JsonNode>
        Message : string
    }

/// Documentation from the Swagger spec
[<HttpClient false ; RestEase.BasePath "/api/v1">]
type IGitea =
    /// Returns the Person actor for a user
    [<RestEase.Get "/activitypub/user/{username}">]
    abstract ActivitypubPerson :
        [<RestEase.Path "username">] username : string * ?ct : System.Threading.CancellationToken ->
            ActivityPub System.Threading.Tasks.Task
```

Notice that we automatically decorate the type with our `[<HttpClient>]` attribute, so if you choose to do so, you can chain another Myriad generated file off this one and you'll get a RestEase-style client stamped out.
(See below, searching on the string `"Generated2SwaggerGitea.fs"`, for an example.)

You don't need to `Content Include` or `EmbeddedResource Include` the JSON schema.
`None Include` will do; we only need the source to be available at build time.

You *do* need to include the following configuration:

```xml
<Compile Include="GeneratedClient.fs">
  <!-- This bit is normal: -->
  <MyriadFile>swagger.json</MyriadFile>
  <!-- This bit is new and required! -->
  <MyriadParams>
    <ClassName>GiteaClient</ClassName>
    <!-- Optionally: -->
    <GenerateMock>true</GenerateMock>
  </MyriadParams>
</Compile>
```

The `<ClassName />` key tells us what to name the resulting interface (it gets an `I` prepended for you).
You can optionally also set `<GenerateMockVisibility>v</GenerateMockVisibility>` to add the `[<GenerateMock>]` attribute to the type
(where `v` should be `internal` or `public`, indicating "resulting mock type is internal" vs "is public"),
so that the following manoeuvre will result in a generated mock:

```xml
<None Include="swagger-gitea.json" />
<Compile Include="GeneratedSwaggerGitea.fs">
  <MyriadFile>swagger-gitea.json</MyriadFile>
  <MyriadParams>
    <GenerateMockVisibility>public</GenerateMockVisibility>
    <ClassName>Gitea</ClassName>
  </MyriadParams>
</Compile>
<Compile Include="Generated2SwaggerGitea.fs">
  <MyriadFile>GeneratedSwaggerGitea.fs</MyriadFile>
</Compile>
```

(Note that you do have to create the `GeneratedSwaggerGitea.fs` file manually before code generation happens. Myriad will throw if that file isn't there, because `Generated2SwaggerGitea.fs` depends on it so Myriad wants to compute its hash. Just make an empty file.)

### What's the point?

[`SwaggerProvider`](https://github.com/fsprojects/SwaggerProvider) is *absolutely magical*, but it's kind of witchcraft.
I fear no man, but that thing… it scares me.

Also, builds using `SwaggerProvider` appear to be inherently nondeterministic, even if the data source doesn't change.

## Limitations

Swagger API specs appear to be pretty cowboy in the wild.
I try to cope with invalid schemas I have seen, but I can't guarantee I do so correctly.
Definitely do perform integration tests and let me know of weird specs you encounter, and bits of the (very extensive) Swagger spec I have omitted!

## `RemoveOptions`

Takes a record like this:

```fsharp
type Foo =
    {
        A : int option
        B : string
        C : float list
    }
```

and stamps out a record like this:

```fsharp
[<RequireQualifiedAccess>]
module Foo =
    type Short =
        {
            A : int
            B : string
            C : float list
        }
```

### What's the point?

The motivating example is argument parsing.
An argument parser naturally wants to express "the user did not supply this, so I will provide a default".
But it's not a very ergonomic experience for the programmer to deal with all these options,
so this Myriad generator stamps out a type *without* any options,
and also stamps out an appropriate constructor function.

### Limitations

This generator is *far* from where I want it, because I haven't really spent any time on it.

* It really wants to be able to recurse into the types within the record, to strip options from them.
* It needs some sort of attribute to mark a field as *not* receiving this treatment.
* What do we do about discriminated unions?

## `HttpClient`

Takes a type like this:

```fsharp
[<WoofWare.Myriad.Plugins.HttpClient>]
type IPureGymApi =
    [<Get "v1/gyms/">]
    abstract GetGyms : ?ct : CancellationToken -> Task<Gym list>

    [<Get "v1/gyms/{gym_id}/attendance">]
    abstract GetGymAttendance : [<Path "gym_id">] gymId : int * ?ct : CancellationToken -> Task<GymAttendance>

    [<Get "v1/member">]
    abstract GetMember : ?ct : CancellationToken -> Task<Member>

    [<Get "v1/gyms/{gym_id}">]
    abstract GetGym : [<Path "gym_id">] gymId : int * ?ct : CancellationToken -> Task<Gym>

    [<Get "v1/member/activity">]
    abstract GetMemberActivity : ?ct : CancellationToken -> Task<MemberActivityDto>

    [<Get "v2/gymSessions/member">]
    abstract GetSessions :
        [<Query>] fromDate : DateTime * [<Query>] toDate : DateTime * ?ct : CancellationToken -> Task<Sessions>
```

and stamps out a type like this:

```fsharp
/// Module for constructing a REST client.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module PureGymApi =
    /// Create a REST client.
    let make (client : System.Net.Http.HttpClient) : IPureGymApi =
        { new IPureGymApi with
            member _.GetGyms (ct : CancellationToken option) =
                async {
                    let! ct = Async.CancellationToken

                    let httpMessage =
                        new System.Net.Http.HttpRequestMessage (
                            Method = System.Net.Http.HttpMethod.Get,
                            RequestUri = System.Uri (client.BaseAddress.ToString () + "v1/gyms/")
                        )

                    let! response = client.SendAsync (httpMessage, ct) |> Async.AwaitTask
                    let response = response.EnsureSuccessStatusCode ()
                    let! stream = response.Content.ReadAsStreamAsync ct |> Async.AwaitTask

                    let! node =
                        System.Text.Json.Nodes.JsonNode.ParseAsync (stream, cancellationToken = ct)
                        |> Async.AwaitTask

                    return node.AsArray () |> Seq.map (fun elt -> Gym.jsonParse elt) |> List.ofSeq
                }
                |> (fun a -> Async.StartAsTask (a, ?cancellationToken = ct))

            // (more methods here)
        }
```

### What's the point?

The motivating example is again ahead-of-time compilation: we wish to avoid the reflection which RestEase does.

### Features

* Variable and constant header values are supported:
  see [the definition of `IApiWithHeaders`](./ConsumePlugin/RestApiExample.fs).

### Limitations

RestEase is complex, and handles a lot of different stuff.

* If you set the `BaseAddress` on your input `HttpClient`, make sure to end with a trailing slash
  on any trailing directories (so `"blah/foo/"` rather than `"blah/foo"`).
  We combine URIs using `UriKind.Relative`, so without a trailing slash, the last component may be chopped off.
* Parameters are serialised naively with `toJsonNode` as though the `JsonSerialize` generator were applied,
  and you can't control the serialisation. You can't yet serialise e.g. a primitive type this way (other than `String`);
  all body parameters must be types which have a suitable `toJsonNode : 'a -> JsonNode` method.
* Deserialisation follows the same logic as the `JsonParse` generator,
  and it generally assumes you're using types which `JsonParse` is applied to.
* Anonymous parameters are currently forbidden.

There are also some design decisions:

* Every function must take an optional `CancellationToken` (which is good practice anyway);
  so arguments are forced to be tupled.
* The `[<Optional>]` attribute is not supported and will probably not be supported, because I consider it to be cursed.

## `GenerateMock` and `GenerateCapturingMock`

`GenerateMock` takes a type like this:

```fsharp
[<GenerateMock>]
type IPublicType =
    abstract Mem1 : string * int -> string list
    abstract Mem2 : string -> int
```

and stamps out a type like this:

```fsharp
/// Mock record type for an interface
type internal PublicTypeMock =
    {
        Mem1 : string * int -> string list
        Mem2 : string -> int
    }

    static member Empty : PublicTypeMock =
        {
            Mem1 = (fun x -> raise (System.NotImplementedException "Unimplemented mock function"))
            Mem2 = (fun x -> raise (System.NotImplementedException "Unimplemented mock function"))
        }

    interface IPublicType with
        member this.Mem1 (arg0, arg1) = this.Mem1 (arg0, arg1)
        member this.Mem2 (arg0) = this.Mem2 (arg0)
```

`GenerateCapturingMock` additionally captures the calls made to each function (except for `Dispose`).
It takes a type like this:

```fsharp
[<GenerateCapturingMock>]
type IPublicType =
    abstract Mem1 : string * int -> string list
    abstract Mem2 : baz : string -> unit -> int
```

and stamps out types like this:

```fsharp
module internal PublicTypeCalls =
    type internal Mem2Call =
        {
            baz : string
            Arg1 : unit
        }

/// Mock record type for an interface
type internal PublicTypeMock =
    {
        Mem1 : string * int -> string list
        Mem2 : string -> int
        Mem2_Calls : ResizeArray<string * int>
        Mem2_Calls : ResizeArray<PublicTypeCalls.Mem2Call>
    }

    static member Empty : PublicTypeMock =
        {
            Mem1 = (fun x -> raise (System.NotImplementedException "Unimplemented mock function"))
            Mem2 = (fun x -> raise (System.NotImplementedException "Unimplemented mock function"))
            Mem1_Calls = ResizeArray ()
            Mem2_Calls = ResizeArray ()
        }

    interface IPublicType with
        member this.Mem1 (arg0, arg1) = this.Mem1 (arg0, arg1)
        member this.Mem2 (arg0) = this.Mem2 (arg0)
```

### What's the point?

Reflective mocking libraries like [Foq](https://github.com/fsprojects/Foq) in my experience are a rich source of flaky tests.
The [Grug-brained developer](https://grugbrain.dev/) would prefer to do this without reflection, and this reduces the rate of strange one-in-ten-thousand "failed to generate IL" errors.
But since F# does not let you partially update an interface definition, we instead stamp out a record,
thereby allowing the programmer to use F#'s record-update syntax.

### Features

* You may supply an `isInternal : bool` argument to the attribute. By default, we make the resulting record type at most internal (never public), since this is intended only to be used in tests; but you can instead make it public with `[<GenerateMock false>]`.

## `CreateCatamorphism`

Takes a collection of mutually recursive discriminated unions:

```fsharp
[<CreateCatamorphism "MyCata">]
type Expr =
    | Const of Const
    | Pair of Expr * Expr * PairOpKind
    | Sequential of Expr list
    | Builder of Expr * ExprBuilder

and ExprBuilder =
    | Child of ExprBuilder
    | Parent of Expr
```

and stamps out a type like this:
```fsharp
type ExprCata<'Expr, 'ExprBuilder> =
    abstract Const : Const -> 'Expr
    abstract Pair : 'Expr -> 'Expr -> PairOpKind -> 'Expr
    abstract Sequential : 'Expr list -> 'Expr
    abstract Builder : 'Expr -> 'ExprBuilder -> 'Expr

type ExprBuilderCata<'Expr, 'ExprBuilder> =
    abstract Child : 'ExprBuilder -> 'ExprBuilder
    abstract Parent : 'Expr -> 'ExprBuilder

type MyCata<'Expr, 'ExprBuilder> =
    {
        Expr : ExprCata<'Expr, 'ExprBuilder>
        ExprBuilder : ExprBuilderCata<'Expr, 'ExprBuilder>
    }

[<RequireQualifiedAccess>]
module ExprCata =
    let runExpr (cata : MyCata<'ExprRet, 'ExprBuilderRet>) (x : Expr) : 'ExprRet =
        failwith "this is implemented"

    let runExprBuilder (cata : MyCata<'ExprRet, 'ExprBuilderRet>) (x : ExprBuilder) : 'ExprBuilderRet =
        failwith "this is implemented"
```

### What's the point?
Recursing over a tree is not easy to get right, especially if you want to avoid stack overflows.
Instead of writing the recursion many times, it's better to do it once,
and then each time you only plug in what you want to do.

### Features

* Mutually recursive DUs are supported (as in the example above).
  Every DU in a recursive `type Foo... and Bar...` knot will be given an appropriate cata, as long as any one of those DUs has the `[<CreateCatamorphism>]` attribute.
* There is *limited* support for records and for lists.
* There is *extremely brittle* support for generics in the DUs you are cata'ing over.
  It is based on the names of the generic parameters, so you must ensure that generic parameters with the same name have the same meaning across the various cases in your recursive knot of DUs.
  (If you overstep the bounds of what this generator can do, you will get compile-time errors, e.g. with generics being constrained to each other's values.)
  See the [List tests](./WoofWare.Myriad.Plugins.Test/TestCataGenerator/TestMyList2.fs) for an example, where we re-implement `FSharpList<'a>`.

### Limitations

**I am not at all convinced of the correctness of this generator**, and I know it is very incomplete (in the sense that there are many possible DUs you could write for which the generator will bail out).
I *strongly* recommend implementing the identity catamorphism for your type and using property-based tests ([as I do](./WoofWare.Myriad.Plugins.Test/TestCataGenerator/TestDirectory.fs)) to assert that the correct thing happens.
Feel free to raise GitHub issues with code I can copy-paste to reproduce a case where the wrong thing happens (though I can't promise to look at them).

* This is a particularly half-baked generator which has so far seen no real-world use.
  It likely has a bunch of [80/20](https://en.wikipedia.org/wiki/Pareto_principle) low-hanging fruit remaining, but it also likely has impossible problems to solve which I don't know about yet.
* Only a very few kinds of DU field are currently implemented.
  For example, this generator can't see through an interface (e.g. the kind of interface one would use to implement the [crate pattern](https://www.patrickstevens.co.uk/posts/2021-10-19-crates/) to represent a [GADT](https://en.wikipedia.org/wiki/Generalized_algebraic_data_type)),
  so the generated cata will simply grant you access to the interface (rather than attempting to descend into it to discover recursive references).
  You can't nest lists deeply. All sorts of other cases are unaddressed.
* This generator does not try to solve the "exponential diamond dependency" problem.
  If you have a case of the form `type Expr = | Branch of Expr * Expr`, the cata will walk into both `Expr`s separately.
  If the `Expr`s happen to be equal, the cata will nevertheless traverse them individually (that is, it will traverse the same `Expr` twice).
  Your type may represent a [DAG](https://en.wikipedia.org/wiki/Directed_acyclic_graph), but we will always effectively expand it into a tree of paths and operate on each of the exponentially-many paths.

# Detailed examples

See the tests.
For example, [PureGymDto.fs](./ConsumePlugin/PureGymDto.fs) is a real-world set of DTOs.

## How to use

* In your `.fsproj` file, define a helper variable so that subsequent steps don't all have to be kept in sync:
    ```xml
    <PropertyGroup>
      <WoofWareMyriadPluginVersion>2.0.1</WoofWareMyriadPluginVersion>
    </PropertyGroup>
    ```
* Take a reference on `WoofWare.Myriad.Plugins.Attributes` (which has no other dependencies), to obtain access to the attributes which the generator will recognise:
    ```xml
    <ItemGroup>
        <PackageReference Include="WoofWare.Myriad.Plugins.Attributes" Version="2.0.2" />
    </ItemGroup>
    ```
* Take a reference (with private assets, to prevent these from propagating to your own assembly) on `WoofWare.Myriad.Plugins`, to obtain the plugins which Myriad will run, and on `Myriad.Sdk`, to obtain the Myriad binary itself:
    ```xml
    <ItemGroup>
        <PackageReference Include="WoofWare.Myriad.Plugins" Version="$(WoofWareMyriadPluginVersion)" PrivateAssets="all" />
        <PackageReference Include="Myriad.Sdk" Version="0.8.3" PrivateAssets="all" />
    </ItemGroup>
    ```
* Point Myriad to the DLL within the NuGet package which is the source of the plugins:
    ```xml
    <ItemGroup>
      <MyriadSdkGenerator Include="$(NuGetPackageRoot)/woofware.myriad.plugins/$(WoofWareMyriadPluginVersion)/lib/net6.0/WoofWare.Myriad.Plugins.dll" />
    </ItemGroup>
    ```

Now you are ready to start using the generators.
For example, this specifies that Myriad is to use the contents of `Client.fs` to generate the file `GeneratedClient.fs`:

```xml
<ItemGroup>
    <Compile Include="Client.fs" />
    <Compile Include="GeneratedClient.fs">
        <MyriadFile>Client.fs</MyriadFile>
    </Compile>
</ItemGroup>
```

## Alternative use without the attributes

You can avoid taking a reference on the `WoofWare.Myriad.Plugins.Attributes` assembly, instead putting all the configuration into the project file.
This is implemented for everything except the SwaggerClientGenerator.

```xml
<Project>
  <ItemGroup>
    <Compile Include="Client.fs" />
    <Compile Include="GeneratedClient.fs">
        <MyriadFile>Client.fs</MyriadFile>
        <MyriadParams>
          <MyTypeName1>GenerateMock(false)!JsonParse</MyTypeName1>
          <SomeOtherTypeName>GenerateMock</SomeOtherTypeName>
        </MyriadParams>
    </Compile>
  </ItemGroup>
  <ItemGroup>
    <PackageReference Include="WoofWare.Myriad.Plugins" Version="$(WoofWareMyriadPluginVersion)" PrivateAssets="all" />
    <PackageReference Include="Myriad.Sdk" Version="0.8.3" PrivateAssets="all" />
  </ItemGroup>
</Project>
```

That is, you specify a `!`-delimited list of the attributes you *would* apply to the type.
Supply "arguments" to the attribute name in the project file as you would to the attribute itself.

(Yes, this is indeed incredibly cumbersome, and you're not interested in the reasons it's all so mad!
I'm hopefully going to get round to writing a more powerful source generation system which won't have these limitations.)

### Myriad Gotchas

* MsBuild doesn't always realise that it needs to invoke Myriad during rebuild.
  You can always save a whitespace change to the source file (e.g. `Client.fs` above),
  and MsBuild will then execute Myriad during the next build.
* [Fantomas](https://github.com/fsprojects/fantomas), the F# source formatter which powers Myriad,
  is customisable with [editorconfig](https://editorconfig.org/),
  but it [does not easily expose](https://github.com/fsprojects/fantomas/issues/3031) this customisation
  except through the standalone Fantomas client.
  So Myriad's output is formatted without respect to any conventions which may hold in the rest of your repository.
  You should probably add these files to your [fantomasignore](https://github.com/fsprojects/fantomas/blob/a999b77ca5a024fbc3409955faac797e29b39d27/docs/docs/end-users/IgnoreFiles.md)
  if you use Fantomas to format your repo;
  the alternative is to manually reformat every time Myriad changes the generated files.

# Licence

The code is MIT-licenced, except for the Swagger API examples in WoofWare.Myriad.Plugins.Test, which are [CC-BY 4.0](https://creativecommons.org/licenses/by/4.0/), copyright 2023 by the OpenAPI Initiative, and obtained from https://learn.openapis.org/examples/ with no changes made.
