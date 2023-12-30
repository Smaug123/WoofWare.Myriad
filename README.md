# WoofWare.Myriad.Plugins

[![NuGet version](https://img.shields.io/nuget/v/WoofWare.Myriad.Plugins.svg?style=flat-square)](https://www.nuget.org/packages/WoofWare.Myriad.Plugins)
[![GitHub Actions status](https://github.com/Smaug123/WoofWare.Myriad/actions/workflows/dotnet.yaml/badge.svg)](https://github.com/Smaug123/WoofWare.Myriad/actions?query=branch%3Amain)
[![License file](https://img.shields.io/github/license/Smaug123/WoofWare.Myriad)](./LICENSE)

![Project logo: the face of a cartoon Shiba Inu, staring with powerful cyborg eyes directly at the viewer, with a background of stylised plugs.](./WoofWare.Myriad.Plugins/logo.png)

Some helpers in [Myriad](https://github.com/MoiraeSoftware/myriad/) which might be useful.

These are currently somewhat experimental, and I personally am their primary customer.
The `RemoveOptions` generator in particular is extremely half-baked.

Currently implemented:

* `JsonParse` (to stamp out `jsonParse : JsonNode -> 'T` methods);
* `RemoveOptions` (to strip `option` modifiers from a type).
* `HttpClient` (to stamp out a [RestEase](https://github.com/canton7/RestEase)-style HTTP client).

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

### Limitations

RestEase is complex, and handles a lot of different stuff.

* If you set the `BaseAddress` on your input `HttpClient`, make sure to end with a trailing slash
  on any trailing directories (so `"blah/foo/"` rather than `"blah/foo"`).
  We combine URIs using `UriKind.Relative`, so without a trailing slash, the last component may be chopped off.
* Parameters are serialised solely with `ToString`, and there's no control over this;
  nor is there control over encoding in any sense.
* Deserialisation follows the same logic as the `JsonParse` generator,
  and it generally assumes you're using types which `JsonParse` is applied to.
* Headers are not yet supported.
* Anonymous parameters are currently forbidden.

There are also some design decisions:

* Every function must take an optional `CancellationToken` (which is good practice anyway);
  so arguments are forced to be tupled.

# Detailed examples

See the tests.
For example, [PureGymDto.fs](./ConsumePlugin/PureGymDto.fs) is a real-world set of DTOs.

## How to use

* In your `.fsproj` file, define a helper variable so that subsequent steps don't all have to be kept in sync:
    ```xml
    <PropertyGroup>
      <WoofWareMyriadPluginVersion>1.1.5</WoofWareMyriadPluginVersion>
    </PropertyGroup>
    ```
* Take a reference on `WoofWare.Myriad.Plugins`:
    ```xml
    <ItemGroup>
        <PackageReference Include="WoofWare.Myriad.Plugins" Version="$(WoofWareMyriadPluginVersion)" />
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
