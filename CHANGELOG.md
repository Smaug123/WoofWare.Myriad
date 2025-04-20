Notable changes are recorded here.

# WoofWare.Myriad.Plugins 5.0.1

We now enforce non-nullability on more types during JSON parse.
We have always expected you to consume nullable types wrapped in an `option`, but now we enforce this in more cases by throwing `ArgumentNullException`.

# WoofWare.Myriad.Plugins 3.0.1

Semantics of `HttpClient`'s URI component composition changed:
we now implicitly insert `/` characters after `[<BaseAddress>]` and `[<BasePath>]`, so that URI composition doesn't silently drop the last component if you didn't put a slash there.

# WoofWare.Myriad.Plugins 2.3.9

`JsonParse` and `JsonSerialize` now interpret `[<JsonExtensionData>]`, which must be on a `Dictionary<string, _>`; this collects any extra components that were present on the JSON object.

# WoofWare.Myriad.Plugins 2.2.1, WoofWare.Myriad.Plugins.Attributes 3.2.1

New generator: `ArgParser`, a basic reflection-free argument parser.

# WoofWare.Myriad.Plugins 2.1.45, WoofWare.Myriad.Plugins.Attributes 3.1.7

The NuGet packages are now attested to through [GitHub Attestations](https://github.blog/2024-05-02-introducing-artifact-attestations-now-in-public-beta/).
You can run `gh attestation verify ~/.nuget/packages/woofware.myriad.plugins/2.1.45/woofware.myriad.plugins.2.1.45.nupkg -o Smaug123`, for example, to verify with GitHub that the GitHub Actions pipeline on this repository produced a nupkg file with the same hash as the one you were served from NuGet.

# WoofWare.Myriad.Plugins 2.1.33

`JsonParse` can now deserialize the discriminated unions which `JsonSerialize` wrote out.

# WoofWare.Myriad.Plugins 2.1.32, WoofWare.Myriad.Plugins.Attributes 3.1.4

`JsonSerialize` can now serialize many discriminated unions.
(This operation is inherently opinionated, because JSON does not model discriminated unions.)

# WoofWare.Myriad.Plugins 2.1.20, WoofWare.Myriad.Plugins.Attributes 3.0.1

We now bundle copies of the RestEase attributes in `WoofWare.Myriad.Plugins.Attributes`, in case you don't want to take a dependency on RestEase.

# WoofWare.Myriad.Plugins 2.1.15

The `GenerateMock` generator now permits a limited amount of inheritance in the record we're mocking out (specifically, `IDisposable`).

# WoofWare.Myriad.Plugins 2.1.8

No change to the packages, but this is when we started creating and tagging GitHub releases, which are a better source of truth than this file.

# WoofWare.Myriad.Plugins 2.0

This transition split the attributes (e.g. `[<JsonParseAttribute>]`) into their own assembly, WoofWare.Myriad.Plugins.Attributes.
The new assembly has minimal dependencies, so you may safely use it from your own code.
