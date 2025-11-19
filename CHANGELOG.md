Notable changes are recorded here.

# WoofWare.Myriad.Plugins 9.1.1, WoofWare.Myriad.Plugins.Attributes 3.8.1

Adds the `[<ArgumentNegateWithPrefix>]` attribute, which can be placed on a boolean or flag-valued field when using the `ArgParser` generator.
This causes the boolean to be specifiable with the `--no-` prefix to negate its value.
(For example, `Foo : bool` is normally specified as `--foo`; this new attribute lets the user additionally give `--no-foo` to get the same semantics as `--foo=false`.)

# WoofWare.Myriad.Plugins 9.0.1

Converts the `static member Empty` field on each generated mock (from `GeneratedMock`) into a function, so as to permit the `GeneratedCapturingMock` to have the same signature.
(`GeneratedCapturingMock` contains mutable state, so must be created afresh each time.)

# WoofWare.Myriad.Plugins 8.1.1

Adds `GenerateCapturingMock`, which is `GenerateMock` but additionally records the calls made to each function.

# WoofWare.Myriad.Plugins 8.0.3

The RestEase-style HTTP client generator now automatically adds the `application/json` content type header to requests which are POSTing a body that is known to be JSON-serialised.
You can override this by setting the `[<RestEase.Header ("Content-Type", "desired content type")>]` header manually on any affected member.

# WoofWare.Myriad.Plugins 7.0.1

All generators should now be compatible with `<Nullable>enable</Nullable>`.

**Please test the results and let me know of unexpected failures.**
There are a number of heuristics in this code, because:

* `System.Text.Json.Nodes` is an unfathomably weird API which simply requires us to make educated guesses about whether a user-provided type is supposed to be nullable, despite this being irrelevant to the operation of `System.Text.Json`;
* Some types (like `Uri` and `String`) have `ToString` methods which can't return `null`, but in general `Object.ToString` can of course return `null`, and as far as I can tell there is simply no way to know from the source alone whether a given type will have a nullable `ToString`.

# WoofWare.Myriad.Plugins 6.0.1

The `ArgParser` generator's type signatures have changed.
The `parse'` method no longer takes `getEnvironmentVariable : string -> string`; it's now `getEnvironmentVariable : string -> string option`.
This is to permit satisfying the `<Nullable>enable</Nullable>` compiler setting.
If you're calling `parse'`, give it `Environment.GetEnvironmentVariable >> Option.ofObj` instead.

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
