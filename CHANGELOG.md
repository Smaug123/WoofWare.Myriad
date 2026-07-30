Notable changes are recorded here.

# WoofWare.Myriad.Plugins 10.7.2

`ArgParserGenerator` now reads `[<ArgumentHelpText>]` from a nested argument record or union of alternative argument sets, and not only from the `[<ArgParser>]`-tagged root, and from a discriminated union's case (and that case's payload record).
The nested type's help heads the group of arguments that type contributes, wherever it is embedded, and a case's help heads its group the same way.

An `[<ArgumentHelpText>]` on the more specific placement overrides the more general one: a field's overrides its nested type's, and a case's overrides its payload record's.
One type, or one payload record, may be embedded or reused at several sites for different purposes, so the more specific placement is the one which can say what a particular occurrence is for.

# WoofWare.Myriad.Plugins 10.7.1

`ArgParserGenerator` help text now groups the arguments contributed by a field whose type is another argument record, or a union of alternative argument sets, under a header line naming that field.
(Previously those arguments were flattened into one undifferentiated list, so nothing indicated which arguments were declared together.)

# WoofWare.Myriad.Plugins 10.6.2

`ArgParserGenerator` now correctly escapes strings from `ArgumentLongForm` where necessary.

# WoofWare.Myriad.Plugins 10.6.1, WoofWare.Myriad.Plugins.Attributes 3.11.1

The `ArgParserGenerator` gains `[<ArgumentPrefix "foo">]`, placed on a field whose type is another argument record or a union of alternative argument sets: every argument that field contributes is namespaced, so `--blah` becomes `--foo-blah`.

The prefix applies to every argument in the subtree, including ones carrying an explicit `[<ArgumentLongForm>]` and ones nested arbitrarily deep, whether or not the intervening records carry prefixes of their own.
Prefixes compose from the outside in (`--outer-inner-blah`), and `[<ArgumentNegateWithPrefix>]` negates outside the prefix (`--no-foo-blah`).
The prefixed names are what appear in help text and what the duplicate-name checks see, so a prefix can both resolve a collision (`{ A : MySubRecord ; B : MySubRecord}`) and create one (`{ FooBar : int ; [<ArgumentPrefix "foo">] Bar : string }`).

The prefix must be a string literal written out in full, must be non-empty, must not contain `=`, and must not start or end with `-` (the separating `-` is inserted for you); it is used exactly as written, and is not case-normalised.
The generator will fail if you try to use this on a leaf field, on a `[<PositionalArgs>]` field, and on a union case.

# WoofWare.Myriad.Plugins 10.5.2

The `ArgParserGenerator` now rejects an `[<ArgumentLongForm>]` placed on a field whose type is another argument record, or a discriminated union of alternative argument sets.
Such a field contributes a whole set of arguments, each named by its own field, so there is no single argument for the attribute to rename; it was previously read and then silently dropped, leaving a parser with names the author did not ask for and no indication why.

# WoofWare.Myriad.Plugins 10.5.1, WoofWare.Myriad.Plugins.Attributes 3.10.1

The `ArgParserGenerator` gains `[<ArgumentDefaultValue foo>]`, which is shorthand for an `[<ArgumentDefaultFunction>]` whose function just returns the constant `foo`.
`foo` must be a literal written out in full: the value is reproduced in the generated file rather than evaluated at your attribute, so anything whose meaning depends on where it is written is rejected.
That covers names standing for constants (a `[<Literal>]` binding or an enum case), since the generated file hoists every `open` in your source above the parser and so a name need not resolve to the same binding there; and F#'s context-sensitive constants (`__LINE__`, `__SOURCE_FILE__`, `__SOURCE_DIRECTORY__`).
Use `[<ArgumentDefaultFunction>]` for those, and for anything which is not a constant at all.

# WoofWare.Myriad.Plugins 10.4.1, WoofWare.Myriad.Plugins.Attributes 3.9.1

The `ArgParserGenerator` now supports `Map<'k, 'v>` fields, which accumulate key-value entries across occurrences as `list` fields accumulate values.
Adds the `[<ArgumentKeyValueSeparator>]` attribute (mandatory on a `Map` field), which gives the character separating a key from its value, and the optional `[<ArgumentMapEntrySeparator>]` attribute, which lets one occurrence carry several entries.
So `--labels=owner:alice --labels team:web` builds a two-entry map, and with an entry separator of `,` you may instead write `--labels=owner:alice,team:web`.

An unsupplied `Map` is empty, so (like a `list`) it may not be an `option` or carry a default.
Supplying the same key twice is an error rather than an overwrite.
Each entry is split at its *first* key-value separator, so a value may contain that separator but a key may not.
This means e.g. that some `Map<string, string>` are inexpressible (if the key contains the key-value separator); use `string list` and parse it yourself into a map if you need something smarter.
Where the spellings are known at generation time we check them: an enumerated key or value with a case that no spelling can express is rejected rather than silently misparsed.
Help text describes each half of an entry in the syntax that half accepts, so a flag-valued map advertises `map<..., bool>` and an enumerated one lists its case names.

# WoofWare.Myriad.Plugins 10.3.1

The `ArgParserGenerator` now supports positional args together with arbitrary discriminated-union args.
(As in 10.2.3, the non-default `[<PositionalArgs true>]`, which collects into the positional args any unrecognised flag-like arguments such as `--foo`, remains banned in combination with a union.)

# WoofWare.Myriad.Plugins 10.2.3

The `ArgParserGenerator` now permits a `[<PositionalArgs>]` field at the top level alongside (though not within) a discriminated-union arg, as long as the positional sink rejects unrecognised flag-like tokens (the default; `[<PositionalArgs true>]` remains banned beside a union, because that would make it very confusing when you typo a DU-case-selecting flag).

# WoofWare.Myriad.Plugins 10.2.1

The `ArgParserGenerator` now ships with (limited) discriminated-union support: you can specify mutually exclusive sets of args and the parser will select the correct set.

# WoofWare.Myriad.Plugins 10.1.1

Fixes a number of bugs in the `ArgParserGenerator` by extracting the "untyped" logic into a standalone module.
The generated code changes substantially, but the only behaviour changes you should observe are correctness fixes.

# WoofWare.Myriad.Plugins 10.0.1

The `JsonSerializeGenerator` now requires `System.Text.Json` at version at least 8, to accommodate `JsonNode.DeepClone`.
(.NET 6 and 7 have been out of support for nearly two years, so the runtime should already be providing you with an acceptable version of this package.)

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
