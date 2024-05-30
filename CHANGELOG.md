Notable changes are recorded here.

# WoofWare.Myriad.Plugins 2.1.30

`JsonSerialize` can now operate on discriminated unions.

# WoofWare.Myriad.Plugins 2.1.20, WoofWare.Myriad.Plugins.Attributes 3.0.1

We now bundle copies of the RestEase attributes in `WoofWare.Myriad.Plugins.Attributes`, in case you don't want to take a dependency on RestEase.

# WoofWare.Myriad.Plugins 2.1.15

The `GenerateMock` generator now permits a limited amount of inheritance in the record we're mocking out (specifically, `IDisposable`).

# WoofWare.Myriad.Plugins 2.1.8

No change to the packages, but this is when we started creating and tagging GitHub releases, which are a better source of truth than this file.

# WoofWare.Myriad.Plugins 2.0

This transition split the attributes (e.g. `[<JsonParseAttribute>]`) into their own assembly, WoofWare.Myriad.Plugins.Attributes.
The new assembly has minimal dependencies, so you may safely use it from your own code.
