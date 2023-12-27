#!/bin/sh

dotnet \
    "/Users/patrick/.nuget/packages/myriad.sdk/0.8.3/build/../tools/net6.0/any/Myriad.dll" \
    --inputfile "/Users/patrick/Documents/GitHub/WoofWare.Myriad.Plugins/ConsumePlugin/RecordFile.fs" \
    --outputfile "/Users/patrick/Documents/GitHub/WoofWare.Myriad.Plugins/ConsumePlugin/Generated.fs" \
    --configfile "/Users/patrick/Documents/GitHub/WoofWare.Myriad.Plugins/ConsumePlugin/myriad.toml" \
    --contextfile "/Users/patrick/Documents/GitHub/WoofWare.Myriad.Plugins/ConsumePlugin/obj/myriad.context.toml" \
    --plugin "/Users/patrick/Documents/GitHub/WoofWare.Myriad.Plugins/WoofWare.Myriad.Plugins/bin/Debug/net8.0/WoofWare.Myriad.Plugins.dll"
