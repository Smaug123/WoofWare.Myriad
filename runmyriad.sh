#!/bin/sh

dotnet \
    "/Users/patrick/.nuget/packages/myriad.sdk/0.8.3/build/../tools/net6.0/any/Myriad.dll" \
    --inputfile "/Users/patrick/Documents/GitHub/MyriadPlugin/ConsumePlugin/RecordFile.fs" \
    --outputfile "/Users/patrick/Documents/GitHub/MyriadPlugin/ConsumePlugin/Generated.fs" \
    --configfile "/Users/patrick/Documents/GitHub/MyriadPlugin/ConsumePlugin/myriad.toml" \
    --contextfile "/Users/patrick/Documents/GitHub/MyriadPlugin/ConsumePlugin/obj/myriad.context.toml" \
    --plugin "/Users/patrick/Documents/GitHub/MyriadPlugin/MyriadPlugin/bin/Debug/net8.0/MyriadPlugin.dll"
