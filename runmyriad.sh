#!/bin/sh

dotnet \
    "/Users/patrick/.nuget/packages/myriad.sdk/0.8.2/build/../tools/net6.0/any/Myriad.dll" \
    --inputfile "/Users/patrick/Documents/GitHub/MyriadPlugin/UsePlugin/RecordFile.fs" \
    --outputfile "/Users/patrick/Documents/GitHub/MyriadPlugin/UsePlugin/Generated.fs" \
    --configfile "/Users/patrick/Documents/GitHub/MyriadPlugin/UsePlugin/myriad.toml" \
    --contextfile "/Users/patrick/Documents/GitHub/MyriadPlugin/UsePlugin/obj/myriad.context.toml"
    --plugin "/Users/patrick/Documents/GitHub/MyriadPlugin/bin/Debug/net7.0/MyriadPlugin.dll"
