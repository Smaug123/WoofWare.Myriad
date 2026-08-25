#!/bin/sh

# Myriad loads this package as a plugin, not as a library: it is handed
# lib/<tfm>/WoofWare.Myriad.Plugins.dll by path and resolves that assembly's references by probing
# the directory the file sits in, with no knowledge of the package's NuGet dependencies. So the
# package is only usable if it carries the plugin's dependency closure beside the plugin. Assert
# that against the artefact that would be published, by loading it the way Myriad does.

set -eu

if [ "$#" -ne 1 ]; then
    echo "usage: $0 <path to WoofWare.Myriad.Plugins nupkg>" >&2
    exit 1
fi

nupkg="$1"
if [ ! -f "$nupkg" ]; then
    echo "not a file: $nupkg" >&2
    exit 1
fi

repo=$(CDPATH='' cd -- "$(dirname -- "$0")/.." && pwd)

workdir=$(mktemp -d)
# shellcheck disable=SC2064 # expand workdir now, while it is still set
trap "rm -rf '$workdir'" EXIT

unzip -q "$nupkg" -d "$workdir/pkg"

plugin=$(find "$workdir/pkg/lib" -name 'WoofWare.Myriad.Plugins.dll' -type f)
if [ "$(printf '%s' "$plugin" | grep -c '')" -ne 1 ]; then
    echo "expected exactly one packed plugin assembly under lib/, found:" >&2
    find "$workdir/pkg" -type f >&2
    exit 1
fi

echo "Packed alongside the plugin:"
ls "$(dirname -- "$plugin")"

# The version consumers are told to use, and the one this repo builds against.
sdk_version=$(xmlstarlet sel -t -v '//PackageReference[@Include="Myriad.Sdk"]/@Version' \
    "$repo/ConsumePlugin/ConsumePlugin.fsproj")
myriad="${NUGET_PACKAGES:-$HOME/.nuget/packages}/myriad.sdk/$sdk_version/tools/net6.0/any/Myriad.dll"
if [ ! -f "$myriad" ]; then
    echo "Myriad.Sdk $sdk_version is not restored at $myriad; run 'dotnet restore' first" >&2
    exit 1
fi

output="$workdir/Generated.fs"
dotnet "$myriad" \
    --inputfile "$repo/ConsumePlugin/RecordFile.fs" \
    --outputfile "$output" \
    --configfile "$repo/ConsumePlugin/myriad.toml" \
    --plugin "$plugin"

if [ ! -s "$output" ]; then
    echo "the packed plugin loaded but generated nothing" >&2
    exit 1
fi

echo "The packed plugin loads and generates."
