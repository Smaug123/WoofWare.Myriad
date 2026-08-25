#!/bin/sh

# The plugin's dependency closure must reach the package by a route that does not depend on the
# build directory already being populated.
#
# The nupkg CI publishes cannot demonstrate that, because CI builds before it packs, so
# $(OutputPath) is full by the time pack evaluates the project. Pack again into an output
# directory that is guaranteed empty, and run the loader check on that package. Relocating
# $(OutputPath) rather than relying on where this sits among the CI steps keeps the check honest
# whatever else the job has done first.

set -eu

if [ "$#" -ne 0 ]; then
    echo "usage: $0" >&2
    exit 1
fi

repo=$(CDPATH='' cd -- "$(dirname -- "$0")/.." && pwd)

workdir=$(mktemp -d)
# shellcheck disable=SC2064 # expand workdir now, while it is still set
trap "rm -rf '$workdir'" EXIT

dotnet pack "$repo/WoofWare.Myriad.Plugins/WoofWare.Myriad.Plugins.fsproj" \
    --configuration Release \
    -p:BaseOutputPath="$workdir/bin/" \
    --output "$workdir/nupkg"

# shellcheck disable=SC2086 # deliberate: expand the glob into the argument list to count matches
set -- "$workdir"/nupkg/*.nupkg
if [ "$#" -ne 1 ] || [ ! -f "$1" ]; then
    echo "expected exactly one nupkg from the clean-output pack, got: $*" >&2
    exit 1
fi

"$repo/scripts/check-packed-plugin-loads.sh" "$1"
