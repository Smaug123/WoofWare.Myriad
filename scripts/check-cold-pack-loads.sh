#!/bin/sh

# The plugin's dependency closure must reach the package by a route that does not depend on the
# build directory already being populated. It used not to: the closure was gathered by a wildcard
# over $(OutputPath), which MSBuild expands when the project is evaluated, before the build that
# fills that directory. A package built that way carries only the plugin itself and fails to load.
#
# Checking the nupkg that CI publishes cannot catch a regression to that, because CI builds before
# it packs, so $(OutputPath) is populated by then and even the wildcard would find everything. So
# pack again into an output directory that is guaranteed empty, and check that package instead.
# Relocating $(OutputPath) rather than reordering the CI steps keeps this honest no matter what
# else the job has done first.

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
