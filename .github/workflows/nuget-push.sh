#!/bin/bash

SOURCE_NUPKG=$(find . -type f -name '*.nupkg')

PACKAGE_VERSION=$(basename "$SOURCE_NUPKG" | rev | cut -d '.' -f 2-4 | rev)

echo "version=$PACKAGE_VERSION" >> "$GITHUB_OUTPUT"

tmp=$(mktemp)

if ! dotnet nuget push "$SOURCE_NUPKG" --api-key "$NUGET_API_KEY" --source https://api.nuget.org/v3/index.json > "$tmp" ; then
    cat "$tmp"
    if grep 'already exists and cannot be modified' "$tmp" ; then
        echo "result=skipped" >> "$GITHUB_OUTPUT"
        exit 0
    else
        echo "Unexpected failure to upload"
        exit 1
    fi
fi

cat "$tmp"

echo "result=published" >> "$GITHUB_OUTPUT"
