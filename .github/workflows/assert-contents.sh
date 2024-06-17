#!/bin/bash

echo "Unzipping version from NuGet"
ls from-nuget.nupkg
mkdir from-nuget && cp from-nuget.nupkg from-nuget/zip.zip && cd from-nuget && unzip zip.zip && rm zip.zip && cd - || exit 1

echo "Unzipping version from local build"
ls packed/
mkdir from-local && cp packed/*.nupkg from-local/zip.zip && cd from-local && unzip zip.zip && rm zip.zip && cd - || exit 1

cd from-local && find . -type f -exec sha256sum {} \; | sort > ../from-local.txt && cd .. || exit 1
cd from-nuget && find . -type f -and -not -name '.signature.p7s' -exec sha256sum {} \; | sort > ../from-nuget.txt && cd .. || exit 1

diff from-local.txt from-nuget.txt
