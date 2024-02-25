#!/bin/sh

find . -maxdepth 1 -type f -name '*.nupkg' -exec sh -c 'tag=$(basename "$1" .nupkg); git tag "$tag"; git push origin "$tag"' shell {} \;

export TAG
TAG=$(find . -maxdepth 1 -type f -name 'WoofWare.Myriad.Plugins.*.nupkg' -exec sh -c 'basename "$1" .nupkg' shell {} \;)

# target_commitish empty indicates the repo default branch
curl -L -X POST -H "Accept: application/vnd.github+json" -H "Authorization: Bearer $GITHUB_TOKEN" -H "X-GitHub-Api-Version: 2022-11-28" https://api.github.com/repos/Smaug123/WoofWare.Myriad/releases -d '{"tag_name":"'"$TAG"'","target_commitish":"","name":"'"$TAG"'","draft":false,"prerelease":false,"generate_release_notes":false}'
