#!/bin/sh

find . -maxdepth 1 -type f -name '*.nupkg' -exec sh -c 'tag=$(basename "$1" .nupkg); git tag "$tag"; git push origin "$tag"' shell {} \;

export TAG
TAG=$(find . -maxdepth 1 -type f -name 'WoofWare.Myriad.Plugins.*.nupkg' -exec sh -c 'basename "$1" .nupkg' shell {} \; | grep -v Attributes)

case "$TAG" in
  *"
"*)
    echo "Error: TAG contains a newline; multiple plugins found."
    exit 1
    ;;
esac

# target_commitish empty indicates the repo default branch
if curl --fail-with-body -L -X POST -H "Accept: application/vnd.github+json" -H "Authorization: Bearer $GITHUB_TOKEN" -H "X-GitHub-Api-Version: 2022-11-28" https://api.github.com/repos/Smaug123/WoofWare.Myriad/releases -d '{"tag_name":"'"$TAG"'","target_commitish":"","name":"'"$TAG"'","draft":false,"prerelease":false,"generate_release_notes":false}' > curl_output.json; then
    echo "Curl succeeded."
else
    exit_message=$(jq -r --exit-status 'if .errors | length == 1 then .errors[0].code else null' curl_output.json)
    if [ "$exit_message" = "already_exists" ] ; then
        echo "Did not create GitHub release because it already exists at this version."
    else
        echo "Unexpected error output from curl: $(cat curl_output.json)"
        echo "JQ output: $(exit_message)"
        exit 2
    fi
fi
