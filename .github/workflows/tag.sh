#!/bin/sh

export DRY_RUN_FLAG
if [ "$DRY_RUN" = 1 ]; then
    DRY_RUN_FLAG="--dry-run"
fi

for file in find . -maxdepth 1 -type f -name '*.nupkg'; do
    tag=$(basename "$file" .nupkg)
    git tag "$tag"
    git push origin "$tag" "$DRY_RUN_FLAG"
done

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
curl_body='{"tag_name":"'"$TAG"'","target_commitish":"","name":"'"$TAG"'","draft":false,"prerelease":false,"generate_release_notes":false}'

echo "cURL body: $curl_body"

failed_output=$(cat <<'EOF'
{
  "message": "Validation Failed",
  "errors": [
    {
      "resource": "Release",
      "code": "already_exists",
      "field": "tag_name"
    }
  ],
  "documentation_url": "https://docs.github.com/rest/releases/releases#create-a-release"
}
EOF
)

success_output=$(cat <<'EOF'
{
  "url": "https://api.github.com/repos/Smaug123/WoofWare.Myriad/releases/158152116",
  "assets_url": "https://api.github.com/repos/Smaug123/WoofWare.Myriad/releases/158152116/assets",
  "upload_url": "https://uploads.github.com/repos/Smaug123/WoofWare.Myriad/releases/158152116/assets{?name,label}",
  "html_url": "https://github.com/Smaug123/WoofWare.Myriad/releases/tag/WoofWare.Myriad.Plugins.2.1.30",
  "id": 158152116,
  "author": {
    "login": "github-actions[bot]",
    "id": 41898282,
    "node_id": "MDM6Qm90NDE4OTgyODI=",
    "avatar_url": "https://avatars.githubusercontent.com/in/15368?v=4",
    "gravatar_id": "",
    "url": "https://api.github.com/users/github-actions%5Bbot%5D",
    "html_url": "https://github.com/apps/github-actions",
    "followers_url": "https://api.github.com/users/github-actions%5Bbot%5D/followers",
    "following_url": "https://api.github.com/users/github-actions%5Bbot%5D/following{/other_user}",
    "gists_url": "https://api.github.com/users/github-actions%5Bbot%5D/gists{/gist_id}",
    "starred_url": "https://api.github.com/users/github-actions%5Bbot%5D/starred{/owner}{/repo}",
    "subscriptions_url": "https://api.github.com/users/github-actions%5Bbot%5D/subscriptions",
    "organizations_url": "https://api.github.com/users/github-actions%5Bbot%5D/orgs",
    "repos_url": "https://api.github.com/users/github-actions%5Bbot%5D/repos",
    "events_url": "https://api.github.com/users/github-actions%5Bbot%5D/events{/privacy}",
    "received_events_url": "https://api.github.com/users/github-actions%5Bbot%5D/received_events",
    "type": "Bot",
    "site_admin": false
  },
  "node_id": "RE_kwDOJfksgc4JbTW0",
  "tag_name": "WoofWare.Myriad.Plugins.2.1.30",
  "target_commitish": "main",
  "name": "WoofWare.Myriad.Plugins.2.1.30",
  "draft": false,
  "prerelease": false,
  "created_at": "2024-05-30T11:00:55Z",
  "published_at": "2024-05-30T11:03:02Z",
  "assets": [

  ],
  "tarball_url": "https://api.github.com/repos/Smaug123/WoofWare.Myriad/tarball/WoofWare.Myriad.Plugins.2.1.30",
  "zipball_url": "https://api.github.com/repos/Smaug123/WoofWare.Myriad/zipball/WoofWare.Myriad.Plugins.2.1.30",
  "body": null
}
EOF
)

HANDLE_OUTPUT=''
handle_error() {
    ERROR_OUTPUT="$1"
    exit_message=$(echo "$ERROR_OUTPUT" | jq -r --exit-status 'if .errors | length == 1 then .errors[0].code else null end')
    if [ "$exit_message" = "already_exists" ] ; then
        HANDLE_OUTPUT="Did not create GitHub release because it already exists at this version."
    else
        echo "Unexpected error output from curl: $(cat curl_output.json)"
        echo "JQ output: $(exit_message)"
        exit 2
    fi
}

run_tests() {
    handle_error "$failed_output"
    if [ "$HANDLE_OUTPUT" != "Did not create GitHub release because it already exists at this version." ]; then
        echo "Bad output from handler: $HANDLE_OUTPUT"
        exit 3
    fi
    HANDLE_OUTPUT=''
    echo "Tests passed."
}

run_tests

if [ "$DRY_RUN" != 1 ] ; then
    if curl --fail-with-body -L -X POST -H "Accept: application/vnd.github+json" -H "Authorization: Bearer $GITHUB_TOKEN" -H "X-GitHub-Api-Version: 2022-11-28" https://api.github.com/repos/Smaug123/WoofWare.Myriad/releases -d "$curl_body" > curl_output.json; then
        echo "Curl succeeded."
    else
        handle_error "$(cat curl_output.json)"
        echo "$HANDLE_OUTPUT"
    fi
fi
