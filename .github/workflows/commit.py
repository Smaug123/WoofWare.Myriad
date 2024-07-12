import datetime
import time
import subprocess
import requests
import os
import base64
from typing import Literal, TypedDict

class TreeEntry(TypedDict):
    path: str
    mode: Literal["100644", "100755", "120000"]
    type: Literal["blob", "tree"]
    sha: str


# GitHub API configuration
GITHUB_API_URL = "https://api.github.com"
GITHUB_TOKEN = os.environ.get("BEARER_TOKEN")
if not GITHUB_TOKEN:
    raise Exception("Supply BEARER_TOKEN env var")
REPO = os.environ.get("GITHUB_REPOSITORY")
if not REPO:
    raise Exception("Supply GITHUB_REPOSITORY env var")
DEFAULT_BRANCH = os.environ.get("DEFAULT_BRANCH") or ""
if not DEFAULT_BRANCH:
    raise Exception("Supply DEFAULT_BRANCH env var")
GITHUB_OUTPUT = os.environ.get("GITHUB_OUTPUT") or ""
if not GITHUB_OUTPUT:
    raise Exception("Supply GITHUB_OUTPUT env var")

headers = {
    "Accept": "application/vnd.github+json",
    "Authorization": f"Bearer {GITHUB_TOKEN}",
    "X-GitHub-Api-Version": "2022-11-28"
}

def get_git_diff():
    """Get the files which have changed in the current repository."""
    return subprocess.check_output(["git", "diff", "--name-only"]).decode("utf-8").splitlines()

def create_blob(content, encoding="utf-8"):
    """Create a blob in the GitHub repository."""
    url = f"{GITHUB_API_URL}/repos/{REPO}/git/blobs"
    data = {
        "content": content,
        "encoding": encoding
    }
    response = requests.post(url, headers=headers, json=data)
    if not response.ok:
        raise Exception(f"bad response: {response}")
    print(f"Blob response: {response.text}")
    return response.json()["sha"]

def create_tree(base_tree: str, changes: list[TreeEntry]) -> str:
    """Create a new tree with the given changes."""
    url = f"{GITHUB_API_URL}/repos/{REPO}/git/trees"
    data = {
        "base_tree": base_tree,
        "tree": changes
    }
    response = requests.post(url, headers=headers, json=data)
    if not response.ok:
        raise Exception(f"bad response: {response}")
    print(f"Tree response: {response.text}")
    return response.json()["sha"]

def create_commit(tree_sha: str, parent_sha: str, message: str):
    """Create a new commit."""
    url = f"{GITHUB_API_URL}/repos/{REPO}/git/commits"
    data = {
        "message": message,
        "tree": tree_sha,
        "parents": [parent_sha]
    }
    print(f"Commit request body: {data}")
    response = requests.post(url, headers=headers, json=data)
    if not response.ok:
        raise Exception(f"bad response: {response}")
    print(f"Commit response: {response.text}")
    json = response.json()
    print(f"Commit: {json}")
    return json["sha"]

def is_executable(filepath: str):
    return os.path.isfile(filepath) and os.access(filepath, os.X_OK)

def get_current_commit() -> str:
    return subprocess.check_output(["git", "rev-parse", "HEAD"]).decode("utf-8").strip()

def get_current_tree() -> str:
    return [line for line in subprocess.check_output(["git", "cat-file", "-p", "HEAD"]).decode("utf-8").splitlines() if line.startswith('tree ')][0][5:]

def create_branch(branch_name: str, commit_sha: str) -> None:
    url = f"{GITHUB_API_URL}/repos/{REPO}/git/refs"
    data = {
        "ref": f"refs/heads/{branch_name}",
        "sha": commit_sha
    }
    print(f"Branch creation request body: {data}")
    response = requests.post(url, headers=headers, json=data)
    if not response.ok:
        raise Exception(f"bad response: {response}")
    print(f"Branch creation response: {response.text}")

def create_pull_request(title: str, branch_name: str, base_branch: str) -> tuple[str, int]:
    """Returns the URL of the new PR."""
    url = f"{GITHUB_API_URL}/repos/{REPO}/pulls"
    data = {
        "title": title,
        "head": branch_name,
        "base": base_branch,
        "body": "Automated pull request.",
        "maintainer_can_modify": True
    }
    print(f"PR creation request body: {data}")
    response = requests.post(url, headers=headers, json=data)
    if not response.ok:
        raise Exception(f"bad response: {response}")
    print(f"PR creation response: {response.text}")
    json = response.json()
    return json["url"], json["number"]

def main():
    changed_files = get_git_diff()

    # Create blobs and prepare tree changes
    tree_changes = []
    for file_path in changed_files:
        with open(file_path, "rb") as file:
            contents = base64.b64encode(file.read()).decode('ascii')
        blob_sha = create_blob(contents, encoding="base64")
        if is_executable(file_path):
            mode = "100755"
        else:
            mode = "100644"
        tree_changes.append(TreeEntry({
            "path": file_path,
            "mode": mode,
            "type": "blob",
            "sha": blob_sha
        }))

    base_tree = get_current_tree()

    # Create a new tree
    new_tree_sha = create_tree(base_tree, tree_changes)
    print(f"Tree: {new_tree_sha}")

    # Create a new commit
    commit_message = "Automated commit"
    new_commit_sha = create_commit(new_tree_sha, get_current_commit(), commit_message)
    print(f"New commit created: {new_commit_sha}")

    branch_name = f"flake_update" + datetime.datetime.fromtimestamp(time.time()).strftime('%Y_%m_%d-%H_%M_%S_%f')
    create_branch(branch_name, new_commit_sha)
    print(f"Branch created: {branch_name}")

    url, pr_num = create_pull_request(title="Upgrade Nix flake and deps", branch_name=branch_name, base_branch=DEFAULT_BRANCH)
    print(f"See PR at: {url}")

    with open(GITHUB_OUTPUT, "a") as output_file:
        output_file.write(f"pull-request-number={pr_num}")


if __name__ == "__main__":
    main()
