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
GITHUB_BASE_REF = os.environ.get("GITHUB_REF")
if not GITHUB_BASE_REF:
    raise Exception("Supply GITHUB_REF env var")

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
    return response.json()["sha"]

def create_tree(base_tree: str, changes: list[TreeEntry]):
    """Create a new tree with the given changes."""
    url = f"{GITHUB_API_URL}/repos/{REPO}/git/trees"
    data = {
        "base_tree": base_tree,
        "tree": changes
    }
    response = requests.post(url, headers=headers, json=data)
    return response.json()["sha"]

def create_commit(tree_sha, parent_sha, message):
    """Create a new commit."""
    url = f"{GITHUB_API_URL}/repos/{REPO}/git/commits"
    data = {
        "message": message,
        "tree": tree_sha,
        "parents": [parent_sha]
    }
    response = requests.post(url, headers=headers, json=data)
    return response.json()["sha"]

def is_executable(filepath: str):
    return os.path.isfile(filepath) and os.access(filepath, os.X_OK)

def get_current_tree() -> str:
    return [line for line in subprocess.check_output(["git", "cat-file", "-p", "HEAD"]).decode("utf-8").splitlines() if line.startswith('tree ')][0][5:]


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

    # Create a new commit
    commit_message = "Automated commit"
    new_commit_sha = create_commit(new_tree_sha, base_tree, commit_message)

    print(f"New commit created: {new_commit_sha}")

if __name__ == "__main__":
    main()
