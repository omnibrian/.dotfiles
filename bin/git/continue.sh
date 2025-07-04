#!/usr/bin/env bash
#
# Continue current git action (merge|rebase|revert)

set -euo pipefail

git-continueaction() {
  if git rev-parse --verify $1 &>/dev/null; then
    echo "Continuing $2..."
    git $2 --continue
  fi
}

git-continueaction CHERRY_PICK_HEAD cherry-pick
git-continueaction MERGE_HEAD merge
git-continueaction REBASE_HEAD rebase
git-continueaction REVERT_HEAD revert
