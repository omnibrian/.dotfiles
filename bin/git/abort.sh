#!/usr/bin/env bash
#
# Abort current git action (merge|rebase|revert)

set -euo pipefail

git-abortaction() {
  if git rev-parse --verify $1 &>/dev/null; then
    echo "Aborting $2..."
    git $2 --abort
  fi
}

git-abortaction CHERRY_PICK_HEAD cherry-pick
git-abortaction MERGE_HEAD merge
git-abortaction REBASE_HEAD rebase
git-abortaction REVERT_HEAD revert
