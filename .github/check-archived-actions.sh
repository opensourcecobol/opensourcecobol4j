#!/usr/bin/env bash
# Warns when .github/workflows/ uses an action whose upstream repository is archived. 

set -uo pipefail

# Run from the repository root so that annotation paths stay repo-relative,
# which is what GitHub needs to attach them to the file in the diff view.
cd "$(dirname "$0")/.." || exit 0
workflow_dir=".github/workflows"

# owner/repo of every remote action, ignoring local reusable workflows (./...)
# and container actions (docker://...).
repos="$(grep -rhoE 'uses:[[:space:]]*[^[:space:]#]+' "$workflow_dir"/*.yml \
  | sed -E 's/uses:[[:space:]]*//' \
  | grep -vE '^(\./|docker://)' \
  | cut -d@ -f1 \
  | cut -d/ -f1,2 \
  | sort -u)"

archived_count=0

for repo in $repos; do
  archived="$(gh api "repos/$repo" --jq '.archived' 2>/dev/null)"
  if [ "$archived" != "true" ]; then
    # Not archived, or the query failed (deleted repo, rate limit, network).
    # Either way there is nothing to report.
    continue
  fi

  archived_count=$((archived_count + 1))
  pattern="$(printf '%s' "$repo" | sed 's/\./\\./g')"

  grep -rnE "uses:[[:space:]]*$pattern([/@])" "$workflow_dir"/*.yml \
    | cut -d: -f1,2 \
    | while IFS=: read -r file line; do
        echo "::warning file=$file,line=$line::$repo is archived upstream. It no longer receives updates and Dependabot cannot bump it. Consider migrating to a maintained action."
      done

  if [ -n "${GITHUB_STEP_SUMMARY:-}" ]; then
    echo "- \`$repo\` is archived upstream" >> "$GITHUB_STEP_SUMMARY"
  fi
done

if [ "$archived_count" -eq 0 ]; then
  echo "No archived actions found."
fi

exit 0
