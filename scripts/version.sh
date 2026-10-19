#!/usr/bin/env bash
# Prints the version of the program. The version is a tag of the repository: a tag that reads vMAJOR.MINOR.PATCH,
# with an optional suffix for a pre-release (v1.3.0-rc.1), and every such tag is a version.
#
# Usage: scripts/version.sh
#
# On a run of the pipeline that a version tag started (GITHUB_REF_TYPE is tag), it prints the tag without its v,
# and fails when the tag does not read as a version. Anywhere else it asks git for the nearest version tag behind
# the commit: that version when the commit is the tagged one, and, when the commit is past it, the version, the
# number of commits past it and the commit (1.3.0-4-g1a2b3c4), with -dirty added for uncommitted changes. When
# there is no version tag, or no git, it prints the version of borba-cad.asd with -dev (and the commit, if
# known), so that a build that is not a release never carries the name of one.

set -euo pipefail

root=$(cd "$(dirname "$0")/.." && pwd)
version_pattern='^[0-9]+\.[0-9]+\.[0-9]+(-[0-9A-Za-z.]+)?$'

if [ "${GITHUB_REF_TYPE:-}" = "tag" ]; then
  version=${GITHUB_REF_NAME#v}
  if [ "${GITHUB_REF_NAME:0:1}" != "v" ] || ! [[ $version =~ $version_pattern ]]; then
    echo "error: the tag '${GITHUB_REF_NAME}' is not a version: write it as vMAJOR.MINOR.PATCH, such as v1.3.0" >&2
    exit 1
  fi
  echo "$version"
  exit 0
fi

if described=$(git -C "$root" describe --tags --match 'v[0-9]*.[0-9]*.[0-9]*' --dirty 2>/dev/null); then
  echo "${described#v}"
  exit 0
fi

declared=$(awk -F'"' '/:version/ {print $2; exit}' "$root/borba-cad.asd")
commit=$(git -C "$root" rev-parse --short HEAD 2>/dev/null || true)
if [ -n "$commit" ]; then
  echo "$declared-dev-g$commit"
else
  echo "$declared-dev"
fi
