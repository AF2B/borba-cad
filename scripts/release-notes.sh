#!/usr/bin/env bash
# Prints the notes of a release: the section of CHANGELOG.md that is headed with the version, without the heading.
#
# Usage: scripts/release-notes.sh VERSION
#
# VERSION is a version as scripts/version.sh prints it. For a pre-release (1.3.0-rc.1) the section of the version
# it leads to is used (1.3.0), and, when the changelog has none yet, the section called Unreleased. Fails, saying
# so, when the changelog has no such section or it is empty, so that a release is never published without notes.
#
# Environment: CHANGELOG (default CHANGELOG.md in the project) names the changelog to read.

set -euo pipefail

if [ "$#" -ne 1 ]; then
  echo "usage: $0 VERSION" >&2
  exit 2
fi

root=$(cd "$(dirname "$0")/.." && pwd)
changelog=${CHANGELOG:-$root/CHANGELOG.md}
version=$1
base=${version%%-*}

section() {
  awk -v heading="## [$1]" '
    index($0, heading) == 1 { found = 1; next }
    found && /^## \[/ { exit }
    found { print }
  ' "$changelog"
}

notes=$(section "$base")
if [ -z "${notes//[[:space:]]/}" ] && [ "$base" != "$version" ]; then
  notes=$(section "Unreleased")
fi

if [ -z "${notes//[[:space:]]/}" ]; then
  echo "error: $changelog has no notes for version $version: add a section headed '## [$base]'" >&2
  exit 1
fi

printf '%s\n' "$notes" | sed '/./,$!d'
