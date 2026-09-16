#!/usr/bin/env bash
set -e

# Resolve a short git SHA for BuildInfo. Docker images copy only `frontend/`,
# so `.git` is not available there. CI/CD must pass the commit as an env var
# or Docker build-arg (see Dockerfile ARG GIT_VERSION / RENDER_GIT_COMMIT).

shorten_sha() {
  local sha="$1"
  if [[ "$sha" =~ ^[0-9a-fA-F]{7,40}$ ]]; then
    echo "${sha:0:7}"
  else
    echo "$sha"
  fi
}

GIT_SHA=""

if [ -n "${GIT_VERSION:-}" ]; then
  GIT_SHA="$GIT_VERSION"
elif [ -n "${GITHUB_SHA:-}" ]; then
  GIT_SHA="$GITHUB_SHA"
elif [ -n "${RENDER_GIT_COMMIT:-}" ]; then
  GIT_SHA="$RENDER_GIT_COMMIT"
elif [ -n "${SOURCE_VERSION:-}" ]; then
  GIT_SHA="$SOURCE_VERSION"
else
  script_dir="$(cd "$(dirname "$0")" && pwd)"
  repo_root="$(cd "$script_dir/.." && pwd)"
  if tmp=$(git -C "$repo_root" rev-parse --short HEAD 2>/dev/null); then
    GIT_SHA="$tmp"
  elif tmp=$(git -C "$script_dir" rev-parse --short HEAD 2>/dev/null); then
    GIT_SHA="$tmp"
  else
    GIT_SHA="unknown"
  fi
fi

GIT_SHA="$(shorten_sha "$GIT_SHA")"

# Universal ISO 8601 timestamp generation
# Use standard date format that works on all Unix-like systems
# Then add colon to timezone offset for proper ISO 8601 format
BUILD_TIME=$(date +"%Y-%m-%dT%H:%M:%S%z" | sed 's/\([+-][0-9][0-9]\)\([0-9][0-9]\)/\1:\2/')

echo "BuildInfo gitVersion=${GIT_SHA} buildTime=${BUILD_TIME}"

cat > static/version.json <<EOF
{"gitVersion":"${GIT_SHA}","buildTime":"${BUILD_TIME}"}
EOF

if [ -f pwa/sw.js ]; then
  sed "s/__PARETO_BUILD_VERSION__/${GIT_SHA}/g" pwa/sw.js > static/sw.js
else
  echo "warning: pwa/sw.js template missing; service worker not generated" >&2
fi

mkdir -p gen/BuildInfo

cat > gen/BuildInfo/BuildInfo.elm <<EOF
module BuildInfo exposing (buildTime, gitVersion)

-- | When the client was last built
buildTime : String
buildTime =
    "${BUILD_TIME}"

-- | The git‐revision of the build
gitVersion : String
gitVersion =
    "${GIT_SHA}"
EOF
