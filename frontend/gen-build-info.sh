#!/usr/bin/env bash
set -e

# get the short git SHA and an ISO timestamp

# determine git SHA: prefer GITHUB_SHA env var if set
if [ -n "$GITHUB_SHA" ]; then
    GIT_SHA="$GITHUB_SHA"
else
    GIT_SHA=$(git --git-dir ../.git rev-parse --short HEAD)
fi
BUILD_TIME=$(date --iso-8601=seconds)

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
