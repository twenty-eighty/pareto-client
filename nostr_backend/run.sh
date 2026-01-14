#!/bin/bash

set -eu

FRONTEND_DIR=../frontend

# Export variables from ../.env (if present) so they're available to Phoenix at runtime
# (e.g. SECRET_KEY_BASE, FOLLOW_LIST_PUBKEY, POSTHOG_*).
if [ -f ../.env ]; then
  set -a
  source ../.env
  set +a
fi

pushd $FRONTEND_DIR

./build.sh

popd

cp -R $FRONTEND_DIR/dist/* priv/static

mix phx.server
