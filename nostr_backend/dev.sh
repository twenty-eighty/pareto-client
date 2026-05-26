#!/bin/bash

set -eu

source ../.env

# Export variables from ../.env (if present) so they're available to Phoenix at runtime
# (e.g. SECRET_KEY_BASE, FOLLOW_LIST_PUBKEY, POSTHOG_*).
if [ -f ../.env ]; then
  set -a
  source ../.env
  set +a
fi

mix phx.server
