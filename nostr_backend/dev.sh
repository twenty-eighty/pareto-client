#!/bin/bash

set -eu

source ../.env

export POSTHOG_HOST
export POSTHOG_API_KEY
export FOLLOW_LIST_PUBKEY

mix phx.server
