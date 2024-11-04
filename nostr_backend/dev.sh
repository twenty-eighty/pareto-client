#!/bin/bash

set -eu

source ../.env

export POSTHOG_HOST
export POSTHOG_API_KEY

mix phx.server
