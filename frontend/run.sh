#!/bin/bash

if [ -f .env ]; then
  set -a
  # shellcheck disable=SC1091
  source .env
  set +a
fi

export ELM_HOME=elm-home/elm-stuff/

node ./elm-kernel-replacements/run_replace-kernel-packages.mjs

ELM_ENV=env ./node_modules/.bin/elm-land server
