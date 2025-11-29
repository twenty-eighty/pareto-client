#!/bin/bash

export ELM_HOME=elm-home/elm-stuff/

node ./elm-kernel-replacements/run_replace-kernel-packages.mjs

ELM_ENV=env elm-land server
