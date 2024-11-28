#!/bin/bash

rm -rf nostr_backend/_build nostr_backend/deps

 # Elm currently does not support ARM64, forcing AMD64 enviroment that will then use rosetta to translate
 # programs written for Intel Macs to run in M1/M2 processors (ref: https://github.com/elm/compiler/issues/2283)
 [ `uname -m` == "arm64" ] && export DOCKER_DEFAULT_PLATFORM=linux/amd64

docker-compose build
