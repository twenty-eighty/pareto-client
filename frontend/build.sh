#!/bin/bash

set -eu

rm -rf dist

npm install

elm-land build
