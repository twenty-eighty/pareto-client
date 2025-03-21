#!/bin/bash

set -eu

# For ARM64 architecture elm and elm-land are avalable globally via PATH and alias, loading them here
source ~/.bashrc
shopt -s expand_aliases

rm -rf dist

npm install

./gentranslations.sh
# ./generate_tailwind_modules.sh

elm-land build
