#!/bin/bash

set -eu

rm -rf dist

npm install

./gentranslations.sh
./generate_tailwind_modules.sh

elm-land build
