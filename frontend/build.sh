#!/bin/bash

set -eu

# For ARM64 architecture elm and elm-land are avalable globally via PATH and alias, loading them here
source ~/.bashrc
shopt -s expand_aliases

rm -rf dist

npm install

./gentranslations.sh
# ./generate_tailwind_modules.sh

./gen-build-info.sh

if [ -n "${ELM_ENV+x}" ]; then
    echo "ELM_ENV: ${ELM_ENV}"
    sed -i "s/env.ELM_ENV/\"${ELM_ENV}\"/g" src/interop.js
fi

if [ -n "${IMAGE_CACHING_SERVER+x}" ]; then
    echo "IMAGE_CACHING_SERVER: ${IMAGE_CACHING_SERVER}"
    sed -i "s|env.IMAGE_CACHING_SERVER|\"${IMAGE_CACHING_SERVER}\"|g" src/interop.js
fi

elm-land build

if [ -n "${ELM_ENV+x}" ]; then
    git checkout -- src/interop.js
fi
