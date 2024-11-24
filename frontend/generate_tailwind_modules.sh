#!/bin/bash

set -eu

npm i --save-dev elm-tailwind-modules tailwindcss postcss
npx elm-tailwind-modules --dir ./gen/Tailwind

