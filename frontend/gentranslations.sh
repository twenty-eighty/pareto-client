#!/bin/bash

set -eu

ROOT="$(cd "$(dirname "$0")" && pwd)"

# Prefer project-local binaries. elm-i18next-gen also shells out to elm, and
# when run via npx that happens under a nested directory where asdf may not
# see this project's .tool-versions.
export PATH="$ROOT/node_modules/.bin:$PATH"
if [ -f "$ROOT/.tool-versions" ]; then
  export ASDF_ELM_VERSION="${ASDF_ELM_VERSION:-$(awk '/^elm /{print $2; exit}' "$ROOT/.tool-versions")}"
fi

if ! command -v elm >/dev/null 2>&1; then
  echo "elm is required to generate translations but was not found on PATH." >&2
  exit 1
fi

if ! command -v elm-i18next-gen >/dev/null 2>&1; then
  echo "elm-i18next-gen is required. Run: pnpm install" >&2
  exit 1
fi

# elm-i18next-gen's codegen/elm.json hardcodes elm-version 0.19.1, which
# fails when the project (and PATH) use 0.19.2. Align before generating.
ELM_VERSION="$(elm --version)"
CODEGEN_ELM_JSON="$ROOT/node_modules/@abradley2/elm-i18next-gen/codegen/elm.json"
if [ -f "$CODEGEN_ELM_JSON" ]; then
  if sed --version >/dev/null 2>&1; then
    sed -i "s/\"elm-version\": \"[^\"]*\"/\"elm-version\": \"$ELM_VERSION\"/" "$CODEGEN_ELM_JSON"
  else
    sed -i '' "s/\"elm-version\": \"[^\"]*\"/\"elm-version\": \"$ELM_VERSION\"/" "$CODEGEN_ELM_JSON"
  fi
fi

elm-i18next-gen --output=gen/Translations --translations=lang/lang-en_US.json

CONFLICTING_FILE=gen/Translations/Language.elm

if [ ! -f "$CONFLICTING_FILE" ]; then
  echo "Translation generation failed: expected $CONFLICTING_FILE was not created." >&2
  exit 1
fi

# Check if the sed version supports --version (GNU sed)
if sed --version >/dev/null 2>&1; then
  # GNU sed detected
  sed -i '1s/module Language/module DefaultLanguage/' "$CONFLICTING_FILE"
else
  # Assume BSD sed (macOS or other systems)
  sed -i '' '1s/module Language/module DefaultLanguage/' "$CONFLICTING_FILE"
fi

mv "$CONFLICTING_FILE" gen/Translations/DefaultLanguage.elm
