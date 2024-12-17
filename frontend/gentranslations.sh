#!/bin/bash

set -eu

ELMI18NEXTGEN="npx @abradley2/elm-i18next-gen"

$ELMI18NEXTGEN --output=gen/Translations --translations=lang/lang-en_US.json

CONFLICTING_FILE=gen/Translations/Language.elm

# Check if the sed version supports --version (GNU sed)
if sed --version >/dev/null 2>&1; then
  # GNU sed detected
  sed -i '1s/module Language/module DefaultLanguage/' "$CONFLICTING_FILE"
else
  # Assume BSD sed (macOS or other systems)
  sed -i '' '1s/module Language/module DefaultLanguage/' "$CONFLICTING_FILE"
fi

mv $CONFLICTING_FILE gen/Translations/DefaultLanguage.elm
