#!/bin/bash

set -eu

ELMI18NEXTGEN="npx @abradley2/elm-i18next-gen"

$ELMI18NEXTGEN --output=gen/Translations --translations=lang/lang-en_US.json

CONFLICTING_FILE=gen/Translations/Language.elm

sed -i '1s/module Language/module DefaultLanguage/' "$CONFLICTING_FILE"

mv $CONFLICTING_FILE gen/Translations/DefaultLanguage.elm
