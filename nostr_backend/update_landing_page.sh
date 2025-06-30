#!/bin/bash

set -u
set -e

INDEX_ORG_EN="https://pareto.onepage.me/en"
INDEX_ORG_DE="https://pareto.onepage.me/de"

INDEX_EN_DESKTOP=priv/static/lp/en/index.html
INDEX_EN_MOBILE=priv/static/lp/en/index_mobile.html
INDEX_DE_DESKTOP=priv/static/lp/de/index.html
INDEX_DE_MOBILE=priv/static/lp/de/index_mobile.html

# Common replacements
COMMON_REPLACEMENTS=(
  -e 's|pareto.onepage.me|pareto.space|g'
  -e 's|/sitemap.xml|/sitemap/sitemap.xml.gz|g'
)

# EN/desktop
curl --compressed "$INDEX_ORG_EN" > $INDEX_EN_DESKTOP

sed -i \
  -e 's|https://pareto.onepage.me/de|/de|g' \
  -e 's|https://pareto.onepage.me/en|https://pareto.space/en|g' \
  -e 's|content="pareto.onepage.me"|content="Pareto Project"|g' \
  -e 's|lang="de"|lang="en"|g' \
  -e 's|"lang":"de"|"lang":"en"|g' \
  "${COMMON_REPLACEMENTS[@]}" \
  "$INDEX_EN_DESKTOP"

# EN/mobile
curl --compressed -H 'user-agent: Mozilla/5.0 (Linux; Android 6.0; Nexus 5 Build/MRA58N) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0.0.0 Mobile Safari/537.36' "$INDEX_ORG_EN" > $INDEX_EN_MOBILE

sed -i \
  -e 's|https:\\u002F\\u002Fpareto.onepage.me\\u002Fde|https:\\u002F\\u002Fpareto.space\\u002Fde|g' \
  -e 's|https://pareto.onepage.me/en|https://pareto.space/en|g' \
  -e 's|content="pareto.onepage.me"|content="The Pareto Project"|g' \
  -e 's|lang="de"|lang="en"|g' \
  -e 's|"lang":"de"|"lang":"en"|g' \
  "${COMMON_REPLACEMENTS[@]}" \
  "$INDEX_EN_MOBILE"

# DE/desktop
curl --compressed "$INDEX_ORG_DE" > $INDEX_DE_DESKTOP

sed -i \
  -e 's|https://pareto.onepage.me/en|/en|g' \
  -e 's|https://pareto.onepage.me/de|https://pareto.space/de|g' \
  -e 's|content="pareto.onepage.me"|content="The Pareto Project"|g' \
  -e 's|lang="de"|lang="en"|g' \
  "${COMMON_REPLACEMENTS[@]}" \
  "$INDEX_DE_DESKTOP"

# DE/mobile
curl --compressed -H 'user-agent: Mozilla/5.0 (Linux; Android 6.0; Nexus 5 Build/MRA58N) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0.0.0 Mobile Safari/537.36' "$INDEX_ORG_DE" > $INDEX_DE_MOBILE

sed -i \
  -e 's|https:\\u002F\\u002Fpareto.onepage.me\\u002Fen|https:\\u002F\\u002Fpareto.space\\u002Fen|g' \
  -e 's|https://pareto.onepage.me/de|https://pareto.space|g' \
  -e 's|content="pareto.onepage.me"|content="Pareto Projekt"|g' \
  -e 's|lang="de"|lang="en"|g' \
  "${COMMON_REPLACEMENTS[@]}" \
  "$INDEX_DE_MOBILE"

