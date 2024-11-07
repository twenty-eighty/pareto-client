#!/bin/bash

INDEX_ORG_EN="https://pareto.onepage.me/2"
INDEX_ORG_DE="https://pareto.onepage.me/de"

INDEX_EN_DESKTOP=priv/static/lp/en/index.html
INDEX_EN_MOBILE=priv/static/lp/en/index_mobile.html
INDEX_DE_DESKTOP=priv/static/lp/de/index.html
INDEX_DE_MOBILE=priv/static/lp/de/index_mobile.html

# EN/desktop
curl --compressed "$INDEX_ORG_EN" > $INDEX_EN_DESKTOP

sed -i -e 's|https://pareto.onepage.me/de|/lp/de/index.html|g' -e 's|https://pareto.onepage.me/2|https://pareto.space|g' -e 's|pareto.onepage.me|pareto.space|g' -e 's|content="pareto.onepage.me"|content="The Pareto Project"|g' -e 's|lang="de"|lang="en"|g' $INDEX_EN_DESKTOP

# EN/mobile
curl --compressed -H 'user-agent: Mozilla/5.0 (Linux; Android 6.0; Nexus 5 Build/MRA58N) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0.0.0 Mobile Safari/537.36' "$INDEX_ORG_EN" > $INDEX_EN_MOBILE

sed -i -e 's|https://pareto.onepage.me/de|/lp/de/index_mobile.html|g' -e 's|https://pareto.onepage.me/2|https://pareto.space|g' -e 's|pareto.onepage.me|pareto.space|g' -e 's|content="pareto.onepage.me"|content="The Pareto Project"|g' -e 's|lang="de"|lang="en"|g' $INDEX_EN_MOBILE

# DE/desktop
curl --compressed "$INDEX_ORG_DE" > $INDEX_DE_DESKTOP

sed -i -e 's|https://pareto.onepage.me/2|/lp/en/index.html|g' -e 's|https://pareto.onepage.me/de|https://pareto.space|g' -e 's|pareto.onepage.me|pareto.space|g' -e 's|content="pareto.onepage.me"|content="The Pareto Project"|g' -e 's|lang="de"|lang="en"|g' $INDEX_DE_DESKTOP

# DE/mobile
curl --compressed -H 'user-agent: Mozilla/5.0 (Linux; Android 6.0; Nexus 5 Build/MRA58N) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0.0.0 Mobile Safari/537.36' "$INDEX_ORG_DE" > $INDEX_DE_MOBILE

sed -i -e 's|https://pareto.onepage.me/2|/lp/en/index.html_mobile|g' -e 's|https://pareto.onepage.me/de|https://pareto.space|g' -e 's|pareto.onepage.me|pareto.space|g' -e 's|content="pareto.onepage.me"|content="The Pareto Project"|g' -e 's|lang="de"|lang="en"|g' $INDEX_DE_MOBILE

