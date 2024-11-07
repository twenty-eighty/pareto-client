#!/bin/bash

INDEX_OLD_EN="https://pareto.onepage.me/2"
INDEX_OLD_DE="https://pareto.onepage.me/de"

INDEX_EN=priv/static/lp/en/index.html
INDEX_DE=priv/static/lp/de/index.html

curl --compressed "$INDEX_OLD_EN" > $INDEX_EN

sed -i -e 's|https://pareto.onepage.me/de|/lp/de/index.html|g' -e 's|https://pareto.onepage.me/2|https://pareto.space|g' -e 's|pareto.onepage.me|pareto.space|g' -e 's|content="pareto.onepage.me"|content="The Pareto Project"|g' -e 's|lang="de"|lang="en"|g' -e 's|https://cashu.space/|<a href="https://cashu.space/">Cashu</a>|g' $INDEX_EN

curl --compressed "$INDEX_OLD_DE" > $INDEX_DE

sed -i -e 's|https://pareto.onepage.me/2|/lp/en/index.html|g' -e 's|https://pareto.onepage.me/de|https://pareto.space|g' -e 's|pareto.onepage.me|pareto.space|g' -e 's|content="pareto.onepage.me"|content="The Pareto Project"|g' -e 's|lang="de"|lang="en"|g' -e 's|https://cashu.space/|<a href="https://cashu.space/">Cashu</a>|g' -e 's|https:\\u002F\\u002Fcashu.space\\u002F|\\u003Ca href\\u003D\\u0022https:\\u002F\\u002Fcashu.space\\u002F\\u0022\\u003ECashu\\u003C\\u002Fa\\u003E|g' $INDEX_DE

