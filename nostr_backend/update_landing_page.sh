#!/bin/bash

INDEX_EN=priv/static/lp/en/index.html
INDEX_DE=priv/static/lp/de/index.html

curl --compressed "https://pareto.onepage.me" > $INDEX_EN

sed -i -e 's|https://pareto.onepage.me|https://pareto.space|g' -e 's|content="pareto.onepage.me"|content="The Pareto Project"|g' -e 's|lang="de"|lang="en"|g' $INDEX_EN

curl --compressed "https://pareto.onepage.me" > $INDEX_DE

sed -i -e 's|https://pareto.onepage.me|https://pareto.space|g' -e 's|content="pareto.onepage.me"|content="Das Pareto Projekt"|g' $INDEX_DE

