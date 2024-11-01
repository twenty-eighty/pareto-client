#!/bin/bash

set -eu

FRONTEND_DIR=../frontend

pushd $FRONTEND_DIR

./build.sh

popd

cp -R $FRONTEND_DIR/dist/* priv/static

mix phx.server
