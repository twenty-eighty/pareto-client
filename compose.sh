#!/bin/bash

rm -rf nostr_backend/_build nostr_backend/deps

docker-compose build
