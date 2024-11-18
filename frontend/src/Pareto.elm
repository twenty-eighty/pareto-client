module Pareto exposing (..)

import Nostr.Types exposing (PubKey)

-- the follow list of this pubkey contains all Pareto authors
authorsKey : PubKey
authorsKey =
    "0f47948ccf4d12064ede2e0aa744868a2443cb1c42b32c06191e0d902205abef"

-- name of client in "client" tag when publishing articles
client : String
client =
    "Pareto"