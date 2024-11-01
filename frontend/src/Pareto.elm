module Pareto exposing (..)

import Nostr.Types exposing (PubKey)

-- the follow list of this pubkey contains all Pareto authors
authorsKey : PubKey
authorsKey =
    "0f4795bf31824a414148daf1b589bb8138fb0a03963f984c84462e40a8365abe"

-- name of client in "client" tag when publishing articles
client : String
client =
    "Pareto"