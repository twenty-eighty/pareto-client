module Nostr.Nip50 exposing (..)

import Nostr.Event exposing (Kind, TagReference)

-- NIP-50 - search

type alias SearchEventFilter =
    { authors : Maybe (List String)
    , ids : Maybe (List String)
    , kinds : Maybe (List Kind)
    , tagReferences : Maybe (List TagReference)
    , limit : Maybe Int
    , search : String
    }
