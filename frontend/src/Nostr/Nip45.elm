module Nostr.Nip45 exposing (..)

import Nostr.Event exposing (EventFilter)


type alias Nip45CountReq =
    { subId : String
    , filter : EventFilter
    }
