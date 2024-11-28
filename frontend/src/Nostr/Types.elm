module Nostr.Types exposing (..)

import Json.Encode as Encode

type alias EventId = String

type alias PubKey = String

type alias RelayUrl = String

type alias OutgoingCommand =
    { command : String
    , value : Encode.Value
    }

type alias IncomingMessage =
    { messageType : String
    , value : Encode.Value
    }
