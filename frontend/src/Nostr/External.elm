module Nostr.External exposing (..)

import Json.Decode as Decode
import Nostr.Event exposing (Event, EventFilter, Kind)
import Nostr.Request exposing (HttpRequestMethod, RequestData(..), RequestId, RequestState(..))
import Nostr.Send exposing (SendRequestId)
import Nostr.Types exposing (Following(..), IncomingMessage, RelayRole(..), RelayUrl)


type alias Hooks msg =
    { connect : List String -> Cmd msg
    , receiveMessage : (IncomingMessage -> msg) -> Sub msg
    , requestEvents : String -> Bool -> RequestId -> List RelayUrl -> EventFilter -> Cmd msg
    , requestBlossomAuth : RequestId -> String -> String -> HttpRequestMethod -> Cmd msg
    , requestNip96Auth : RequestId -> String -> String -> HttpRequestMethod -> Cmd msg
    , searchEvents : String -> Bool -> RequestId -> List RelayUrl -> List EventFilter -> Cmd msg
    , sendEvent : SendRequestId -> List String -> Event -> Cmd msg
    }


decodeEvents : Decode.Value -> Result Decode.Error (List Event)
decodeEvents value =
    Decode.decodeValue (Decode.field "events" (Decode.list Nostr.Event.decodeEvent)) value


decodeEventsKind : Decode.Value -> Result Decode.Error Kind
decodeEventsKind value =
    Decode.decodeValue (Decode.field "kind" Nostr.Event.kindDecoder) value
