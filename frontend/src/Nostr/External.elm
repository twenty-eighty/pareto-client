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


decodeRequestId : Decode.Value -> Result Decode.Error RequestId
decodeRequestId value =
    Decode.decodeValue (Decode.field "requestId" Decode.int) value


decodeSendId : Decode.Value -> Result Decode.Error SendRequestId
decodeSendId value =
    Decode.decodeValue (Decode.field "sendId" Decode.int) value


decodeReason : Decode.Value -> Result Decode.Error String
decodeReason value =
    Decode.decodeValue (Decode.field "reason" Decode.string) value


decodeAuthHeaderReceived : Decode.Decoder AuthHeaderReceived
decodeAuthHeaderReceived =
    Decode.map6 AuthHeaderReceived
        (Decode.field "requestId" Decode.int)
        (Decode.maybe (Decode.field "fileId" Decode.int))
        (Decode.field "method" Decode.string)
        (Decode.field "authHeader" Decode.string)
        (Decode.field "serverUrl" Decode.string)
        (Decode.field "apiUrl" Decode.string)


type alias AuthHeaderReceived =
    { requestId : Int
    , fileId : Maybe Int
    , method : String
    , authHeader : String
    , serverUrl : String
    , apiUrl : String
    }
