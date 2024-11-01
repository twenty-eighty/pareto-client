port module Ports exposing (..)

import Json.Encode as Encode
import Nostr exposing (IncomingMessage, OutgoingCommand)
import Nostr.Event exposing (Event, EventFilter, Kind(..), TagReference(..), encodeEvent, encodeEventFilter)
import Nostr.Request exposing (RequestId)
import Nostr.Send exposing (SendRequestId)


port sendCommand : OutgoingCommand -> Cmd msg

port receiveMessage : (IncomingMessage -> msg) -> Sub msg

connect : List String -> Cmd msg
connect relays =
    sendCommand { command = "connect", value = Encode.list Encode.string relays }


loginWithExtension : Cmd msg
loginWithExtension =
    sendCommand { command = "loginWithExtension", value = Encode.null }

requestUser : Cmd msg
requestUser =
    sendCommand { command = "requestUser", value = Encode.null }

requestEvents : String -> Bool -> RequestId -> EventFilter -> Cmd msg
requestEvents description closeOnEose requestId filter =
    sendCommand
        { command = "requestEvents"
        , value = 
            Encode.object
                [ ("requestId", Encode.int requestId)
                , ("filter", encodeEventFilter filter)
                , ("closeOnEose", Encode.bool closeOnEose)
                , ("description", Encode.string description)
                ]
        }

requestBlossomListAuth : RequestId -> String -> Cmd msg
requestBlossomListAuth requestId server =
    sendCommand
        { command = "requestBlossomListAuth"
        , value = 
            Encode.object
                [ ("requestId", Encode.int requestId)
                , ("server", Encode.string server)
                ]
        }

requestNip96Auth : RequestId -> String -> String -> Cmd msg
requestNip96Auth requestId url method =
    sendCommand
        { command = "requestNip96Auth"
        , value = 
            Encode.object
                [ ("requestId", Encode.int requestId)
                , ("url", Encode.string url)
                , ("method", Encode.string method)
                ]
        }

sendEvent : SendRequestId -> Event -> Cmd msg
sendEvent sendRequestId event =
    sendCommand
        { command = "sendEvent"
        , value = 
            Encode.object
                [ ("sendId", Encode.int sendRequestId)
                , ("event", encodeEvent event)
                ]
        }