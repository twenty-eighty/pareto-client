port module Ports exposing (..)

import Json.Encode as Encode
import Nostr exposing (IncomingMessage, OutgoingCommand)
import Nostr.Event exposing (Event, EventFilter, Kind(..), TagReference(..), encodeEvent, encodeEventFilter)
import Nostr.Request exposing (HttpRequestMethod(..), RequestId)
import Nostr.Send exposing (SendRequestId)


port sendCommand : OutgoingCommand -> Cmd msg

port receiveMessage : (IncomingMessage -> msg) -> Sub msg

connect : List String -> Cmd msg
connect relays =
    sendCommand { command = "connect", value = Encode.list Encode.string relays }


loginWithExtension : Cmd msg
loginWithExtension =
    sendCommand { command = "loginWithExtension", value = Encode.null }

loginSignUp : Cmd msg
loginSignUp =
    sendCommand { command = "loginSignUp", value = Encode.null }

requestUser : Cmd msg
requestUser =
    sendCommand { command = "requestUser", value = Encode.null }

requestEvents : String -> Bool -> RequestId -> Maybe (List String) -> EventFilter -> Cmd msg
requestEvents description closeOnEose requestId maybeRelays filter =
    let
        relaysValue =
            case maybeRelays of
                Just relays ->
                    Encode.list Encode.string relays

                Nothing ->
                    Encode.null
    in
    sendCommand
        { command = "requestEvents"
        , value = 
            Encode.object
                [ ("requestId", Encode.int requestId)
                , ("filter", encodeEventFilter filter)
                , ("closeOnEose", Encode.bool closeOnEose)
                , ("description", Encode.string description)
                , ("relays", relaysValue)
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

requestNip96Auth : RequestId -> String -> String -> HttpRequestMethod -> Cmd msg
requestNip96Auth requestId serverUrl apiUrl method =
    let
        methodParams =
            case method of
                GetRequest ->
                    [ ("method", Encode.string "GET") ]

                DeleteRequest fileId ->
                    [ ("method", Encode.string "DELETE")
                    , ("fileId", Encode.int fileId)
                    ]

                PostRequest fileId hash ->
                    [ ("method", Encode.string "POST")
                    , ("fileId", Encode.int fileId)
                    , ("hash", Encode.string hash)
                    ]

                PutRequest fileId hash ->
                    [ ("method", Encode.string "PUT")
                    , ("fileId", Encode.int fileId)
                    , ("hash", Encode.string hash)
                    ]

                PatchRequest fileId hash ->
                    [ ("method", Encode.string "PATCH")
                    , ("fileId", Encode.int fileId)
                    , ("hash", Encode.string hash)
                    ]

    in
    sendCommand
        { command = "requestNip96Auth"
        , value = 
            Encode.object
                ([ ("requestId", Encode.int requestId)
                , ("serverUrl", Encode.string serverUrl)
                , ("apiUrl", Encode.string apiUrl)
                ] ++ methodParams
                )
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