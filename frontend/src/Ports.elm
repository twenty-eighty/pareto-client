port module Ports exposing (..)

import Json.Encode as Encode
import Nostr.Types exposing (IncomingMessage, OutgoingCommand)
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

requestEvents : String -> Bool -> RequestId -> List String -> EventFilter -> Cmd msg
requestEvents description closeOnEose requestId relays filter =
    sendCommand
        { command = "requestEvents"
        , value = 
            Encode.object
                [ ("requestId", Encode.int requestId)
                , ("filter", encodeEventFilter filter)
                , ("closeOnEose", Encode.bool closeOnEose)
                , ("description", Encode.string description)
                , ("relays", Encode.list Encode.string relays)
                ]
        }

searchEvents : String -> Bool -> RequestId -> List String -> List EventFilter -> Cmd msg
searchEvents description closeOnEose requestId relays filters =
    sendCommand
        { command = "searchEvents"
        , value = 
            Encode.object
                [ ("requestId", Encode.int requestId)
                , ("filters", Encode.list encodeEventFilter filters)
                , ("closeOnEose", Encode.bool closeOnEose)
                , ("description", Encode.string description)
                , ("relays", Encode.list Encode.string relays)
                ]
        }

requestBlossomAuth : RequestId -> String -> String -> HttpRequestMethod -> Cmd msg
requestBlossomAuth requestId server content method =
    sendCommand
        { command = "requestBlossomAuth"
        , value = 
            Encode.object
                ([ ("requestId", Encode.int requestId)
                , ("serverUrl", Encode.string server)
                , ("content", Encode.string content)
                ] ++ httpMethodParams method
                )
        }

requestNip96Auth : RequestId -> String -> String -> HttpRequestMethod -> Cmd msg
requestNip96Auth requestId serverUrl apiUrl method =
    sendCommand
        { command = "requestNip96Auth"
        , value = 
            Encode.object
                ([ ("requestId", Encode.int requestId)
                , ("serverUrl", Encode.string serverUrl)
                , ("apiUrl", Encode.string apiUrl)
                ] ++ httpMethodParams method
                )
        }

httpMethodParams : HttpRequestMethod -> List (String, Encode.Value)
httpMethodParams method =
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


sendEvent : SendRequestId -> List String -> Event -> Cmd msg
sendEvent sendRequestId relays event =
    sendCommand
        { command = "sendEvent"
        , value = 
            Encode.object
                [ ("sendId", Encode.int sendRequestId)
                , ("event", encodeEvent event)
                , ("relays", Encode.list Encode.string relays)
                ]
        }