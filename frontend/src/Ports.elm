port module Ports exposing (..)

import Json.Encode as Encode
import Nostr.Event exposing (Event, EventFilter, Kind(..), TagReference(..), buildAddress, encodeEvent, encodeEventFilter)
import Nostr.Request exposing (HttpRequestMethod(..), RequestId)
import Nostr.Send exposing (SendRequestId)
import Nostr.Types exposing (IncomingMessage, OutgoingCommand)
import Pareto


port sendCommand : OutgoingCommand -> Cmd msg


port receiveMessage : (IncomingMessage -> msg) -> Sub msg


connect : List String -> Cmd msg
connect relays =
    sendCommand
        { command = "connect"
        , value =
            Encode.object
                [ ( "client", Encode.string Pareto.client )
                , ( "nip89", Encode.string <| buildAddress ( KindHandlerInformation, Pareto.paretoClientPubKey, Pareto.handlerIdentifier ) )
                , ( "relays", Encode.list Encode.string relays )
                ]
        }


login : String -> Cmd msg
login nsec =
    sendCommand { command = "login", value = Encode.object [ ( "nsec", Encode.string nsec ) ] }


loginSignUp : Cmd msg
loginSignUp =
    sendCommand { command = "loginSignUp", value = Encode.null }


signUp : Cmd msg
signUp =
    sendCommand { command = "signUp", value = Encode.null }


requestEvents : String -> Bool -> RequestId -> List String -> List EventFilter -> Cmd msg
requestEvents description closeOnEose requestId relays filters =
    sendCommand
        { command = "requestEvents"
        , value =
            Encode.object
                [ ( "requestId", Encode.int requestId )
                , ( "filters", Encode.list encodeEventFilter filters )
                , ( "closeOnEose", Encode.bool closeOnEose )
                , ( "description", Encode.string description )
                , ( "relays", Encode.list Encode.string relays )
                ]
        }


searchEvents : String -> Bool -> RequestId -> List String -> List EventFilter -> Cmd msg
searchEvents description closeOnEose requestId relays filters =
    sendCommand
        { command = "searchEvents"
        , value =
            Encode.object
                [ ( "requestId", Encode.int requestId )
                , ( "filters", Encode.list encodeEventFilter filters )
                , ( "closeOnEose", Encode.bool closeOnEose )
                , ( "description", Encode.string description )
                , ( "relays", Encode.list Encode.string relays )
                ]
        }


setTestMode : Bool -> Cmd msg
setTestMode testMode =
    sendCommand
        { command = "setTestMode"
        , value = Encode.bool testMode
        }


toggleArticleInfo : Cmd msg
toggleArticleInfo =
    sendCommand
        { command = "toggleArticleInfo"
        , value = Encode.null
        }


shareLink : { url : String, title : String, text : String } -> Cmd msg
shareLink { url, title, text } =
    sendCommand
        { command = "shareLink"
        , value = Encode.object [ ( "url", Encode.string url ), ( "title", Encode.string title ), ( "text", Encode.string text ) ]
        }


requestBlossomAuth : RequestId -> String -> String -> HttpRequestMethod -> Cmd msg
requestBlossomAuth requestId server content method =
    sendCommand
        { command = "requestBlossomAuth"
        , value =
            Encode.object
                ([ ( "requestId", Encode.int requestId )
                 , ( "serverUrl", Encode.string server )
                 , ( "content", Encode.string content )
                 ]
                    ++ httpMethodParams method
                )
        }


requestNip96Auth : RequestId -> String -> String -> HttpRequestMethod -> Cmd msg
requestNip96Auth requestId serverUrl apiUrl method =
    sendCommand
        { command = "requestNip96Auth"
        , value =
            Encode.object
                ([ ( "requestId", Encode.int requestId )
                 , ( "serverUrl", Encode.string serverUrl )
                 , ( "apiUrl", Encode.string apiUrl )
                 ]
                    ++ httpMethodParams method
                )
        }


httpMethodParams : HttpRequestMethod -> List ( String, Encode.Value )
httpMethodParams method =
    case method of
        GetRequest ->
            [ ( "method", Encode.string "GET" ) ]

        DeleteRequest fileId ->
            [ ( "method", Encode.string "DELETE" )
            , ( "fileId", Encode.int fileId )
            ]

        PostRequest fileId hash ->
            [ ( "method", Encode.string "POST" )
            , ( "fileId", Encode.int fileId )
            , ( "hash", Encode.string hash )
            ]

        PutRequest fileId hash ->
            [ ( "method", Encode.string "PUT" )
            , ( "fileId", Encode.int fileId )
            , ( "hash", Encode.string hash )
            ]

        PatchRequest fileId hash ->
            [ ( "method", Encode.string "PATCH" )
            , ( "fileId", Encode.int fileId )
            , ( "hash", Encode.string hash )
            ]


sendEvent : SendRequestId -> List String -> Event -> Cmd msg
sendEvent sendRequestId relays event =
    sendCommand
        { command = "sendEvent"
        , value =
            Encode.object
                [ ( "sendId", Encode.int sendRequestId )
                , ( "event", encodeEvent event )
                , ( "relays", Encode.list Encode.string relays )
                ]
        }


encryptString : String -> Cmd msg
encryptString data =
    sendCommand
        { command = "encryptString"
        , value =
            Encode.object
                [ ( "data", Encode.string data )
                ]
        }


downloadAndDecryptFile : String -> String -> String -> Cmd msg
downloadAndDecryptFile url keyHex ivHex =
    sendCommand
        { command = "downloadAndDecryptFile"
        , value =
            Encode.object
                [ ( "url", Encode.string url )
                , ( "key", Encode.string keyHex )
                , ( "iv", Encode.string ivHex )
                ]
        }
