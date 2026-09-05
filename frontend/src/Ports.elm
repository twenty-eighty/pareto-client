port module Ports exposing (..)

import Json.Encode as Encode
import Nostr.Event exposing (Event, EventFilter, Kind(..), TagReference(..), buildAddress, encodeEvent, encodeEventFilter)
import Nostr.Request exposing (HttpRequestMethod(..), RequestId)
import Nostr.Send exposing (SendRequestId)
import Nostr.Types exposing (IncomingMessage, OutgoingCommand, PubKey)
import Newsletters.Types exposing (Subscriber, encodeSubscribers)
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


listIdentities : Cmd msg
listIdentities =
    sendCommand { command = "listIdentities", value = Encode.null }


loginWithExtension : Cmd msg
loginWithExtension =
    sendCommand { command = "loginWithExtension", value = Encode.null }


loginWithNpub : String -> Cmd msg
loginWithNpub npub =
    sendCommand
        { command = "loginWithNpub"
        , value = Encode.object [ ( "npub", Encode.string npub ) ]
        }


loginWithBunker : String -> Cmd msg
loginWithBunker bunkerUri =
    sendCommand
        { command = "loginWithBunker"
        , value = Encode.object [ ( "bunkerUri", Encode.string bunkerUri ) ]
        }


loginWithNcryptsec : String -> String -> Cmd msg
loginWithNcryptsec ncryptsec password =
    sendCommand
        { command = "loginWithNcryptsec"
        , value =
            Encode.object
                [ ( "ncryptsec", Encode.string ncryptsec )
                , ( "password", Encode.string password )
                ]
        }


generateEncryptedKey : String -> Cmd msg
generateEncryptedKey password =
    sendCommand
        { command = "generateEncryptedKey"
        , value = Encode.object [ ( "password", Encode.string password ) ]
        }


unlockEmailAccount :
    { email : String
    , password : String
    , ncryptsec : String
    , publicKeyHint : String
    , displayName : Maybe String
    }
    -> Cmd msg
unlockEmailAccount params =
    sendCommand
        { command = "unlockEmailAccount"
        , value =
            Encode.object
                [ ( "email", Encode.string params.email )
                , ( "password", Encode.string params.password )
                , ( "ncryptsec", Encode.string params.ncryptsec )
                , ( "publicKeyHint", Encode.string params.publicKeyHint )
                , ( "displayName"
                  , params.displayName
                        |> Maybe.map Encode.string
                        |> Maybe.withDefault Encode.null
                  )
                ]
        }


loginWithPasskey : Cmd msg
loginWithPasskey =
    sendCommand { command = "loginWithPasskey", value = Encode.null }


createPasskey : Maybe String -> Cmd msg
createPasskey maybeDisplayName =
    sendCommand
        { command = "createPasskey"
        , value =
            Encode.object
                [ ( "displayName"
                  , maybeDisplayName
                        |> Maybe.map Encode.string
                        |> Maybe.withDefault Encode.null
                  )
                ]
        }


dismissPasskeyPrompt : String -> Cmd msg
dismissPasskeyPrompt pubKey =
    sendCommand
        { command = "dismissPasskeyPrompt"
        , value = Encode.object [ ( "pubKey", Encode.string pubKey ) ]
        }


checkPasskeySupport : Cmd msg
checkPasskeySupport =
    sendCommand { command = "checkPasskeySupport", value = Encode.null }


markBootstrapDone : String -> Cmd msg
markBootstrapDone pubKey =
    sendCommand
        { command = "markBootstrapDone"
        , value = Encode.object [ ( "pubKey", Encode.string pubKey ) ]
        }


activateIdentity : String -> Maybe String -> Cmd msg
activateIdentity id maybePassword =
    sendCommand
        { command = "activateIdentity"
        , value =
            Encode.object
                [ ( "id", Encode.string id )
                , ( "password"
                  , maybePassword
                        |> Maybe.map Encode.string
                        |> Maybe.withDefault Encode.null
                  )
                ]
        }


removeIdentity : String -> Cmd msg
removeIdentity id =
    sendCommand
        { command = "removeIdentity"
        , value = Encode.object [ ( "id", Encode.string id ) ]
        }


logout : Cmd msg
logout =
    sendCommand { command = "logout", value = Encode.null }


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


requestNip96Auth : RequestId -> String -> String -> String -> HttpRequestMethod -> Cmd msg
requestNip96Auth requestId serverUrl apiUrl content method =
    sendCommand
        { command = "requestNip96Auth"
        , value =
            Encode.object
                ([ ( "requestId", Encode.int requestId )
                 , ( "serverUrl", Encode.string serverUrl )
                 , ( "apiUrl", Encode.string apiUrl )
                 , ( "content", Encode.string content )
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


{-| Sign an event without publishing. Reply arrives as messageType "signedEvent".
-}
signEvent : Int -> Event -> Cmd msg
signEvent requestId event =
    sendCommand
        { command = "signEvent"
        , value =
            Encode.object
                [ ( "requestId", Encode.int requestId )
                , ( "event", encodeEvent event )
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

-- CONTACTS

initContactDatabase : String -> PubKey -> Cmd msg
initContactDatabase url pubkey =
    sendCommand
        { command = "initContactDatabase"
        , value = Encode.object
            [ ( "url", Encode.string url )
            , ( "pubkey", Encode.string pubkey )
            ]
        }


loadContacts : Int -> Int -> Cmd msg
loadContacts page perPage =
    sendCommand
        { command = "loadContacts"
        , value = Encode.object
            [ ( "page", Encode.int page )
            , ( "perPage", Encode.int perPage )
            ]
        }


storeContacts : List Subscriber -> Cmd msg
storeContacts subscribers =
    sendCommand
        { command = "storeContacts"
        , value = Encode.object
            [ ( "subscribers", encodeSubscribers subscribers )
            ]
        }


loadContactTags : PubKey -> Cmd msg
loadContactTags pubkey =
    sendCommand
        { command = "loadContactTags"
        , value = Encode.object
            [ ( "pubkey", Encode.string pubkey )
            ]
        }


addContactTag : String -> Cmd msg
addContactTag tag =
    sendCommand
        { command = "addContactTag"
        , value = Encode.object
            [ ( "tag", Encode.string tag )
            ]
        }


deleteContactTag : String -> Cmd msg
deleteContactTag tag =
    sendCommand
        { command = "deleteContactTag"
        , value = Encode.object
            [ ( "tag", Encode.string tag )
            ]
        }


-- NEWSLETTERS

type alias NewsletterData =
    { author : PubKey
    , authorName : String
    , title : String
    , summary : String
    , content : String
    , imageUrl : String
    , language : Maybe String
    , identifier : String
    , test : Bool
    }


cancelNewsletter : Cmd msg
cancelNewsletter =
    sendCommand
        { command = "cancelNewsletter"
        , value = Encode.null
        }


sendNewsletter : NewsletterData -> Maybe { url : String, keyHex : String, ivHex : String } -> Cmd msg
sendNewsletter newsletterData maybeBlob =
    sendCommand
        { command = "sendNewsletter"
        , value = Encode.object
            [ ( "author", Encode.string newsletterData.author )
            , ( "newsletterData", encodeNewsletterData newsletterData )
            , ( "subscriberBlob"
              , maybeBlob
                    |> Maybe.map encodeSubscriberBlob
                    |> Maybe.withDefault Encode.null
              )
            ]
        }

sendNewsletterTest : String -> NewsletterData -> Cmd msg
sendNewsletterTest email newsletterData =
    sendCommand
        { command = "sendNewsletterTest"
        , value = Encode.object
            [ ( "email", Encode.string email )
            , ( "author", Encode.string newsletterData.author )
            , ( "newsletterData", encodeNewsletterData newsletterData )
            ]
        }


getNewsletterStatus : String -> String -> Cmd msg
getNewsletterStatus author identifier =
    sendCommand
        { command = "getNewsletterStatus"
        , value = Encode.object
            [ ( "author", Encode.string author )
            , ( "identifier", Encode.string identifier )
            ]
        }


getNewsletterRecipientCount : String -> Maybe { url : String, keyHex : String, ivHex : String } -> Cmd msg
getNewsletterRecipientCount author maybeBlob =
    sendCommand
        { command = "getNewsletterRecipientCount"
        , value = Encode.object
            [ ( "author", Encode.string author )
            , ( "subscriberBlob"
              , maybeBlob
                    |> Maybe.map encodeSubscriberBlob
                    |> Maybe.withDefault Encode.null
              )
            ]
        }

encodeSubscriberBlob : { url : String, keyHex : String, ivHex : String } -> Encode.Value
encodeSubscriberBlob blob =
    Encode.object
        [ ( "url", Encode.string blob.url )
        , ( "key", Encode.string blob.keyHex )
        , ( "iv", Encode.string blob.ivHex )
        ]


encodeNewsletterData : NewsletterData -> Encode.Value
encodeNewsletterData newsletterData =
    Encode.object
        [ ( "title", Encode.string newsletterData.title )
        , ( "summary", Encode.string newsletterData.summary )
        , ( "content", Encode.string newsletterData.content )
        , ( "imageUrl", Encode.string newsletterData.imageUrl )
        , ( "language", Encode.string <| Maybe.withDefault "" newsletterData.language )
        , ( "identifier", Encode.string <| newsletterData.identifier )
        , ( "authorName", Encode.string newsletterData.authorName )
        , ( "test", Encode.bool newsletterData.test )
        ]
