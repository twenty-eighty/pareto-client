module Newsletters.ContactDatabase exposing (..)

import Effect exposing (Effect)
import Http
import Json.Decode as Decode
import Newsletters.Types exposing (Subscriber)
import Nostr
import Nostr.External exposing (decodeAuthHeaderReceived)
import Nostr.Shared exposing (httpErrorToString)
import Nostr.Request exposing (RequestData(..), HttpRequestMethod(..))
import Nostr.Types exposing (IncomingMessage, PubKey)
import Ports
import Shared.Msg
import Newsletters.Subscribers as Subscribers

type alias Model =  
    { subscribers : List Subscriber
    , errors : List String
    , pubkey : PubKey
    }

type alias JwtTokenString = String

type Msg
    = ReceivedMessage IncomingMessage



contactDatabaseServerUrl : String
contactDatabaseServerUrl =
    "http://localhost:4003"

init : PubKey -> (Model, Effect Msg)
init pubkey =
    ( { subscribers = []
      , errors = []
      , pubkey = pubkey
      }
    , initContactDatabase contactDatabaseServerUrl pubkey
    )

initContactDatabase : String -> PubKey -> Effect Msg
initContactDatabase url pubkey =
    Ports.initContactDatabase url pubkey
        |> Effect.sendCmd


loadContacts : Int -> Int -> Effect Msg
loadContacts page perPage =
    Ports.loadContacts page perPage
        |> Effect.sendCmd


storeSubscribers : List Subscriber -> Effect Msg
storeSubscribers subscribers =
    Ports.storeContacts subscribers
        |> Effect.sendCmd


update : Nostr.Model -> Msg -> Model -> (Model, Effect Msg)
update nostr msg model =
    case msg of
        ReceivedMessage message ->
            updateWithMessage model message


updateWithMessage : Model -> IncomingMessage -> ( Model, Effect Msg )
updateWithMessage model message =
    case message.messageType of
        "contacts" ->
            case ( Decode.decodeValue (Decode.field "contacts" Subscribers.subscribersDecoder) message.value
                 , Decode.decodeValue (Decode.field "errors" (Decode.list Decode.string)) message.value ) of
                ( Ok decoded, Ok errors ) ->
                    ( { model | subscribers = decoded, errors = errors }
                    , Effect.none
                    )

                ( Err error, _ ) ->
                    ( { model | errors = ("Error receiving contacts: " ++ Decode.errorToString error) :: model.errors }, Effect.none )
                ( _, Err error ) ->
                    ( { model | errors = ("Error receiving errors: " ++ Decode.errorToString error) :: model.errors }, Effect.none )

        _ ->
            ( model, Effect.none )

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.receiveMessage ReceivedMessage
