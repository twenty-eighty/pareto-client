module Newsletters.ContactDatabase exposing (..)

import Effect exposing (Effect)
import Http
import Json.Decode as Decode
import List.Extra as ListExtra
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
    , tags : List String
    , errors : List String
    , loadingFlags : List LoadingFlag
    , pubkey : PubKey
    }

type alias JwtTokenString = String

type Msg
    = ReceivedMessage IncomingMessage



contactDatabaseServerUrl : String
contactDatabaseServerUrl =
    "http://localhost:4003"


init : PubKey -> List LoadingFlag -> (Model, Effect Msg)
init pubkey loadingFlags =
    ( { subscribers = []
      , errors = []
      , loadingFlags = loadingFlags
      , pubkey = pubkey
      , tags = []
      }
    , initContactDatabase contactDatabaseServerUrl pubkey
    )

type LoadingFlag
    = LoadTags
    | LoadContacts

tags : Model -> List String
tags model =
    model.tags


initContactDatabase : String -> PubKey -> Effect Msg
initContactDatabase url pubkey =
    Ports.initContactDatabase url pubkey
        |> Effect.sendCmd


loadContacts : Int -> Int -> Effect Msg
loadContacts page perPage =
    Ports.loadContacts page perPage
        |> Effect.sendCmd


addTag : String -> Effect Msg
addTag tag =
    Ports.addContactTag tag
        |> Effect.sendCmd


deleteTag : String -> Effect Msg
deleteTag tag =
    Ports.deleteContactTag tag
        |> Effect.sendCmd


storeSubscribers : List Subscriber -> Effect Msg
storeSubscribers subscribers =
    Ports.storeContacts subscribers
        |> Effect.sendCmd


loadContactTags : PubKey -> Cmd msg
loadContactTags pubkey =
    Ports.loadContactTags pubkey


update : Msg -> Model -> (Model, Effect Msg)
update msg model =
    case msg of
        ReceivedMessage message ->
            updateWithMessage model message


updateWithMessage : Model -> IncomingMessage -> ( Model, Effect Msg )
updateWithMessage model message =
    case message.messageType of
        "contactDatabaseAuthenticated" ->
            let
                loadTagsEffect =
                    if List.member LoadTags model.loadingFlags then
                        loadContactTags model.pubkey
                            |> Effect.sendCmd
                    else
                        Effect.none

                loadContactsEffect =
                    if List.member LoadContacts model.loadingFlags then
                        loadContacts 1 100
                    else
                        Effect.none
            in
            ( model |> Debug.log "contactDatabaseAuthenticated"
            , Effect.batch [ loadTagsEffect, loadContactsEffect ]
            )

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

        "contactTags" ->
            case Decode.decodeValue (Decode.field "tags" (Decode.list Decode.string)) message.value of
                Ok decodedTags ->
                    ( { model | tags = decodedTags }, Effect.none )

                Err error ->
                    ( { model | errors = ("Error receiving tags: " ++ Decode.errorToString error) :: model.errors }, Effect.none )

        "contactTagAdded" ->
            case Decode.decodeValue (Decode.field "tag" Decode.string) message.value of
                Ok decoded ->
                    ( { model | tags = addTagToList model.tags decoded }, Effect.none )

                Err error ->
                    ( { model | errors = ("Error receiving tags: " ++ Decode.errorToString error) :: model.errors }, Effect.none )

        "contactTagDeleted" ->
            case Decode.decodeValue (Decode.field "tag" Decode.string) message.value of
                Ok decoded ->
                    ( { model | tags = filterTags decoded model.tags }, Effect.none )

                Err error ->
                    ( { model | errors = ("Error receiving tags: " ++ Decode.errorToString error) :: model.errors }, Effect.none )

        _ ->
            ( model, Effect.none )


-- HELPERS

addTagToList : List String -> String -> List String
addTagToList tagList tag =
    tag :: tagList
        |> List.sort
        |> ListExtra.unique


filterTags : String -> List String -> List String
filterTags tag tagList =
    tagList
        |> List.filter (\t -> t /= tag)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.receiveMessage ReceivedMessage
