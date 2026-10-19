module Newsletters.ContactDatabase exposing (..)

import Effect exposing (Effect)
import Dict exposing (Dict)
import Json.Decode as Decode
import List.Extra as ListExtra
import Newsletters.Subscribers as Subscribers
import Newsletters.Types exposing (Subscriber)
import Nostr.Types exposing (IncomingMessage, PubKey)
import Ports


type alias Model =
    { subscribers : List Subscriber
    , total : Maybe Int
    , databaseTotal : Maybe Int
    , tags : List String
    , errors : List String
    , loadingFlags : List LoadingFlag
    , pubkey : PubKey
    , authenticated : Bool
    , contactsLoaded : Bool
    , contactIds : Dict String String
    , loading : Bool
    , requestId : Int
    }


pageSize : Int
pageSize =
    25


type Msg
    = ReceivedMessage IncomingMessage


init : PubKey -> String -> List LoadingFlag -> ( Model, Effect Msg )
init pubkey serverUrl loadingFlags =
    ( { subscribers = []
      , total = Nothing
      , databaseTotal = Nothing
      , errors = []
      , loadingFlags = loadingFlags
      , pubkey = pubkey
      , tags = []
      , authenticated = False
      , contactsLoaded = False
      , contactIds = Dict.empty
      , loading = False
      , requestId = 0
      }
    , initContactDatabase serverUrl pubkey
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


loadContacts : Int -> Int -> Int -> String -> Bool -> Effect Msg
loadContacts requestId page perPage sortColumn sortReversed =
    Ports.loadContacts requestId page perPage sortColumn sortReversed
        |> Effect.sendCmd


searchContacts : Int -> String -> Int -> Int -> Effect Msg
searchContacts requestId term page perPage =
    Ports.searchContacts requestId term page perPage
        |> Effect.sendCmd


filterContacts : Int -> Decode.Value -> Int -> Int -> Effect Msg
filterContacts requestId filter page perPage =
    Ports.filterContacts requestId filter page perPage
        |> Effect.sendCmd


prepareRequest : Model -> ( Model, Int )
prepareRequest model =
    let
        requestId =
            model.requestId + 1
    in
    ( { model | requestId = requestId, loading = True }, requestId )


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


addContact : Subscriber -> Effect Msg
addContact subscriber =
    Ports.addContact subscriber
        |> Effect.sendCmd


updateContact : String -> Subscriber -> Effect Msg
updateContact contactId subscriber =
    Ports.updateContact contactId subscriber
        |> Effect.sendCmd


loadContactTags : PubKey -> Cmd msg
loadContactTags pubkey =
    Ports.loadContactTags pubkey


update : Msg -> Model -> ( Model, Effect Msg )
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
            in
            ( { model | authenticated = True }
            , loadTagsEffect
            )

        "contactDatabaseError" ->
            case Decode.decodeValue (Decode.field "error" Decode.string) message.value of
                Ok error ->
                    ( { model | errors = error :: model.errors, loading = False }, Effect.none )

                Err error ->
                    ( { model | errors = ("Error receiving contact database error: " ++ Decode.errorToString error) :: model.errors }, Effect.none )

        "contacts" ->
            case Decode.decodeValue (Decode.field "requestId" Decode.int) message.value of
                Ok requestId ->
                    if requestId /= model.requestId then
                        ( model, Effect.none )

                    else
                        applyContacts model message

                Err _ ->
                    applyContacts model message

        "contactTags" ->
            case Decode.decodeValue (Decode.field "tags" (Decode.list Decode.string)) message.value of
                Ok decodedTags ->
                    ( { model | tags = sortTags decodedTags }, Effect.none )

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


applyContacts : Model -> IncomingMessage -> ( Model, Effect Msg )
applyContacts model message =
    case
        ( Decode.decodeValue (Decode.field "contacts" (Decode.list contactRecordDecoder)) message.value
        , Decode.decodeValue (Decode.oneOf [ Decode.field "errors" (Decode.list Decode.string), Decode.succeed [] ]) message.value
        , Decode.decodeValue (Decode.maybe (Decode.field "total" Decode.int)) message.value
        )
    of
        ( Ok decoded, Ok errors, Ok maybeTotal ) ->
            let
                maybeDatabaseTotal =
                    Decode.decodeValue (Decode.maybe (Decode.field "databaseTotal" Decode.int)) message.value
                        |> Result.withDefault Nothing
            in
            ( { model
                | subscribers = List.map .subscriber decoded
                , contactIds =
                    decoded
                        |> List.filterMap
                            (\record ->
                                if record.id == "" then
                                    Nothing

                                else
                                    Just ( record.subscriber.email, record.id )
                            )
                        |> Dict.fromList
                , contactsLoaded = True
                , loading = False
                , errors =
                    errors
                        ++ List.filter (\existing -> not (List.member existing errors)) model.errors
                , total =
                    case maybeTotal of
                        Just totalCount ->
                            Just totalCount

                        Nothing ->
                            model.total
                , databaseTotal =
                    case maybeDatabaseTotal of
                        Just storedCount ->
                            Just storedCount

                        Nothing ->
                            model.databaseTotal
              }
            , Effect.none
            )

        ( Err error, _, _ ) ->
            ( { model | loading = False, errors = ("Error receiving contacts: " ++ Decode.errorToString error) :: model.errors }, Effect.none )

        ( _, Err error, _ ) ->
            ( { model | loading = False, errors = ("Error receiving errors: " ++ Decode.errorToString error) :: model.errors }, Effect.none )

        ( _, _, Err error ) ->
            ( { model | loading = False, errors = ("Error receiving contact count: " ++ Decode.errorToString error) :: model.errors }, Effect.none )


addTagToList : List String -> String -> List String
addTagToList tagList tag =
    tag :: tagList
        |> sortTags
        |> ListExtra.unique


sortTags : List String -> List String
sortTags tagList =
    List.sortBy String.toLower tagList


contactRecordDecoder : Decode.Decoder { id : String, subscriber : Subscriber }
contactRecordDecoder =
    Decode.map2 (\id subscriber -> { id = id, subscriber = subscriber })
        (Decode.oneOf [ Decode.field "id" Decode.string, Decode.succeed "" ])
        Subscribers.subscriberDecoder


filterTags : String -> List String -> List String
filterTags tag tagList =
    tagList
        |> List.filter (\t -> t /= tag)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.receiveMessage ReceivedMessage
