module Nostr.Community exposing (..)

import BrowserEnv exposing (BrowserEnv)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, maybe, string, succeed, list, nullable)
import Json.Decode.Pipeline exposing (required, optional)
import Json.Decode exposing (fail)
import Nostr.Event exposing (Event, EventFilter, ImageSize, Kind, Tag(..), imageSizeDecoder)
import Nostr.Profile exposing (Profile, ProfileValidation(..))
import Nostr.Types exposing (EventId, PubKey)
import Time exposing (Month(..))

-- NIP-72

-- Types

type alias Moderator =
    { pubKey : String
    , relay : String
    , role : String
    }


type alias Image =
    { url : String
    , size : Maybe ImageSize
    }

type alias Relay =
    { url : String
    , relayType : RelayType
    }


type RelayType
    = RelayTypeAuthor
    | RelayTypeRequests
    | RelayTypeApprovals
    | RelayTypeUnknown
    | RelayTypeGeneric


type alias Community =
    { dtag : Maybe String
    , pubKey : PubKey
    , name : Maybe String
    , description : Maybe String
    , image : Maybe Image
    , moderators : List Moderator
    , relay : Maybe String -- the relay this event was loaded from
    , relays : List Relay
    }

communityDefinitionFromEvent : Event -> Community
communityDefinitionFromEvent event =
    event.tags
    |> List.foldl (\tag acc ->
        case tag of 
            EventDelegationTag dIdentifier ->
                {acc | dtag = Just dIdentifier }

            NameTag name ->
                {acc | name = Just name }

            DescriptionTag description ->
                {acc | description = Just description }

            ImageTag url size ->
                {acc | image = Just { url = url, size = size } }

            _ ->
                acc
            ) (emptyCommunity event.pubKey event.relay )


emptyCommunity : PubKey -> Maybe String -> Community
emptyCommunity pubKey relay =
    { dtag = Nothing
    , pubKey = pubKey
    , name = Nothing
    , description = Nothing
    , image = Nothing
    , moderators = []
    , relay = relay
    , relays = []
    }

communityMatchesFilter : EventFilter -> Community -> Bool
communityMatchesFilter filter community =
    True

communityName : Community -> String
communityName community =
    case community.name of
        Just name ->
            name

        Nothing ->
            Maybe.withDefault "" community.dtag

-- Decoders

relayTypeDecoder : Decoder RelayType
relayTypeDecoder =
    Decode.oneOf
        [ string
            |> Decode.andThen
                (\relayTypeString ->
                    case relayTypeString of
                        "author" ->
                            succeed RelayTypeAuthor

                        "requests" ->
                            succeed RelayTypeRequests

                        "approvals" ->
                            succeed RelayTypeApprovals

                        _ ->
                            succeed RelayTypeUnknown
                )
        , succeed RelayTypeGeneric
        ]

relayDecoder : Decoder Relay
relayDecoder =
    Decode.succeed Relay
        |> required "url" string
        |> optional "type" relayTypeDecoder RelayTypeGeneric


moderatorDecoder : Decoder Moderator
moderatorDecoder =
    Decode.succeed Moderator
        |> required "pubkey" string
        |> required "relay" string
        |> required "role" string


imageDecoder : Decoder Image
imageDecoder =
    Decode.succeed Image
        |> required "url" string
        |> optional "resolution" (maybe imageSizeDecoder) Nothing


communityDecoder : Decoder Community
communityDecoder =
    Decode.succeed Community
        |> optional "dtag" (maybe string) Nothing
        |> required "pubkey" string
        |> required "name" (nullable string)
        |> optional "description" (maybe string) Nothing
        |> optional "image" (maybe imageDecoder) Nothing
        |> required "moderators" (list moderatorDecoder)
        |> optional "relay" (maybe string) Nothing
        |> required "relays" (list relayDecoder)

