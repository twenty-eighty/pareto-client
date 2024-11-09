module Nostr.Community exposing (..)

import BrowserEnv exposing (BrowserEnv)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, maybe, string, succeed, list, nullable)
import Json.Decode.Pipeline exposing (required, optional)
import Json.Decode exposing (fail)
import Nostr.Event exposing (Event, EventFilter, Kind, Tag(..))
import Nostr.Profile exposing (Profile, ProfileValidation(..), profileDisplayName)
import Nostr.Types exposing (EventId, PubKey)
import Time exposing (Month(..))

-- Types

type alias Moderator =
    { pubKey : String
    , relay : String
    , role : String
    }


type alias Image =
    { url : String
    , resolution : Maybe ImageSize
    }

type ImageSize
    = ImageSize Int Int

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
    { dtag : String
    , pubKey : PubKey
    , name : Maybe String
    , description : Maybe String
    , image : Maybe Image
    , moderators : List Moderator
    , relay : String -- the relay this event was loaded from
    , relays : List Relay
    }

communityDefinitionFromEvent : Event -> Community
communityDefinitionFromEvent event =
    event.tags
    |> List.foldl (\tag acc ->
        case tag of 
            DescriptionTag description ->
                {acc | description = Just description }

            _ ->
                acc
            ) (emptyCommunity "event.dtag" event.pubKey)


emptyCommunity : String -> PubKey -> Community
emptyCommunity dtag pubKey =
    { dtag = dtag
    , pubKey = pubKey
    , name = Nothing
    , description = Nothing
    , image = Nothing
    , moderators = []
    , relay = "wss"
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
            community.dtag

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

imageResolutionDecoder : Decoder ImageSize
imageResolutionDecoder =
    string
        |> Decode.andThen
            (\sizeString ->
                case String.split "x" sizeString of
                    [ widthString, heightString ] ->
                        case ( String.toInt widthString, String.toInt heightString ) of
                            ( Just width, Just height ) ->
                                succeed <| ImageSize width height

                            _ ->
                                fail <| "Invalid numbers in image size: " ++ sizeString

                    _ ->
                        fail <| "Invalid image size format: " ++ sizeString
            )


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
        |> optional "resolution" (maybe imageResolutionDecoder) Nothing


communityDecoder : Decoder Community
communityDecoder =
    Decode.succeed Community
        |> required "dtag" string
        |> required "pubkey" string
        |> required "name" (nullable string)
        |> optional "description" (maybe string) Nothing
        |> optional "image" (maybe imageDecoder) Nothing
        |> required "moderators" (list moderatorDecoder)
        |> required "relay" string
        |> required "relays" (list relayDecoder)

