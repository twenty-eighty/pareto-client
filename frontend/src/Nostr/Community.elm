module Nostr.Community exposing (..)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, maybe, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Nostr.Event exposing (Event, EventFilter, ImageSize, Kind(..), Tag(..), TagReference(..), emptyEventFilter, imageSizeDecoder)
import Nostr.Profile exposing (ProfileValidation(..))
import Nostr.Types exposing (PubKey, RelayUrl)
import Time exposing (Month(..))



-- NIP-72
-- Types


type alias Moderator =
    { pubKey : String
    , relay : RelayUrl
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
    , relays : List Relay
    }


communityDefinitionFromEvent : Event -> Community
communityDefinitionFromEvent event =
    event.tags
        |> List.foldl
            (\tag acc ->
                case tag of
                    EventDelegationTag dIdentifier ->
                        { acc | dtag = Just dIdentifier }

                    NameTag name ->
                        { acc | name = Just name }

                    DescriptionTag description ->
                        { acc | description = Just description }

                    ImageTag url size ->
                        { acc | image = Just { url = url, size = size } }

                    _ ->
                        acc
            )
            (emptyCommunity event.pubKey)


ingest : Dict PubKey (List Community) -> List Event -> Dict PubKey (List Community)
ingest dict events =
    events
        |> List.map communityDefinitionFromEvent
        |> List.foldl
            (\communityDefinition acc ->
                Dict.insert communityDefinition.pubKey [ communityDefinition ] acc
            )
            dict


emptyCommunity : PubKey -> Community
emptyCommunity pubKey =
    { dtag = Nothing
    , pubKey = pubKey
    , name = Nothing
    , description = Nothing
    , image = Nothing
    , moderators = []
    , relays = []
    }


communityMatchesFilter : EventFilter -> Community -> Bool
communityMatchesFilter _ _ =
    False


communityName : Community -> String
communityName community =
    case community.name of
        Just name ->
            name

        Nothing ->
            Maybe.withDefault "" community.dtag


postApprovalFilter : Community -> EventFilter
postApprovalFilter community =
    { emptyEventFilter
        | authors = Just [ community.pubKey ]
        , kinds = Just [ KindCommunityPostApproval ]
        , tagReferences = Just [ TagReferenceCode ( KindCommunityDefinition, community.pubKey, Maybe.withDefault "" community.dtag ) ]
    }



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


imageDecoder : Decoder Image
imageDecoder =
    Decode.succeed Image
        |> required "url" string
        |> optional "resolution" (maybe imageSizeDecoder) Nothing
