module Nostr.MarketplaceService exposing (..)

--import Route.Path exposing (Path(..))

import Nostr.Event exposing (Event, Kind, Tag(..))
import Nostr.Nip19 as Nip19
import Nostr.Shared
import Nostr.Types exposing (EventId, PubKey, RelayUrl)
import Set exposing (Set)
import Time


type Pricing
    = Sats
    | SatsPerHour


type ServiceState
    = Active
    | Inactive


type alias MarketplaceService =
    { author : PubKey
    , id : EventId
    , kind : Kind
    , content : String
    , createdAt : Time.Posix
    , title : String
    , identifier : String
    , hashtags : List String
    , images : List String
    , pricing : Pricing
    , amount : Int
    , state : ServiceState
    , orders : List Nip19.NIP19Type
    , zapWeights : List ( PubKey, RelayUrl, Int )
    , otherTags : List Tag
    , relays : Set String
    }


emptyMarketplaceService : PubKey -> EventId -> Kind -> Time.Posix -> String -> List RelayUrl -> MarketplaceService
emptyMarketplaceService author eventId kind createdAt content relayUrls =
    { author = author
    , id = eventId
    , kind = kind
    , content = content
    , createdAt = createdAt
    , title = ""
    , identifier = ""
    , hashtags = []
    , images = []
    , pricing = Sats
    , amount = 0
    , state = Inactive
    , orders = []
    , zapWeights = []
    , otherTags = []
    , relays = Set.fromList relayUrls
    }


marketplaceServiceFromEvent : Event -> Result (List String) MarketplaceService
marketplaceServiceFromEvent event =
    let
        marketplaceWithoutTags =
            emptyMarketplaceService event.pubKey event.id event.kind event.createdAt event.content (Maybe.withDefault [] event.relays)

        ( builtMarketplaceService, buildingErrors ) =
            event.tags
                |> List.foldl
                    (\tag ( service, errors ) ->
                        case tag of
                            EventDelegationTag id ->
                                ( { service | identifier = id }, errors )

                            HashTag hashtag ->
                                ( { service | hashtags = service.hashtags ++ [ hashtag ] }, errors )

                            ImageTag image _ ->
                                -- HTTP images make the client appear unsafe
                                -- all images should be served with HTTPS in 2024
                                if image /= "" then
                                    ( { service | images = service.images ++ [ Nostr.Shared.ensureHttps image ] }, errors )

                                else
                                    ( service, errors )

                            TitleTag title ->
                                ( { service | title = title }, errors )

                            AmountTag amountS ->
                                ( { service | amount = String.toInt amountS |> Maybe.withDefault 0 }, errors )

                            ZapTag pubKey relayUrl maybeWeight ->
                                ( { service | zapWeights = service.zapWeights ++ [ ( pubKey, relayUrl, Maybe.withDefault 0 maybeWeight ) ] }, errors )

                            _ ->
                                ( { service | otherTags = service.otherTags ++ [ tag ] }, errors )
                    )
                    ( marketplaceWithoutTags, [] )
    in
    case ( builtMarketplaceService, buildingErrors ) of
        ( marketplaceService, [] ) ->
            Ok marketplaceService

        ( _, errors ) ->
            Err errors
