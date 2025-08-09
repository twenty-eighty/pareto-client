module Nostr.MarketplaceService exposing (..)

import Dict exposing (Dict)
import Nostr.Event exposing (ImageMetadata, Kind, Tag)
import Nostr.Nip19 as Nip19
import Nostr.Types exposing (EventId, PubKey, RelayUrl)
import Route.Path exposing (Path(..))
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
    , title : Maybe String
    , identifier : Maybe String
    , hashtags : List String
    , images : Dict String ImageMetadata
    , pricing : Pricing
    , amount : Int
    , state : ServiceState
    , orders : List Nip19.NIP19Type
    , zapWeights : List ( PubKey, RelayUrl, Int )
    , otherTags : List Tag
    , relays : Set String
    }
