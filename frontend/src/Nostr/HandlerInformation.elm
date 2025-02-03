module Nostr.HandlerInformation exposing (..)

import Nostr.Event as Event exposing (Event, Kind(..))
import Nostr.Profile exposing (Profile, profileToJson)
import Nostr.Types exposing (PubKey, RelayUrl)
import Time


type alias HandlerInformation =
    { alt : String
    , handlerIdentifier : String
    , hashtags : List String
    , kinds : List Kind
    , pubKey : PubKey
    , profile : Profile
    , references : List ( String, Maybe String )
    , time : Time.Posix
    , webTargets : List WebTarget
    , zapTargets : List ZapTarget
    }


type alias WebTarget =
    ( String, Maybe String )


type alias ZapTarget =
    ( PubKey, RelayUrl, Maybe Int )


buildHandlerInformation : HandlerInformation -> Event
buildHandlerInformation info =
    { pubKey = info.pubKey
    , createdAt = info.time
    , kind = KindHandlerInformation
    , tags =
        []
            |> Event.addDTag info.handlerIdentifier
            |> Event.addPublishedAtTag info.time
            |> Event.addHashtagListToTags info.hashtags
            |> Event.addAltTag info.alt
            |> Event.addReferenceTags info.references
            |> Event.addKindTags info.kinds
            |> Event.addWebTargetTags info.webTargets
            |> Event.addZapTags info.zapTargets
    , content = profileToJson info.profile
    , id = ""
    , sig = Nothing
    , relays = Nothing
    }



--      [ EventDelegationTag handlerInformation.handlerIdentifier
--      , KindTag KindLongFormContent
--      , GenericTag4 "a" (buildAddress (KindLongFormContent, handlerInformation.pubKey, handlerInformation.handlerIdentifier)) Pareto.paretoRelay "web"
--      , PublishedAtTag time
