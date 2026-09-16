module Nostr.Reposts exposing
    ( ingest
    , forAddress
    , forEventId
    , countForAddress
    , countForEventId
    )

{-| Repost store updates and lookups.
-}

import Dict exposing (Dict)
import Nostr.Event exposing (AddressComponents, Event, buildAddress)
import Nostr.Nip18 exposing (Repost, repostFromEvent)
import Nostr.Types exposing (Address, EventId, PubKey)


type alias Store a =
    { a
        | repostsByAddress : Dict Address (Dict PubKey Repost)
        , repostsByEventId : Dict EventId (Dict PubKey Repost)
    }


ingest :
    { repostsByAddress : Dict Address (Dict PubKey Repost)
    , repostsByEventId : Dict EventId (Dict PubKey Repost)
    }
    -> List Event
    ->
        { repostsByAddress : Dict Address (Dict PubKey Repost)
        , repostsByEventId : Dict EventId (Dict PubKey Repost)
        }
ingest store events =
    let
        ( repostsByAddress, repostsByEventId ) =
            events
                |> List.map repostFromEvent
                |> List.foldl
                    (\repost ( accAddress, accEvent ) ->
                        ( insertByAddress repost accAddress
                        , insertByEventId repost accEvent
                        )
                    )
                    ( store.repostsByAddress, store.repostsByEventId )
    in
    { repostsByAddress = repostsByAddress
    , repostsByEventId = repostsByEventId
    }


forAddress : Store a -> AddressComponents -> Maybe (Dict PubKey Repost)
forAddress store addressComponents =
    Dict.get (buildAddress addressComponents) store.repostsByAddress


forEventId : Store a -> EventId -> Maybe (Dict PubKey Repost)
forEventId store eventId =
    Dict.get eventId store.repostsByEventId


countForAddress : Store a -> AddressComponents -> Maybe Int
countForAddress store addressComponents =
    forAddress store addressComponents
        |> Maybe.map Dict.size


countForEventId : Store a -> EventId -> Maybe Int
countForEventId store eventId =
    forEventId store eventId
        |> Maybe.map Dict.size


insertByAddress : Repost -> Dict Address (Dict PubKey Repost) -> Dict Address (Dict PubKey Repost)
insertByAddress repost dict =
    case repost.repostedAddress of
        Just ( addressComponents, _ ) ->
            let
                address =
                    buildAddress addressComponents
            in
            case Dict.get address dict of
                Just dictForAddress ->
                    Dict.insert address (Dict.insert repost.pubKey repost dictForAddress) dict

                Nothing ->
                    Dict.insert address (Dict.singleton repost.pubKey repost) dict

        Nothing ->
            dict


insertByEventId : Repost -> Dict EventId (Dict PubKey Repost) -> Dict EventId (Dict PubKey Repost)
insertByEventId repost dict =
    case repost.repostedEvent of
        Just ( eventId, _ ) ->
            case Dict.get eventId dict of
                Just dictForEvent ->
                    Dict.insert eventId (Dict.insert repost.pubKey repost dictForEvent) dict

                Nothing ->
                    Dict.insert eventId (Dict.singleton repost.pubKey repost) dict

        Nothing ->
            dict
