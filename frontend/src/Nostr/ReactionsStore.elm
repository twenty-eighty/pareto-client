module Nostr.ReactionsStore exposing
    ( ingest
    , forAddress
    , forEventId
    , countForAddress
    , countForEventId
    , reactionForAddress
    , reactionForEventId
    )

{-| Reaction storage updates and lookups.
Named ReactionsStore to avoid clashing with `Nostr.Reactions`.
-}

import Dict exposing (Dict)
import Nostr.Event exposing (AddressComponents, Event, buildAddress)
import Nostr.Reactions exposing (Reaction, reactionFromEvent)
import Nostr.Types exposing (Address, EventId, PubKey)


type alias Store a =
    { a
        | reactionsForEventId : Dict EventId (Dict PubKey Reaction)
        , reactionsForAddress : Dict Address (Dict PubKey Reaction)
    }


ingest :
    { reactionsForEventId : Dict EventId (Dict PubKey Reaction)
    , reactionsForAddress : Dict Address (Dict PubKey Reaction)
    }
    -> List Event
    ->
        { reactionsForEventId : Dict EventId (Dict PubKey Reaction)
        , reactionsForAddress : Dict Address (Dict PubKey Reaction)
        }
ingest store events =
    let
        reactions =
            events
                |> List.map reactionFromEvent

        reactionsForEventId =
            reactions
                |> List.foldl insertByEventId store.reactionsForEventId

        reactionsForAddress =
            reactions
                |> List.foldl insertByAddress store.reactionsForAddress
    in
    { reactionsForEventId = reactionsForEventId
    , reactionsForAddress = reactionsForAddress
    }


forAddress : Store a -> AddressComponents -> Maybe (Dict PubKey Reaction)
forAddress store addressComponents =
    Dict.get (buildAddress addressComponents) store.reactionsForAddress


forEventId : Store a -> EventId -> Maybe (Dict PubKey Reaction)
forEventId store eventId =
    Dict.get eventId store.reactionsForEventId


countForAddress : Store a -> AddressComponents -> Maybe Int
countForAddress store addressComponents =
    forAddress store addressComponents
        |> Maybe.map Dict.size


countForEventId : Store a -> EventId -> Maybe Int
countForEventId store eventId =
    forEventId store eventId
        |> Maybe.map Dict.size


reactionForAddress : Store a -> PubKey -> AddressComponents -> Maybe Reaction
reactionForAddress store pubKey addressComponents =
    forAddress store addressComponents
        |> Maybe.andThen (Dict.get pubKey)


reactionForEventId : Store a -> PubKey -> EventId -> Maybe Reaction
reactionForEventId store pubKey eventId =
    forEventId store eventId
        |> Maybe.andThen (Dict.get pubKey)


insertByEventId : Reaction -> Dict EventId (Dict PubKey Reaction) -> Dict EventId (Dict PubKey Reaction)
insertByEventId reaction acc =
    case reaction.noteIdReactedTo of
        Just noteId ->
            case Dict.get noteId acc of
                Just dict ->
                    Dict.insert noteId (Dict.insert reaction.pubKey reaction dict) acc

                Nothing ->
                    Dict.insert noteId (Dict.singleton reaction.pubKey reaction) acc

        _ ->
            acc


insertByAddress : Reaction -> Dict Address (Dict PubKey Reaction) -> Dict Address (Dict PubKey Reaction)
insertByAddress reaction acc =
    case reaction.addressComponentsReactedTo of
        Just addressComponents ->
            let
                address =
                    buildAddress addressComponents
            in
            case Dict.get address acc of
                Just dict ->
                    Dict.insert address (Dict.insert reaction.pubKey reaction dict) acc

                Nothing ->
                    Dict.insert address (Dict.singleton reaction.pubKey reaction) acc

        _ ->
            acc
