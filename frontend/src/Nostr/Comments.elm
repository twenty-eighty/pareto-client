module Nostr.Comments exposing (ingest)

{-| Comment storage updates. Orchestration (follow-up requests) stays in `Nostr`.
-}

import Dict exposing (Dict)
import Nostr.Event exposing (Event, buildAddress)
import Nostr.Nip22 as Nip22 exposing (CommentType, commentEventId, commentFromEvent, commentRootAddress)
import Nostr.Types exposing (Address, EventId)


ingest :
    Dict Address (Dict EventId CommentType)
    -> List Event
    -> ( Dict Address (Dict EventId CommentType), List CommentType )
ingest commentsByAddress events =
    let
        comments =
            events
                |> List.filterMap commentFromEvent

        updated =
            comments
                |> List.foldl insertComment commentsByAddress
    in
    ( updated, comments )


insertComment : CommentType -> Dict Address (Dict EventId CommentType) -> Dict Address (Dict EventId CommentType)
insertComment comment dict =
    let
        address =
            commentRootAddress comment
                |> buildAddress

        eventId =
            commentEventId comment
    in
    case Dict.get address dict of
        Just dictForAddress ->
            Dict.insert address (Dict.insert eventId comment dictForAddress) dict

        Nothing ->
            Dict.insert address (Dict.singleton eventId comment) dict
