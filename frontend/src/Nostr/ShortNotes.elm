module Nostr.ShortNotes exposing (ingest)

{-| Short text-note (kind 1) store updates. Related requests stay in `Nostr`.
-}

import Dict exposing (Dict)
import Nostr.Event exposing (Event)
import Nostr.Nip10 exposing (TextNote, textNoteFromEvent)
import Nostr.Types exposing (EventId)


ingest :
    Dict EventId TextNote
    -> List Event
    -> ( Dict EventId TextNote, List TextNote )
ingest shortTextNotes events =
    let
        notes =
            events
                |> List.map textNoteFromEvent

        updated =
            notes
                |> List.foldl
                    (\textNote acc ->
                        Dict.insert textNote.eventId textNote acc
                    )
                    shortTextNotes
    in
    ( updated, notes )
