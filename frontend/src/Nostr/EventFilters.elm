module Nostr.EventFilters exposing
    ( forAuthors
    , forReactions
    , forDeletionRequests
    )

{-| Common event filters used when requesting related Nostr kinds.
-}

import Nostr.Event exposing (EventFilter, Kind(..), TagReference, emptyEventFilter)
import Nostr.Types exposing (PubKey)


forAuthors : List PubKey -> Maybe EventFilter
forAuthors authors =
    if List.isEmpty authors then
        Nothing

    else
        Just
            { emptyEventFilter
                | authors = Just authors
                , kinds = Just [ KindUserMetadata ]
            }


forDeletionRequests : List TagReference -> Maybe EventFilter
forDeletionRequests tagsReferences =
    if List.isEmpty tagsReferences then
        Nothing

    else
        Just
            { emptyEventFilter
                | kinds = Just [ KindEventDeletionRequest ]
                , tagReferences = Just tagsReferences
            }


forReactions : List TagReference -> Maybe EventFilter
forReactions tagReferences =
    if List.isEmpty tagReferences then
        Nothing

    else
        Just
            { emptyEventFilter
                | kinds =
                    Just
                        [ KindZapReceipt
                        , KindComment
                        , KindHighlights
                        , KindRepost
                        , KindGenericRepost
                        , KindShortTextNote
                        , KindReaction
                        , KindBookmarkList
                        , KindBookmarkSets
                        ]
                , tagReferences = Just tagReferences
            }
