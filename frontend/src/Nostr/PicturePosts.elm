module Nostr.PicturePosts exposing (ingest)

{-| Picture post (NIP-68) store updates. Related requests stay in `Nostr`.
-}

import Dict exposing (Dict)
import Nostr.Event exposing (Event)
import Nostr.Nip68 exposing (PicturePost, picturePostFromEvent)
import Nostr.Types exposing (EventId)


ingest :
    Dict EventId PicturePost
    -> List Event
    -> ( Dict EventId PicturePost, List PicturePost )
ingest picturePosts events =
    let
        posts =
            events
                |> List.map picturePostFromEvent

        updated =
            posts
                |> List.foldl
                    (\picture dict ->
                        Dict.insert picture.id picture dict
                    )
                    picturePosts
    in
    ( updated, posts )
