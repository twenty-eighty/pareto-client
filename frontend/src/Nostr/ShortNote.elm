module Nostr.ShortNote exposing (..)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Nostr.Event exposing (Event, Kind(..), Tag(..), TagReference(..))
import Nostr.Types exposing (PubKey)
import Time


type alias ShortNote =
    { id : String
    , content : Maybe String
    , createdAt : Time.Posix
    , pubKey : PubKey
    }

{-
-}

shortNoteFromEvent : Event -> ShortNote
shortNoteFromEvent event =
    event.tags
    |> List.foldl (\tag res ->
        case tag of 

            _ ->
                res
            )
        { id = event.id
        , content = Just event.content
        , createdAt = event.createdAt
        , pubKey = event.pubKey
        }

tagReference : ShortNote -> TagReference
tagReference shortNote =
    TagReferenceEventId shortNote.id

