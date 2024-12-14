module Nostr.BookmarkList exposing (..)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Nostr.Event exposing (Event, Kind(..), Tag(..), TagReference(..), parseAddress)
import Nostr.Types exposing (EventId, PubKey)


type alias BookmarkList =
    { notes : List EventId
    , articles : List TagReference
    , hashtags : List String
    , urls : List String
    }

{-
-}

bookmarkListFromEvent : Event -> (PubKey, BookmarkList)
bookmarkListFromEvent event =
    let
        bookmarkList =
            event.tags
            |> List.foldl (\tag bml ->
                case tag of 
                    AddressTag address ->
                        case parseAddress address of
                            Just (kind, pubKey, identifier) ->
                                { bml | articles = TagReferenceCode kind pubKey identifier :: bml.articles }

                            Nothing ->
                                bml

                    HashTag hashtag ->
                        { bml | hashtags = hashtag :: bml.hashtags }

                    EventDelegationTag identifier ->
                        { bml | notes = identifier :: bml.notes }

                    UrlTag urls _ ->
                        { bml | urls = urls :: bml.urls }

                    _ ->
                        bml
                    )
                { notes = []
                , articles = []
                , hashtags = []
                , urls = []
                }
    in
    (event.pubKey, bookmarkList )