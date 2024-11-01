module Nostr.BookmarkSet exposing (..)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Nostr.Event exposing (Event, Kind(..), Tag(..), TagReference(..), parseAddress)
import Nostr.Types exposing (EventId, PubKey)


type alias BookmarkSet =
    { notes : List EventId
    , articles : List TagReference
    , hashtags : List String
    , relays : List String
    }

{-
-}

bookmarkSetFromEvent : Event -> (PubKey, BookmarkSet)
bookmarkSetFromEvent event =
    let
        bookmarkSet =
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

                    UrlTag relay _ ->
                        { bml | relays = relay :: bml.relays }

                    _ ->
                        bml
                    )
                { notes = []
                , articles = []
                , hashtags = []
                , relays = []
                }
    in
    (event.pubKey, bookmarkSet )