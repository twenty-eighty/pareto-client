module Nostr.BookmarkSet exposing (..)

import Nostr.Event exposing (AddressComponents, Event, Kind(..), Tag(..), TagReference(..))
import Nostr.Types exposing (EventId, PubKey)


type alias BookmarkSet =
    { notes : List EventId
    , articles : List AddressComponents
    , hashtags : List String
    , urls : List String
    }



{- -}


bookmarkSetFromEvent : Event -> ( PubKey, BookmarkSet )
bookmarkSetFromEvent event =
    let
        bookmarkSet =
            event.tags
                |> List.foldl
                    (\tag bml ->
                        case tag of
                            AddressTag addressComponents _ _ ->
                                { bml | articles = bml.articles ++ [ addressComponents ] }

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
    ( event.pubKey, bookmarkSet )
