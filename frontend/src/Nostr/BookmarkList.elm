module Nostr.BookmarkList exposing (..)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Nostr.Event exposing (AddressComponents, Event, Kind(..), Tag(..), TagReference(..), addAddressTags, emptyEvent, parseAddress)
import Nostr.Types exposing (EventId, PubKey)
import Json.Decode exposing (list)


type alias BookmarkList =
    { notes : List EventId
    , articles : List AddressComponents
    , hashtags : List String
    , urls : List String
    }

type BookmarkType
    = ArticleBookmark
    | HashtagBookmark
    | NoteBookmark
    | UrlBookmark


{-
-}
emptyBookmarkList : BookmarkList
emptyBookmarkList =
    { notes = []
    , articles = []
    , hashtags = []
    , urls = []
    }

bookmarksCount : BookmarkList -> Int
bookmarksCount bookmarks =
    List.length bookmarks.notes +
    List.length bookmarks.articles +
    List.length bookmarks.hashtags +
    List.length bookmarks.urls

bookmarkListFromEvent : Event -> (PubKey, BookmarkList)
bookmarkListFromEvent event =
    let
        bookmarkList =
            event.tags
            |> List.foldl (\tag bml ->
                case tag of 
                    AddressTag addressComponents ->
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
    (event.pubKey, bookmarkList )

bookmarkListWithArticle : BookmarkList -> AddressComponents -> BookmarkList
bookmarkListWithArticle bookmarks addressComponents =
    let
        listContainsArticle =
            bookmarks.articles
            |> List.filter (\referencedAddressComponents ->
                referencedAddressComponents == addressComponents
                )
            |> List.isEmpty
            |> not

        -- don't duplicate entry
        articlesWithAddress =
            if not listContainsArticle then
                bookmarks.articles ++ [ addressComponents ]
            else
                bookmarks.articles

    in
    { bookmarks | articles = articlesWithAddress }


bookmarkListWithoutArticle : BookmarkList -> AddressComponents -> BookmarkList
bookmarkListWithoutArticle bookmarks addressComponents =
    let
        articlesWithoutAddress =
            bookmarks.articles
            |> List.filter (\referencedAddressComponents -> referencedAddressComponents /= addressComponents)
    in
    { bookmarks | articles = articlesWithoutAddress }


bookmarkListWithShortNote : BookmarkList -> EventId -> BookmarkList
bookmarkListWithShortNote bookmarks eventId =
    let
        listContainsNote =
            bookmarks.notes
            |> List.filter (\referencedAddressComponents ->
                referencedAddressComponents == eventId
                )
            |> List.isEmpty
            |> not

        -- don't duplicate entry
        notesWithEventId =
            if not listContainsNote then
                bookmarks.notes ++ [ eventId ]
            else
                bookmarks.notes

    in
    { bookmarks | notes = notesWithEventId }


bookmarkListWithoutShortNote : BookmarkList -> EventId -> BookmarkList
bookmarkListWithoutShortNote bookmarks eventId =
    let
        notesWithoutAddress =
            bookmarks.notes
            |> List.filter (\referencedEventId -> referencedEventId /= eventId)
    in
    { bookmarks | notes = notesWithoutAddress }


bookmarkListEvent : PubKey -> BookmarkList -> Event
bookmarkListEvent pubKey list =
    let
        event = 
            emptyEvent pubKey KindBookmarkList

    in
    { event
        | tags =
            []
            |> addAddressTags list.articles
    }
