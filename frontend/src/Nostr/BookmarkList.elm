module Nostr.BookmarkList exposing (..)

import Dict exposing (Dict)
import Json.Decode exposing (list)
import Nostr.Event exposing (AddressComponents, Event, Kind(..), Tag(..), TagReference(..), addAddressTags, addEventIdTags, emptyEvent)
import Nostr.Types exposing (EventId, PubKey)


type alias BookmarkList =
    { notes : List EventId
    , articles : List AddressComponents
    }


type BookmarkType
    = ArticleBookmark
    | NoteBookmark



{- -}


emptyBookmarkList : BookmarkList
emptyBookmarkList =
    { notes = []
    , articles = []
    }


bookmarksCount : BookmarkList -> Int
bookmarksCount bookmarks =
    List.length bookmarks.notes
        + List.length bookmarks.articles


containsAddress : BookmarkList -> AddressComponents -> Bool
containsAddress bookmarkList ( kind, author, identifier ) =
    bookmarkList.articles
        |> List.filter
            (\( articleKind, articleAuthor, articleIdentifier ) ->
                (articleKind == kind)
                    && (articleAuthor == author)
                    && (articleIdentifier == identifier)
            )
        |> List.isEmpty
        |> not


containsEventId : BookmarkList -> EventId -> Bool
containsEventId bookmarkList eventId =
    List.member eventId bookmarkList.notes


countAddressAcross : Dict PubKey BookmarkList -> AddressComponents -> Int
countAddressAcross bookmarkLists addressComponents =
    bookmarkLists
        |> Dict.values
        |> List.map
            (\bookmarkList ->
                bookmarkList.articles
                    |> List.filter (\articleAddressComponents -> articleAddressComponents == addressComponents)
                    |> List.length
            )
        |> List.sum


countEventIdAcross : Dict PubKey BookmarkList -> EventId -> Int
countEventIdAcross bookmarkLists eventId =
    bookmarkLists
        |> Dict.values
        |> List.map
            (\bookmarkList ->
                bookmarkList.notes
                    |> List.filter (\noteEventId -> noteEventId == eventId)
                    |> List.length
            )
        |> List.sum


bookmarkListFromEvent : Event -> ( PubKey, BookmarkList )
bookmarkListFromEvent event =
    let
        bookmarkList =
            event.tags
                |> List.foldl
                    (\tag bml ->
                        case tag of
                            AddressTag addressComponents _ _ ->
                                { bml | articles = bml.articles ++ [ addressComponents ] }

                            EventIdTag eventId _ _ _ ->
                                { bml | notes = eventId :: bml.notes }

                            _ ->
                                bml
                    ) emptyBookmarkList
    in
    ( event.pubKey, bookmarkList )


ingest : Dict PubKey BookmarkList -> List Event -> Dict PubKey BookmarkList
ingest dict events =
    events
        |> List.map bookmarkListFromEvent
        |> List.foldl
            (\( pubKey, bookmarkList ) acc ->
                Dict.insert pubKey bookmarkList acc
            )
            dict


bookmarkListWithArticle : BookmarkList -> AddressComponents -> BookmarkList
bookmarkListWithArticle bookmarks addressComponents =
    let
        listContainsArticle =
            bookmarks.articles
                |> List.filter
                    (\referencedAddressComponents ->
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
                |> List.filter
                    (\referencedEventId ->
                        referencedEventId == eventId
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
                |> addAddressTags list.articles Nothing
                |> addEventIdTags list.notes Nothing Nothing Nothing
    }
