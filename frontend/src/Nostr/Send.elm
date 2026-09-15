module Nostr.Send exposing
    ( SendRequestId
    , SendRequest(..)
    , SendPayload
    , prepare
    , reactionEvent
    )

{-| Outgoing publish requests and event preparation.
-}

import Nostr.BookmarkList as BookmarkList exposing (BookmarkList, bookmarkListEvent, bookmarkListWithArticle, bookmarkListWithShortNote, bookmarkListWithoutArticle, bookmarkListWithoutShortNote, emptyBookmarkList)
import Nostr.Event exposing (AddressComponents, Event, Kind(..), Tag(..), addAddressTags, emptyEvent)
import Nostr.FollowList as FollowList exposing (emptyFollowList, followListEvent, followListWithPubKey, followListWithoutPubKey)
import Nostr.Highlights as Highlights
import Nostr.Types exposing (EventId, Following, PubKey, RelayUrl)


type alias SendRequestId =
    Int


type SendRequest
    = SendApplicationData Event
    | SendBookmarkListWithArticle PubKey AddressComponents
    | SendBookmarkListWithoutArticle PubKey AddressComponents
    | SendBookmarkListWithShortNote PubKey EventId
    | SendBookmarkListWithoutShortNote PubKey EventId
    | SendClientRecommendation (List RelayUrl) Event
    | SendComment (List RelayUrl) Event
    | SendDeletionRequest (List RelayUrl) Event
    | SendFileStorageServerList (List RelayUrl) Event
    | SendFollowList PubKey (List Following)
    | SendFollowListWithPubKey PubKey PubKey
    | SendFollowListWithoutPubKey PubKey PubKey
    | SendHandlerInformation (List RelayUrl) Event
    | SendHighlight PubKey EventId PubKey AddressComponents Kind String (Maybe String)
    | SendLongFormDraft (List RelayUrl) Event
    | SendLongFormArticle (List RelayUrl) Event
    | SendProfile (List RelayUrl) Event
    | SendReaction PubKey EventId PubKey (Maybe AddressComponents)
    | SendRelayList (List RelayUrl) Event
    | SendRepost (List RelayUrl) Event


type alias SendPayload =
    { relays : List RelayUrl
    , event : Event
    }


type alias PrepareContext =
    { getBookmarks : PubKey -> Maybe BookmarkList
    , getFollowList : PubKey -> Maybe (List Following)
    , writeRelaysFor : PubKey -> List RelayUrl
    , applicationDataRelays : List RelayUrl
    }


prepare : PrepareContext -> SendRequest -> SendPayload
prepare context sendRequest =
    case sendRequest of
        SendApplicationData event ->
            { relays = context.applicationDataRelays, event = event }

        SendBookmarkListWithArticle pubKey address ->
            { relays = context.writeRelaysFor pubKey
            , event =
                context.getBookmarks pubKey
                    |> Maybe.withDefault emptyBookmarkList
                    |> (\bookmarkList -> bookmarkListWithArticle bookmarkList address)
                    |> bookmarkListEvent pubKey
            }

        SendBookmarkListWithoutArticle pubKey address ->
            { relays = context.writeRelaysFor pubKey
            , event =
                context.getBookmarks pubKey
                    |> Maybe.withDefault emptyBookmarkList
                    |> (\bookmarkList -> bookmarkListWithoutArticle bookmarkList address)
                    |> bookmarkListEvent pubKey
            }

        SendBookmarkListWithShortNote pubKey eventId ->
            { relays = context.writeRelaysFor pubKey
            , event =
                context.getBookmarks pubKey
                    |> Maybe.withDefault emptyBookmarkList
                    |> (\bookmarkList -> bookmarkListWithShortNote bookmarkList eventId)
                    |> bookmarkListEvent pubKey
            }

        SendBookmarkListWithoutShortNote pubKey eventId ->
            { relays = context.writeRelaysFor pubKey
            , event =
                context.getBookmarks pubKey
                    |> Maybe.withDefault emptyBookmarkList
                    |> (\bookmarkList -> bookmarkListWithoutShortNote bookmarkList eventId)
                    |> bookmarkListEvent pubKey
            }

        SendClientRecommendation relays event ->
            { relays = relays, event = event }

        SendComment relays event ->
            { relays = relays, event = event }

        SendFollowList userPubKey followList ->
            { relays = context.writeRelaysFor userPubKey
            , event = followListEvent userPubKey followList
            }

        SendFollowListWithPubKey userPubKey toBeFollowedPubKey ->
            { relays = context.writeRelaysFor userPubKey
            , event =
                context.getFollowList userPubKey
                    |> Maybe.withDefault emptyFollowList
                    |> (\followList -> followListWithPubKey followList toBeFollowedPubKey)
                    |> followListEvent userPubKey
            }

        SendFollowListWithoutPubKey userPubKey toBeUnfollowedPubKey ->
            { relays = context.writeRelaysFor userPubKey
            , event =
                context.getFollowList userPubKey
                    |> Maybe.withDefault emptyFollowList
                    |> (\followList -> followListWithoutPubKey followList toBeUnfollowedPubKey)
                    |> followListEvent userPubKey
            }

        SendHandlerInformation relays event ->
            { relays = relays, event = event }

        SendHighlight userPubKey articleEventId articleAuthor addressComponents articleKind content maybeContext ->
            { relays = context.writeRelaysFor userPubKey
            , event =
                Highlights.highlightEvent userPubKey
                    { content = content
                    , context = maybeContext
                    , articleEventId = articleEventId
                    , articleAuthor = articleAuthor
                    , addressComponents = addressComponents
                    , articleKind = articleKind
                    }
            }

        SendLongFormArticle relays event ->
            { relays = relays, event = event }

        SendLongFormDraft relays event ->
            { relays = relays, event = event }

        SendFileStorageServerList relays event ->
            { relays = relays, event = event }

        SendDeletionRequest relays event ->
            { relays = relays, event = event }

        SendReaction userPubKey eventId articlePubKey addressComponents ->
            { relays = context.writeRelaysFor userPubKey
            , event = reactionEvent userPubKey eventId articlePubKey addressComponents
            }

        SendRepost relays event ->
            { relays = relays, event = event }

        SendRelayList relays event ->
            { relays = relays, event = event }

        SendProfile relays event ->
            { relays = relays, event = event }


reactionEvent : PubKey -> EventId -> PubKey -> Maybe AddressComponents -> Event
reactionEvent userPubKey eventId articlePubKey addressComponents =
    let
        event =
            emptyEvent userPubKey KindReaction
    in
    { event
        | content = "+"
        , tags =
            [ EventIdTag eventId Nothing Nothing Nothing
            , PublicKeyTag articlePubKey Nothing Nothing
            ]
                |> addAddressTags (addressComponents |> Maybe.map List.singleton |> Maybe.withDefault []) Nothing
    }
