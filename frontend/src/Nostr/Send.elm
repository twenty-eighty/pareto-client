module Nostr.Send exposing (..)

import Nostr.Event exposing (AddressComponents, Event)
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
    | SendLongFormDraft (List RelayUrl) Event
    | SendLongFormArticle (List RelayUrl) Event
    | SendProfile (List RelayUrl) Event
    | SendReaction PubKey EventId PubKey AddressComponents
    | SendRelayList (List RelayUrl) Event
    | SendRepost (List RelayUrl) Event
