module Nostr.Send exposing (..)

import Nostr.Event exposing (AddressComponents, Event)
import Nostr.Types exposing (EventId, PubKey, RelayUrl)

type alias SendRequestId = Int

type SendRequest
    = SendClientRecommendation (List RelayUrl) Event
    | SendBookmarkListWithArticle PubKey AddressComponents
    | SendBookmarkListWithoutArticle PubKey AddressComponents
    | SendBookmarkListWithShortNote PubKey EventId
    | SendBookmarkListWithoutShortNote PubKey EventId
    | SendDeletionRequest (List RelayUrl) Event
    | SendFileStorageServerList (List RelayUrl) Event
    | SendHandlerInformation (List RelayUrl) Event
    | SendLongFormDraft (List RelayUrl) Event
    | SendLongFormArticle (List RelayUrl) Event
    | SendProfile (List RelayUrl) Event
    | SendRelayList (List RelayUrl) Event