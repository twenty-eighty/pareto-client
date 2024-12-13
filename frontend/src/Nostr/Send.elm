module Nostr.Send exposing (..)

import Nostr.Event exposing (Event)

type alias SendRequestId = Int

type SendRequest
    = SendClientRecommendation (List String) Event
    | SendDeletionRequest (List String) Event
    | SendFileStorageServerList (List String) Event
    | SendHandlerInformation (List String) Event
    | SendLongFormDraft (List String) Event
    | SendLongFormArticle (List String) Event
    | SendProfile (List String) Event
    | SendRelayList (List String) Event