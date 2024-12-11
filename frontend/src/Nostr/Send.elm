module Nostr.Send exposing (..)

import Nostr.Event exposing (Event)

type alias SendRequestId = Int

type SendRequest
    = SendDeletionRequest (List String) Event
    | SendFileStorageServerList (List String) Event
    | SendLongFormDraft (List String) Event
    | SendLongFormArticle (List String) Event
    | SendRelayList (List String) Event