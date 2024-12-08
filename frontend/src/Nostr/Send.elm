module Nostr.Send exposing (..)

import Nostr.Event exposing (Event)

type alias SendRequestId = Int

type SendRequest
    = SendLongFormDraft (List String) Event
    | SendLongFormArticle (List String) Event
    | SendFileStorageServerList (List String) Event
    | SendDeletionRequest (List String) Event