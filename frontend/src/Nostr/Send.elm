module Nostr.Send exposing (..)

import Nostr.Event exposing (Event)

type alias SendRequestId = Int

type SendRequest
    = SendLongFormDraft (List String) Event
    | SendFileStorageServerList (List String) Event