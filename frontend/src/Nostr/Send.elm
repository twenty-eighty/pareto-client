module Nostr.Send exposing (..)

import Nostr.Event exposing (Event)

type alias SendRequestId = Int

type SendRequest =
    SendLongFormDraft Event