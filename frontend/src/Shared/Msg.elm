module Shared.Msg exposing (Msg(..))


{-| -}

import BrowserEnv
import Nostr
import Nostr.Request exposing (Request)
import Nostr.Send exposing (SendRequest)
import Nostr.Types exposing (IncomingMessage)
import Shared.Model exposing (ClientRole)


{-| Normally, this value would live in "Shared.elm"
but that would lead to a circular dependency import cycle.

For that reason, both `Shared.Model` and `Shared.Msg` are in their
own file, so they can be imported by `Effect.elm`

-}
type Msg
    = TriggerLogin
    | ReceivedPortMessage IncomingMessage
    | NostrMsg Nostr.Msg
    | BrowserEnvMsg BrowserEnv.Msg
    | RequestNostrEvents Request
    | ResetArticles
    | SendNostrEvent SendRequest
    | SwitchClientRole
    | SetClientRole ClientRole
