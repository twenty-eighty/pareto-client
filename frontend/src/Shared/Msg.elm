module Shared.Msg exposing (Msg(..))


{-| -}

import BrowserEnv
import Nostr
import Nostr.Request exposing (Request)
import Nostr.Send exposing (SendRequest)


{-| Normally, this value would live in "Shared.elm"
but that would lead to a circular dependency import cycle.

For that reason, both `Shared.Model` and `Shared.Msg` are in their
own file, so they can be imported by `Effect.elm`

-}
type Msg
    = TriggerLogin
    | ReceivedPortMessage Nostr.IncomingMessage
    | NostrMsg Nostr.Msg
    | BrowserEnvMsg BrowserEnv.Msg
    | RequestNostrEvents Request
    | SendNostrEvent SendRequest
    | SwitchClientRole
