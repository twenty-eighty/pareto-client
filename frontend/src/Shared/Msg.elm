module Shared.Msg exposing (Msg(..))

{-| -}

import Browser.Dom
import BrowserEnv exposing (TestMode)
import Components.AlertTimerMessage as AlertTimerMessage
import Nostr
import Nostr.ConfigCheck as ConfigCheck
import Nostr.Nip05 as Nip05
import Nostr.Request exposing (Request)
import Nostr.Send exposing (SendRequest)
import Nostr.Types exposing (IncomingMessage, PubKey)
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
    | RequestNostrCount Request
    | ResetArticles
    | SendNostrEvent SendRequest
    | SetClientRole Bool ClientRole
    | SetTestMode TestMode
    | DelayedCheckConfiguration
    | CheckConfiguration ()
    | ConfigCheckMsg ConfigCheck.Msg
    | LoadUserDataByPubKey PubKey
    | LoadUserDataByNip05 Nip05.Nip05
    | ShowAlert String
    | AlertSent AlertTimerMessage.Msg
    | ScrollContentToTop
    | DomError (Result Browser.Dom.Error ())
    | ChangeLocale String
