module Shared.Model exposing (ClientRole(..), Model)

import BrowserEnv exposing (BrowserEnv)
import Components.AlertTimerMessage as AlertTimerMessage
import Nostr
import Nostr.ConfigCheck as ConfigCheck
import Nostr.Types exposing (LoginStatus)
import Ui.Styles exposing (Theme)


{-| Normally, this value would live in "Shared.elm"
but that would lead to a circular dependency import cycle.

For that reason, both `Shared.Model` and `Shared.Msg` are in their
own file, so they can be imported by `Effect.elm`

-}
type alias Model =
    { loginStatus : LoginStatus
    , browserEnv : BrowserEnv
    , configCheck : ConfigCheck.Model
    , nostr : Nostr.Model
    , role : ClientRole
    , theme : Theme
    , alertTimerMessage : AlertTimerMessage.Model
    , readPageScrollPosition : Float
    }


type ClientRole
    = ClientReader
    | ClientCreator
