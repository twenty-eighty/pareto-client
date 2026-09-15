module Shared.Model exposing (ClientRole(..), Model)

import BrowserEnv exposing (BrowserEnv)
import Components.AlertTimerMessage as AlertTimerMessage
import Components.AuthDialog as AuthDialog
import Dict exposing (Dict)
import Nostr
import Nostr.ConfigCheck as ConfigCheck
import Nostr.Types exposing (LoginStatus, PubKey)
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
    , authDialog : AuthDialog.Model
    , notificationsLastSeen : Dict PubKey Int
    }


type ClientRole
    = ClientReader
    | ClientCreator
