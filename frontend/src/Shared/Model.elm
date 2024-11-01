module Shared.Model exposing (Model, LoginStatus(..), ClientRole(..))

import BrowserEnv exposing (BrowserEnv)
import Nostr
import Nostr.Types exposing (PubKey)

{-| Normally, this value would live in "Shared.elm"
but that would lead to a circular dependency import cycle.

For that reason, both `Shared.Model` and `Shared.Msg` are in their
own file, so they can be imported by `Effect.elm`

-}
type alias Model =
    { loginStatus : LoginStatus
    , browserEnv : BrowserEnv
    , nostr : Nostr.Model
    , role : ClientRole
    }

type LoginStatus
    = LoggedOut
    | LoggedInUnknown
    | LoggedIn PubKey

type ClientRole
    = ClientConsumer
    | ClientCreator