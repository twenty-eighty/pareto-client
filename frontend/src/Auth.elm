module Auth exposing (User, onPageLoad, viewCustomPage)

import Auth.Action
import Dict
import Nostr.Profile exposing (Profile)
import Nostr.Types exposing (PubKey)
import Route exposing (Route)
import Route.Path
import Shared
import View exposing (View)
import Shared.Model exposing (LoginStatus(..))


type alias User =
    { pubKey : PubKey
    }


{-| Called before an auth-only page is loaded.
-}
onPageLoad : Shared.Model -> Route () -> Auth.Action.Action User
onPageLoad shared route =
    case shared.loginStatus of
        LoggedIn pubKey ->
            Auth.Action.loadPageWithUser
                { pubKey = pubKey
                }
        _ ->
            Auth.Action.pushRoute
                { path = Route.Path.Home_
                , query = Dict.empty
                , hash = Nothing
                }


{-| Renders whenever `Auth.Action.loadCustomPage` is returned from `onPageLoad`.
-}
viewCustomPage : Shared.Model -> Route () -> View Never
viewCustomPage shared route =
    View.fromString "Loading..."
