module Auth exposing (User, onPageLoad, viewCustomPage)

import Auth.Action
import Dict
import Nostr.Types exposing (LoginStatus(..), PubKey)
import Route exposing (Route)
import Route.Path
import Shared
import View exposing (View)


type alias User =
    { pubKey : PubKey
    }


{-| Called before an auth-only page is loaded.
-}
onPageLoad : Shared.Model -> Route () -> Auth.Action.Action User
onPageLoad shared route =
    case shared.loginStatus of
        LoggedIn pubKey _ ->
            Auth.Action.loadPageWithUser { pubKey = pubKey }

        -- Session restore still in progress — stay on the destination URL and
        -- show Auth.viewCustomPage until Shared settles to LoggedIn or LoggedOut.
        LoggedInUnknown ->
            Auth.Action.loadCustomPage

        LoggedOut ->
            Auth.Action.pushRoute
                { path = Route.Path.SignIn
                , query =
                    -- Keep original query (e.g. settings?category=wallet) so SignIn can restore it.
                    route.query
                        |> Dict.insert "from" route.url.path
                , hash = route.hash
                }


{-| Renders whenever `Auth.Action.loadCustomPage` is returned from `onPageLoad`.
-}
viewCustomPage : Shared.Model -> Route () -> View Never
viewCustomPage _ _ =
    View.fromString "Loading..."
