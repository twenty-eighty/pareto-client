module Auth exposing (User, onPageLoad, viewCustomPage)

import Auth.Action
import Dict
import Nostr.Types exposing (PubKey)
import Route exposing (Route)
import Route.Path
import Shared
import Nostr.Types exposing (LoginStatus(..), loggedInPubKey)
import View exposing (View)


type alias User =
    { pubKey : PubKey
    }


{-| Called before an auth-only page is loaded.
-}
onPageLoad : Shared.Model -> Route () -> Auth.Action.Action User
onPageLoad shared route =
    shared.loginStatus
    |> loggedInPubKey
    |> Maybe.map (\pubKey -> Auth.Action.loadPageWithUser { pubKey = pubKey })
    |> Maybe.withDefault (
        Auth.Action.pushRoute
            { path = Route.Path.SignIn
            , query = Dict.fromList [ ( "from", route.url.path ) ]
            , hash = Nothing
            }
        )



{-| Renders whenever `Auth.Action.loadCustomPage` is returned from `onPageLoad`.
-}
viewCustomPage : Shared.Model -> Route () -> View Never
viewCustomPage _ _ =
    View.fromString "Loading..."
