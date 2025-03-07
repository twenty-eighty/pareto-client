module Pages.Privacy exposing (Model, Msg, page)

import Locale exposing (Language(..))
import Page exposing (Page)
import Pareto
import Route exposing (Route)
import Shared
import Shared.Model exposing (ClientRole(..))
import Ui.MarkdownPageUtils as PageUtils exposing (..)


page : Shared.Model -> Route () -> Page Model Msg
page shared _ =
    Page.new
        { init = init (privacyPolicyPath shared) shared
        , update = update
        , subscriptions = subscriptions
        , view = view shared
        }
        |> Page.withLayout (toLayout shared.theme)


privacyPolicyPath : Shared.Model -> String
privacyPolicyPath shared =
    Pareto.privacyPolicy shared.browserEnv.language |> Maybe.withDefault Pareto.privacyPolicyGerman


type alias Model =
    PageUtils.Model


type alias Msg =
    PageUtils.Msg
