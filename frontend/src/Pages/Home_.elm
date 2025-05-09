module Pages.Home_ exposing (Model, Msg, page)

import Layouts
import Layouts.Sidebar
import Page exposing (Page)
import Pages.Read as Implementation
import Route exposing (Route)
import Shared
import Ui.Styles exposing (Theme)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = Implementation.init shared route
        , update = Implementation.update shared
        , subscriptions = Implementation.subscriptions
        , view = Implementation.view shared
        }
        |> Page.withLayout (toLayout shared.theme)


toLayout : Theme -> Model -> Layouts.Layout Msg
toLayout theme _ =
    Layouts.Sidebar.new
        { theme = theme
        }
        |> Layouts.Sidebar


type alias Model =
    Implementation.Model


type alias Msg =
    Implementation.Msg
