module Pages.Home_ exposing (Model, Msg, page)

import Layouts
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Pages.Read as Implementation
import Ui.Styles exposing (Theme)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = Implementation.init shared
        , update = Implementation.update shared
        , subscriptions = Implementation.subscriptions
        , view = Implementation.view shared
        }
        |> Page.withLayout (toLayout shared.theme)

toLayout : Theme -> Model -> Layouts.Layout Msg
toLayout theme _ =
    Layouts.Sidebar
        { styles = Ui.Styles.stylesForTheme theme }

type alias Model = Implementation.Model
type alias Msg = Implementation.Msg
