module Pages.Home_ exposing (Model, Msg, page)

import Layouts
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Pages.Read as Implementation
import Ui.Styles exposing (referenceDesignStyles)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = Implementation.init shared
        , update = Implementation.update shared
        , subscriptions = Implementation.subscriptions
        , view = Implementation.view shared
        }
        |> Page.withLayout (toLayout)

toLayout : Model -> Layouts.Layout Msg
toLayout _ =
    Layouts.Sidebar
        { styles = referenceDesignStyles }

type alias Model = Implementation.Model
type alias Msg = Implementation.Msg
