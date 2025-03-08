module Pages.TechDetails exposing (Model, Msg, page)

import Nostr.Event exposing (KindInformationLink(..))
import Page exposing (Page)
import Pareto
import Route exposing (Route)
import Shared
import Time exposing (Month(..))
import Ui.MarkdownPageUtils as PageUtils exposing (..)


page : Shared.Model -> Route () -> Page Model Msg
page shared _ =
    Page.new
        { init = init Pareto.technicalDetails shared
        , update = update
        , subscriptions = subscriptions
        , view = view shared
        }
        |> Page.withLayout (toLayout shared.theme)


type alias Model =
    PageUtils.Model


type alias Msg =
    PageUtils.Msg
