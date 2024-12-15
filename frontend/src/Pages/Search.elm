module Pages.Search exposing (Model, Msg, page)

import Effect exposing (Effect)
import Route exposing (Route)
import Html.Styled as Html exposing (Html, div)
import Layouts
import Page exposing (Page)
import Shared
import Translations.Search as Translations
import View exposing (View)
import Ui.Styles exposing (Theme)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view shared
        }
        |> Page.withLayout (toLayout shared.theme)

toLayout : Theme -> Model -> Layouts.Layout Msg
toLayout theme model =
    Layouts.Sidebar
        { styles = Ui.Styles.stylesForTheme theme }



-- INIT


type alias Model =
    {}


init : () -> ( Model, Effect Msg )
init () =
    ( {}
    , Effect.none
    )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Effect.none
            )
-- Nostr.getSearchRelayUrls model maybePubKey


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = Translations.pageTitle [ shared.browserEnv.translations ]
    , body =
        [ viewSearch shared model
        ] 
    }


viewSearch : Shared.Model -> Model -> Html Msg
viewSearch shared model =
    div
        [
        ]
        [
        ]