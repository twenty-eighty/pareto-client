module Pages.Search exposing (Model, Msg, page)

import Effect exposing (Effect)
import Route exposing (Route)
import Html
import Layouts
import Page exposing (Page)
import Shared
import View exposing (View)
import Ui.Styles exposing (referenceDesignStyles)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
        |> Page.withLayout (toLayout)

toLayout : Model -> Layouts.Layout Msg
toLayout model =
    Layouts.Sidebar
        { styles = referenceDesignStyles }



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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    View.fromString "Pages.Search"
