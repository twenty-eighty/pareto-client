module Pages.Search exposing (Model, Msg, page)

import Components.SearchBar as SearchBar exposing (SearchBar)
import Effect exposing (Effect)
import Route exposing (Route)
import Html.Styled as Html exposing (Html, div, p)
import Html.Styled.Attributes exposing (css)
import Layouts
import Nostr.Event exposing (AddressComponents, EventFilter, Kind(..), kindDecoder, emptyEventFilter)
import Page exposing (Page)
import Shared
import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw
import Tailwind.Theme as Theme
import Translations.Search as Translations
import Ui.Styles exposing (Theme, stylesForTheme)
import View exposing (View)
import Css


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
    { searchBar : SearchBar.Model
    }


init : () -> ( Model, Effect Msg )
init () =
    ( { searchBar = SearchBar.init {} }
    , Effect.none
    )



-- UPDATE


type Msg
    = Search String
    | SearchBarSent (SearchBar.Msg Msg)


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        Search searchText ->
            ( model
            , Effect.none
            )

        SearchBarSent innerMsg ->
            SearchBar.update
                { msg = innerMsg
                , model = model.searchBar
                , toModel = \searchBar -> { model | searchBar = searchBar }
                , toMsg = SearchBarSent
                }

-- Nostr.getSearchRelayUrls model maybePubKey

searchEventFilter : EventFilter
searchEventFilter =
    { emptyEventFilter | kinds = Just [KindLongFormContent], limit = Just 20 }

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
    let
        styles =
            stylesForTheme shared.theme
    in
    div
        [ css
            [ Tw.flex
            , Tw.justify_center
            , Tw.max_w_full
            , Tw.m_4
            ]
        ]
        [ p
            []
            [ Html.text <| Translations.explanation1 [ shared.browserEnv.translations ]
            ]
        , p
            []
            [ Html.text <| Translations.explanation2 [ shared.browserEnv.translations ]
            ]
        , SearchBar.new
            { model = model.searchBar
            , toMsg = SearchBarSent
            , onSearch = Search
            , browserEnv = shared.browserEnv
            , styles = styles
            }
            |> SearchBar.view
        ]