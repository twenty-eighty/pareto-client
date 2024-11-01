module Pages.Home_ exposing (Model, Msg, page)

import BrowserEnv exposing (BrowserEnv)
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, a, article, aside, button, div, h2, h3, h4, img, main_, p, span, text)
import Html.Styled.Attributes as Attr exposing (class, css, href)
import Html.Styled.Events as Events exposing (..)
import Layouts
import Nostr
import Nostr.Article exposing (Article)
import Nostr.Event exposing (EventFilter, Kind(..), kindDecoder, emptyEventFilter)
import Nostr.Request exposing (RequestData(..))
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Shared.Model
import Shared.Msg
import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw
import Tailwind.Theme as Theme
import Translations
import Translations.Read
import View exposing (View)
import Ports
import Time
import Set


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init shared
        , update = update shared
        , subscriptions = subscriptions
        , view = view shared
        }
        |> Page.withLayout (toLayout)

toLayout : Model -> Layouts.Layout Msg
toLayout model =
    Layouts.Sidebar
        {}


-- INIT


type alias Model =
    { }


init : Shared.Model -> () -> ( Model, Effect Msg )
init shared () =
    let
        filter =
            { emptyEventFilter | kinds = Just [KindLongFormContent] , limit = Just 20 }
    in
    ( { }
    , RequestArticlesFeed filter
      |> Nostr.createRequest shared.nostr "Long-form articles" [KindUserMetadata]
      |> Shared.Msg.RequestNostrEvents
      |> Effect.sendSharedMsg
    )



-- UPDATE


type Msg
    = OpenGetStarted


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        OpenGetStarted ->
            ( model, Effect.sendCmd <| Ports.requestUser
            )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Shared.Model.Model -> Model -> View Msg
view shared model =
    { title = Translations.readMenuItemText [shared.browserEnv.translations]
    , body = [                                  {- Main Content -}
            div
                [ css
                    [ Tw.flex
                    , Tw.space_x_4
                    , Tw.mb_6
                    ]
                ]
                [ button
                    [ css
                        [ Tw.bg_color Theme.purple_100
                        , Tw.text_color Theme.purple_600
                        , Tw.px_4
                        , Tw.py_2
                        , Tw.rounded_full
                        ]
                    ]
                    [ text <| Translations.Read.globalFeedCategory [ shared.browserEnv.translations ] ]
                , button
                    [ css
                        [ Tw.bg_color Theme.gray_100
                        , Tw.text_color Theme.gray_600
                        , Tw.px_4
                        , Tw.py_2
                        , Tw.rounded_full
                        ]
                    ]
                    [ text <| Translations.Read.highlighterFeedCategory [ shared.browserEnv.translations ] ]
                ]
            , Nostr.getArticlesByDate shared.nostr
             |> Nostr.viewArticlePreviews shared.browserEnv shared.nostr 
            ]
    }
