module Pages.About exposing (Model, Msg, page)

import BrowserEnv exposing (BrowserEnv)
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, a, article, aside, button, code, div, h2, h3, h4, img, input, label, node, p, span, text, textarea)
import Html.Styled.Attributes as Attr exposing (class, css, style)
import Html.Styled.Events as Events exposing (..)
import Layouts
import Locale
import Nostr
import Nostr.Article exposing (Article)
import Nostr.Event exposing (EventFilter, Kind(..), TagReference(..), emptyEventFilter)
import Nostr.Request exposing (RequestData(..))
import Nostr.Types exposing (PubKey)
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Tailwind.Utilities as Tw
import Translations
import View exposing (View)
import Ui.Article
import Ui.Styles exposing (Theme)
import Shared.Msg


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init shared
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
    { pubKey : PubKey
    , identifier : String
    }

init : Shared.Model -> () -> ( Model, Effect Msg )
init shared () =
    let
        model =
            modelForLanguage shared.browserEnv

        filter =
            { emptyEventFilter | authors = Just [model.pubKey]
                , kinds = Just [KindLongFormContent]
                , tagReferences = Just [ TagReferenceIdentifier model.identifier ]
            }
    in
    ( model
    , RequestArticle Nothing filter
    |> Nostr.createRequest shared.nostr "About article" []
    |> Shared.Msg.RequestNostrEvents
    |> Effect.sendSharedMsg)

modelForLanguage : BrowserEnv -> Model
modelForLanguage browserEnv =
    case browserEnv.language of
        Locale.German _ ->
            { pubKey = "0f4795bf31824a414148daf1b589bb8138fb0a03963f984c84462e40a8365abe"
            , identifier = "1728747315068"
            }

        _ ->
            { pubKey = "0f4795bf31824a414148daf1b589bb8138fb0a03963f984c84462e40a8365abe"
            , identifier = "1728747025246"
            }



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Effect.none )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = Translations.aboutMenuItemText [shared.browserEnv.translations]
    , body =
        [ Nostr.getArticleWithIdentifier shared.nostr model.pubKey model.identifier
            |> viewArticle shared.theme shared.browserEnv
        ]
    }

viewArticle : Theme -> BrowserEnv -> Maybe Article -> Html Msg
viewArticle theme browserEnv maybeArticle =
    case maybeArticle of
        Just article ->
            Ui.Article.viewArticleInternal (Ui.Styles.stylesForTheme theme) browserEnv article

        Nothing ->
            div
                [ css
                    [ Tw.h_full
                    ]
                ]
                []
