module Pages.WhatIs exposing (Model, Msg, page)

import BrowserEnv exposing (BrowserEnv)
import Css
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, a, article, aside, button, code, div, h2, h3, h4, img, input, label, node, p, span, text, textarea)
import Html.Styled.Attributes as Attr exposing (class, css, style)
import Html.Styled.Events as Events exposing (..)
import Layouts
import Locale
import Nostr
import Nostr.Article exposing (Article)
import Nostr.Event exposing (EventFilter, Kind(..), TagReference(..))
import Nostr.Types exposing (IncomingMessage)
import Page exposing (Page)
import Ports
import Route exposing (Route)
import Shared
import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw
import Tailwind.Theme as Theme
import View exposing (View)
import Ui.ArticleOld
import Ui.Styles exposing (Styles, Theme)


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
    { article : Maybe Article
    , filter : EventFilter
    }

init : Shared.Model -> () -> ( Model, Effect Msg )
init shared () =
    let
        filter = articleFilterForLanguage shared.browserEnv
    in
    ( { article = Nothing
      , filter = filter
       }, Effect.sendCmd <| Ports.requestEvents "What is page" True -1 Nothing filter
    )

articleFilterForLanguage : BrowserEnv -> EventFilter
articleFilterForLanguage browserEnv =
    case browserEnv.language of
        Locale.German _ ->
            { authors = Just ["0f4795bf31824a414148daf1b589bb8138fb0a03963f984c84462e40a8365abe"]
            , ids = Nothing
            , kinds = Just [KindLongFormContent]
            , tagReferences = Just [ TagReferenceIdentifier "1728727973614" ]
            , limit = Just 1
            , since = Nothing
            , until = Nothing
            }

        _ ->
            { authors = Just ["0f4795bf31824a414148daf1b589bb8138fb0a03963f984c84462e40a8365abe"]
            , ids = Nothing
            , kinds = Just [KindLongFormContent]
            , tagReferences = Just [ TagReferenceIdentifier "1728726380368" ]
            , limit = Just 1
            , since = Nothing
            , until = Nothing
            }

-- UPDATE

type Msg
    = ReceivedMessage IncomingMessage


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ReceivedMessage message ->
{-
            case message.messageType of
                "article" ->
                    case Decode.decodeValue Nostr.Article.nostrArticleDecoder message.value of
                        Ok article ->
                            ( { model | article = Just article }, Effect.none )
                        
                        Err error ->
                            ( model, Effect.none )

                _ ->
-}
                    ( model, Effect.none )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Ports.receiveMessage ReceivedMessage



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "What Is Pareto"
    , body =
        [ viewArticle (Ui.Styles.stylesForTheme shared.theme) shared.browserEnv model.article
        ]
    }

viewArticle : Styles Msg -> BrowserEnv -> Maybe Article -> Html Msg
viewArticle styles browserEnv maybeArticle =
    case maybeArticle of
        Just article ->
            Ui.ArticleOld.viewArticleInternal styles browserEnv article

        Nothing ->
            div [][]
