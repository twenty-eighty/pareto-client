module Pages.Privacy exposing (Model, Msg, init, page, subscriptions, update, view)

import Dict
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, a, article, aside, button, div, h2, h3, h4, img, input, label, main_, p, span, text)
import Html.Styled.Attributes as Attr exposing (class, css)
import Html.Styled.Events as Events exposing (..)
import Http
import Layouts
import Locale exposing (Language(..))
import Markdown
import Nostr.Types exposing (IncomingMessage)
import Page exposing (Page)
import Pareto
import Ports
import Route exposing (Route)
import Route.Path
import Shared
import Shared.Model exposing (ClientRole(..))
import Shared.Msg
import Tailwind.Breakpoints as Bp
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw
import Translations.SignIn as Translations
import Ui.Styles exposing (Theme)
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init shared route
        , update = update shared
        , subscriptions = subscriptions
        , view = view shared
        }
        |> Page.withLayout (toLayout shared.theme)


toLayout : Theme -> Model -> Layouts.Layout Msg
toLayout theme model =
    Layouts.Sidebar
        { styles = Ui.Styles.stylesForTheme theme }


type alias Model =
    { privacyPolicyMarkdown : Maybe String
    }


init : Shared.Model -> Route () -> () -> ( Model, Effect Msg )
init shared _ () =
    ( { privacyPolicyMarkdown = Nothing }
    , requestMarkdown shared.browserEnv.language
        |> Effect.sendCmd
    )


requestMarkdown : Language -> Cmd Msg
requestMarkdown language =
    Http.get
        { url = markdownUrl language
        , expect = Http.expectString ReceivedMarkdown
        }


markdownUrl : Language -> String
markdownUrl language =
    case language of
        German _ ->
            Pareto.privacyPolicyGerman

        -- extend here for other versions
        _ ->
            Pareto.privacyPolicyGerman


type Msg
    = ReceivedMarkdown (Result Http.Error String)
    | NoOp


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        ReceivedMarkdown (Ok markdown) ->
            ( { model | privacyPolicyMarkdown = Just markdown }, Effect.none )

        ReceivedMarkdown (Err _) ->
            ( model, Effect.none )

        NoOp ->
            ( model, Effect.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Shared.Model -> Model -> View Msg
view shared model =
    let
        styles =
            Ui.Styles.stylesForTheme shared.theme
    in
    { title = Translations.pageTitle [ shared.browserEnv.translations ]
    , body =
        [ div
            (styles.colorStyleBackground
                ++ styles.colorStyleGrayscaleTitle
                ++ [ css
                        [ Tw.flex
                        , Tw.flex_col
                        , Tw.gap_3
                        , Tw.m_10
                        ]
                   ]
            )
            [ case model.privacyPolicyMarkdown of
                Just privacyPolicyMarkdown ->
                    case Markdown.markdownViewHtml styles Nothing (\_ -> Nothing) privacyPolicyMarkdown of
                        Ok html ->
                            html

                        Err error ->
                            div [] [ text <| "Error rendering Markdown: " ++ error ]

                Nothing ->
                    div [] []
            ]
        ]
    }
