module Pages.Privacy exposing (Model, Msg, init, page, subscriptions, update, view)

import Effect exposing (Effect)
import Html.Styled as Html exposing (div, text)
import Html.Styled.Attributes exposing (css)
import Http
import Layouts
import Locale exposing (Language(..))
import Markdown
import Page exposing (Page)
import Pareto
import Route exposing (Route)
import Shared
import Shared.Model exposing (ClientRole(..))
import Tailwind.Utilities as Tw
import Translations.Privacy as Translations
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
toLayout theme _ =
    Layouts.Sidebar
        { styles = Ui.Styles.stylesForTheme theme }


type alias Model =
    { privacyPolicyMarkdown : Maybe String
    }


init : Shared.Model -> Route () -> () -> ( Model, Effect Msg )
init shared _ () =
    ( { privacyPolicyMarkdown = Nothing }
    , Pareto.privacyPolicy shared.browserEnv.language
        |> Maybe.withDefault Pareto.privacyPolicyGerman
        |> requestMarkdown
        |> Effect.sendCmd
    )


requestMarkdown : String -> Cmd Msg
requestMarkdown url =
    Http.get
        { url = url
        , expect = Http.expectString ReceivedMarkdown
        }


type Msg
    = ReceivedMarkdown (Result Http.Error String)
    | NoOp


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update _ msg model =
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
