module Pages.Internals exposing (Model, Msg, init, page, subscriptions, update, view)

import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, div, text)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events exposing (..)
import I18Next
import Layouts
import Nostr
import Page exposing (Page)
import Pareto
import Route exposing (Route)
import Shared
import Shared.Model exposing (ClientRole(..))
import Tailwind.Utilities as Tw
import Translations.Internals as Translations
import Ui.Styles exposing (Theme, stylesForTheme)
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared _ =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view shared
        }
        |> Page.withLayout (toLayout shared.theme)


toLayout : Theme -> Model -> Layouts.Layout Msg
toLayout theme _ =
    Layouts.Sidebar
        { styles = Ui.Styles.stylesForTheme theme }


type alias Model =
    {}


init : () -> ( Model, Effect Msg )
init () =
    ( {}, Effect.none )


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Effect.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Shared.Model -> Model -> View Msg
view shared _ =
    let
        styles =
            stylesForTheme shared.theme
    in
    { title = Translations.pageTitle [ shared.browserEnv.translations ]
    , body =
        [ div
            [ css
                [ Tw.flex
                , Tw.flex_col
                , Tw.gap_3
                , Tw.m_4
                ]
            ]
            [ Html.p [] [ text <| Translations.explanation [ shared.browserEnv.translations ] ]
            , Html.p [] [ text <| Translations.errorReporting [ shared.browserEnv.translations ] ]
            , Html.a
                (styles.textStyleLinks
                    ++ [ Attr.href <| "mailto:" ++ Pareto.supportEmail ]
                )
                [ text <| Translations.contactInformation [ shared.browserEnv.translations ] ++ " " ++ Pareto.supportEmail
                ]
            , showErrorMessages shared.theme shared.browserEnv.translations shared.nostr
            ]
        ]
    }


showErrorMessages : Theme -> I18Next.Translations -> Nostr.Model -> Html Msg
showErrorMessages theme translations nostr =
    let
        styles =
            stylesForTheme theme
    in
    case Nostr.getErrorMessages nostr of
        [] ->
            div [] []

        errorMessages ->
            div []
                [ div
                    (styles.textStyleH1
                        ++ [ css
                                []
                           ]
                    )
                    [ text <| Translations.errorMessagesTitle [ translations ]
                    ]
                , errorMessages
                    |> List.map Html.text
                    |> div
                        [ css
                            [ Tw.flex
                            , Tw.flex_col
                            , Tw.gap_2
                            ]
                        ]
                ]
