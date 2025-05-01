module Pages.Internals exposing (Model, Msg, init, page, subscriptions, update, view)

import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, div, text)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events exposing (..)
import I18Next
import Layouts
import Layouts.Sidebar
import Nostr
import Page exposing (Page)
import Pareto
import Route exposing (Route)
import Shared
import Shared.Model exposing (ClientRole(..), LoginMethod(..), LoginStatus(..))
import Tailwind.Utilities as Tw
import Translations.Internals as Translations
import Ui.Shared exposing (emptyHtml)
import Ui.Styles exposing (Theme, stylesForTheme)
import Ui.Shared exposing (emptyHtml, viewConfigIssues)
import View exposing (View)
import Nostr.ConfigCheck as ConfigCheck
import Nostr.Nip19 as Nip19


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
    Layouts.Sidebar.new
        { styles = Ui.Styles.stylesForTheme theme
        }
        |> Layouts.Sidebar


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
            , viewLoginInfo shared
            , viewConfigIssues shared.browserEnv.translations (Translations.configurationIssuesTitle [ shared.browserEnv.translations ]) (ConfigCheck.getIssues shared.configCheck)
            , viewErrorMessages shared.theme shared.browserEnv.translations shared.nostr
            ]
        ]
    }


viewLoginInfo : Shared.Model -> Html Msg
viewLoginInfo shared =
    let
        styles =
            stylesForTheme shared.theme
    in
    div []
        [ Html.h2
            (styles.textStyleH2
                ++ [ css
                        []
                   ]
            )
            [ text <| Translations.loginInfoTitle [ shared.browserEnv.translations ]
            ]
        , div
            []
            [ text <| loginStatusToString shared.browserEnv.translations shared.loginStatus
            ]
        ]


loginStatusToString : I18Next.Translations -> LoginStatus -> String
loginStatusToString translations loginStatus =
    case loginStatus of
        LoggedOut ->
            Translations.loggedOutStatusText [ translations ]

        LoggedInUnknown ->
            Translations.unknownLoginStatusText [ translations ]

        LoggedIn pubKey loginMethod ->
            let
                keyInfo =
                    Nip19.Npub pubKey
                    |> Nip19.encode
                    |> Result.toMaybe
                    |> Maybe.withDefault pubKey
            in
            Translations.loggedInStatusText [ translations ] ++ " " ++ keyInfo ++ " (" ++ loginMethodToString loginMethod ++ ")"


loginMethodToString : LoginMethod -> String
loginMethodToString loginMethod =
    case loginMethod of
        LoginMethodConnect ->
            "connect"

        LoginMethodExtension ->
            "extension"

        LoginMethodLocal ->
            "local"

        LoginMethodOther method ->
            method

        LoginMethodReadOnly ->
            "read only"


viewErrorMessages : Theme -> I18Next.Translations -> Nostr.Model -> Html Msg
viewErrorMessages theme translations nostr =
    let
        styles =
            stylesForTheme theme
    in
    case Nostr.getErrorMessages nostr of
        [] ->
            emptyHtml

        errorMessages ->
            div []
                [ Html.h2
                    (styles.textStyleH2
                        ++ [ css
                                []
                           ]
                    )
                    [ text <| Translations.errorMessagesTitle [ translations ]
                    ]
                , errorMessages
                    |> List.map
                        (\htmlText ->
                            htmlText
                                |> String.split "\n"
                                |> List.map Html.text
                                |> List.intersperse (Html.br [] [])
                                |> div []
                        )
                    |> div
                        [ css
                            [ Tw.flex
                            , Tw.flex_col
                            , Tw.gap_2
                            ]
                        ]
                ]
