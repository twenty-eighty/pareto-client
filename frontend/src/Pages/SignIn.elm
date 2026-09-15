module Pages.SignIn exposing (Model, Msg, init, page, subscriptions, update, view)

import Dict exposing (Dict)
import Effect exposing (Effect)
import Html.Styled as Html exposing (a, div, text)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events as Events exposing (..)
import Layouts
import Layouts.Sidebar
import Nostr.Types exposing (IncomingMessage, loggedInPubKey)
import Page exposing (Page)
import Ports
import Route exposing (Route)
import Route.Path
import Shared
import Shared.Model exposing (ClientRole)
import Shared.Msg
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
toLayout theme _ =
    Layouts.Sidebar.new
        { theme = theme
        }
        |> Layouts.Sidebar


type alias Model =
    { from : Maybe Route.Path.Path
    , hash : Maybe String
    , query : Dict String String
    , clientRole : Maybe ClientRole
    }


init : Shared.Model -> Route () -> () -> ( Model, Effect Msg )
init shared route () =
    let
        from =
            Dict.get fromParamName route.query
                |> Maybe.andThen Route.Path.fromString

        maybeNsec =
            Dict.get nsecParamName route.query

        -- Return destination query without SignIn-only secrets / control params.
        -- Keep `from` in the browser URL so remounts still know where to go.
        returnQuery =
            route.query
                |> Dict.remove nsecParamName
                |> Dict.remove fromParamName
                |> Dict.remove "confirmed"

        urlQuery =
            route.query
                |> Dict.remove nsecParamName
                |> Dict.remove "confirmed"

        clientRole =
            from
                |> Maybe.map (Layouts.Sidebar.clientRoleForRoutePath shared.browserEnv.environment)

        model =
            { from = from
            , hash = route.hash
            , query = returnQuery
            , clientRole = clientRole
            }

        cleanUrlEffect =
            Effect.pushRoute
                { path = route.path
                , query = urlQuery
                , hash = route.hash
                }

        redirectAfterLoginEffect =
            redirectToDestination shared model
    in
    case loggedInPubKey shared.loginStatus of
        Just _ ->
            -- Already signed in (e.g. session restored after a mistaken bounce here).
            ( model
            , Effect.batch [ cleanUrlEffect, redirectAfterLoginEffect ]
            )

        Nothing ->
            let
                loginEffect =
                    case maybeNsec of
                        Just nsec ->
                            Effect.sendCmd (Ports.login nsec)

                        Nothing ->
                            if Dict.member "confirmed" route.query then
                                Effect.sendSharedMsg Shared.Msg.TriggerEmailLogin

                            else
                                Effect.sendSharedMsg Shared.Msg.TriggerLogin
            in
            ( model
            , Effect.batch [ loginEffect, cleanUrlEffect ]
            )


fromParamName : String
fromParamName =
    "from"


nsecParamName : String
nsecParamName =
    "nsec"


type Msg
    = ReceivedPortMessage IncomingMessage
    | TriggerLoginSignup


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        ReceivedPortMessage portMessage ->
            updateWithPortMessage shared model portMessage

        TriggerLoginSignup ->
            ( model, Effect.sendSharedMsg Shared.Msg.TriggerLogin )


redirectToDestination : Shared.Model -> Model -> Effect Msg
redirectToDestination shared model =
    case model.from of
        Just from ->
            let
                clientRole =
                    model.clientRole
                        |> Maybe.withDefault
                            (Layouts.Sidebar.clientRoleForRoutePath shared.browserEnv.environment from)
            in
            Effect.batch
                [ Effect.sendSharedMsg (Shared.Msg.SetClientRole False clientRole)
                , Effect.pushRoute { path = from, query = model.query, hash = model.hash }
                ]

        Nothing ->
            Effect.pushRoutePath Route.Path.Read


updateWithPortMessage : Shared.Model -> Model -> IncomingMessage -> ( Model, Effect Msg )
updateWithPortMessage shared model portMessage =
    case portMessage.messageType of
        "user" ->
            ( model, redirectToDestination shared model )

        _ ->
            ( model, Effect.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.receiveMessage ReceivedPortMessage


view : Shared.Model -> Model -> View Msg
view shared _ =
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
                        ]
                   ]
            )
            [ div
                (styles.textStyleH1
                    ++ [ Events.onClick TriggerLoginSignup
                       , css
                            [ Tw.cursor_pointer
                            ]
                       ]
                )
                [ text <| Translations.signInRequest [ shared.browserEnv.translations ]
                ]
            , a
                (styles.textStyleH3
                    ++ [ Attr.href <| Route.Path.toString Route.Path.Read
                       ]
                )
                [ text <| Translations.continueWithoutSigningInMessage [ shared.browserEnv.translations ]
                ]
            ]
        ]
    }
