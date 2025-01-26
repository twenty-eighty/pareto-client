module Pages.SignIn exposing (Model, Msg, init, page, subscriptions, update, view)

import Dict
import Effect exposing (Effect)
import Html.Styled as Html exposing (a, div, text)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events as Events exposing (..)
import Layouts
import Layouts.Sidebar
import Nostr.Types exposing (IncomingMessage)
import Page exposing (Page)
import Ports
import Route exposing (Route)
import Route.Path
import Shared
import Shared.Model exposing (ClientRole(..))
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
    Layouts.Sidebar
        { styles = Ui.Styles.stylesForTheme theme }


type alias Model =
    { from : Maybe Route.Path.Path
    , clientRole : Maybe ClientRole
    }


init : Shared.Model -> Route () -> () -> ( Model, Effect Msg )
init shared route () =
    let
        from =
            Dict.get "from" route.query
                |> Maybe.andThen Route.Path.fromString
    in
    ( { from =
            from
      , clientRole =
            from
                |> Maybe.map (Layouts.Sidebar.clientRoleForRoutePath shared.browserEnv.environment)
      }
    , Effect.sendCmd Ports.loginSignUp
    )


type Msg
    = ReceivedPortMessage IncomingMessage
    | TriggerLoginSignup


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        ReceivedPortMessage portMessage ->
            updateWithPortMessage shared model portMessage

        TriggerLoginSignup ->
            ( model, Effect.sendCmd Ports.requestUser )


updateWithPortMessage : Shared.Model -> Model -> IncomingMessage -> ( Model, Effect Msg )
updateWithPortMessage _ model portMessage =
    case portMessage.messageType of
        "user" ->
            case ( model.from, model.clientRole ) of
                ( Just from, Just clientRole ) ->
                    ( model
                    , Effect.batch
                        [ Effect.sendSharedMsg (Shared.Msg.SetClientRole clientRole)
                        , Effect.pushRoutePath from
                        ]
                    )

                ( _, _ ) ->
                    ( model, Effect.pushRoutePath Route.Path.Read )

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
