module Pages.Subscribers exposing (Model, Msg, page)

import Auth
import BrowserEnv exposing (BrowserEnv)
import Components.AlertTimerMessage as AlertTimerMessage
import Components.Button as Button
import Components.EmailImportDialog as EmailImportDialog
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, div, text)
import Html.Styled.Attributes exposing (css)
import Layouts
import Nostr.Event exposing (Kind(..))
import Nostr.External
import Nostr.Request exposing (RequestData(..))
import Nostr.Send exposing (SendRequest(..))
import Nostr.Types exposing (IncomingMessage)
import Page exposing (Page)
import Ports
import Route exposing (Route)
import Shared
import Shared.Model
import Shared.Msg
import Subscribers exposing (Subscriber)
import Tailwind.Utilities as Tw
import Translations.Sidebar
import Translations.Subscribers as Translations
import Ui.Styles exposing (Theme)
import Ui.View exposing (ArticlePreviewType(..))
import View exposing (View)


page : Auth.User -> Shared.Model -> Route () -> Page Model Msg
page user shared _ =
    Page.new
        { init = init user shared
        , update = update user shared
        , subscriptions = subscriptions
        , view = view user shared
        }
        |> Page.withLayout (toLayout shared.theme)


toLayout : Theme -> Model -> Layouts.Layout Msg
toLayout theme _ =
    Layouts.Sidebar
        { styles = Ui.Styles.stylesForTheme theme }



-- INIT


type alias Model =
    { alertTimerMessage : AlertTimerMessage.Model
    , emailImportDialog : EmailImportDialog.Model
    , errors : List String
    , state : ModelState
    , subscribers : List Subscriber
    }


type ModelState
    = Loading
    | Loaded
    | Modified
    | Saving
    | Saved


init : Auth.User -> Shared.Model -> () -> ( Model, Effect Msg )
init user shared () =
    ( { alertTimerMessage = AlertTimerMessage.init {}
      , emailImportDialog = EmailImportDialog.init {}
      , errors = []
      , state = Loading
      , subscribers = []
      }
    , Subscribers.load shared.nostr user.pubKey
        |> Effect.sendSharedMsg
    )



-- UPDATE


type Msg
    = ImportClicked
    | ExportClicked
    | SaveClicked
    | AlertTimerMessageSent AlertTimerMessage.Msg
    | EmailImportDialogSent (EmailImportDialog.Msg Msg)
    | AddSubscribers (List Subscriber) Bool
    | ReceivedMessage IncomingMessage


update : Auth.User -> Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update user shared msg model =
    case msg of
        ImportClicked ->
            ( { model | emailImportDialog = EmailImportDialog.show model.emailImportDialog }, Effect.none )

        ExportClicked ->
            -- TODO: not yet implemented
            ( model, Effect.none )

        SaveClicked ->
            ( { model | state = Saving }
            , Subscribers.subscriberDataEvent shared.browserEnv user.pubKey model.subscribers
                |> SendApplicationData
                |> Shared.Msg.SendNostrEvent
                |> Effect.sendSharedMsg
            )

        AlertTimerMessageSent innerMsg ->
            AlertTimerMessage.update
                { msg = innerMsg
                , model = model.alertTimerMessage
                , toModel = \alertTimerMessage -> { model | alertTimerMessage = alertTimerMessage }
                , toMsg = AlertTimerMessageSent
                }

        EmailImportDialogSent innerMsg ->
            EmailImportDialog.update
                { msg = innerMsg
                , model = model.emailImportDialog
                , browserEnv = shared.browserEnv
                , nostr = shared.nostr
                , onImport = AddSubscribers
                , pubKey = user.pubKey
                , toModel = \emailImportDialog -> { model | emailImportDialog = emailImportDialog }
                , toMsg = EmailImportDialogSent
                }

        AddSubscribers subscribers overwriteExisting ->
            ( { model
                | emailImportDialog = EmailImportDialog.hide model.emailImportDialog
                , state = Modified
                , subscribers = Subscribers.merge overwriteExisting model.subscribers subscribers
              }
            , Effect.none
            )

        ReceivedMessage message ->
            updateWithMessage user shared model message


updateWithMessage : Auth.User -> Shared.Model.Model -> Model -> IncomingMessage -> ( Model, Effect Msg )
updateWithMessage user shared model message =
    case message.messageType of
        "events" ->
            case Nostr.External.decodeEventsKind message.value of
                Ok KindApplicationSpecificData ->
                    case Nostr.External.decodeEvents message.value of
                        Ok events ->
                            let
                                ( subscribers, errors ) =
                                    Subscribers.processEvents events
                            in
                            ( { model | subscribers = subscribers, errors = model.errors ++ errors }, Effect.none )

                        _ ->
                            ( model, Effect.none )

                _ ->
                    ( model, Effect.none )

        "published" ->
            -- currently this page only publishes the list of subscribers so we don't have to check details
            update user shared (AlertTimerMessageSent (AlertTimerMessage.AddMessage "saved subscribers sucessfully" 1000)) { model | state = Saved }

        "error" ->
            -- currently this page only publishes the list of subscribers so we don't have to check details
            update user shared (AlertTimerMessageSent (AlertTimerMessage.AddMessage "error saving subscribers" 2000)) { model | state = Modified }

        _ ->
            ( model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.receiveMessage ReceivedMessage



-- VIEW


view : Auth.User -> Shared.Model.Model -> Model -> View Msg
view user shared model =
    { title = Translations.Sidebar.subscribersMenuItemText [ shared.browserEnv.translations ]
    , body =
        [ div
            [ css
                [ Tw.flex
                , Tw.flex_col
                , Tw.gap_2
                , Tw.m_2
                ]
            ]
            [ div
                [ css
                    [ Tw.flex
                    , Tw.flex_row
                    , Tw.gap_2
                    , Tw.m_2
                    ]
                ]
                [ Button.new
                    { label = Translations.importButtonTitle [ shared.browserEnv.translations ]
                    , onClick = Just <| ImportClicked
                    , theme = shared.theme
                    }
                    |> Button.withTypePrimary
                    |> Button.view
                , Button.new
                    { label = Translations.exportButtonTitle [ shared.browserEnv.translations ]
                    , onClick = Just <| ExportClicked
                    , theme = shared.theme
                    }
                    |> Button.withTypePrimary
                    |> Button.withDisabled True
                    |> Button.view
                , Button.new
                    { label = Translations.saveButtonTitle [ shared.browserEnv.translations ]
                    , onClick = Just <| SaveClicked
                    , theme = shared.theme
                    }
                    |> Button.withTypePrimary
                    |> Button.withDisabled (model.state /= Modified)
                    |> Button.view
                ]
            , viewSubscribers shared.browserEnv model.subscribers
            , EmailImportDialog.new
                { model = model.emailImportDialog
                , toMsg = EmailImportDialogSent
                , nostr = shared.nostr
                , pubKey = user.pubKey
                , browserEnv = shared.browserEnv
                , theme = shared.theme
                }
                |> EmailImportDialog.view
            , AlertTimerMessage.new
                { model = model.alertTimerMessage
                , theme = shared.theme
                }
                |> AlertTimerMessage.view
            ]
        ]
    }


viewSubscribers : BrowserEnv -> List Subscriber -> Html Msg
viewSubscribers browserEnv subscribers =
    case subscribers of
        [] ->
            div
                []
                [ text <| Translations.noSubscribersText [ browserEnv.translations ]
                ]

        _ ->
            div
                [ css
                    [ Tw.flex
                    , Tw.flex_col
                    ]
                ]
                (List.map viewSubscriber subscribers)


viewSubscriber : Subscriber -> Html Msg
viewSubscriber subscriber =
    div
        [ css
            [ Tw.flex
            , Tw.flex_row
            ]
        ]
        [ text subscriber.email ]
