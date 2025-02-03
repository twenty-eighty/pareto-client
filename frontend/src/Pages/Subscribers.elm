module Pages.Subscribers exposing (Model, Msg, page)

import Auth
import BrowserEnv exposing (BrowserEnv)
import Components.AlertTimerMessage as AlertTimerMessage
import Components.Button as Button
import Components.EmailImportDialog as EmailImportDialog
import Components.Icon as Icon
import Dict exposing (Dict)
import Effect exposing (Effect)
import FeatherIcons
import Html.Styled as Html exposing (Html, b, div, li, text, ul)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events as Events
import Json.Decode as Decode
import Layouts
import Nostr
import Nostr.Event exposing (Kind(..))
import Nostr.External
import Nostr.Request exposing (RequestData(..), RequestId)
import Nostr.Send exposing (SendRequest(..))
import Nostr.Types exposing (IncomingMessage)
import Page exposing (Page)
import Ports
import Route exposing (Route)
import Shared
import Shared.Model
import Shared.Msg
import Subscribers exposing (Email, Modification(..), Subscriber, modificationToString)
import Svg.Loaders as Loaders
import Tailwind.Theme as Theme
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
    , modifications : List Modification
    , requestId : RequestId
    , state : ModelState
    , subscribers : Dict Email Subscriber
    }


type ModelState
    = Loading
    | Loaded
    | ErrorLoadingSubscribers String
    | Modified
    | Saving
    | Saved


init : Auth.User -> Shared.Model -> () -> ( Model, Effect Msg )
init user shared () =
    ( { alertTimerMessage = AlertTimerMessage.init {}
      , emailImportDialog = EmailImportDialog.init {}
      , errors = []
      , modifications = []
      , requestId = Nostr.getLastRequestId shared.nostr
      , state = Loading
      , subscribers = Dict.empty
      }
    , [ Subscribers.load shared.nostr user.pubKey
      , Subscribers.loadModifications shared.nostr user.pubKey
      ]
        |> List.map Effect.sendSharedMsg
        |> Effect.batch
    )



-- UPDATE


type Msg
    = ImportClicked
    | ExportClicked
    | SaveClicked
    | ProcessModificationsClicked
    | AlertTimerMessageSent AlertTimerMessage.Msg
    | EmailImportDialogSent (EmailImportDialog.Msg Msg)
    | AddSubscribers (List Subscriber) Bool
    | RemoveSubscriber String
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
            , Subscribers.subscriberDataEvent shared.browserEnv user.pubKey (Dict.values model.subscribers)
                |> SendApplicationData
                |> Shared.Msg.SendNostrEvent
                |> Effect.sendSharedMsg
            )

        ProcessModificationsClicked ->
            ( { model | state = Modified, subscribers = Subscribers.processModifications model.subscribers model.modifications }, Effect.none )

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

        RemoveSubscriber email ->
            ( { model
                | state = Modified
                , subscribers = Subscribers.remove model.subscribers [ email ]
              }
            , Effect.none
            )

        ReceivedMessage message ->
            updateWithMessage user shared model message


updateWithMessage : Auth.User -> Shared.Model.Model -> Model -> IncomingMessage -> ( Model, Effect Msg )
updateWithMessage user shared model message =
    case message.messageType of
        "events" ->
            case Nostr.External.decodeRequestId message.value of
                Ok incomingRequestId ->
                    if model.requestId == incomingRequestId then
                        case Nostr.External.decodeEvents message.value of
                            Ok [] ->
                                ( { model | state = Loaded }, Effect.none )

                            Ok events ->
                                case Nostr.External.decodeEventsKind message.value of
                                    Ok KindApplicationSpecificData ->
                                        let
                                            ( subscribers, modifications, errors ) =
                                                Subscribers.processEvents user.pubKey model.subscribers model.modifications events
                                        in
                                        ( { model | state = Loaded, modifications = modifications, subscribers = subscribers, errors = model.errors ++ errors }, Effect.none )

                                    _ ->
                                        ( model, Effect.none )

                            Err error ->
                                ( { model | state = ErrorLoadingSubscribers (Decode.errorToString error) }, Effect.none )

                    else
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
            , viewSubscribers shared.browserEnv model
            , viewModifications shared.theme shared.browserEnv model
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


viewSubscribers : BrowserEnv -> Model -> Html Msg
viewSubscribers browserEnv model =
    case ( model.state, Dict.size model.subscribers ) of
        ( Loading, _ ) ->
            div
                [ css
                    [ Tw.flex
                    , Tw.flex_row
                    , Tw.gap_2
                    , Tw.m_2
                    ]
                ]
                [ text "Loading subscribers..."
                , Loaders.rings [] |> Html.fromUnstyled
                ]

        ( ErrorLoadingSubscribers error, _ ) ->
            div
                [ css
                    [ Tw.flex
                    , Tw.flex_row
                    , Tw.gap_2
                    , Tw.m_2
                    ]
                ]
                [ text <| "Error loading subscribers: " ++ error
                ]

        ( _, 0 ) ->
            div
                []
                [ text <| Translations.noSubscribersText [ browserEnv.translations ]
                ]

        ( _, _ ) ->
            ul
                [ css
                    [ Tw.flex
                    , Tw.flex_col
                    , Tw.m_2
                    ]
                ]
                (model.subscribers
                    |> Dict.values
                    |> List.map viewSubscriber
                )


viewSubscriber : Subscriber -> Html Msg
viewSubscriber subscriber =
    li
        [ css
            [ Tw.flex
            , Tw.flex_row
            , Tw.gap_2
            ]
        ]
        [ text subscriber.email
        , removeSubscriberButton (RemoveSubscriber subscriber.email)
        ]


removeSubscriberButton : Msg -> Html Msg
removeSubscriberButton removeMsg =
    div
        [ css
            [ Tw.text_color Theme.slate_500
            , Tw.cursor_pointer
            ]
        , Events.onClick removeMsg
        ]
        [ Icon.FeatherIcon FeatherIcons.delete
            |> Icon.view
        ]


viewModifications : Theme -> BrowserEnv -> Model -> Html Msg
viewModifications theme browserEnv model =
    let
        unprocessedModifications =
            model.modifications
                |> List.filter
                    (\modification ->
                        case modification of
                            Subscription { email } ->
                                not <| Dict.member email model.subscribers

                            Unsubscription { email } ->
                                Dict.member email model.subscribers
                    )
    in
    if List.length unprocessedModifications > 0 then
        div
            [ css
                [ Tw.flex
                , Tw.flex_col
                , Tw.gap_2
                , Tw.m_2
                ]
            ]
            [ b
                []
                [ text "Requested modifications" ]
            , Button.new
                { label = Translations.processModificationsButtonTitle [ browserEnv.translations ]
                , onClick = Just <| ProcessModificationsClicked
                , theme = theme
                }
                |> Button.withTypeSecondary
                |> Button.view
            , ul
                []
                (List.map viewModification unprocessedModifications)
            ]

    else
        div [] []


viewModification : Modification -> Html Msg
viewModification modification =
    case modification of
        Subscription subscriber ->
            text <| modificationToString modification ++ ": " ++ subscriber.email

        Unsubscription subscriber ->
            text <| modificationToString modification ++ ": " ++ subscriber.email
