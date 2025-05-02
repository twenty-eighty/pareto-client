module Components.SubscriberEditDialog exposing (Model, Msg, SubscriberEditDialog, hide, init, new, show, subscriptions, update, view)

import BrowserEnv exposing (BrowserEnv)
import Components.Button as Button
import Components.ModalDialog as ModalDialog
import Css
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, div, input, label, text)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events as Events
import Locale exposing (Language(..))
import Shared.Model exposing (Model)
import Shared.Msg exposing (Msg)
import Subscribers exposing (Subscriber, SubscriberField(..))
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw
import Translations.SubscriberEditDialog as Translations
import Ui.Shared exposing (emptyHtml)
import Ui.Styles exposing (Theme(..))


type Msg
    = CloseDialog
    | UpdateSubscriber Subscriber
    | SubmitSubscriber


type Model
    = Model
        { state : DialogState
        }


type DialogState
    = DialogHidden
    | DialogVisible EmailSubscriptionData


type alias EmailSubscriptionData =
    { email : String
    , subscriber : Subscriber
    }


type SubscriberEditDialog msg
    = Settings
        { model : Model
        , toMsg : Msg -> msg
        , browserEnv : BrowserEnv
        , theme : Theme
        }


new :
    { model : Model
    , toMsg : Msg -> msg
    , browserEnv : BrowserEnv
    , theme : Theme
    }
    -> SubscriberEditDialog msg
new props =
    Settings
        { model = props.model
        , toMsg = props.toMsg
        , browserEnv = props.browserEnv
        , theme = props.theme
        }


init : {} -> Model
init _ =
    Model
        { state = DialogHidden
        }


show : Model -> Subscriber -> Model
show (Model model) subscriber =
    Model { model | state = DialogVisible { email = subscriber.email, subscriber = subscriber } }


hide : Model -> Model
hide (Model model) =
    Model { model | state = DialogHidden }


update :
    { msg : Msg
    , model : Model
    , toModel : Model -> model
    , toMsg : Msg -> msg
    , submit : String -> Subscriber -> msg
    }
    -> ( model, Effect msg )
update props =
    let
        (Model model) =
            props.model

        toParentModel : ( Model, Effect msg ) -> ( model, Effect msg )
        toParentModel ( innerModel, effect ) =
            ( props.toModel innerModel
            , effect
            )
    in
    toParentModel <|
        case props.msg of
            CloseDialog ->
                ( Model { model | state = DialogHidden }
                , Effect.none
                )

            UpdateSubscriber subscriber ->
                case model.state of
                    DialogVisible emailSubscriptionData ->
                        ( Model { model | state = DialogVisible { emailSubscriptionData | subscriber = subscriber } }, Effect.none )

                    _ ->
                        ( Model model, Effect.none )

            SubmitSubscriber ->
                case model.state of
                    DialogVisible emailSubscriptionData ->
                        ( Model { model | state = DialogHidden }
                        , Effect.sendMsg <| props.submit emailSubscriptionData.email emailSubscriptionData.subscriber
                        )

                    _ ->
                        ( Model model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : SubscriberEditDialog msg -> Html msg
view dialog =
    let
        (Settings settings) =
            dialog

        (Model model) =
            settings.model
    in
    case model.state of
        DialogHidden ->
            emptyHtml

        DialogVisible emailSubscriptionData ->
            let
                emailIsValid =
                    Subscribers.emailValid emailSubscriptionData.subscriber.email
            in
            ModalDialog.new
                { title = Translations.dialogTitle [ settings.browserEnv.translations ]
                , content =
                    [ div
                        [ css
                            [ Tw.w_full
                            , Tw.max_w_sm
                            , Tw.mt_2
                            ]
                        ]
                        [ div
                            [ css
                                [ Tw.flex
                                , Tw.flex_col
                                , Tw.gap_3
                                ]
                            ]
                            [ entryField settings.browserEnv Subscribers.FieldEmail emailSubscriptionData.subscriber
                            , entryField settings.browserEnv Subscribers.FieldFirstName emailSubscriptionData.subscriber
                            , entryField settings.browserEnv Subscribers.FieldLastName emailSubscriptionData.subscriber
                            ]
                        ]
                    ]
                , onClose = CloseDialog
                , theme = settings.theme
                , buttons =
                    [ Button.new
                        { label = Translations.submitButtonTitle [ settings.browserEnv.translations ]
                        , onClick = Just SubmitSubscriber
                        , theme = settings.theme
                        }
                        |> Button.withTypePrimary
                        |> Button.withDisabled (not emailIsValid)
                        |> Button.view
                    ]
                }
                |> ModalDialog.view
                |> Html.map settings.toMsg


entryField : BrowserEnv -> SubscriberField -> Subscriber -> Html Msg
entryField browserEnv field subscriber =
    let
        styles =
            Ui.Styles.stylesForTheme ParetoTheme
    in
    div
        [ css
            [ Tw.mb_1
            ]
        ]
        [ label
            ([ Attr.for (Subscribers.fieldName field)
             , css
                [ Tw.block
                , Tw.mb_2
                , Tw.text_sm
                , Tw.font_medium
                ]
             ]
                ++ styles.colorStyleLabel
            )
            [ text <| Subscribers.translatedFieldName browserEnv.translations field ]
        , input
            ([ Attr.type_ "email"
             , Attr.id (Subscribers.fieldName field)
             , Attr.name "email"
             , css
                [ Tw.w_full
                , Tw.px_4
                , Tw.py_2
                , Tw.border
                , Tw.rounded
                , Css.focus
                    [ Tw.outline_none
                    , Tw.ring_2
                    , Tw.ring_color Theme.blue_500
                    ]
                ]
             , Attr.required True
             , Attr.value (Subscribers.subscriberValue browserEnv subscriber field)
             , Events.onInput
                (\value ->
                    Subscribers.setSubscriberField field value subscriber
                        |> UpdateSubscriber
                )
             ]
                ++ styles.colorStyleBackground
                ++ styles.colorStyleGrayscaleText
                ++ styles.colorStyleBorders
            )
            []
        ]
