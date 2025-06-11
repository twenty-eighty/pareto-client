module Components.SubscriberEditDialog exposing (Model, Msg, SubscriberEditDialog, hide, init, new, show, subscriptions, update, view)

import BrowserEnv exposing (BrowserEnv)
import Components.Button as Button
import Components.Checkbox as Checkbox
import Components.EntryField as EntryField
import Components.ModalDialog as ModalDialog
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, div)
import Html.Styled.Attributes exposing (css)
import Locale exposing (Language(..))
import Shared.Model exposing (Model)
import Shared.Msg exposing (Msg)
import Subscribers exposing (Subscriber, SubscriberField(..))
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

                subscriber =
                    emailSubscriptionData.subscriber
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
                            [ entryField settings.theme settings.browserEnv Subscribers.FieldEmail emailSubscriptionData.subscriber
                            , entryField settings.theme settings.browserEnv Subscribers.FieldFirstName emailSubscriptionData.subscriber
                            , entryField settings.theme settings.browserEnv Subscribers.FieldLastName emailSubscriptionData.subscriber
                            , Checkbox.new
                                { label = (Subscribers.translatedFieldName settings.browserEnv.translations FieldDnd)
                                , onClick = (\value -> { subscriber | dnd = Just value } |> UpdateSubscriber)
                                , checked = subscriber.dnd |> Maybe.withDefault False
                                , theme = settings.theme
                                }
                                |> Checkbox.view
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


entryField : Theme -> BrowserEnv -> SubscriberField -> Subscriber -> Html Msg
entryField theme browserEnv field subscriber =
    EntryField.new
        { value = (Subscribers.subscriberValue browserEnv subscriber field)
        , onInput = (\value -> Subscribers.setSubscriberField field value subscriber |> UpdateSubscriber)
        , theme = theme
        }
        |> EntryField.withLabel (Subscribers.translatedFieldName browserEnv.translations field)
        |> EntryField.withType (entryFieldType field)
        |> EntryField.view

 
entryFieldType : SubscriberField -> EntryField.FieldType
entryFieldType field =
    case field of
        FieldDnd -> EntryField.FieldTypeText
        FieldEmail -> EntryField.FieldTypeEmail
        FieldName -> EntryField.FieldTypeText
        FieldFirstName -> EntryField.FieldTypeText
        FieldLastName -> EntryField.FieldTypeText
        FieldPubKey -> EntryField.FieldTypeText
        FieldSource -> EntryField.FieldTypeText
        FieldDateSubscription -> EntryField.FieldTypeDate
        FieldDateUnsubscription -> EntryField.FieldTypeDate
        FieldTags -> EntryField.FieldTypeText
        FieldUndeliverable -> EntryField.FieldTypeText
        FieldLocale -> EntryField.FieldTypeText

