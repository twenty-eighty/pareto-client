module Components.SubscriberEditDialog exposing (Model, Msg, SubscriberEditDialog, hide, init, new, show, subscriptions, update, view, withTags)

import BrowserEnv exposing (BrowserEnv)
import Components.Button as Button
import Components.Checkbox as Checkbox
import Components.EntryField as EntryField
import Components.ModalDialog as ModalDialog
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, div, text)
import Html.Styled.Attributes exposing (css)
import Locale exposing (Language(..))
import Newsletters.Subscribers as Subscribers
import Newsletters.Types exposing (Subscriber, SubscriberField(..))
import Shared.Model exposing (Model)
import Shared.Msg exposing (Msg)
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
        , tags : List String
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
        , tags = []
        }


withTags : List String -> SubscriberEditDialog msg -> SubscriberEditDialog msg
withTags tags (Settings settings) =
    Settings { settings | tags = tags }


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
                            [ entryField settings.theme settings.browserEnv FieldEmail emailSubscriptionData.subscriber
                            , entryField settings.theme settings.browserEnv FieldFirstName emailSubscriptionData.subscriber
                            , entryField settings.theme settings.browserEnv FieldLastName emailSubscriptionData.subscriber
                            , Checkbox.new
                                { label = (Subscribers.translatedFieldName settings.browserEnv.translations FieldDnd)
                                , onClick = (\value -> { subscriber | dnd = Just value } |> UpdateSubscriber)
                                , checked = subscriber.dnd |> Maybe.withDefault False
                                , theme = settings.theme
                                }
                                |> Checkbox.view
                            , viewTags settings.theme settings.browserEnv settings.tags subscriber
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


viewTags : Theme -> BrowserEnv -> List String -> Subscriber -> Html Msg
viewTags theme browserEnv availableTags subscriber =
    let
        selected =
            subscriber.tags |> Maybe.withDefault []

        tags =
            uniqueSorted (selected ++ availableTags)
    in
    case tags of
        [] ->
            emptyHtml

        _ ->
            div
                [ css
                    [ Tw.flex
                    , Tw.flex_col
                    , Tw.gap_2
                    ]
                ]
                [ text <| Subscribers.translatedFieldName browserEnv.translations FieldTags
                , div
                    [ css
                        [ Tw.flex
                        , Tw.flex_row
                        , Tw.flex_wrap
                        , Tw.gap_x_4
                        , Tw.gap_y_1
                        ]
                    ]
                    (List.map (tagCheckbox theme selected subscriber) tags)
                ]


tagCheckbox : Theme -> List String -> Subscriber -> String -> Html Msg
tagCheckbox theme selected subscriber tag =
    Checkbox.new
        { label = tag
        , onClick = \checked -> UpdateSubscriber (setTag checked tag subscriber)
        , checked = List.member tag selected
        , theme = theme
        }
        |> Checkbox.view


setTag : Bool -> String -> Subscriber -> Subscriber
setTag checked tag subscriber =
    let
        selected =
            subscriber.tags |> Maybe.withDefault []

        next =
            if checked then
                tag :: List.filter (\existing -> existing /= tag) selected

            else
                List.filter (\existing -> existing /= tag) selected
    in
    { subscriber
        | tags =
            case uniqueSorted next of
                [] ->
                    Nothing

                tags ->
                    Just tags
    }


uniqueSorted : List String -> List String
uniqueSorted values =
    values
        |> List.filter (\value -> String.trim value /= "")
        |> List.sort
        |> List.foldl
            (\value acc ->
                case acc of
                    latest :: _ ->
                        if latest == value then
                            acc

                        else
                            value :: acc

                    [] ->
                        [ value ]
            )
            []
        |> List.reverse


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

