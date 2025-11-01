module Components.SendNewsletterDialog exposing (Model, Msg(..), NewsletterData, SendNewsletterDialog, hide, init, new, show, subscriptions, update, view)

import BrowserEnv exposing (BrowserEnv, Environment(..))
import Components.Button as Button
import Components.Checkbox as Checkbox
import Components.CriteriaBuilder as CriteriaBuilder
import Components.Dropdown as Dropdown
import Components.EntryField as EntryField
import Components.Icon as Icon
import Components.ModalDialog as ModalDialog
import Dict exposing (Dict)
import Effect exposing (Effect)
import FeatherIcons
import Html.Styled as Html exposing (Html, div, h2, li, p, text, ul)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events exposing (..)
import I18Next
import Json.Decode as Decode
import Newsletters.ContactDatabase as ContactDatabase
import Nostr
import Nostr.Event exposing (Kind(..))
import Nostr.External
import Nostr.Relay as Relay exposing (Relay)
import Nostr.RelayListMetadata exposing (RelayMetadata, eventWithRelayList, extendRelayList)
import Nostr.Send exposing (SendRequest(..))
import Nostr.Types exposing (IncomingMessage, PubKey, RelayRole(..), RelayUrl)
import Pareto
import Ports
import Shared.Model exposing (Model)
import Shared.Msg exposing (Msg)
import Newsletters.Subscribers as Subscribers
import String
import Tailwind.Utilities as Tw
import Translations.SendNewsletterDialog as Translations
import Ui.Shared exposing (emptyHtml)
import Ui.Styles exposing (Theme(..))


type Msg msg
    = CloseDialog
    | SendClicked
    | ReceivedMessage IncomingMessage
    | ContactDatabaseMsg ContactDatabase.Msg
    | CriteriaBuilderSent CriteriaBuilder.Msg
    | UpdateTestEmail String
    | SubmitTestEmail String


type ExistingStatus
    = StatusUnknown
    | StatusChecking
    | StatusFound Decode.Value
    | StatusNotFound
    | StatusError String


type alias NewsletterData =
    { author : PubKey
    , title : String
    , summary : String
    , content : String
    , imageUrl : String
    , language : Maybe String
    , identifier : String
    , test : Bool
    }


type NewsletterStatusResponse
    = NewsletterStatus NewsletterStatusPayload
    | NewsletterStatusError String

type alias NewsletterStatusPayload =
    { identifier : Maybe String
    , status : Maybe Decode.Value
    , exists : Maybe Bool
    }


type alias StatusSummary =
    { state : Maybe String
    , updatedAt : Maybe String
    , expectedJobs : Maybe Int
    , uploadedJobs : Maybe Int
    }


type Model
    = Model
        { contactDatabase : ContactDatabase.Model
        , criteriaBuilder : CriteriaBuilder.Model
        , errors : List String
        , state : DialogState
        , testEmailState : TestEmailState
        , existingStatus : ExistingStatus
        , recipientCount : Maybe Int
        }

type TestEmailState
    = TestEmailEmpty
    | TestEmailEditing String
    | TestEmailSending String
    | TestEmailSent
    | TestEmailError String

type DialogState
    = DialogHidden
    | DialogPreparation NewsletterData
    | DialogSending
    | DialogSent
    | DialogError String



type SendNewsletterDialog msg
    = Settings
        { model : Model
        , toMsg : Msg msg -> msg
        , pubKey : PubKey
        , browserEnv : BrowserEnv
        , theme : Theme
        }


new :
    { model : Model
    , toMsg : Msg msg -> msg
    , pubKey : PubKey
    , browserEnv : BrowserEnv
    , theme : Theme
    }
    -> SendNewsletterDialog msg
new props =
    Settings
        { model = props.model
        , toMsg = props.toMsg
        , pubKey = props.pubKey
        , browserEnv = props.browserEnv
        , theme = props.theme
        }


init : { pubKey : PubKey } -> ( Model, Effect (Msg msg) )
init props =
    let
        ( contactDatabase, contactDatabaseEffects ) =
            ContactDatabase.init props.pubKey [ ContactDatabase.LoadTags ]
    in
    ( Model
        { contactDatabase = contactDatabase
        , criteriaBuilder = CriteriaBuilder.init {}
        , errors = []
        , state = DialogHidden
        , testEmailState = TestEmailEmpty
        , existingStatus = StatusUnknown
        , recipientCount = Nothing
        }
    , contactDatabaseEffects
        |> Effect.map ContactDatabaseMsg
    )

hide : Model -> Model
hide (Model model) =
    Model { model | state = DialogHidden, existingStatus = StatusUnknown, recipientCount = Nothing }


show : Model -> NewsletterData -> (Model, Effect (Msg msg))
show (Model model) newsletterData =
    ( Model
        { model
            | state = DialogPreparation newsletterData
            , existingStatus = StatusChecking
            , recipientCount = Nothing
        }
    , Effect.batch
        [ Ports.getNewsletterStatus newsletterData.author newsletterData.identifier
            |> Effect.sendCmd
        , Ports.getNewsletterRecipientCount newsletterData.author
            |> Effect.sendCmd
        ]
    )


update :
    { msg : Msg msg
    , model : Model
    , toModel : Model -> model
    , toMsg : Msg msg -> msg
    , nostr : Nostr.Model
    , pubKey : PubKey
    , testMode : BrowserEnv.TestMode
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
                ( hide (Model model)
                , Effect.none
                )

            SendClicked ->
                case model.state of
                    DialogPreparation newsletterData ->
                        ( Model { model | state = DialogSending }
                        , Ports.sendNewsletter newsletterData
                            |> Effect.sendCmd
                        )

                    _ ->
                        ( Model model, Effect.none )

            ReceivedMessage message ->
                updateWithMessage (Model model) props.pubKey message

            ContactDatabaseMsg msg ->
                let
                    ( contactDatabase, contactDatabaseEffects ) =
                        ContactDatabase.update msg model.contactDatabase
                in
                ( Model { model | contactDatabase = contactDatabase }
                , contactDatabaseEffects
                    |> Effect.map ContactDatabaseMsg
                    |> Effect.map props.toMsg
                )

            CriteriaBuilderSent msg ->
                CriteriaBuilder.update
                    { msg = msg
                    , model = model.criteriaBuilder
                    , toModel = \criteriaBuilder -> Model { model | criteriaBuilder = criteriaBuilder }
                    , toMsg = CriteriaBuilderSent >> props.toMsg
                    }

            UpdateTestEmail email ->
                if email /= "" then
                    ( Model { model | testEmailState = TestEmailEditing email } , Effect.none)

                else
                    ( Model { model | testEmailState = TestEmailEmpty } , Effect.none)

            SubmitTestEmail email ->
                case model.state of
                    DialogPreparation newsletterData ->
                        ( Model { model | testEmailState = TestEmailSending email }
                        , sendTestEmail email newsletterData
                            |> Effect.sendCmd
                        )

                    _ ->
                        ( Model model, Effect.none )


newsletterStatusDecoder : Decode.Decoder NewsletterStatusResponse
newsletterStatusDecoder =
    Decode.oneOf [
        Decode.map NewsletterStatus (Decode.map3 NewsletterStatusPayload
            (Decode.field "identifier" (Decode.nullable Decode.string))
            (Decode.field "status" (Decode.nullable Decode.value))
            (Decode.field "exists" (Decode.maybe Decode.bool))
        )
        , Decode.map NewsletterStatusError (Decode.field "error" Decode.string)
    ]


statusSummaryDecoder : Decode.Decoder StatusSummary
statusSummaryDecoder =
    Decode.map4 StatusSummary
        (Decode.maybe (Decode.field "state" Decode.string))
        (Decode.maybe (Decode.field "updated_at" Decode.string))
        (Decode.maybe (Decode.field "expected_jobs" Decode.int))
        (Decode.maybe (Decode.field "uploaded_jobs" Decode.int))


sendTestEmail : String -> NewsletterData -> Cmd msg
sendTestEmail email newsletterData =
    Ports.sendNewsletterTest email newsletterData


updateWithMessage : Model -> PubKey -> IncomingMessage -> ( Model, Effect msg )
updateWithMessage (Model model) userPubKey message =
    case message.messageType of
        "newsletterTestProgress" ->
            case Decode.decodeValue Decode.float message.value of
                Ok progress ->
                    if progress >= 1.0 then
                        ( Model { model | testEmailState = TestEmailSent }, Effect.none )

                    else
                        ( Model model, Effect.none )

                Err _ ->
                    ( Model model, Effect.none )

        "newsletterStatus" ->
            case Decode.decodeValue newsletterStatusDecoder message.value of
                Ok (NewsletterStatus payload) ->
                    let
                        nextStatus =
                            case payload.status of
                                Just value ->
                                    StatusFound value

                                Nothing ->
                                    StatusNotFound
                    in
                    ( Model { model | existingStatus = nextStatus }, Effect.none )

                Ok (NewsletterStatusError errorMsg) ->
                    ( Model { model | existingStatus = StatusError errorMsg }, Effect.none )

                Err decodeError ->
                    ( Model { model | existingStatus = StatusError (Decode.errorToString decodeError) }, Effect.none )

        "newsletterRecipientCount" ->
            case Decode.decodeValue (Decode.field "count" (Decode.nullable Decode.int)) message.value of
                Ok maybeCount ->
                    ( Model { model | recipientCount = maybeCount }, Effect.none )

                Err _ ->
                    case Decode.decodeValue (Decode.field "error" Decode.string) message.value of
                        Ok errorMsg ->
                            ( Model { model | recipientCount = Nothing, errors = errorMsg :: model.errors }, Effect.none )

                        Err _ ->
                            ( Model model, Effect.none )

        _ ->
            ( Model model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> (Msg msg -> msg) -> Sub msg
subscriptions (Model model) toMsg =
    Sub.batch
        [ Ports.receiveMessage ReceivedMessage
            |> Sub.map toMsg
        , ContactDatabase.subscriptions model.contactDatabase
            |> Sub.map ContactDatabaseMsg
            |> Sub.map toMsg
        ]


-- VIEW


view : SendNewsletterDialog msg -> Html msg
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

        DialogPreparation newsletterData ->
            viewPreparationDialog dialog newsletterData

        DialogSending ->
            viewSendingDialog dialog

        DialogSent ->
            viewSentDialog dialog
            
        DialogError error ->
            viewErrorDialog dialog error


viewPreparationDialog : SendNewsletterDialog msg -> NewsletterData -> Html msg
viewPreparationDialog dialog newsletterData =
    let
        (Settings settings) =
            dialog
    in
    ModalDialog.new
        { title = Translations.dialogTitle [ settings.browserEnv.translations ]
        , content =
            [ viewSendNewsletterDialog dialog
            ]
        , onClose = CloseDialog
        , theme = settings.theme
        , buttons =
            [ Button.new
                { label = Translations.sendButtonTitle [ settings.browserEnv.translations ]
                , onClick = Just SendClicked
                , theme = settings.theme
                }
                |> Button.withTypePrimary
                |> Button.withDisabled (numberOfRecipients settings.model < 1)
                |> Button.view
            ]
        }
        |> ModalDialog.view
        |> Html.map settings.toMsg

numberOfRecipients : Model -> Int
numberOfRecipients (Model model) =
    Maybe.withDefault 0 model.recipientCount


viewSendingDialog : SendNewsletterDialog msg -> Html msg
viewSendingDialog dialog =
    let
        (Settings settings) =
            dialog
    in
    ModalDialog.new
        { title = Translations.dialogTitle [ settings.browserEnv.translations ]
        , content = [ viewSendNewsletterDialog dialog ]
        , onClose = CloseDialog
        , theme = settings.theme
        , buttons = [ ]
        }
        |> ModalDialog.view
        |> Html.map settings.toMsg


viewSentDialog : SendNewsletterDialog msg -> Html msg
viewSentDialog dialog =
    let
        (Settings settings) =
            dialog
    in
    ModalDialog.new
        { title = Translations.dialogTitle [ settings.browserEnv.translations ]
        , content = [ viewSendNewsletterDialog dialog ]
        , onClose = CloseDialog
        , theme = settings.theme
        , buttons = [ ]
        }
        |> ModalDialog.view
        |> Html.map settings.toMsg

viewErrorDialog : SendNewsletterDialog msg -> String -> Html msg
viewErrorDialog dialog error =
    let
        (Settings settings) =
            dialog
    in
    ModalDialog.new
        { title = Translations.dialogTitle [ settings.browserEnv.translations ]
        , content = [ viewSendNewsletterDialog dialog ]
        , onClose = CloseDialog
        , theme = settings.theme
        , buttons = [  ]
        }
        |> ModalDialog.view
        |> Html.map settings.toMsg

viewSendNewsletterDialog : SendNewsletterDialog msg -> Html (Msg msg)
viewSendNewsletterDialog (Settings settings) =
    let
        (Model model) =
            settings.model
    in
    div
        [ css
            [ Tw.my_4
            , Tw.flex
            , Tw.flex_col
            , Tw.justify_start
            , Tw.gap_2
            ]
        ]
        [ viewNewsletterStatus (Settings settings)
        , viewTestEmailField (Settings settings)
        , viewRecipientCount (Settings settings)
{-
          text <| Translations.sendNewsletterToCriteriaBuilderText [ settings.browserEnv.translations ]
        , CriteriaBuilder.new
            { model = model.criteriaBuilder
            , toMsg = CriteriaBuilderSent
            , browserEnv = settings.browserEnv
            , tags = ContactDatabase.tags model.contactDatabase
            , theme = settings.theme
            }
                |> CriteriaBuilder.view
-}
        ]


viewNewsletterStatus : SendNewsletterDialog msg -> Html (Msg msg)
viewNewsletterStatus (Settings settings) =
    let
        (Model model) =
            settings.model

        statusInfo =
            case model.existingStatus of
                StatusUnknown ->
                    Nothing

                StatusChecking ->
                    Just
                        { message = "Checking for existing newsletter…"
                        , isChecking = True
                        , details = Nothing
                        }

                StatusFound value ->
                    Just
                        { message = "This newsletter has already been sent."
                        , isChecking = False
                        , details = statusSummaryView value
                        }

                StatusNotFound ->
                    Nothing

                StatusError errorMsg ->
                    Just
                        { message = "Unable to check newsletter status: " ++ errorMsg
                        , isChecking = False
                        , details = Nothing
                        }
    in
    case statusInfo of
        Nothing ->
            emptyHtml

        Just info ->
            statusContainer <|
                List.concat
                    [ [ text info.message ]
                    , info.details |> Maybe.map List.singleton |> Maybe.withDefault []
                    ]


statusContainer : List (Html (Msg msg)) -> Html (Msg msg)
statusContainer children =
    div
        [ css
            [ Tw.mt_2
            , Tw.flex
            , Tw.flex_col
            , Tw.gap_2
            , Tw.p_3
            , Tw.rounded_lg
            , Tw.border
            ]
        ]
        children


statusSummaryView : Decode.Value -> Maybe (Html (Msg msg))
statusSummaryView value =
    case Decode.decodeValue statusSummaryDecoder value of
        Ok summary ->
            let
                rows =
                    [ summary.state |> Maybe.map (\state -> text ("State: " ++ state))
                    , summary.updatedAt |> Maybe.map (\updated -> text ("Updated at: " ++ updated))
                    , summary.uploadedJobs |> Maybe.map (\count -> text ("Uploaded jobs: " ++ String.fromInt count))
                    , summary.expectedJobs |> Maybe.map (\count -> text ("Expected jobs: " ++ String.fromInt count))
                    ]
                        |> List.filterMap identity
            in
            if List.isEmpty rows then
                Nothing

            else
                Just <|
                    div
                        [ css
                            [ Tw.flex
                            , Tw.flex_col
                            , Tw.gap_1
                            ]
                        ]
                        rows

        Err _ ->
            Nothing

viewTestEmailField : SendNewsletterDialog msg -> Html (Msg msg)
viewTestEmailField (Settings settings) =
    let
        (Model model) =
            settings.model

        (testEmail, sendingTestEmail) =
            case model.testEmailState of
                TestEmailEditing email ->
                    (Just email, False)
                TestEmailEmpty ->
                    (Nothing, False)
                TestEmailSending email ->
                    (Just email, True)
                TestEmailSent ->
                    (Nothing, False)
                TestEmailError error ->
                    (Nothing, False)
    in
    div
        [ css
            [ Tw.my_4
            , Tw.flex
            , Tw.flex_col
            , Tw.justify_center
            , Tw.gap_2
            ]
        ]
        [ div [ css [ Tw.flex, Tw.flex_row, Tw.items_end, Tw.gap_2 ] ]
            [ EntryField.new
                { value = Maybe.withDefault "" testEmail
                , onInput = UpdateTestEmail
                , theme = settings.theme
                }
                |> EntryField.withLabel (Translations.testEmailFieldLabel [ settings.browserEnv.translations ])
                |> EntryField.withPlaceholder (Translations.testEmailFieldPlaceholder [ settings.browserEnv.translations ])
                |> EntryField.withRequired
                |> EntryField.withType EntryField.FieldTypeEmail
                |> EntryField.view
            , Button.new
                { label = Translations.submitTestEmailButtonTitle [ settings.browserEnv.translations ]
                , onClick = Maybe.map SubmitTestEmail testEmail
                , theme = settings.theme
                }
                |> Button.withTypeSecondary
                |> Button.withDisabled (testEmail == Nothing)
                |> Button.withIntermediateState sendingTestEmail
                |> Button.view
            ]
        , viewTestEmailStatus (Settings settings)
        ]

viewTestEmailStatus : SendNewsletterDialog msg -> Html (Msg msg)
viewTestEmailStatus (Settings settings) =
    let
        (Model model) =
            settings.model

        styles =
            Ui.Styles.stylesForTheme settings.theme

        formatDiv =
            \content ->
                content
                    |> List.singleton
                    |> div (styles.colorStyleGrayscaleMuted ++ styles.textStyle14)
    in
    case model.testEmailState of
        TestEmailEmpty ->
            emptyHtml
        TestEmailEditing _ ->
            emptyHtml
        TestEmailSending _ ->
            (text <| Translations.testEmailSendingText [ settings.browserEnv.translations ])
                |> formatDiv
        TestEmailSent ->
            (text <| Translations.testEmailSentText [ settings.browserEnv.translations ])
                |> formatDiv
        TestEmailError error ->
            (text <| Translations.testEmailErrorText [ settings.browserEnv.translations ] { error = error })
                |> formatDiv


viewRecipientCount : SendNewsletterDialog msg -> Html (Msg msg)
viewRecipientCount (Settings settings) =
    let
        (Model model) =
            settings.model

        styles =
            Ui.Styles.stylesForTheme settings.theme

        message =
            case model.recipientCount of
                Just count ->
                    "Recipients matching criteria: " ++ String.fromInt count

                Nothing ->
                    "Recipients matching criteria: …"
    in
    div (styles.colorStyleGrayscaleMuted ++ styles.textStyle14)
        [ text message ]