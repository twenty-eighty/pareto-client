module Components.SendNewsletterDialog exposing (Model, Msg(..), NewsletterData, SendNewsletterDialog, hide, init, new, show, subscriptions, update, view)

import BrowserEnv exposing (BrowserEnv, Environment(..))
import Components.Button as Button
import Components.EntryField as EntryField
import Components.ModalDialog as ModalDialog
import Effect exposing (Effect)
import EmailValidation
import Html.Styled as Html exposing (Html, div, text)
import I18Next
import Html.Styled.Attributes as Attr exposing (css)
import Iso8601
import Json.Decode as Decode
import Newsletters.Subscribers as Subscribers
import Nostr
import Nostr.Event exposing (Kind(..))
import Nostr.External
import Nostr.Types exposing (IncomingMessage, PubKey)
import Ports
import Svg.Loaders
import Tailwind.Utilities as Tw
import Translations.SendNewsletterDialog as Translations
import Ui.Shared exposing (emptyHtml)
import Ui.Styles exposing (Theme(..))


type Msg msg
    = CloseDialog
    | SendClicked
    | CancelSendClicked
    | ReceivedMessage IncomingMessage
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
    , authorName : String
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
    , delivery : Maybe String
    , updatedAt : Maybe String
    , expectedJobs : Maybe Int
    , uploadedJobs : Maybe Int
    , doneJobs : Maybe Int
    }


type alias JobError =
    { idem : Maybe String
    , code : Maybe String
    , hint : Maybe String
    }


type alias SendProgress =
    { phase : String
    , totals : Maybe SendTotals
    , delivery : Maybe String
    , recentErrors : List JobError
    , sent : Maybe Int
    , total : Maybe Int
    }


type alias SendTotals =
    { fetched : Int
    , built : Int
    , accepted : Int
    , errors : Int
    }


initialSendProgress : SendProgress
initialSendProgress =
    { phase = "starting"
    , totals = Nothing
    , delivery = Nothing
    , recentErrors = []
    , sent = Nothing
    , total = Nothing
    }


type Model
    = Model
        { errors : List String
        , state : DialogState
        , testEmailState : TestEmailState
        , existingStatus : ExistingStatus
        , recipientCount : Maybe Int
        , subscriberEventData : Maybe Subscribers.SubscriberEventData
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
    | DialogSending NewsletterData SendProgress
    | DialogSent SendProgress
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
init _ =
    ( Model
        { errors = []
        , state = DialogHidden
        , testEmailState = TestEmailEmpty
        , existingStatus = StatusUnknown
        , recipientCount = Nothing
        , subscriberEventData = Nothing
        }
    , Effect.none
    )

hide : Model -> Model
hide (Model model) =
    Model { model | state = DialogHidden, existingStatus = StatusUnknown, recipientCount = Nothing, subscriberEventData = Nothing }


show : Nostr.Model -> PubKey -> Model -> NewsletterData -> ( Model, Effect (Msg msg) )
show nostr pubKey (Model model) newsletterData =
    ( Model
        { model
            | state = DialogPreparation newsletterData
            , existingStatus = StatusChecking
            , recipientCount = Nothing
            , subscriberEventData = Nothing
        }
    , Effect.batch
        [ Ports.getNewsletterStatus newsletterData.author newsletterData.identifier
            |> Effect.sendCmd
        , Subscribers.load nostr pubKey
            |> Effect.sendSharedMsg
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
                , cancelIfSending model.state
                )

            CancelSendClicked ->
                case model.state of
                    DialogSending newsletterData _ ->
                        ( Model { model | state = DialogPreparation newsletterData }
                        , Ports.cancelNewsletter
                            |> Effect.sendCmd
                        )

                    _ ->
                        ( Model model, Effect.none )

            SendClicked ->
                case model.state of
                    DialogPreparation newsletterData ->
                        ( Model { model | state = DialogSending newsletterData initialSendProgress }
                        , Ports.sendNewsletter newsletterData
                            (model.subscriberEventData |> Maybe.map subscriberBlobFromEvent)
                            |> Effect.sendCmd
                        )

                    _ ->
                        ( Model model, Effect.none )

            ReceivedMessage message ->
                updateWithMessage (Model model) props.pubKey message

            UpdateTestEmail email ->
                if email /= "" then
                    ( Model { model | testEmailState = TestEmailEditing email } , Effect.none)

                else
                    ( Model { model | testEmailState = TestEmailEmpty } , Effect.none)

            SubmitTestEmail email ->
                case model.state of
                    DialogPreparation newsletterData ->
                        if EmailValidation.emailValid (String.trim email) then
                            ( Model { model | testEmailState = TestEmailSending email }
                            , sendTestEmail (String.trim email) newsletterData
                                |> Effect.sendCmd
                            )

                        else
                            ( Model model, Effect.none )

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
    Decode.map6 StatusSummary
        (Decode.maybe (Decode.field "state" Decode.string))
        (Decode.maybe (Decode.field "delivery" Decode.string))
        (Decode.maybe (Decode.field "updated_at" Decode.string))
        (Decode.maybe (Decode.field "expected_jobs" Decode.int))
        (Decode.maybe (Decode.field "uploaded_jobs" Decode.int))
        (Decode.maybe (Decode.at [ "counts", "done" ] Decode.int))


jobErrorDecoder : Decode.Decoder JobError
jobErrorDecoder =
    Decode.map3 JobError
        (Decode.maybe (Decode.field "idem" Decode.string))
        (Decode.maybe (Decode.at [ "error", "code" ] Decode.string))
        (Decode.maybe (Decode.at [ "error", "hint" ] Decode.string))


sendProgressDecoder : Decode.Decoder SendProgress
sendProgressDecoder =
    Decode.map6 SendProgress
        (Decode.field "phase" Decode.string)
        (Decode.maybe (Decode.field "totals" sendTotalsDecoder))
        (Decode.maybe (Decode.field "delivery" Decode.string))
        (Decode.oneOf
            [ Decode.field "recent_errors" (Decode.list jobErrorDecoder)
            , Decode.succeed []
            ]
        )
        sentCountDecoder
        totalCountDecoder


sentCountDecoder : Decode.Decoder (Maybe Int)
sentCountDecoder =
    Decode.oneOf
        [ Decode.field "sent" Decode.int |> Decode.map Just
        , Decode.at [ "counts", "done" ] Decode.int |> Decode.map Just
        , Decode.succeed Nothing
        ]


totalCountDecoder : Decode.Decoder (Maybe Int)
totalCountDecoder =
    Decode.oneOf
        [ Decode.field "total" Decode.int |> Decode.map Just
        , Decode.field "expected_jobs" Decode.int |> Decode.map Just
        , Decode.succeed Nothing
        ]


progressWithCounts : { a | state : DialogState } -> SendProgress -> SendProgress
progressWithCounts model progress =
    let
        previous =
            case model.state of
                DialogSending _ current ->
                    Just current

                DialogSent current ->
                    Just current

                _ ->
                    Nothing
    in
    { progress
        | sent =
            case progress.sent of
                Just _ ->
                    progress.sent

                Nothing ->
                    previous |> Maybe.andThen .sent
        , total =
            case progress.total of
                Just _ ->
                    progress.total

                Nothing ->
                    previous
                        |> Maybe.andThen .total
                        |> (\maybeTotal ->
                                case maybeTotal of
                                    Just _ ->
                                        maybeTotal

                                    Nothing ->
                                        previous
                                            |> Maybe.andThen .totals
                                            |> Maybe.map .accepted
                           )
    }


sendTotalsDecoder : Decode.Decoder SendTotals
sendTotalsDecoder =
    Decode.map4 SendTotals
        (Decode.oneOf [ Decode.field "fetched" Decode.int, Decode.succeed 0 ])
        (Decode.oneOf [ Decode.field "built" Decode.int, Decode.succeed 0 ])
        (Decode.oneOf [ Decode.field "accepted" Decode.int, Decode.succeed 0 ])
        (Decode.oneOf [ Decode.field "errors" Decode.int, Decode.succeed 0 ])


sendTestEmail : String -> NewsletterData -> Cmd msg
sendTestEmail email newsletterData =
    Ports.sendNewsletterTest email newsletterData


updateWithMessage : Model -> PubKey -> IncomingMessage -> ( Model, Effect msg )
updateWithMessage (Model model) userPubKey message =
    case message.messageType of
        "newsletterProgress" ->
            case Decode.decodeValue sendProgressDecoder message.value of
                Ok progress ->
                    case progress.phase of
                        "done" ->
                            ( Model { model | state = DialogSent (completedSendProgress (progressWithCounts model progress)) }, Effect.none )

                        "sent" ->
                            ( Model { model | state = DialogSent (completedSendProgress (progressWithCounts model progress)) }, Effect.none )

                        "failed" ->
                            ( Model { model | state = DialogError (deliveryError progress "All newsletter deliveries failed") }, Effect.none )

                        "partial" ->
                            ( Model { model | state = DialogError (deliveryError progress "Delivered to some recipients. Some deliveries failed.") }, Effect.none )

                        "cancelled" ->
                            case model.state of
                                DialogSending newsletterData _ ->
                                    ( Model { model | state = DialogPreparation newsletterData }, Effect.none )

                                _ ->
                                    ( Model model, Effect.none )

                        "commit_failed" ->
                            ( Model { model | state = DialogError (progressError message.value "Commit failed") }, Effect.none )

                        "error" ->
                            ( Model { model | state = DialogError (progressError message.value "Failed to send newsletter") }, Effect.none )

                        _ ->
                            case model.state of
                                DialogSending newsletterData _ ->
                                    ( Model { model | state = DialogSending newsletterData (progressWithCounts model progress) }, Effect.none )

                                _ ->
                                    ( Model model, Effect.none )

                Err _ ->
                    ( Model model, Effect.none )

        "newsletterTestProgress" ->
            case Decode.decodeValue (Decode.field "phase" Decode.string) message.value of
                Ok "done" ->
                    ( Model { model | testEmailState = TestEmailSent }, Effect.none )

                Ok "sent" ->
                    ( Model { model | testEmailState = TestEmailSent }, Effect.none )

                Ok "cancelled" ->
                    ( Model { model | testEmailState = TestEmailEmpty }, Effect.none )

                Ok "failed" ->
                    ( Model { model | testEmailState = TestEmailError (progressError message.value "Test email failed") }, Effect.none )

                Ok "partial" ->
                    ( Model { model | testEmailState = TestEmailError (progressError message.value "Test email failed") }, Effect.none )

                Ok "error" ->
                    ( Model { model | testEmailState = TestEmailError (progressError message.value "Failed to send test email") }, Effect.none )

                _ ->
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
                Ok (Just count) ->
                    ( Model { model | recipientCount = Just count }, Effect.none )

                _ ->
                    ( Model model, Effect.none )

        "events" ->
            case Nostr.External.decodeEventsKind message.value of
                Ok KindApplicationSpecificData ->
                    case Nostr.External.decodeEvents message.value of
                        Ok events ->
                            let
                                ( maybeSubscriberEventData, _, errors ) =
                                    Subscribers.processEvents userPubKey [] events

                                countCmd =
                                    case maybeSubscriberEventData of
                                        Just data ->
                                            Ports.getNewsletterRecipientCount userPubKey (Just (subscriberBlobFromEvent data))
                                                |> Effect.sendCmd

                                        Nothing ->
                                            Effect.none
                            in
                            ( Model
                                { model
                                    | subscriberEventData = maybeSubscriberEventData
                                    , recipientCount =
                                        case maybeSubscriberEventData of
                                            Just data ->
                                                Just data.active

                                            Nothing ->
                                                model.recipientCount
                                    , errors = model.errors ++ errors
                                }
                            , countCmd
                            )

                        _ ->
                            ( Model model, Effect.none )

                _ ->
                    ( Model model, Effect.none )

        _ ->
            ( Model model, Effect.none )



-- SUBSCRIPTIONS


cancelIfSending : DialogState -> Effect msg
cancelIfSending state =
    case state of
        DialogSending _ _ ->
            Ports.cancelNewsletter
                |> Effect.sendCmd

        _ ->
            Effect.none


progressError : Decode.Value -> String -> String
progressError value fallback =
    Decode.decodeValue (Decode.field "error" Decode.string) value
        |> Result.withDefault fallback


deliveryError : SendProgress -> String -> String
deliveryError progress fallback =
    let
        codes =
            progress.recentErrors
                |> List.filterMap
                    (\err ->
                        case ( err.code, err.hint ) of
                            ( Just code, Just hint ) ->
                                Just (code ++ " (" ++ hint ++ ")")

                            ( Just code, Nothing ) ->
                                Just code

                            ( Nothing, Just hint ) ->
                                Just hint

                            ( Nothing, Nothing ) ->
                                err.idem
                    )
    in
    if List.isEmpty codes then
        fallback

    else
        fallback ++ " " ++ String.join ", " codes


subscriptions : Model -> (Msg msg -> msg) -> Sub msg
subscriptions _ toMsg =
    Ports.receiveMessage ReceivedMessage
        |> Sub.map toMsg


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

        DialogSending _ progress ->
            viewSendingDialog dialog progress

        DialogSent progress ->
            viewSentDialog dialog progress

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
        |> ModalDialog.withFixedWidth
        |> ModalDialog.view
        |> Html.map settings.toMsg

subscriberBlobFromEvent : Subscribers.SubscriberEventData -> { url : String, keyHex : String, ivHex : String }
subscriberBlobFromEvent data =
    { url = data.url
    , keyHex = data.keyHex
    , ivHex = data.ivHex
    }


completedSendProgress : SendProgress -> SendProgress
completedSendProgress progress =
    case ( progress.sent, progress.total ) of
        ( Just sent, Just total ) ->
            if sent < total && (progress.phase == "sent" || progress.phase == "done" || progress.delivery == Just "sent") then
                { progress | sent = Just total }

            else
                progress

        ( Nothing, Just total ) ->
            if progress.phase == "sent" || progress.phase == "done" || progress.delivery == Just "sent" then
                { progress | sent = Just total }

            else
                progress

        _ ->
            progress


progressCountText : List I18Next.Translations -> SendProgress -> Maybe String
progressCountText translations progress =
    case ( progress.sent, progress.total ) of
        ( Just sent, Just total ) ->
            Just
                (Translations.sendProgressSent translations
                    { sent = String.fromInt sent
                    , total = String.fromInt total
                    }
                )

        _ ->
            Nothing


existingSentCountText : List I18Next.Translations -> StatusSummary -> Maybe String
existingSentCountText translations summary =
    case ( summary.doneJobs, summary.expectedJobs ) of
        ( Just sent, Just total ) ->
            Just
                (Translations.sendProgressSent translations
                    { sent = String.fromInt sent
                    , total = String.fromInt total
                    }
                )

        ( Just sent, Nothing ) ->
            Just (Translations.emailsSentCount translations { sent = String.fromInt sent })

        _ ->
            Nothing


statusTimestampText : BrowserEnv -> StatusSummary -> Maybe String
statusTimestampText browserEnv summary =
    summary.updatedAt
        |> Maybe.andThen (Iso8601.toTime >> Result.toMaybe)
        |> Maybe.map (BrowserEnv.formatDate browserEnv)
        |> Maybe.andThen
            (\formattedDate ->
                let
                    translations =
                        [ browserEnv.translations ]
                in
                case summary.delivery of
                    Just "sent" ->
                        Just (Translations.sentAtText translations { date = formattedDate })

                    Just "failed" ->
                        Just (Translations.failedAtText translations { date = formattedDate })

                    Just "partial" ->
                        Just (Translations.lastSentAtText translations { date = formattedDate })

                    _ ->
                        Nothing
            )


numberOfRecipients : Model -> Int
numberOfRecipients (Model model) =
    Maybe.withDefault 0 model.recipientCount


viewSendingDialog : SendNewsletterDialog msg -> SendProgress -> Html msg
viewSendingDialog dialog progress =
    let
        (Settings settings) =
            dialog
    in
    ModalDialog.new
        { title = Translations.dialogTitle [ settings.browserEnv.translations ]
        , content =
            [ viewSendProgress (Settings settings) progress
            , viewRecipientCount (Settings settings)
            ]
        , onClose = CloseDialog
        , theme = settings.theme
        , buttons =
            [ Button.new
                { label = Translations.cancelButtonTitle [ settings.browserEnv.translations ]
                , onClick =
                    if sendCanBeCancelled progress then
                        Just CancelSendClicked

                    else
                        Nothing
                , theme = settings.theme
                }
                |> Button.withTypeSecondary
                |> Button.withDisabled (not (sendCanBeCancelled progress))
                |> Button.view
            , Button.new
                { label = Translations.sendButtonTitle [ settings.browserEnv.translations ]
                , onClick = Nothing
                , theme = settings.theme
                }
                |> Button.withTypePrimary
                |> Button.withDisabled True
                |> Button.withIntermediateState True
                |> Button.view
            ]
        }
        |> ModalDialog.withFixedWidth
        |> ModalDialog.view
        |> Html.map settings.toMsg


viewSentDialog : SendNewsletterDialog msg -> SendProgress -> Html msg
viewSentDialog dialog progress =
    let
        (Settings settings) =
            dialog
    in
    ModalDialog.new
        { title = Translations.dialogTitle [ settings.browserEnv.translations ]
        , content =
            [ statusContainer
                [ div [] [ text (Translations.sentMessageText [ settings.browserEnv.translations ]) ]
                , progressCountText [ settings.browserEnv.translations ] (completedSendProgress progress)
                    |> Maybe.map (\count -> div [] [ text count ])
                    |> Maybe.withDefault emptyHtml
                ]
            , viewRecipientCount (Settings settings)
            ]
        , onClose = CloseDialog
        , theme = settings.theme
        , buttons = [ closeButton (Settings settings) ]
        }
        |> ModalDialog.withFixedWidth
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
        , content =
            [ statusContainer
                [ text (Translations.errorMessageText [ settings.browserEnv.translations ] { error = error })
                ]
            ]
        , onClose = CloseDialog
        , theme = settings.theme
        , buttons = [ closeButton (Settings settings) ]
        }
        |> ModalDialog.withFixedWidth
        |> ModalDialog.view
        |> Html.map settings.toMsg


closeButton : SendNewsletterDialog msg -> Html (Msg msg)
closeButton (Settings settings) =
    Button.new
        { label = Translations.closeButtonTitle [ settings.browserEnv.translations ]
        , onClick = Just CloseDialog
        , theme = settings.theme
        }
        |> Button.withTypePrimary
        |> Button.view


viewSendProgress : SendNewsletterDialog msg -> SendProgress -> Html (Msg msg)
viewSendProgress (Settings settings) progress =
    let
        styles =
            Ui.Styles.stylesForTheme settings.theme

        translations =
            [ settings.browserEnv.translations ]

        phaseText =
            progressPhaseText translations progress.phase

        totalsText =
            case progressCountText translations progress of
                Just sentText ->
                    Just sentText

                Nothing ->
                    progress.totals
                        |> Maybe.map
                            (\totals ->
                                Translations.sendProgressRecipients translations
                                    { accepted = String.fromInt totals.accepted
                                    , fetched = String.fromInt totals.fetched
                                    }
                            )

        errorsText =
            if List.isEmpty progress.recentErrors then
                Nothing

            else
                progress.recentErrors
                    |> List.filterMap .code
                    |> String.join ", "
                    |> Just
    in
    statusContainer
        [ div
            [ css [ Tw.flex, Tw.flex_row, Tw.items_center, Tw.gap_3 ]
            , Attr.attribute "aria-live" "polite"
            ]
            [ div [ css [ Tw.flex, Tw.items_center, Tw.justify_center ] ]
                [ Svg.Loaders.puff [ Svg.Loaders.size 24, Svg.Loaders.color "currentColor" ]
                    |> Html.fromUnstyled
                ]
            , div
                (styles.textStyle14
                    ++ [ css [ Tw.flex, Tw.flex_col, Tw.gap_1 ] ]
                )
                ([ Just phaseText
                 , totalsText
                 , errorsText |> Maybe.map (\codes -> "Errors: " ++ codes)
                 ]
                    |> List.filterMap identity
                    |> List.map (\row -> div [] [ text row ])
                )
            ]
        ]


sendCanBeCancelled : SendProgress -> Bool
sendCanBeCancelled progress =
    case progress.phase of
        "sending" ->
            False

        "sent" ->
            False

        "failed" ->
            False

        "partial" ->
            False

        "committed" ->
            False

        _ ->
            True


progressPhaseText : List I18Next.Translations -> String -> String
progressPhaseText translations phase =
    case phase of
        "preparing" ->
            "Loading subscriber list…"

        "authenticating" ->
            "Signing in to the email queue…"

        "creating_campaign" ->
            "Creating campaign…"

        "start" ->
            "Preparing recipients…"

        "queueing" ->
            "Queueing…"

        "page_built" ->
            "Encrypting recipient jobs…"

        "page_enqueued" ->
            "Uploading jobs to the queue…"

        "committed" ->
            "Sending…"

        "sending" ->
            "Sending…"

        "sent" ->
            "Sent"

        _ ->
            Translations.sendingMessageText translations

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
            , Tw.w_full
            ]
        ]
        [ viewNewsletterStatus (Settings settings)
        , viewTestEmailField (Settings settings)
        , viewRecipientCount (Settings settings)
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
                        { message = existingStatusMessage [ settings.browserEnv.translations ] value
                        , isChecking = False
                        , details = statusSummaryView settings.browserEnv value
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
                    [ [ div [] [ text info.message ] ]
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
            , Tw.w_full
            , Tw.break_words
            ]
        ]
        children


existingStatusMessage : List I18Next.Translations -> Decode.Value -> String
existingStatusMessage translations value =
    case Decode.decodeValue statusSummaryDecoder value of
        Ok summary ->
            case summary.delivery of
                Just "queueing" ->
                    "A draft of this newsletter is still queueing."

                Just "sending" ->
                    "This newsletter is already sending."

                Just "failed" ->
                    "The last send of this newsletter failed."

                Just "partial" ->
                    "The last send of this newsletter only delivered to some recipients."

                Just "sent" ->
                    Translations.sentMessageText translations

                _ ->
                    Translations.sentMessageText translations

        Err _ ->
            Translations.sentMessageText translations


statusSummaryView : BrowserEnv -> Decode.Value -> Maybe (Html (Msg msg))
statusSummaryView browserEnv value =
    case Decode.decodeValue statusSummaryDecoder value of
        Ok summary ->
            let
                translations =
                    [ browserEnv.translations ]

                rows =
                    [ existingSentCountText translations summary
                    , statusTimestampText browserEnv summary
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
                        (List.map (\row -> div [] [ text row ]) rows)

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

        testEmailValid =
            testEmail
                |> Maybe.map (String.trim >> EmailValidation.emailValid)
                |> Maybe.withDefault False
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
                , onClick =
                    if testEmailValid then
                        Maybe.map SubmitTestEmail testEmail

                    else
                        Nothing
                , theme = settings.theme
                }
                |> Button.withTypeSecondary
                |> Button.withDisabled (not testEmailValid)
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
                    "Active subscribers: " ++ String.fromInt count

                Nothing ->
                    "Recipients: …"
    in
    div (styles.colorStyleGrayscaleMuted ++ styles.textStyle14)
        [ text message ]