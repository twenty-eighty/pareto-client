module Components.ZapButtonDialog exposing
    ( Model
    , Msg
    , ZapButtonDialog
    , init
    , new
    , subscriptions
    , update
    , view
    , withInstanceId
    , withRelayUrls
    , withoutDialog
    , withoutLabel
    )

{-| Combined zap button + dialog (native Elm LNURL-pay flow).
-}

import BrowserEnv exposing (BrowserEnv)
import Components.Button as Button
import Components.EntryField as EntryField
import Components.Icon as Icon
import Components.InteractionButton as InteractionButton exposing (InteractionObject(..))
import Components.ModalDialog as ModalDialog
import Effect exposing (Effect)
import FeatherIcons
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Events
import Html.Styled.Keyed as Keyed
import Http
import I18Next
import Json.Decode as Decode
import Json.Encode as Encode
import Nostr
import Nostr.CashuWallet as CashuWallet
import Nostr.Event exposing (Event, EventFilter, Kind(..), Tag(..), TagReference(..), emptyEvent, emptyEventFilter)
import Nostr.External as External
import Json.Decode.Pipeline as DecodePipeline
import Nostr.Lud16 as Lud16
import Nostr.Profile exposing (Profile)
import Nostr.Relay as Relay exposing (RelayUrl)
import Nostr.Send exposing (SendRequest(..))
import Nostr.Types exposing (IncomingMessage, LoginStatus(..), PubKey, loggedInPubKey, loggedInSigningPubKey)
import Nostr.Zaps as Zaps exposing (Invoice)
import Pareto
import Ports
import Process
import QRCode
import Set exposing (Set)
import Shared.Msg
import Svg.Attributes as SvgAttr
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw
import Task
import Time
import Translations.ZapDialog as Translations
import Ui.Links
import Ui.Shared exposing (emptyHtml)
import Ui.Styles exposing (Theme, stylesForTheme)
import Url



-- MODEL


type Model
    = Model
        { button : InteractionButton.Model
        , dialog : DialogState
        , nextRequestId : Int
        , nwcConnected : Bool
        , canAutoPay : Bool
        }


type DialogState
    = Hidden
    | LoadingPayData Target LoadingFlags
    | Ready ReadyData
    | SigningZapRequest ReadyData
    | LoadingInvoice ReadyData
    | SendingNutzap ReadyData Int
    | ShowingInvoice InvoiceView
    | PayingWithNwc InvoiceView
    | Success SuccessView
    | ErrorState String (Maybe ReadyData)


type alias LoadingFlags =
    { waitingLightning : Bool
    , waitingCashu : Bool
    , payData : Maybe Lud16.LightningPaymentData
    , cashuOption : Maybe CashuOption
    }


type PaymentMethod
    = Lightning
    | Cashu


type alias CashuOption =
    { mintUrl : String
    , p2pkPubkey : String
    , relays : List RelayUrl
    , availableBalance : Int
    , proofEventIds : List String
    , proofs : List Encode.Value
    }


type alias Target =
    { interactionObject : InteractionObject
    , lud16 : Maybe Lud16.Lud16
    , recipientName : String
    , recipientPicture : Maybe String
    , recipientPubKey : PubKey
    , relays : Set String
    }


type alias ReadyData =
    { target : Target
    , payData : Maybe Lud16.LightningPaymentData
    , cashuOption : Maybe CashuOption
    , method : PaymentMethod
    , amountDraft : String
    , amountInputKey : Int
    , comment : String
    , signRequestId : Maybe Int
    }


type alias InvoiceView =
    { ready : ReadyData
    , bolt11 : String
    , knownReceiptIds : Set String
    , watchingSince : Time.Posix
    }


type alias SuccessView =
    { ready : ReadyData
    , amountSats : Int
    }


init : Model
init =
    Model
        { button = InteractionButton.init
        , dialog = Hidden
        , nextRequestId = 1
        , nwcConnected = False
        , canAutoPay = False
        }



-- UPDATE


type Msg
    = InteractionButtonMsg (InteractionButton.Msg Msg)
    | OpenDialog (Set String)
    | CloseDialog
    | SetAmount Int
    | SetAmountInput String
    | SetComment String
    | SetPaymentMethod PaymentMethod
    | ReceivedPayData (Result Http.Error Lud16.LightningPaymentData)
    | ConfirmZap
    | ReceivedMessage IncomingMessage
    | ReceivedInvoice (Result String Invoice)
    | CheckZapPaid Time.Posix
    | AutoCloseSuccess
    | NoOp


update :
    { msg : Msg
    , model : Model
    , nostr : Nostr.Model
    , loginStatus : LoginStatus
    , browserEnv : BrowserEnv
    , interactionObject : InteractionObject
    , toModel : Model -> model
    , toMsg : Msg -> msg
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
            InteractionButtonMsg interactionMsg ->
                let
                    ( updatedButton, effect ) =
                        InteractionButton.update
                            { msg = interactionMsg
                            , model = model.button
                            , nostr = props.nostr
                            , toModel = \buttonModel -> Model { model | button = buttonModel }
                            , translations = props.browserEnv.translations
                            }
                in
                ( updatedButton, effect |> Effect.map props.toMsg )

            OpenDialog relayUrls ->
                openDialog props relayUrls (Model model)

            CloseDialog ->
                ( Model { model | dialog = Hidden }
                , Effect.none
                )

            SetAmount amountSats ->
                ( Model
                    { model
                        | dialog =
                            updateReadyAmountPreset model.dialog (String.fromInt amountSats)
                    }
                , Effect.none
                )

            SetAmountInput amountDraft ->
                ( Model
                    { model
                        | dialog = updateReadyAmount model.dialog (digitsOnly amountDraft)
                    }
                , Effect.none
                )

            SetComment comment ->
                ( Model { model | dialog = updateReadyComment model.dialog comment }
                , Effect.none
                )

            SetPaymentMethod method ->
                ( Model { model | dialog = updateReadyMethod model.dialog method }
                , Effect.none
                )

            ReceivedPayData (Ok payData) ->
                case model.dialog of
                    LoadingPayData target flags ->
                        finishLoading props
                            (Model model)
                            target
                            { flags | waitingLightning = False, payData = Just payData }

                    _ ->
                        ( Model model, Effect.none )

            ReceivedPayData (Err _) ->
                case model.dialog of
                    LoadingPayData target flags ->
                        finishLoading props
                            (Model model)
                            target
                            { flags | waitingLightning = False, payData = Nothing }

                    _ ->
                        ( Model
                            { model
                                | dialog = ErrorState (Translations.errorText [ props.browserEnv.translations ]) Nothing
                            }
                        , Effect.none
                        )

            ConfirmZap ->
                confirmZap props (Model model)

            ReceivedMessage message ->
                handleIncomingMessage props (Model model) message

            ReceivedInvoice (Ok invoice) ->
                case model.dialog of
                    LoadingInvoice ready ->
                        showInvoice props (Model model) ready invoice.pr

                    SigningZapRequest ready ->
                        showInvoice props (Model model) ready invoice.pr

                    _ ->
                        ( Model model, Effect.none )

            ReceivedInvoice (Err errorMessage) ->
                let
                    maybeReady =
                        case model.dialog of
                            LoadingInvoice ready ->
                                Just ready

                            SigningZapRequest ready ->
                                Just ready

                            Ready ready ->
                                Just ready

                            _ ->
                                Nothing
                in
                ( Model { model | dialog = ErrorState errorMessage maybeReady }
                , Effect.none
                )

            CheckZapPaid now ->
                checkZapPaid props (Model model) now

            AutoCloseSuccess ->
                case model.dialog of
                    Success _ ->
                        ( Model { model | dialog = Hidden }
                        , Effect.none
                        )

                    _ ->
                        ( Model model, Effect.none )

            NoOp ->
                ( Model model, Effect.none )


showInvoice :
    { props
        | nostr : Nostr.Model
        , browserEnv : BrowserEnv
        , toMsg : Msg -> msg
    }
    -> Model
    -> ReadyData
    -> String
    -> ( Model, Effect msg )
showInvoice props (Model model) ready bolt11 =
    let
        watchingSince =
            props.browserEnv.now
                |> Time.posixToMillis
                |> (\ms -> ms - 60 * 1000)
                |> Time.millisToPosix

        invoiceView =
            { ready = ready
            , bolt11 = bolt11
            , knownReceiptIds = knownReceiptIdsForTarget props.nostr ready.target
            , watchingSince = watchingSince
            }
    in
    if invoiceIsPaid props.nostr invoiceView then
        succeedZap props (Model model) ready

    else
        let
            requestId =
                model.nextRequestId

            watchEffect =
                watchZapReceipts requestId invoiceView
                    |> Effect.map props.toMsg
        in
        if model.canAutoPay then
            ( Model
                { model
                    | dialog = PayingWithNwc invoiceView
                    , nextRequestId = requestId + 1
                }
            , Effect.batch
                [ watchEffect
                , Effect.sendCmd (Ports.payInvoiceNwc bolt11)
                ]
            )

        else
            ( Model
                { model
                    | dialog = ShowingInvoice invoiceView
                    , nextRequestId = requestId + 1
                }
            , watchEffect
            )


checkZapPaid :
    { props
        | nostr : Nostr.Model
        , browserEnv : BrowserEnv
        , toMsg : Msg -> msg
    }
    -> Model
    -> Time.Posix
    -> ( Model, Effect msg )
checkZapPaid props (Model model) _ =
    case model.dialog of
        ShowingInvoice invoiceView ->
            if invoiceIsPaid props.nostr invoiceView then
                succeedZap props (Model model) invoiceView.ready

            else
                -- ndk.fetchEvents resolves at EOSE; re-query like the old zap dialog
                let
                    requestId =
                        model.nextRequestId
                in
                ( Model { model | nextRequestId = requestId + 1 }
                , watchZapReceipts requestId invoiceView
                    |> Effect.map props.toMsg
                )

        PayingWithNwc invoiceView ->
            if invoiceIsPaid props.nostr invoiceView then
                succeedZap props (Model model) invoiceView.ready

            else
                let
                    requestId =
                        model.nextRequestId
                in
                ( Model { model | nextRequestId = requestId + 1 }
                , watchZapReceipts requestId invoiceView
                    |> Effect.map props.toMsg
                )

        _ ->
            ( Model model, Effect.none )


invoiceIsPaid : Nostr.Model -> InvoiceView -> Bool
invoiceIsPaid nostr invoiceView =
    Nostr.hasZapReceiptWithBolt11 nostr invoiceView.bolt11
        || hasNewReceiptForTarget nostr invoiceView


hasNewReceiptForTarget : Nostr.Model -> InvoiceView -> Bool
hasNewReceiptForTarget nostr invoiceView =
    knownReceiptIdsForTarget nostr invoiceView.ready.target
        |> Set.diff invoiceView.knownReceiptIds
        |> Set.isEmpty
        |> not


knownReceiptIdsForTarget : Nostr.Model -> Target -> Set String
knownReceiptIdsForTarget nostr target =
    interactionObjectTagReferences target.interactionObject
        |> List.map (Nostr.zapReceiptIdsForTagReference nostr)
        |> List.foldl Set.union Set.empty


interactionObjectTagReferences : InteractionObject -> List TagReference
interactionObjectTagReferences interactionObject =
    case interactionObject of
        Article eventId addressComponents ->
            [ TagReferenceEventId eventId
            , TagReferenceCode addressComponents
            ]

        Comment eventId _ ->
            [ TagReferenceEventId eventId ]

        PicturePost eventId _ ->
            [ TagReferenceEventId eventId ]

        ProfilePubKey _ ->
            []


watchZapReceipts : Int -> InvoiceView -> Effect Msg
watchZapReceipts requestId invoiceView =
    let
        -- Public writeable relays only — Pareto relays reject non-author writes,
        -- so wallets cannot publish receipts there.
        relays =
            invoiceView.ready.target.relays
                |> Set.union zapReceiptWatchRelays
                |> Set.toList
                |> List.map Relay.fromString

        -- Match the old zap dialog: watch kind 9735 since payment started,
        -- then match bolt11 client-side. Tag filters alone miss some wallets.
        filter : EventFilter
        filter =
            { emptyEventFilter
                | kinds = Just [ KindZapReceipt ]
                , since = Just invoiceView.watchingSince
                , limit = Just 50
            }
    in
    Ports.requestEvents "Zap receipt watch" True requestId relays [ filter ]
        |> Effect.sendCmd


succeedZap :
    { props
        | toMsg : Msg -> msg
    }
    -> Model
    -> ReadyData
    -> ( Model, Effect msg )
succeedZap props (Model model) ready =
    let
        amountSats =
            parsedAmountSats ready
                |> Maybe.withDefault 0
    in
    ( Model { model | dialog = Success { ready = ready, amountSats = amountSats } }
    , Process.sleep 5000
        |> Task.perform (\_ -> AutoCloseSuccess)
        |> Effect.sendCmd
        |> Effect.map props.toMsg
    )


openDialog :
    { props
        | nostr : Nostr.Model
        , loginStatus : LoginStatus
        , interactionObject : InteractionObject
        , browserEnv : BrowserEnv
        , toMsg : Msg -> msg
    }
    -> Set String
    -> Model
    -> ( Model, Effect msg )
openDialog props relayUrls (Model model) =
    let
        recipientPubKey =
            InteractionButton.pubKeyOfInteractionObject props.interactionObject

        maybeProfile =
            Nostr.getProfile props.nostr recipientPubKey

        maybeLud16 =
            maybeProfile
                |> Maybe.andThen .lud16
                |> Maybe.andThen Lud16.parseLud16

        senderHasWallet =
            Nostr.getCashuWallet props.nostr /= Nothing
                && (loggedInSigningPubKey props.loginStatus /= Nothing)

        target =
            { interactionObject = props.interactionObject
            , lud16 = maybeLud16
            , recipientName = recipientDisplayName maybeProfile recipientPubKey
            , recipientPicture = maybeProfile |> Maybe.andThen .picture
            , recipientPubKey = recipientPubKey
            , relays = extendedZapRelays relayUrls props.nostr (loggedInPubKey props.loginStatus)
            }

        flags =
            { waitingLightning = maybeLud16 /= Nothing
            , waitingCashu = senderHasWallet
            , payData = Nothing
            , cashuOption = Nothing
            }
    in
    if not flags.waitingLightning && not flags.waitingCashu then
        ( Model
            { model
                | dialog = ErrorState (Translations.noPaymentMethodText [ props.browserEnv.translations ]) Nothing
            }
        , Effect.none
        )

    else
        let
            requestId =
                model.nextRequestId

            lnEffect =
                case maybeLud16 of
                    Just lud16 ->
                        Lud16.requestLightningPaymentData ReceivedPayData lud16
                            |> Effect.sendCmd
                            |> Effect.map props.toMsg

                    Nothing ->
                        Effect.none

            cashuEffect =
                if senderHasWallet then
                    let
                        relays =
                            target.relays
                                |> Set.toList
                                |> List.map Relay.fromString

                        filter =
                            { emptyEventFilter
                                | kinds = Just [ KindNutzapMintRecommendation ]
                                , authors = Just [ recipientPubKey ]
                                , limit = Just 1
                            }
                    in
                    Ports.requestEvents "Recipient nutzap mint recommendation" False requestId relays [ filter ]
                        |> Effect.sendCmd
                        |> Effect.map props.toMsg

                else
                    Effect.none
        in
        ( Model
            { model
                | dialog = LoadingPayData target flags
                , nextRequestId =
                    if senderHasWallet then
                        requestId + 1

                    else
                        requestId
            }
        , Effect.batch
            [ lnEffect
            , cashuEffect
            , Effect.sendCmd Ports.getNwcStatus
            ]
        )


finishLoading :
    { props
        | browserEnv : BrowserEnv
        , toMsg : Msg -> msg
    }
    -> Model
    -> Target
    -> LoadingFlags
    -> ( Model, Effect msg )
finishLoading props (Model model) target flags =
    if flags.waitingLightning || flags.waitingCashu then
        ( Model { model | dialog = LoadingPayData target flags }
        , Effect.none
        )

    else
        case ( flags.payData, flags.cashuOption ) of
            ( Nothing, Nothing ) ->
                ( Model
                    { model
                        | dialog = ErrorState (Translations.noPaymentMethodText [ props.browserEnv.translations ]) Nothing
                    }
                , Effect.none
                )

            ( maybePayData, maybeCashu ) ->
                let
                    method =
                        case ( maybePayData, maybeCashu ) of
                            ( Just _, _ ) ->
                                Lightning

                            ( Nothing, Just _ ) ->
                                Cashu

                            _ ->
                                Lightning

                    amountDraft =
                        case maybePayData of
                            Just payData ->
                                String.fromInt (defaultAmountSats payData)

                            Nothing ->
                                "21"
                in
                ( Model
                    { model
                        | dialog =
                            Ready
                                { target = target
                                , payData = maybePayData
                                , cashuOption = maybeCashu
                                , method = method
                                , amountDraft = amountDraft
                                , amountInputKey = 0
                                , comment = ""
                                , signRequestId = Nothing
                                }
                    }
                , Effect.none
                )


confirmZap :
    { props
        | loginStatus : LoginStatus
        , browserEnv : BrowserEnv
        , nostr : Nostr.Model
        , toMsg : Msg -> msg
    }
    -> Model
    -> ( Model, Effect msg )
confirmZap props (Model model) =
    case model.dialog of
        Ready ready ->
            case parsedAmountSats ready of
                Nothing ->
                    ( Model
                        { model
                            | dialog =
                                ErrorState (Translations.invalidAmountText [ props.browserEnv.translations ]) (Just ready)
                        }
                    , Effect.none
                    )

                Just amountSats ->
                    case ready.method of
                        Cashu ->
                            confirmNutzap props (Model model) ready amountSats

                        Lightning ->
                            confirmLightningZap props (Model model) ready amountSats

        _ ->
            ( Model model, Effect.none )


confirmLightningZap :
    { props
        | loginStatus : LoginStatus
        , browserEnv : BrowserEnv
        , toMsg : Msg -> msg
    }
    -> Model
    -> ReadyData
    -> Int
    -> ( Model, Effect msg )
confirmLightningZap props (Model model) ready amountSats =
    case ready.payData of
        Nothing ->
            ( Model
                { model
                    | dialog = ErrorState (Translations.noLud16Text [ props.browserEnv.translations ]) (Just ready)
                }
            , Effect.none
            )

        Just payData ->
            let
                amountMsats =
                    amountSats * 1000

                wantsNostr =
                    payData.allowsNostr == Just True

                maybeSignerPubKey =
                    loggedInSigningPubKey props.loginStatus
            in
            if wantsNostr then
                let
                    ( signerPubKey, anonymous ) =
                        case maybeSignerPubKey of
                            Just pubKey ->
                                ( pubKey, False )

                            Nothing ->
                                ( Pareto.anonymousPublicKey, True )

                    zapRequest =
                        buildZapRequest signerPubKey ready amountMsats props.browserEnv.now anonymous

                    requestId =
                        model.nextRequestId
                in
                ( Model
                    { model
                        | dialog = SigningZapRequest { ready | signRequestId = Just requestId }
                        , nextRequestId = requestId + 1
                    }
                , Ports.signEvent requestId zapRequest
                    |> Effect.sendCmd
                    |> Effect.map props.toMsg
                )

            else
                ( Model { model | dialog = LoadingInvoice ready }
                , requestInvoice ready Nothing
                    |> Effect.map props.toMsg
                )


confirmNutzap :
    { props
        | loginStatus : LoginStatus
        , browserEnv : BrowserEnv
        , toMsg : Msg -> msg
    }
    -> Model
    -> ReadyData
    -> Int
    -> ( Model, Effect msg )
confirmNutzap props (Model model) ready amountSats =
    case ( loggedInSigningPubKey props.loginStatus, ready.cashuOption ) of
        ( Just _, Just cashu ) ->
            if amountSats > cashu.availableBalance then
                ( Model
                    { model
                        | dialog = ErrorState (Translations.insufficientBalanceText [ props.browserEnv.translations ]) (Just ready)
                    }
                , Effect.none
                )

            else
                let
                    requestId =
                        model.nextRequestId
                in
                ( Model
                    { model
                        | dialog = SendingNutzap ready requestId
                        , nextRequestId = requestId + 1
                    }
                , Ports.sendNutzap
                    { requestId = requestId
                    , mintUrl = cashu.mintUrl
                    , proofs = cashu.proofs
                    , amount = amountSats
                    , recipientP2pk = cashu.p2pkPubkey
                    }
                    |> Effect.sendCmd
                    |> Effect.map props.toMsg
                )

        _ ->
            ( Model
                { model
                    | dialog = ErrorState (Translations.loginRequiredText [ props.browserEnv.translations ]) (Just ready)
                }
            , Effect.none
            )


handleZapReceiptMessage :
    { props
        | browserEnv : BrowserEnv
        , nostr : Nostr.Model
        , toMsg : Msg -> msg
    }
    -> Model
    -> InvoiceView
    -> Decode.Value
    -> ( Model, Effect msg )
handleZapReceiptMessage props (Model model) invoiceView value =
    case Decode.decodeValue (Decode.list Zaps.nostrZapReceiptDecoder) value of
        Ok receipts ->
            if List.any (\receipt -> receipt.bolt11 == invoiceView.bolt11) receipts then
                succeedZap props (Model model) invoiceView.ready

            else if hasNewReceiptForTarget props.nostr invoiceView then
                succeedZap props (Model model) invoiceView.ready

            else
                ( Model model, Effect.none )

        Err _ ->
            ( Model model
            , Process.sleep 100
                |> Task.andThen (\_ -> Time.now)
                |> Task.perform CheckZapPaid
                |> Effect.sendCmd
                |> Effect.map props.toMsg
            )


handleIncomingMessage :
    { props
        | browserEnv : BrowserEnv
        , nostr : Nostr.Model
        , loginStatus : LoginStatus
        , toMsg : Msg -> msg
    }
    -> Model
    -> IncomingMessage
    -> ( Model, Effect msg )
handleIncomingMessage props (Model model) message =
    case message.messageType of
        "signedEvent" ->
            case model.dialog of
                SigningZapRequest ready ->
                    case Decode.decodeValue signedEventResponseDecoder message.value of
                        Ok { requestId, eventJson } ->
                            if Just requestId == ready.signRequestId then
                                ( Model { model | dialog = LoadingInvoice ready }
                                , requestInvoice ready (Just eventJson)
                                    |> Effect.map props.toMsg
                                )

                            else
                                ( Model model, Effect.none )

                        Err _ ->
                            ( Model { model | dialog = ErrorState (Translations.errorText [ props.browserEnv.translations ]) (Just ready) }
                            , Effect.none
                            )

                _ ->
                    ( Model model, Effect.none )

        "error" ->
            case model.dialog of
                SigningZapRequest ready ->
                    let
                        reason =
                            Decode.decodeValue (Decode.field "reason" Decode.string) message.value
                                |> Result.withDefault (Translations.signingErrorText [ props.browserEnv.translations ])
                    in
                    if matchesSignRequestId ready message.value then
                        ( Model { model | dialog = ErrorState reason (Just ready) }
                        , Effect.none
                        )

                    else
                        ( Model model, Effect.none )

                _ ->
                    ( Model model, Effect.none )

        "zap_receipts" ->
            case model.dialog of
                ShowingInvoice invoiceView ->
                    handleZapReceiptMessage props (Model model) invoiceView message.value

                PayingWithNwc invoiceView ->
                    handleZapReceiptMessage props (Model model) invoiceView message.value

                _ ->
                    ( Model model, Effect.none )

        "nwcStatus" ->
            case
                Decode.decodeValue
                    (Decode.map2 Tuple.pair
                        (Decode.field "connected" Decode.bool)
                        (Decode.oneOf
                            [ Decode.field "canAutoPay" Decode.bool
                            , Decode.succeed False
                            ]
                        )
                    )
                    message.value
            of
                Ok ( connected, canAutoPay ) ->
                    ( Model
                        { model
                            | nwcConnected = connected
                            , canAutoPay = canAutoPay || connected
                        }
                    , Effect.none
                    )

                Err _ ->
                    ( Model model, Effect.none )

        "nwcPaySkipped" ->
            case model.dialog of
                PayingWithNwc invoiceView ->
                    ( Model
                        { model
                            | dialog = ShowingInvoice invoiceView
                            , nwcConnected = False
                            , canAutoPay = False
                        }
                    , Effect.none
                    )

                _ ->
                    ( Model model, Effect.none )

        "nwcPaySucceeded" ->
            case model.dialog of
                PayingWithNwc invoiceView ->
                    succeedZap props (Model model) invoiceView.ready

                _ ->
                    ( Model model, Effect.none )

        "nwcPayFailed" ->
            case model.dialog of
                PayingWithNwc invoiceView ->
                    -- Fall back to manual invoice payment.
                    ( Model { model | dialog = ShowingInvoice invoiceView }
                    , Effect.none
                    )

                _ ->
                    ( Model model, Effect.none )

        "events" ->
            handleRecipientMintEvents props (Model model) message.value

        "eventsComplete" ->
            case model.dialog of
                LoadingPayData target flags ->
                    if flags.waitingCashu then
                        finishLoading props
                            (Model model)
                            target
                            { flags | waitingCashu = False }

                    else
                        ( Model model, Effect.none )

                _ ->
                    ( Model model, Effect.none )

        "nutzapSent" ->
            case model.dialog of
                SendingNutzap ready requestId ->
                    case Decode.decodeValue nutzapSentDecoder message.value of
                        Ok sent ->
                            if sent.requestId == requestId then
                                publishNutzapSend props (Model model) ready sent

                            else
                                ( Model model, Effect.none )

                        Err _ ->
                            ( Model { model | dialog = ErrorState (Translations.errorText [ props.browserEnv.translations ]) (Just ready) }
                            , Effect.none
                            )

                _ ->
                    ( Model model, Effect.none )

        "nutzapSendFailed" ->
            case model.dialog of
                SendingNutzap ready requestId ->
                    let
                        failedId =
                            Decode.decodeValue (Decode.field "requestId" Decode.int) message.value
                                |> Result.withDefault -1

                        reason =
                            Decode.decodeValue (Decode.field "reason" Decode.string) message.value
                                |> Result.withDefault (Translations.errorText [ props.browserEnv.translations ])
                    in
                    if failedId == requestId then
                        ( Model { model | dialog = ErrorState reason (Just ready) }
                        , Effect.none
                        )

                    else
                        ( Model model, Effect.none )

                _ ->
                    ( Model model, Effect.none )

        _ ->
            ( Model model, Effect.none )


handleRecipientMintEvents :
    { props
        | browserEnv : BrowserEnv
        , nostr : Nostr.Model
        , toMsg : Msg -> msg
    }
    -> Model
    -> Decode.Value
    -> ( Model, Effect msg )
handleRecipientMintEvents props (Model model) value =
    case model.dialog of
        LoadingPayData target flags ->
            if not flags.waitingCashu then
                ( Model model, Effect.none )

            else
                case ( External.decodeEventsKind value, External.decodeEvents value ) of
                    ( Ok KindNutzapMintRecommendation, Ok events ) ->
                        let
                            cashuOption =
                                events
                                    |> List.map CashuWallet.mintRecommendationFromEvent
                                    |> List.filter (\rec -> rec.pubKey == target.recipientPubKey)
                                    |> List.head
                                    |> Maybe.andThen (cashuOptionFromRecommendation props.nostr)
                        in
                        finishLoading props
                            (Model model)
                            target
                            { flags
                                | waitingCashu = False
                                , cashuOption = cashuOption
                            }

                    _ ->
                        ( Model model, Effect.none )

        Ready ready ->
            case ( External.decodeEventsKind value, External.decodeEvents value ) of
                ( Ok KindNutzapMintRecommendation, Ok events ) ->
                    let
                        cashuOption =
                            events
                                |> List.map CashuWallet.mintRecommendationFromEvent
                                |> List.filter (\rec -> rec.pubKey == ready.target.recipientPubKey)
                                |> List.head
                                |> Maybe.andThen (cashuOptionFromRecommendation props.nostr)
                    in
                    ( Model
                        { model
                            | dialog =
                                Ready
                                    { ready
                                        | cashuOption = cashuOption
                                        , method =
                                            case ( ready.payData, cashuOption ) of
                                                ( Nothing, Just _ ) ->
                                                    Cashu

                                                _ ->
                                                    ready.method
                                    }
                        }
                    , Effect.none
                    )

                _ ->
                    ( Model model, Effect.none )

        _ ->
            ( Model model, Effect.none )


cashuOptionFromRecommendation : Nostr.Model -> CashuWallet.NutzapMintRecommendation -> Maybe CashuOption
cashuOptionFromRecommendation nostr rec =
    case rec.p2pkPubkey of
        Nothing ->
            Nothing

        Just p2pk ->
            if List.isEmpty rec.mints then
                Nothing

            else
                rec.mints
                    |> List.filterMap
                        (\mintUrl ->
                            let
                                ( proofs, eventIds ) =
                                    Nostr.getCashuProofsForMint nostr mintUrl

                                balance =
                                    proofs |> List.map .amount |> List.sum
                            in
                            if balance > 0 then
                                Just
                                    { mintUrl = mintUrl
                                    , p2pkPubkey = p2pk
                                    , relays = List.map Relay.fromString rec.relays
                                    , availableBalance = balance
                                    , proofEventIds = eventIds
                                    , proofs = List.map CashuWallet.encodeProof proofs
                                    }

                            else
                                Nothing
                        )
                    |> List.sortBy (\opt -> negate opt.availableBalance)
                    |> List.head


type alias NutzapSent =
    { requestId : Int
    , mintUrl : String
    , amount : Int
    , keepProofs : List CashuWallet.CashuProof
    , sendProofs : List Encode.Value
    }


nutzapSentDecoder : Decode.Decoder NutzapSent
nutzapSentDecoder =
    Decode.map5 NutzapSent
        (Decode.field "requestId" Decode.int)
        (Decode.field "mintUrl" Decode.string)
        (Decode.field "amount" Decode.int)
        (Decode.field "keepProofs" (Decode.list cashuProofDecoder))
        (Decode.field "sendProofs" (Decode.list Decode.value))


cashuProofDecoder : Decode.Decoder CashuWallet.CashuProof
cashuProofDecoder =
    Decode.succeed CashuWallet.CashuProof
        |> DecodePipeline.required "id" Decode.string
        |> DecodePipeline.required "amount" Decode.int
        |> DecodePipeline.required "secret" Decode.string
        |> DecodePipeline.required "C" Decode.string


publishNutzapSend :
    { props
        | browserEnv : BrowserEnv
        , loginStatus : LoginStatus
        , toMsg : Msg -> msg
    }
    -> Model
    -> ReadyData
    -> NutzapSent
    -> ( Model, Effect msg )
publishNutzapSend props (Model model) ready sent =
    case ( loggedInSigningPubKey props.loginStatus, ready.cashuOption ) of
        ( Just senderPubKey, Just cashu ) ->
            let
                relays =
                    if List.isEmpty cashu.relays then
                        ready.target.relays
                            |> Set.toList
                            |> List.map Relay.fromString

                    else
                        cashu.relays

                nutzapEvt =
                    CashuWallet.nutzapEvent senderPubKey
                        { recipientPubKey = ready.target.recipientPubKey
                        , mintUrl = sent.mintUrl
                        , comment = ready.comment
                        , proofs = sent.sendProofs
                        , interactionTags = interactionObjectTags ready.target.interactionObject
                        }

                tokenEvt =
                    CashuWallet.tokenEvent senderPubKey sent.mintUrl sent.keepProofs cashu.proofEventIds

                historyEvt =
                    CashuWallet.historyEvent senderPubKey
                        { direction = CashuWallet.HistoryOut
                        , amount = sent.amount
                        , nutzapEventId = Nothing
                        , counterpartPubKey = ready.target.recipientPubKey
                        , createdTokenEventId = Nothing
                        }
            in
            ( Model { model | dialog = Success { ready = ready, amountSats = sent.amount } }
            , Effect.batch
                [ SendNutzap relays nutzapEvt
                    |> Shared.Msg.SendNostrEvent
                    |> Effect.sendSharedMsg
                , SendCashuTokens tokenEvt
                    |> Shared.Msg.SendNostrEvent
                    |> Effect.sendSharedMsg
                , SendCashuHistory historyEvt
                    |> Shared.Msg.SendNostrEvent
                    |> Effect.sendSharedMsg
                , Process.sleep 5000
                    |> Task.perform (\_ -> AutoCloseSuccess)
                    |> Effect.sendCmd
                    |> Effect.map props.toMsg
                ]
            )

        _ ->
            ( Model { model | dialog = ErrorState (Translations.loginRequiredText [ props.browserEnv.translations ]) (Just ready) }
            , Effect.none
            )


matchesSignRequestId : ReadyData -> Decode.Value -> Bool
matchesSignRequestId ready value =
    case ( ready.signRequestId, Decode.decodeValue (Decode.field "requestId" Decode.int) value ) of
        ( Just expected, Ok actual ) ->
            expected == actual

        ( Just _, Err _ ) ->
            -- Signing errors may omit requestId; treat as ours while signing.
            True

        _ ->
            False


requestInvoice : ReadyData -> Maybe String -> Effect Msg
requestInvoice ready maybeNostrJson =
    case ( ready.payData, parsedAmountSats ready ) of
        ( Just payData, Just amountSats ) ->
            Zaps.fetchInvoice ReceivedInvoice
                (Url.toString payData.callback)
                (amountSats * 1000)
                (Just ready.comment)
                maybeNostrJson
                |> Effect.sendCmd

        _ ->
            Effect.none


parsedAmountSats : ReadyData -> Maybe Int
parsedAmountSats ready =
    case String.toInt (String.trim ready.amountDraft) of
        Just amountSats ->
            if amountSats <= 0 then
                Nothing

            else
                case ready.method of
                    Lightning ->
                        case ready.payData of
                            Just payData ->
                                let
                                    amountMsats =
                                        amountSats * 1000
                                in
                                if amountMsats >= payData.minSendable && amountMsats <= payData.maxSendable then
                                    Just amountSats

                                else
                                    Nothing

                            Nothing ->
                                Nothing

                    Cashu ->
                        case ready.cashuOption of
                            Just cashu ->
                                if amountSats <= cashu.availableBalance then
                                    Just amountSats

                                else
                                    Nothing

                            Nothing ->
                                Nothing

        Nothing ->
            Nothing


signedEventResponseDecoder : Decode.Decoder { requestId : Int, eventJson : String }
signedEventResponseDecoder =
    Decode.map2
        (\requestId eventValue ->
            { requestId = requestId
            , eventJson = Encode.encode 0 eventValue
            }
        )
        (Decode.field "requestId" Decode.int)
        (Decode.field "event" Decode.value)


updateReadyAmount : DialogState -> String -> DialogState
updateReadyAmount dialog amountDraft =
    case dialog of
        Ready ready ->
            Ready { ready | amountDraft = amountDraft }

        ErrorState _ (Just ready) ->
            Ready { ready | amountDraft = amountDraft }

        _ ->
            dialog


updateReadyAmountPreset : DialogState -> String -> DialogState
updateReadyAmountPreset dialog amountDraft =
    case dialog of
        Ready ready ->
            Ready
                { ready
                    | amountDraft = amountDraft
                    , amountInputKey = ready.amountInputKey + 1
                }

        ErrorState _ (Just ready) ->
            Ready
                { ready
                    | amountDraft = amountDraft
                    , amountInputKey = ready.amountInputKey + 1
                }

        _ ->
            dialog


digitsOnly : String -> String
digitsOnly value =
    String.filter Char.isDigit value


updateReadyComment : DialogState -> String -> DialogState
updateReadyComment dialog comment =
    case dialog of
        Ready ready ->
            Ready { ready | comment = comment }

        ErrorState _ (Just ready) ->
            Ready { ready | comment = comment }

        _ ->
            dialog


updateReadyMethod : DialogState -> PaymentMethod -> DialogState
updateReadyMethod dialog method =
    case dialog of
        Ready ready ->
            Ready { ready | method = method }

        ErrorState message (Just ready) ->
            ErrorState message (Just { ready | method = method })

        _ ->
            dialog


defaultAmountSats : Lud16.LightningPaymentData -> Int
defaultAmountSats payData =
    let
        minSats =
            payData.minSendable // 1000

        preferred =
            21
    in
    if preferred < minSats then
        minSats

    else
        preferred


recipientDisplayName : Maybe Profile -> PubKey -> String
recipientDisplayName maybeProfile pubKey =
    maybeProfile
        |> Maybe.andThen
            (\profile ->
                case ( profile.displayName, profile.name ) of
                    ( Just displayName, _ ) ->
                        Just displayName

                    ( Nothing, Just name ) ->
                        Just name

                    _ ->
                        Nothing
            )
        |> Maybe.withDefault (String.left 8 pubKey ++ "…")


buildZapRequest : PubKey -> ReadyData -> Int -> Time.Posix -> Bool -> Event
buildZapRequest signerPubKey ready amountMsats now anonymous =
    let
        base =
            emptyEvent signerPubKey KindZapRequest

        relayList =
            ready.target.relays
                |> Set.toList

        tags =
            [ PublicKeyTag ready.target.recipientPubKey Nothing Nothing
            , RelaysTag relayList
            , GenericTag [ "amount", String.fromInt amountMsats ]
            ]
                ++ interactionObjectTags ready.target.interactionObject
                ++ (if anonymous then
                        [ GenericTag [ "anon" ] ]

                    else
                        []
                   )
    in
    { base
        | content = ready.comment
        , createdAt = now
        , tags = tags
    }


interactionObjectTags : InteractionObject -> List Tag
interactionObjectTags interactionObject =
    case interactionObject of
        Article eventId addressComponents ->
            [ EventIdTag eventId Nothing Nothing Nothing
            , AddressTag addressComponents Nothing Nothing
            ]

        Comment eventId _ ->
            [ EventIdTag eventId Nothing Nothing Nothing
            , KindTag KindComment
            ]

        PicturePost eventId _ ->
            [ EventIdTag eventId Nothing Nothing Nothing
            , KindTag KindPicture
            ]

        ProfilePubKey _ ->
            []



-- SETTINGS


type ZapButtonDialog msg
    = Settings
        { browserEnv : BrowserEnv
        , model : Model
        , instanceId : Maybe String
        , interactionObject : InteractionObject
        , loginStatus : LoginStatus
        , nostr : Nostr.Model
        , showDialog : Bool
        , showLabel : Bool
        , relayUrls : Set String
        , toMsg : Msg -> msg
        , theme : Theme
        }


new :
    { browserEnv : BrowserEnv
    , model : Model
    , interactionObject : InteractionObject
    , loginStatus : LoginStatus
    , nostr : Nostr.Model
    , toMsg : Msg -> msg
    , theme : Theme
    }
    -> ZapButtonDialog msg
new props =
    Settings
        { browserEnv = props.browserEnv
        , model = props.model
        , instanceId = Nothing
        , interactionObject = props.interactionObject
        , loginStatus = props.loginStatus
        , nostr = props.nostr
        , showDialog = True
        , showLabel = True
        , relayUrls = Set.empty
        , toMsg = props.toMsg
        , theme = props.theme
        }


withoutLabel : ZapButtonDialog msg -> ZapButtonDialog msg
withoutLabel (Settings settings) =
    Settings { settings | showLabel = False }


withoutDialog : ZapButtonDialog msg -> ZapButtonDialog msg
withoutDialog (Settings settings) =
    Settings { settings | showDialog = False }


withInstanceId : String -> ZapButtonDialog msg -> ZapButtonDialog msg
withInstanceId instanceId (Settings settings) =
    Settings { settings | instanceId = Just instanceId }


withRelayUrls : Set String -> ZapButtonDialog msg -> ZapButtonDialog msg
withRelayUrls relayUrls (Settings settings) =
    Settings { settings | relayUrls = relayUrls }



-- VIEW


view : ZapButtonDialog msg -> Html msg
view (Settings settings) =
    let
        (Model model) =
            settings.model

        label =
            if settings.showLabel then
                Just (getZapAmount settings.browserEnv settings.nostr settings.interactionObject)

            else
                Nothing

        dialog =
            if settings.showDialog then
                viewDialog (Settings settings)
                    |> Html.map settings.toMsg

            else
                emptyHtml
    in
    Html.div []
        [ InteractionButton.new
            { model = model.button
            , unreactedIcon = Icon.FeatherIcon FeatherIcons.zap
            , reactedIcon = Icon.FeatherIcon FeatherIcons.zap
            , reacted = False
            , toMsg = InteractionButtonMsg
            , theme = settings.theme
            }
            |> InteractionButton.withLabel label
            |> InteractionButton.withOnClickAction (Just (InteractionButton.SendMsg (OpenDialog settings.relayUrls)))
            |> InteractionButton.withTestAttribute "zap-button"
            |> InteractionButton.view
            |> Html.map settings.toMsg
        , dialog
        ]


viewDialog : ZapButtonDialog msg -> Html Msg
viewDialog (Settings settings) =
    let
        (Model model) =
            settings.model
    in
    case model.dialog of
        Hidden ->
            emptyHtml

        LoadingPayData target flags ->
            dialogShell settings.theme
                (Translations.dialogTitle [ settings.browserEnv.translations ])
                [ viewBusyContent settings.browserEnv
                    target
                    (loadingPayDataStatus settings.browserEnv.translations flags)
                ]

        SigningZapRequest ready ->
            dialogShell settings.theme
                (Translations.dialogTitle [ settings.browserEnv.translations ])
                [ viewBusyContent settings.browserEnv
                    ready.target
                    (Translations.signingZapText [ settings.browserEnv.translations ])
                ]

        LoadingInvoice ready ->
            dialogShell settings.theme
                (Translations.dialogTitle [ settings.browserEnv.translations ])
                [ viewBusyContent settings.browserEnv
                    ready.target
                    (Translations.loadingInvoiceText [ settings.browserEnv.translations ])
                ]

        SendingNutzap ready _ ->
            dialogShell settings.theme
                (Translations.dialogTitle [ settings.browserEnv.translations ])
                [ viewBusyContent settings.browserEnv
                    ready.target
                    (Translations.sendingNutzapText [ settings.browserEnv.translations ])
                ]

        Ready ready ->
            dialogShell settings.theme
                (Translations.dialogTitle [ settings.browserEnv.translations ])
                [ viewReadyContent settings.browserEnv settings.theme ready ]

        ShowingInvoice invoiceView ->
            dialogShell settings.theme
                (Translations.invoiceTitle [ settings.browserEnv.translations ])
                [ viewInvoiceContent settings.browserEnv settings.theme settings.instanceId invoiceView ]

        PayingWithNwc invoiceView ->
            dialogShell settings.theme
                (Translations.invoiceTitle [ settings.browserEnv.translations ])
                [ viewBusyContent settings.browserEnv
                    invoiceView.ready.target
                    (Translations.payingWithNwcText [ settings.browserEnv.translations ])
                ]

        Success successView ->
            dialogShell settings.theme
                (Translations.successTitle [ settings.browserEnv.translations ])
                [ viewSuccessContent settings.browserEnv settings.theme successView ]

        ErrorState message maybeReady ->
            dialogShell settings.theme
                (Translations.dialogTitle [ settings.browserEnv.translations ])
                [ Html.p
                    [ Attr.css
                        [ Tw.text_sm
                        , Tw.font_medium
                        , Tw.text_color Theme.red_600
                        , Tw.mb_3
                        ]
                    ]
                    [ Html.text message ]
                , case maybeReady of
                    Just ready ->
                        viewReadyContent settings.browserEnv settings.theme ready

                    Nothing ->
                        emptyHtml
                ]


dialogShell : Theme -> String -> List (Html Msg) -> Html Msg
dialogShell theme title content =
    ModalDialog.new
        { title = title
        , buttons = []
        , content = content
        , onClose = CloseDialog
        , theme = theme
        }
        |> ModalDialog.view


viewReadyContent : BrowserEnv -> Theme -> ReadyData -> Html Msg
viewReadyContent browserEnv theme ready =
    let
        amountPresets =
            case ( ready.method, ready.payData, ready.cashuOption ) of
                ( Lightning, Just payData, _ ) ->
                    [ 21, 69, 420, 1337, 5000, 10000, 21000 ]
                        |> List.filter (\sats -> sats * 1000 >= payData.minSendable && sats * 1000 <= payData.maxSendable)

                ( Cashu, _, Just cashu ) ->
                    [ 21, 69, 420, 1337, 5000, 10000, 21000 ]
                        |> List.filter (\sats -> sats <= cashu.availableBalance)

                _ ->
                    [ 21 ]

        selectedSats =
            String.toInt (String.trim ready.amountDraft)

        showMethodToggle =
            ready.payData /= Nothing && ready.cashuOption /= Nothing
    in
    Html.div
        [ Attr.css [ Tw.flex, Tw.flex_col, Tw.gap_4, Tw.min_w_64 ] ]
        [ viewRecipientHeader browserEnv ready
        , if showMethodToggle then
            Html.div [ Attr.css [ Tw.flex, Tw.flex_row, Tw.gap_2 ] ]
                [ methodButton theme ready.method Lightning (Translations.lightningMethodLabel [ browserEnv.translations ])
                , methodButton theme ready.method Cashu (Translations.cashuMethodLabel [ browserEnv.translations ])
                ]

          else
            emptyHtml
        , case ready.cashuOption of
            Just cashu ->
                if ready.method == Cashu then
                    Html.p [ Attr.css [ Tw.text_sm, Tw.text_color Theme.gray_500 ] ]
                        [ Html.text
                            (Translations.cashuBalanceLabel [ browserEnv.translations ]
                                ++ ": "
                                ++ String.fromInt cashu.availableBalance
                                ++ " sats"
                            )
                        ]

                else
                    emptyHtml

            Nothing ->
                emptyHtml
        , Html.div []
            [ Html.p [ Attr.css [ Tw.text_sm, Tw.font_medium, Tw.mb_2 ] ] [ Html.text (Translations.amountLabel [ browserEnv.translations ]) ]
            , Html.div [ Attr.css [ Tw.flex, Tw.flex_wrap, Tw.gap_2, Tw.mb_3 ] ]
                (List.map (amountButton theme selectedSats) amountPresets)
            , viewAmountInput browserEnv theme ready
            ]
        , viewCommentField browserEnv theme ready
        , Html.div [ Attr.css [ Tw.flex, Tw.flex_row, Tw.gap_2, Tw.justify_end ] ]
            [ Button.new
                { label = Translations.closeButtonTitle [ browserEnv.translations ]
                , onClick = Just CloseDialog
                , theme = theme
                }
                |> Button.withTypeSecondary
                |> Button.view
            , Button.new
                { label = Translations.zapButtonTitle [ browserEnv.translations ]
                , onClick = Just ConfirmZap
                , theme = theme
                }
                |> Button.withTypePrimary
                |> Button.view
            ]
        ]


viewAmountInput : BrowserEnv -> Theme -> ReadyData -> Html Msg
viewAmountInput browserEnv theme ready =
    let
        styles =
            stylesForTheme theme
    in
    Keyed.node "div"
        [ Attr.css [ Tw.w_full ] ]
        [ ( "zap-amount-" ++ String.fromInt ready.amountInputKey
          , Html.input
                (styles.colorStyleBackground
                    ++ styles.colorStyleGrayscaleText
                    ++ [ Attr.type_ "text"
                       , Attr.attribute "inputmode" "numeric"
                       , Attr.attribute "pattern" "[0-9]*"
                       , Attr.autocomplete False
                       , Attr.spellcheck False
                       , Attr.placeholder (Translations.amountPlaceholder [ browserEnv.translations ])
                       , Attr.value ready.amountDraft
                       , Events.onInput SetAmountInput
                       , Attr.css
                            [ Tw.appearance_none
                            , Tw.bg_scroll
                            , Tw.bg_clip_border
                            , Tw.rounded_md
                            , Tw.border_2
                            , Tw.box_border
                            , Tw.cursor_text
                            , Tw.block
                            , Tw.ps_2
                            , Tw.pe_2
                            , Tw.pl_2
                            , Tw.pr_2
                            , Tw.h_10
                            , Tw.w_full
                            ]
                       ]
                )
                []
          )
        ]
viewCommentField : BrowserEnv -> Theme -> ReadyData -> Html Msg
viewCommentField browserEnv theme ready =
    let
        allowComment =
            case ready.method of
                Cashu ->
                    True

                Lightning ->
                    Maybe.withDefault 0 (Maybe.andThen .commentAllowed ready.payData) > 0
    in
    if allowComment then
        EntryField.new
            { value = ready.comment
            , onInput = SetComment
            , theme = theme
            }
            |> EntryField.withPlaceholder (Translations.commentPlaceholder [ browserEnv.translations ])
            |> EntryField.withRows 2
            |> EntryField.view

    else
        emptyHtml


methodButton : Theme -> PaymentMethod -> PaymentMethod -> String -> Html Msg
methodButton theme selected method label =
    Button.new
        { label = label
        , onClick = Just (SetPaymentMethod method)
        , theme = theme
        }
        |> (if selected == method then
                Button.withTypePrimary

            else
                Button.withTypeSecondary
           )
        |> Button.view


viewBusyContent : BrowserEnv -> Target -> String -> Html Msg
viewBusyContent browserEnv target statusText =
    Html.div
        [ Attr.css
            [ Tw.flex
            , Tw.flex_col
            , Tw.items_center
            , Tw.gap_4
            , Tw.py_4
            , Tw.min_w_64
            ]
        ]
        [ viewTargetHeader browserEnv target Nothing
        , Icon.FeatherIcon FeatherIcons.loader
            |> Icon.viewWithSize 28
        , Html.p
            [ Attr.css
                [ Tw.text_sm
                , Tw.font_medium
                , Tw.text_center
                , Tw.text_color Theme.gray_600
                ]
            ]
            [ Html.text statusText ]
        ]


loadingPayDataStatus : I18Next.Translations -> LoadingFlags -> String
loadingPayDataStatus translations flags =
    case ( flags.waitingLightning, flags.waitingCashu ) of
        ( True, True ) ->
            Translations.loadingPaymentOptionsText [ translations ]

        ( True, False ) ->
            Translations.loadingLightningText [ translations ]

        ( False, True ) ->
            Translations.loadingCashuText [ translations ]

        ( False, False ) ->
            Translations.loadingText [ translations ]


viewTargetHeader : BrowserEnv -> Target -> Maybe String -> Html Msg
viewTargetHeader browserEnv target maybeSubtitle =
    let
        defaultPicture =
            "/images/avatars/placeholder_01.webp"

        pictureSources =
            case target.recipientPicture of
                Just url ->
                    Ui.Links.scaledImageSources browserEnv.environment 80 url

                Nothing ->
                    { src = defaultPicture
                    , srcset = defaultPicture ++ " 1x, " ++ defaultPicture ++ " 2x"
                    }

        subtitle =
            case maybeSubtitle of
                Just value ->
                    value

                Nothing ->
                    target.lud16
                        |> Maybe.map Lud16.lud16ToString
                        |> Maybe.withDefault ""
    in
    Html.div
        [ Attr.css [ Tw.flex, Tw.flex_col, Tw.items_center, Tw.gap_2, Tw.text_center ] ]
        [ Html.p [ Attr.css [ Tw.text_lg, Tw.font_semibold ] ] [ Html.text target.recipientName ]
        , Html.img
            [ Attr.src pictureSources.src
            , Attr.attribute "srcset" pictureSources.srcset
            , Attr.alt target.recipientName
            , Attr.css
                [ Tw.w_20
                , Tw.h_20
                , Tw.rounded_full
                , Tw.object_cover
                ]
            ]
            []
        , if String.isEmpty subtitle then
            emptyHtml

          else
            Html.p [ Attr.css [ Tw.text_sm, Tw.text_color Theme.gray_500 ] ] [ Html.text subtitle ]
        ]


viewRecipientHeader : BrowserEnv -> ReadyData -> Html Msg
viewRecipientHeader browserEnv ready =
    let
        subtitle =
            case ( ready.method, ready.target.lud16, ready.cashuOption ) of
                ( Lightning, Just lud16, _ ) ->
                    Just (Lud16.lud16ToString lud16)

                ( Cashu, _, Just cashu ) ->
                    Just cashu.mintUrl

                ( _, Just lud16, _ ) ->
                    Just (Lud16.lud16ToString lud16)

                ( _, _, Just cashu ) ->
                    Just cashu.mintUrl

                _ ->
                    Nothing
    in
    viewTargetHeader browserEnv ready.target subtitle


amountButton : Theme -> Maybe Int -> Int -> Html Msg
amountButton theme selectedSats sats =
    let
        isSelected =
            selectedSats == Just sats
    in
    Button.new
        { label = String.fromInt sats
        , onClick = Just (SetAmount sats)
        , theme = theme
        }
        |> (if isSelected then
                Button.withTypePrimary

            else
                Button.withTypeSecondary
           )
        |> Button.view


viewInvoiceContent : BrowserEnv -> Theme -> Maybe String -> InvoiceView -> Html Msg
viewInvoiceContent browserEnv theme instanceId invoiceView =
    let
        qrCode =
            invoiceView.bolt11
                |> QRCode.fromString
                |> Result.map
                    (\qrcode ->
                        qrcode
                            |> QRCode.toSvg
                                [ SvgAttr.width "220px"
                                , SvgAttr.height "220px"
                                ]
                            |> Html.fromUnstyled
                    )
                |> Result.withDefault (Html.text "")

        buttonElementId =
            "zap-invoice-copy-"
                ++ (instanceId |> Maybe.withDefault "0")

        amountSats =
            parsedAmountSats invoiceView.ready
                |> Maybe.withDefault 0
    in
    Html.div
        [ Attr.css [ Tw.flex, Tw.flex_col, Tw.items_center, Tw.gap_3, Tw.min_w_64 ] ]
        [ viewRecipientHeader browserEnv invoiceView.ready
        , if amountSats > 0 then
            Html.p [ Attr.css [ Tw.text_lg, Tw.font_semibold ] ]
                [ Html.text (String.fromInt amountSats ++ " sats") ]

          else
            emptyHtml
        , Html.div
            [ Attr.css [ Tw.bg_color Theme.white, Tw.p_2, Tw.rounded_md ] ]
            [ qrCode ]
        , Html.p
            [ Attr.css [ Tw.text_xs, Tw.break_all, Tw.max_w_xs, Tw.text_center ] ]
            [ Html.text (String.left 40 invoiceView.bolt11 ++ "…") ]
        , Button.new
            { label = Translations.copyInvoiceButtonTitle [ browserEnv.translations ]
            , onClick = Just NoOp
            , theme = theme
            }
            |> Button.withId buttonElementId
            |> Button.withTypeSecondary
            |> Button.view
        , Html.node "js-clipboard-component"
            [ Attr.property "buttonId" (Encode.string buttonElementId)
            , Attr.property "copyContent" (Encode.string invoiceView.bolt11)
            , Events.on "copiedToClipboard" (Decode.succeed NoOp)
            ]
            []
        , Button.new
            { label = Translations.closeButtonTitle [ browserEnv.translations ]
            , onClick = Just CloseDialog
            , theme = theme
            }
            |> Button.withTypePrimary
            |> Button.view
        ]


viewSuccessContent : BrowserEnv -> Theme -> SuccessView -> Html Msg
viewSuccessContent browserEnv theme successView =
    Html.div
        [ Attr.css [ Tw.flex, Tw.flex_col, Tw.items_center, Tw.gap_3, Tw.py_2, Tw.min_w_64 ] ]
        [ viewRecipientHeader browserEnv successView.ready
        , Icon.FeatherIcon FeatherIcons.checkCircle
            |> Icon.viewWithSize 48
        , Html.p [ Attr.css [ Tw.text_base, Tw.font_medium, Tw.text_center ] ]
            [ Html.text (Translations.successText [ browserEnv.translations ]) ]
        , if successView.amountSats > 0 then
            Html.p [ Attr.css [ Tw.text_lg, Tw.font_semibold ] ]
                [ Html.text (String.fromInt successView.amountSats ++ " sats") ]

          else
            emptyHtml
        , Button.new
            { label = Translations.closeButtonTitle [ browserEnv.translations ]
            , onClick = Just CloseDialog
            , theme = theme
            }
            |> Button.withTypePrimary
            |> Button.view
        ]



-- HELPERS (from ZapButton)


{-| Relays where zap receipts can actually be published (not Pareto author-only).
-}
zapReceiptWatchRelays : Set String
zapReceiptWatchRelays =
    Pareto.recommendedOutboxRelays
        |> List.map Relay.toWire
        |> Set.fromList


extendedZapRelays : Set String -> Nostr.Model -> Maybe PubKey -> Set String
extendedZapRelays zapRelays nostr maybePubKey =
    let
        pubKeyRelays =
            maybePubKey
                |> Maybe.map (pubkeyRelays nostr)
                |> Maybe.withDefault Set.empty

        candidateRelays =
            Set.union zapRelays pubKeyRelays
    in
    if Set.size candidateRelays == Set.size zapRelays || Set.size candidateRelays == Set.size pubKeyRelays then
        Set.union candidateRelays zapReceiptWatchRelays

    else
        candidateRelays


pubkeyRelays : Nostr.Model -> PubKey -> Set String
pubkeyRelays nostrModel pubKey =
    pubKey
        |> Nostr.getNip65RelaysForPubKey nostrModel
        |> List.map (\( _, relay ) -> Relay.toWire relay.url)
        |> Set.fromList


getZapAmount : BrowserEnv -> Nostr.Model -> InteractionObject -> String
getZapAmount browserEnv nostr interactionObject =
    let
        zapPart =
            case interactionObject of
                Article _ addressComponents ->
                    TagReferenceCode addressComponents
                        |> Nostr.getZapReceiptsCountForTagReference nostr
                        |> Maybe.withDefault 0
                        |> formatZapNum browserEnv

                Comment eventId _ ->
                    TagReferenceEventId eventId
                        |> Nostr.getZapReceiptsCountForTagReference nostr
                        |> Maybe.withDefault 0
                        |> formatZapNum browserEnv

                PicturePost eventId _ ->
                    TagReferenceEventId eventId
                        |> Nostr.getZapReceiptsCountForTagReference nostr
                        |> Maybe.withDefault 0
                        |> formatZapNum browserEnv

                ProfilePubKey _ ->
                    ""

        nutzapPart =
            case interactionObject of
                Article _ addressComponents ->
                    TagReferenceCode addressComponents
                        |> Nostr.getNutzapsCountForTagReference nostr
                        |> Maybe.withDefault 0

                Comment eventId _ ->
                    TagReferenceEventId eventId
                        |> Nostr.getNutzapsCountForTagReference nostr
                        |> Maybe.withDefault 0

                PicturePost eventId _ ->
                    TagReferenceEventId eventId
                        |> Nostr.getNutzapsCountForTagReference nostr
                        |> Maybe.withDefault 0

                ProfilePubKey _ ->
                    0
    in
    if nutzapPart > 0 then
        zapPart ++ " · " ++ formatZapNum browserEnv (nutzapPart * 1000)

    else
        zapPart


formatZapNum : BrowserEnv -> Int -> String
formatZapNum browserEnv milliSats =
    browserEnv.formatNumber "0 a" <| toFloat (milliSats // 1000)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions (Model model) =
    Sub.batch
        [ InteractionButton.subscriptions model.button
            |> Sub.map InteractionButtonMsg
        , case model.dialog of
            Hidden ->
                Sub.none

            Ready _ ->
                -- Avoid port-driven re-renders while the user edits amount/comment.
                Sub.none

            ErrorState _ _ ->
                Sub.none

            Success _ ->
                Sub.none

            LoadingPayData _ _ ->
                Ports.receiveMessage ReceivedMessage

            SigningZapRequest _ ->
                Ports.receiveMessage ReceivedMessage

            LoadingInvoice _ ->
                Sub.none

            SendingNutzap _ _ ->
                Ports.receiveMessage ReceivedMessage

            ShowingInvoice _ ->
                Sub.batch
                    [ Ports.receiveMessage ReceivedMessage
                    , Time.every 3000 CheckZapPaid
                    ]

            PayingWithNwc _ ->
                Sub.batch
                    [ Ports.receiveMessage ReceivedMessage
                    , Time.every 3000 CheckZapPaid
                    ]
        ]
