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
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Nostr
import Nostr.Event exposing (Event, EventFilter, Kind(..), Tag(..), TagReference(..), emptyEvent, emptyEventFilter)
import Nostr.Lud16 as Lud16
import Nostr.Profile exposing (Profile)
import Nostr.Relay exposing (websocketUrl)
import Nostr.Types exposing (IncomingMessage, LoginStatus(..), PubKey, RelayUrl, loggedInPubKey, loggedInSigningPubKey)
import Nostr.Zaps as Zaps exposing (Invoice)
import Pareto
import Ports
import Process
import QRCode
import Set exposing (Set)
import Svg.Attributes as SvgAttr
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw
import Task
import Time
import Translations.ZapDialog as Translations
import Ui.Links
import Ui.Shared exposing (emptyHtml)
import Ui.Styles exposing (Theme)
import Url



-- MODEL


type Model
    = Model
        { button : InteractionButton.Model
        , dialog : DialogState
        , nextRequestId : Int
        }


type DialogState
    = Hidden
    | LoadingPayData Target
    | Ready ReadyData
    | SigningZapRequest ReadyData
    | LoadingInvoice ReadyData
    | ShowingInvoice InvoiceView
    | Success SuccessView
    | ErrorState String (Maybe ReadyData)


type alias Target =
    { interactionObject : InteractionObject
    , lud16 : Lud16.Lud16
    , recipientName : String
    , recipientPicture : Maybe String
    , recipientPubKey : PubKey
    , relays : Set RelayUrl
    }


type alias ReadyData =
    { target : Target
    , payData : Lud16.LightningPaymentData
    , amountDraft : String
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
        }



-- UPDATE


type Msg
    = InteractionButtonMsg (InteractionButton.Msg Msg)
    | OpenDialog (Set RelayUrl)
    | CloseDialog
    | SetAmount Int
    | SetAmountInput String
    | SetComment String
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
                ( Model { model | dialog = updateReadyAmount model.dialog (String.fromInt amountSats) }
                , Effect.none
                )

            SetAmountInput amountDraft ->
                ( Model { model | dialog = updateReadyAmount model.dialog amountDraft }
                , Effect.none
                )

            SetComment comment ->
                ( Model { model | dialog = updateReadyComment model.dialog comment }
                , Effect.none
                )

            ReceivedPayData (Ok payData) ->
                case model.dialog of
                    LoadingPayData target ->
                        ( Model
                            { model
                                | dialog =
                                    Ready
                                        { target = target
                                        , payData = payData
                                        , amountDraft = String.fromInt (defaultAmountSats payData)
                                        , comment = ""
                                        , signRequestId = Nothing
                                        }
                            }
                        , Effect.none
                        )

                    _ ->
                        ( Model model, Effect.none )

            ReceivedPayData (Err _) ->
                ( Model { model | dialog = ErrorState (Translations.errorText [ props.browserEnv.translations ]) Nothing }
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
        in
        ( Model
            { model
                | dialog = ShowingInvoice invoiceView
                , nextRequestId = requestId + 1
            }
        , watchZapReceipts requestId invoiceView
            |> Effect.map props.toMsg
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
        relays =
            invoiceView.ready.target.relays
                |> Set.insert "wss://relay.nostr.band"
                |> Set.toList

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
    -> Set RelayUrl
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
    in
    case maybeLud16 of
        Nothing ->
            ( Model
                { model
                    | dialog = ErrorState (Translations.noLud16Text [ props.browserEnv.translations ]) Nothing
                }
            , Effect.none
            )

        Just lud16 ->
            let
                target =
                    { interactionObject = props.interactionObject
                    , lud16 = lud16
                    , recipientName = recipientDisplayName maybeProfile recipientPubKey
                    , recipientPicture = maybeProfile |> Maybe.andThen .picture
                    , recipientPubKey = recipientPubKey
                    , relays = extendedZapRelays relayUrls props.nostr (loggedInPubKey props.loginStatus)
                    }
            in
            ( Model { model | dialog = LoadingPayData target }
            , Lud16.requestLightningPaymentData ReceivedPayData lud16
                |> Effect.sendCmd
                |> Effect.map props.toMsg
            )


confirmZap :
    { props
        | loginStatus : LoginStatus
        , browserEnv : BrowserEnv
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
                    let
                        amountMsats =
                            amountSats * 1000

                        wantsNostr =
                            ready.payData.allowsNostr == Just True

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

        _ ->
            ( Model model, Effect.none )


handleIncomingMessage :
    { props
        | browserEnv : BrowserEnv
        , nostr : Nostr.Model
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
                    case Decode.decodeValue (Decode.list Zaps.nostrZapReceiptDecoder) message.value of
                        Ok receipts ->
                            if List.any (\receipt -> receipt.bolt11 == invoiceView.bolt11) receipts then
                                succeedZap props (Model model) invoiceView.ready

                            else if hasNewReceiptForTarget props.nostr invoiceView then
                                succeedZap props (Model model) invoiceView.ready

                            else
                                ( Model model, Effect.none )

                        Err _ ->
                            -- Shared may still decode/store these; re-check shortly.
                            ( Model model
                            , Process.sleep 100
                                |> Task.andThen (\_ -> Time.now)
                                |> Task.perform CheckZapPaid
                                |> Effect.sendCmd
                                |> Effect.map props.toMsg
                            )

                _ ->
                    ( Model model, Effect.none )

        _ ->
            ( Model model, Effect.none )


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
    case parsedAmountSats ready of
        Just amountSats ->
            Zaps.fetchInvoice ReceivedInvoice
                (Url.toString ready.payData.callback)
                (amountSats * 1000)
                (Just ready.comment)
                maybeNostrJson
                |> Effect.sendCmd

        Nothing ->
            Effect.none


parsedAmountSats : ReadyData -> Maybe Int
parsedAmountSats ready =
    case String.toInt (String.trim ready.amountDraft) of
        Just amountSats ->
            let
                amountMsats =
                    amountSats * 1000
            in
            if amountSats > 0 && amountMsats >= ready.payData.minSendable && amountMsats <= ready.payData.maxSendable then
                Just amountSats

            else
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


updateReadyComment : DialogState -> String -> DialogState
updateReadyComment dialog comment =
    case dialog of
        Ready ready ->
            Ready { ready | comment = comment }

        ErrorState _ (Just ready) ->
            Ready { ready | comment = comment }

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
                |> List.map websocketUrl

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
        , showLabel : Bool
        , relayUrls : Set RelayUrl
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
        , showLabel = True
        , relayUrls = Set.empty
        , toMsg = props.toMsg
        , theme = props.theme
        }


withoutLabel : ZapButtonDialog msg -> ZapButtonDialog msg
withoutLabel (Settings settings) =
    Settings { settings | showLabel = False }


withInstanceId : String -> ZapButtonDialog msg -> ZapButtonDialog msg
withInstanceId instanceId (Settings settings) =
    Settings { settings | instanceId = Just instanceId }


withRelayUrls : Set RelayUrl -> ZapButtonDialog msg -> ZapButtonDialog msg
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
        , viewDialog (Settings settings)
            |> Html.map settings.toMsg
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

        LoadingPayData _ ->
            dialogShell settings.theme (Translations.dialogTitle [ settings.browserEnv.translations ]) [ Html.text (Translations.loadingText [ settings.browserEnv.translations ]) ]

        SigningZapRequest _ ->
            dialogShell settings.theme (Translations.dialogTitle [ settings.browserEnv.translations ]) [ Html.text (Translations.loadingText [ settings.browserEnv.translations ]) ]

        LoadingInvoice _ ->
            dialogShell settings.theme (Translations.dialogTitle [ settings.browserEnv.translations ]) [ Html.text (Translations.loadingText [ settings.browserEnv.translations ]) ]

        Ready ready ->
            dialogShell settings.theme
                (Translations.dialogTitle [ settings.browserEnv.translations ])
                [ viewReadyContent settings.browserEnv settings.theme ready ]

        ShowingInvoice invoiceView ->
            dialogShell settings.theme
                (Translations.invoiceTitle [ settings.browserEnv.translations ])
                [ viewInvoiceContent settings.browserEnv settings.theme settings.instanceId invoiceView ]

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
            [ 21, 69, 420, 1337, 5000, 10000, 21000 ]
                |> List.filter (\sats -> sats * 1000 >= ready.payData.minSendable && sats * 1000 <= ready.payData.maxSendable)

        selectedSats =
            String.toInt (String.trim ready.amountDraft)
    in
    Html.div
        [ Attr.css [ Tw.flex, Tw.flex_col, Tw.gap_4, Tw.min_w_64 ] ]
        [ viewRecipientHeader browserEnv ready
        , Html.div []
            [ Html.p [ Attr.css [ Tw.text_sm, Tw.font_medium, Tw.mb_2 ] ] [ Html.text (Translations.amountLabel [ browserEnv.translations ]) ]
            , Html.div [ Attr.css [ Tw.flex, Tw.flex_wrap, Tw.gap_2, Tw.mb_3 ] ]
                (List.map (amountButton theme selectedSats) amountPresets)
            , EntryField.new
                { value = ready.amountDraft
                , onInput = SetAmountInput
                , theme = theme
                }
                |> EntryField.withType EntryField.FieldTypeNumber
                |> EntryField.withPlaceholder (Translations.amountPlaceholder [ browserEnv.translations ])
                |> EntryField.view
            ]
        , if Maybe.withDefault 0 ready.payData.commentAllowed > 0 then
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


viewRecipientHeader : BrowserEnv -> ReadyData -> Html Msg
viewRecipientHeader browserEnv ready =
    let
        defaultPicture =
            "/images/avatars/placeholder_01.webp"

        pictureSources =
            case ready.target.recipientPicture of
                Just url ->
                    Ui.Links.scaledImageSources browserEnv.environment 80 url

                Nothing ->
                    { src = defaultPicture
                    , srcset = defaultPicture ++ " 1x, " ++ defaultPicture ++ " 2x"
                    }
    in
    Html.div
        [ Attr.css [ Tw.flex, Tw.flex_col, Tw.items_center, Tw.gap_2, Tw.text_center ] ]
        [ Html.p [ Attr.css [ Tw.text_lg, Tw.font_semibold ] ] [ Html.text ready.target.recipientName ]
        , Html.img
            [ Attr.src pictureSources.src
            , Attr.attribute "srcset" pictureSources.srcset
            , Attr.alt ready.target.recipientName
            , Attr.css
                [ Tw.w_20
                , Tw.h_20
                , Tw.rounded_full
                , Tw.object_cover
                ]
            ]
            []
        , Html.p [ Attr.css [ Tw.text_sm, Tw.text_color Theme.gray_500 ] ] [ Html.text (Lud16.lud16ToString ready.target.lud16) ]
        ]


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
    in
    Html.div
        [ Attr.css [ Tw.flex, Tw.flex_col, Tw.items_center, Tw.gap_3 ] ]
        [ Html.div
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


extendedZapRelays : Set String -> Nostr.Model -> Maybe PubKey -> Set String
extendedZapRelays zapRelays nostr maybePubKey =
    let
        pubKeyRelays =
            maybePubKey
                |> Maybe.map (pubkeyRelays nostr)
                |> Maybe.withDefault Set.empty

        defaultRelays =
            Set.fromList nostr.defaultRelays
                |> Set.map websocketUrl

        candidateRelays =
            Set.union zapRelays pubKeyRelays
                |> Set.map websocketUrl
    in
    if Set.size candidateRelays == Set.size zapRelays || Set.size candidateRelays == Set.size pubKeyRelays then
        Set.union candidateRelays defaultRelays

    else
        candidateRelays


pubkeyRelays : Nostr.Model -> PubKey -> Set String
pubkeyRelays nostrModel pubKey =
    pubKey
        |> Nostr.getNip65RelaysForPubKey nostrModel
        |> List.map (\( _, relay ) -> websocketUrl relay.urlWithoutProtocol)
        |> Set.fromList


getZapAmount : BrowserEnv -> Nostr.Model -> InteractionObject -> String
getZapAmount browserEnv nostr interactionObject =
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


formatZapNum : BrowserEnv -> Int -> String
formatZapNum browserEnv milliSats =
    browserEnv.formatNumber "0 a" <| toFloat (milliSats // 1000)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions (Model model) =
    Sub.batch
        [ InteractionButton.subscriptions model.button
            |> Sub.map InteractionButtonMsg
        , Ports.receiveMessage ReceivedMessage
        , case model.dialog of
            ShowingInvoice _ ->
                Time.every 3000 CheckZapPaid

            _ ->
                Sub.none
        ]
