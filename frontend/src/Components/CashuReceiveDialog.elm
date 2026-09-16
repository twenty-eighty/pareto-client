module Components.CashuReceiveDialog exposing
    ( Model
    , Msg(..)
    , CashuReceiveDialog
    , init
    , isOpen
    , new
    , update
    , view
    )

{-| Lightning → Cashu mint receive dialog (Settings eCash).
-}

import BrowserEnv exposing (BrowserEnv)
import Components.Button as Button
import Components.ModalDialog as ModalDialog
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, div, input, option, p, select, text)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events as Events
import Json.Decode as Decode
import Json.Decode.Pipeline as DecodePipeline
import Json.Encode as Encode
import Nostr.CashuWallet as CashuWallet
import Nostr.Send exposing (SendRequest(..))
import Nostr.Types exposing (IncomingMessage, PubKey)
import Ports
import QRCode
import Shared.Msg
import Svg.Attributes as SvgAttr
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw
import Translations.CashuReceiveDialog as Translations
import Ui.Shared exposing (emptyHtml)
import Ui.Styles exposing (Theme, stylesForTheme)


type CashuReceiveDialog msg
    = Settings
        { model : Model
        , toMsg : Msg -> msg
        , browserEnv : BrowserEnv
        , theme : Theme
        , mints : List String
        , userPubKey : PubKey
        }


new :
    { model : Model
    , toMsg : Msg -> msg
    , browserEnv : BrowserEnv
    , theme : Theme
    , mints : List String
    , userPubKey : PubKey
    }
    -> CashuReceiveDialog msg
new props =
    Settings
        { model = props.model
        , toMsg = props.toMsg
        , browserEnv = props.browserEnv
        , theme = props.theme
        , mints = props.mints
        , userPubKey = props.userPubKey
        }


type Model
    = Model Internal


type alias Internal =
    { open : Bool
    , amountDraft : String
    , selectedMintUrl : Maybe String
    , receiveState : ReceiveState
    , nextRequestId : Int
    }


type ReceiveState
    = ReceiveIdle
    | ReceiveCreating Int
    | ReceiveAwaitingPayment Invoice
    | ReceiveSuccess Int
    | ReceiveError String


type alias Invoice =
    { requestId : Int
    , mintUrl : String
    , amount : Int
    , quote : String
    , bolt11 : String
    }


init : List String -> Model
init mints =
    Model
        { open = False
        , amountDraft = "100"
        , selectedMintUrl = List.head mints
        , receiveState = ReceiveIdle
        , nextRequestId = 1
        }


isOpen : Model -> Bool
isOpen (Model model) =
    model.open


type Msg
    = OpenDialog
    | CloseDialog
    | UpdateAmount String
    | SelectMint String
    | StartReceive
    | CancelReceive
    | ResetReceive
    | NoOp
    | ReceivedPortMessage IncomingMessage


update :
    { msg : Msg
    , model : Model
    , toModel : Model -> model
    , toMsg : Msg -> msg
    , mints : List String
    , userPubKey : PubKey
    , browserEnv : BrowserEnv
    }
    -> ( model, Effect msg )
update props =
    let
        (Model model) =
            props.model

        toParent ( newInternal, effect ) =
            ( props.toModel (Model newInternal)
            , Effect.map props.toMsg effect
            )
    in
    case props.msg of
        OpenDialog ->
            toParent
                ( { model
                    | open = True
                    , selectedMintUrl =
                        case model.selectedMintUrl of
                            Just selected ->
                                if List.member selected props.mints then
                                    Just selected

                                else
                                    List.head props.mints

                            Nothing ->
                                List.head props.mints
                    , receiveState = ReceiveIdle
                  }
                , Effect.none
                )

        CloseDialog ->
            cancelReceive model
                |> (\( cancelled, effect ) ->
                        toParent ( { cancelled | open = False, receiveState = ReceiveIdle }, effect )
                   )

        UpdateAmount draft ->
            toParent ( { model | amountDraft = digitsOnly draft }, Effect.none )

        SelectMint mintUrl ->
            toParent ( { model | selectedMintUrl = Just mintUrl }, Effect.none )

        StartReceive ->
            case ( parseAmount model.amountDraft, selectedMint model props.mints ) of
                ( Just amount, Just mintUrl ) ->
                    if receiveInProgress model.receiveState then
                        ( props.toModel props.model, Effect.none )

                    else
                        let
                            requestId =
                                model.nextRequestId
                        in
                        toParent
                            ( { model
                                | receiveState = ReceiveCreating requestId
                                , nextRequestId = requestId + 1
                              }
                            , Effect.sendCmd
                                (Ports.createCashuMintQuote
                                    { requestId = requestId
                                    , mintUrl = mintUrl
                                    , amount = amount
                                    }
                                )
                            )

                _ ->
                    toParent
                        ( { model
                            | receiveState =
                                ReceiveError (Translations.amountPlaceholder [ props.browserEnv.translations ])
                          }
                        , Effect.none
                        )

        CancelReceive ->
            cancelReceive model
                |> (\( cancelled, effect ) ->
                        toParent ( { cancelled | receiveState = ReceiveIdle }, effect )
                   )

        ResetReceive ->
            toParent ( { model | receiveState = ReceiveIdle }, Effect.none )

        NoOp ->
            ( props.toModel props.model, Effect.none )

        ReceivedPortMessage message ->
            updateWithPortMessage props.userPubKey model message
                |> (\( newInternal, effect ) ->
                        toParent ( newInternal, effect )
                   )


cancelReceive : Internal -> ( Internal, Effect Msg )
cancelReceive model =
    case receiveRequestId model.receiveState of
        Just requestId ->
            ( model
            , Effect.sendCmd (Ports.cancelCashuMintQuote requestId)
            )

        Nothing ->
            ( model, Effect.none )


updateWithPortMessage : PubKey -> Internal -> IncomingMessage -> ( Internal, Effect Msg )
updateWithPortMessage userPubKey model message =
    case message.messageType of
        "cashuMintInvoice" ->
            case Decode.decodeValue invoiceDecoder message.value of
                Ok invoice ->
                    if matchesRequestId model.receiveState invoice.requestId then
                        ( { model | receiveState = ReceiveAwaitingPayment invoice }
                        , Effect.none
                        )

                    else
                        ( model, Effect.none )

                Err _ ->
                    ( model, Effect.none )

        "cashuMinted" ->
            case Decode.decodeValue mintedDecoder message.value of
                Ok minted ->
                    if matchesRequestId model.receiveState minted.requestId then
                        let
                            tokenEvt =
                                CashuWallet.tokenEvent userPubKey minted.mintUrl minted.proofs []

                            historyEvt =
                                CashuWallet.historyEvent userPubKey
                                    { direction = CashuWallet.HistoryIn
                                    , amount = minted.amount
                                    , nutzapEventId = Nothing
                                    , counterpartPubKey = ""
                                    , createdTokenEventId = Nothing
                                    }
                        in
                        ( { model | receiveState = ReceiveSuccess minted.amount }
                        , Effect.batch
                            [ Shared.Msg.AddCashuBalance minted.amount
                                |> Effect.sendSharedMsg
                            , tokenEvt
                                |> SendCashuTokens
                                |> Shared.Msg.SendNostrEvent
                                |> Effect.sendSharedMsg
                            , historyEvt
                                |> SendCashuHistory
                                |> Shared.Msg.SendNostrEvent
                                |> Effect.sendSharedMsg
                            ]
                        )

                    else
                        ( model, Effect.none )

                Err _ ->
                    ( model, Effect.none )

        "cashuMintFailed" ->
            case Decode.decodeValue failedDecoder message.value of
                Ok failed ->
                    if matchesRequestId model.receiveState failed.requestId then
                        ( { model | receiveState = ReceiveError failed.reason }
                        , Effect.none
                        )

                    else
                        ( model, Effect.none )

                Err _ ->
                    ( model, Effect.none )

        "cashuMintCancelled" ->
            case Decode.decodeValue (Decode.field "requestId" Decode.int) message.value of
                Ok requestId ->
                    if matchesRequestId model.receiveState requestId then
                        ( { model | receiveState = ReceiveIdle }
                        , Effect.none
                        )

                    else
                        ( model, Effect.none )

                Err _ ->
                    ( model, Effect.none )

        _ ->
            ( model, Effect.none )


view : CashuReceiveDialog msg -> Html msg
view (Settings settings) =
    let
        (Model model) =
            settings.model

        translations =
            settings.browserEnv.translations

        openButton =
            Button.new
                { label = Translations.dialogTitle [ translations ]
                , onClick = Just (settings.toMsg OpenDialog)
                , theme = settings.theme
                }
                |> Button.withTypePrimary
                |> Button.withDisabled (List.isEmpty settings.mints)
                |> Button.view
    in
    div
        [ css [ Tw.flex, Tw.flex_col, Tw.gap_2 ]
        , Attr.attribute "data-test" "ecash-receive"
        ]
        [ openButton
        , if model.open then
            viewDialog settings model
                |> Html.map settings.toMsg

          else
            emptyHtml
        ]


viewDialog : { a | browserEnv : BrowserEnv, theme : Theme, mints : List String } -> Internal -> Html Msg
viewDialog settings model =
    let
        translations =
            settings.browserEnv.translations

        styles =
            stylesForTheme settings.theme
    in
    ModalDialog.new
        { title = Translations.dialogTitle [ translations ]
        , onClose = CloseDialog
        , theme = settings.theme
        , buttons = []
        , content =
            [ p
                (styles.colorStyleGrayscaleText ++ styles.textStyleBody)
                [ text <| Translations.dialogDescription [ translations ] ]
            , viewDialogBody settings model
            ]
        }
        |> ModalDialog.view


viewDialogBody : { a | browserEnv : BrowserEnv, theme : Theme, mints : List String } -> Internal -> Html Msg
viewDialogBody settings model =
    let
        translations =
            settings.browserEnv.translations

        styles =
            stylesForTheme settings.theme

        selectedMintUrl =
            selectedMint model settings.mints

        amountValid =
            parseAmount model.amountDraft /= Nothing

        canCreate =
            amountValid && selectedMintUrl /= Nothing && not (receiveInProgress model.receiveState)
    in
    case model.receiveState of
        ReceiveAwaitingPayment invoice ->
            viewInvoice settings invoice

        ReceiveCreating _ ->
            div
                [ css [ Tw.flex, Tw.flex_col, Tw.gap_3, Tw.items_center, Tw.py_2 ] ]
                [ p
                    (styles.colorStyleGrayscaleText ++ styles.textStyleBody)
                    [ text <| Translations.creatingInvoiceButtonTitle [ translations ] ]
                , Button.new
                    { label = Translations.cancelButtonTitle [ translations ]
                    , onClick = Just CancelReceive
                    , theme = settings.theme
                    }
                    |> Button.withTypeSecondary
                    |> Button.view
                ]

        ReceiveSuccess amount ->
            div
                [ css [ Tw.flex, Tw.flex_col, Tw.gap_3, Tw.items_center, Tw.py_2 ] ]
                [ p
                    (styles.colorStyleGrayscaleText ++ styles.textStyleBody)
                    [ text <|
                        Translations.receiveSuccessText [ translations ]
                            { amount = String.fromInt amount }
                    ]
                , Button.new
                    { label = Translations.receiveAgainButtonTitle [ translations ]
                    , onClick = Just ResetReceive
                    , theme = settings.theme
                    }
                    |> Button.withTypePrimary
                    |> Button.view
                , Button.new
                    { label = Translations.closeButtonTitle [ translations ]
                    , onClick = Just CloseDialog
                    , theme = settings.theme
                    }
                    |> Button.withTypeSecondary
                    |> Button.view
                ]

        ReceiveError reason ->
            div
                [ css [ Tw.flex, Tw.flex_col, Tw.gap_3, Tw.items_center, Tw.py_2 ] ]
                [ p
                    (styles.colorStyleGrayscaleText ++ styles.textStyleBody)
                    [ text reason ]
                , Button.new
                    { label = Translations.receiveAgainButtonTitle [ translations ]
                    , onClick = Just ResetReceive
                    , theme = settings.theme
                    }
                    |> Button.withTypeSecondary
                    |> Button.view
                ]

        ReceiveIdle ->
            div
                [ css [ Tw.flex, Tw.flex_col, Tw.gap_3, Tw.min_w_64 ] ]
                [ if List.length settings.mints > 1 then
                    div
                        [ css [ Tw.flex, Tw.flex_col, Tw.gap_1 ] ]
                        [ p
                            (styles.colorStyleGrayscaleText ++ styles.textStyleBody)
                            [ text <| Translations.mintLabel [ translations ] ]
                        , select
                            (styles.colorStyleBackground
                                ++ styles.colorStyleGrayscaleText
                                ++ [ Attr.value (Maybe.withDefault "" selectedMintUrl)
                                   , Events.onInput SelectMint
                                   , css
                                        [ Tw.h_10
                                        , Tw.rounded_md
                                        , Tw.border_2
                                        , Tw.px_2
                                        , Tw.w_full
                                        ]
                                   ]
                            )
                            (List.map
                                (\mint ->
                                    option
                                        [ Attr.value mint
                                        , Attr.selected (Just mint == selectedMintUrl)
                                        ]
                                        [ text mint ]
                                )
                                settings.mints
                            )
                        ]

                  else
                    emptyHtml
                , div
                    [ css [ Tw.flex, Tw.flex_col, Tw.gap_1 ] ]
                    [ p
                        (styles.colorStyleGrayscaleText ++ styles.textStyleBody)
                        [ text <| Translations.amountLabel [ translations ] ]
                    , input
                        (styles.colorStyleBackground
                            ++ styles.colorStyleGrayscaleText
                            ++ [ Attr.type_ "text"
                               , Attr.attribute "inputmode" "numeric"
                               , Attr.attribute "autocomplete" "off"
                               , Attr.placeholder <| Translations.amountPlaceholder [ translations ]
                               , Attr.value model.amountDraft
                               , Events.onInput UpdateAmount
                               , css
                                    [ Tw.h_10
                                    , Tw.rounded_md
                                    , Tw.border_2
                                    , Tw.px_3
                                    , Tw.w_40
                                    ]
                               ]
                        )
                        []
                    ]
                , Button.new
                    { label = Translations.createInvoiceButtonTitle [ translations ]
                    , onClick =
                        if canCreate then
                            Just StartReceive

                        else
                            Nothing
                    , theme = settings.theme
                    }
                    |> Button.withTypePrimary
                    |> Button.withDisabled (not canCreate)
                    |> Button.view
                ]


viewInvoice : { a | browserEnv : BrowserEnv, theme : Theme } -> Invoice -> Html Msg
viewInvoice settings invoice =
    let
        translations =
            settings.browserEnv.translations

        styles =
            stylesForTheme settings.theme

        qrCode =
            invoice.bolt11
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
                |> Result.withDefault (text "")

        buttonElementId =
            "cashu-receive-invoice-copy"
    in
    div
        [ css
            [ Tw.flex
            , Tw.flex_col
            , Tw.items_center
            , Tw.gap_3
            ]
        ]
        [ p
            (styles.colorStyleGrayscaleText ++ styles.textStyleBody)
            [ text <| Translations.waitingForPaymentText [ translations ] ]
        , p
            [ css [ Tw.text_lg, Tw.font_semibold ] ]
            [ text (String.fromInt invoice.amount ++ " sats") ]
        , div
            [ css [ Tw.bg_color Theme.white, Tw.p_2, Tw.rounded_md ] ]
            [ qrCode ]
        , p
            [ css [ Tw.text_xs, Tw.break_all, Tw.max_w_xs, Tw.text_center ] ]
            [ text (String.left 40 invoice.bolt11 ++ "…") ]
        , Button.new
            { label = Translations.copyInvoiceButtonTitle [ translations ]
            , onClick = Just NoOp
            , theme = settings.theme
            }
            |> Button.withId buttonElementId
            |> Button.withTypeSecondary
            |> Button.view
        , Html.node "js-clipboard-component"
            [ Attr.property "buttonId" (Encode.string buttonElementId)
            , Attr.property "copyContent" (Encode.string invoice.bolt11)
            , Events.on "copiedToClipboard" (Decode.succeed NoOp)
            ]
            []
        , Button.new
            { label = Translations.cancelButtonTitle [ translations ]
            , onClick = Just CancelReceive
            , theme = settings.theme
            }
            |> Button.withTypeSecondary
            |> Button.view
        ]



-- HELPERS


selectedMint : Internal -> List String -> Maybe String
selectedMint model mints =
    case model.selectedMintUrl of
        Just mintUrl ->
            if List.member mintUrl mints then
                Just mintUrl

            else
                List.head mints

        Nothing ->
            List.head mints


parseAmount : String -> Maybe Int
parseAmount draft =
    case String.toInt (String.trim draft) of
        Just amount ->
            if amount > 0 then
                Just amount

            else
                Nothing

        Nothing ->
            Nothing


digitsOnly : String -> String
digitsOnly value =
    String.filter Char.isDigit value


receiveRequestId : ReceiveState -> Maybe Int
receiveRequestId state =
    case state of
        ReceiveCreating requestId ->
            Just requestId

        ReceiveAwaitingPayment invoice ->
            Just invoice.requestId

        _ ->
            Nothing


receiveInProgress : ReceiveState -> Bool
receiveInProgress state =
    case state of
        ReceiveCreating _ ->
            True

        ReceiveAwaitingPayment _ ->
            True

        _ ->
            False


matchesRequestId : ReceiveState -> Int -> Bool
matchesRequestId state requestId =
    receiveRequestId state == Just requestId


invoiceDecoder : Decode.Decoder Invoice
invoiceDecoder =
    Decode.succeed Invoice
        |> DecodePipeline.required "requestId" Decode.int
        |> DecodePipeline.required "mintUrl" Decode.string
        |> DecodePipeline.required "amount" Decode.int
        |> DecodePipeline.required "quote" Decode.string
        |> DecodePipeline.required "bolt11" Decode.string


type alias Minted =
    { requestId : Int
    , mintUrl : String
    , amount : Int
    , proofs : List CashuWallet.CashuProof
    }


mintedDecoder : Decode.Decoder Minted
mintedDecoder =
    Decode.succeed Minted
        |> DecodePipeline.required "requestId" Decode.int
        |> DecodePipeline.required "mintUrl" Decode.string
        |> DecodePipeline.required "amount" Decode.int
        |> DecodePipeline.required "proofs" (Decode.list cashuProofDecoder)


type alias Failed =
    { requestId : Int
    , reason : String
    }


failedDecoder : Decode.Decoder Failed
failedDecoder =
    Decode.map2 Failed
        (Decode.field "requestId" Decode.int)
        (Decode.field "reason" Decode.string)


cashuProofDecoder : Decode.Decoder CashuWallet.CashuProof
cashuProofDecoder =
    Decode.succeed CashuWallet.CashuProof
        |> DecodePipeline.required "id" Decode.string
        |> DecodePipeline.required "amount" Decode.int
        |> DecodePipeline.required "secret" Decode.string
        |> DecodePipeline.required "C" Decode.string
