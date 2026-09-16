module Components.CashuWithdrawDialog exposing
    ( Model
    , Msg(..)
    , CashuWithdrawDialog
    , init
    , isOpen
    , new
    , update
    , view
    )

{-| Cashu → Lightning melt/withdraw dialog (Settings eCash).
-}

import BrowserEnv exposing (BrowserEnv)
import Components.Button as Button
import Components.ModalDialog as ModalDialog
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, div, input, option, p, select, text, textarea)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events as Events
import Json.Decode as Decode
import Json.Decode.Pipeline as DecodePipeline
import Nostr
import Nostr.CashuWallet as CashuWallet
import Nostr.Send exposing (SendRequest(..))
import Nostr.Types exposing (EventId, IncomingMessage, PubKey)
import Ports
import Shared.Msg
import Tailwind.Utilities as Tw
import Translations.CashuWithdrawDialog as Translations
import Ui.Shared exposing (emptyHtml)
import Ui.Styles exposing (Styles, Theme, stylesForTheme)


type CashuWithdrawDialog msg
    = Settings
        { model : Model
        , toMsg : Msg -> msg
        , browserEnv : BrowserEnv
        , theme : Theme
        , mints : List String
        , userPubKey : PubKey
        , nostr : Nostr.Model
        }


new :
    { model : Model
    , toMsg : Msg -> msg
    , browserEnv : BrowserEnv
    , theme : Theme
    , mints : List String
    , userPubKey : PubKey
    , nostr : Nostr.Model
    }
    -> CashuWithdrawDialog msg
new props =
    Settings
        { model = props.model
        , toMsg = props.toMsg
        , browserEnv = props.browserEnv
        , theme = props.theme
        , mints = props.mints
        , userPubKey = props.userPubKey
        , nostr = props.nostr
        }


type Model
    = Model Internal


type alias Internal =
    { open : Bool
    , invoiceDraft : String
    , selectedMintUrl : Maybe String
    , withdrawState : WithdrawState
    , nextRequestId : Int
    , pendingProofEventIds : List EventId
    }


type WithdrawState
    = WithdrawIdle
    | WithdrawCheckingQuote Int
    | WithdrawQuoteReady QuotePreview
    | WithdrawPaying Int
    | WithdrawSuccess Int
    | WithdrawError String


type alias QuotePreview =
    { requestId : Int
    , mintUrl : String
    , invoice : String
    , amount : Int
    , feeReserve : Int
    , total : Int
    }


init : List String -> Model
init mints =
    Model
        { open = False
        , invoiceDraft = ""
        , selectedMintUrl = List.head mints
        , withdrawState = WithdrawIdle
        , nextRequestId = 1
        , pendingProofEventIds = []
        }


isOpen : Model -> Bool
isOpen (Model model) =
    model.open


type Msg
    = OpenDialog
    | CloseDialog
    | UpdateInvoice String
    | SelectMint String
    | CheckQuote
    | ConfirmPay
    | ResetWithdraw
    | ReceivedPortMessage IncomingMessage


update :
    { msg : Msg
    , model : Model
    , toModel : Model -> model
    , toMsg : Msg -> msg
    , mints : List String
    , userPubKey : PubKey
    , browserEnv : BrowserEnv
    , nostr : Nostr.Model
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
                        preferredMint props.nostr props.mints model.selectedMintUrl
                    , withdrawState = WithdrawIdle
                    , invoiceDraft = ""
                    , pendingProofEventIds = []
                  }
                , Effect.none
                )

        CloseDialog ->
            toParent
                ( { model
                    | open = False
                    , withdrawState = WithdrawIdle
                    , invoiceDraft = ""
                    , pendingProofEventIds = []
                  }
                , Effect.none
                )

        UpdateInvoice draft ->
            toParent
                ( { model
                    | invoiceDraft = String.trim draft
                    , withdrawState =
                        case model.withdrawState of
                            WithdrawQuoteReady _ ->
                                WithdrawIdle

                            WithdrawError _ ->
                                WithdrawIdle

                            other ->
                                other
                  }
                , Effect.none
                )

        SelectMint mintUrl ->
            toParent
                ( { model
                    | selectedMintUrl = Just mintUrl
                    , withdrawState = WithdrawIdle
                  }
                , Effect.none
                )

        CheckQuote ->
            case ( normalizeInvoice model.invoiceDraft, selectedMint model props.mints ) of
                ( Just invoice, Just mintUrl ) ->
                    let
                        requestId =
                            model.nextRequestId
                    in
                    toParent
                        ( { model
                            | withdrawState = WithdrawCheckingQuote requestId
                            , nextRequestId = requestId + 1
                          }
                        , Effect.sendCmd
                            (Ports.createCashuMeltQuote
                                { requestId = requestId
                                , mintUrl = mintUrl
                                , invoice = invoice
                                }
                            )
                        )

                _ ->
                    toParent
                        ( { model
                            | withdrawState =
                                WithdrawError (Translations.invalidInvoiceText [ props.browserEnv.translations ])
                          }
                        , Effect.none
                        )

        ConfirmPay ->
            case model.withdrawState of
                WithdrawQuoteReady quote ->
                    let
                        ( proofs, eventIds ) =
                            Nostr.getCashuProofsForMint props.nostr quote.mintUrl

                        available =
                            List.map .amount proofs |> List.sum
                    in
                    if List.isEmpty proofs then
                        toParent
                            ( { model
                                | withdrawState =
                                    WithdrawError
                                        (Translations.insufficientBalanceText [ props.browserEnv.translations ]
                                            { needed = String.fromInt quote.total
                                            , available = "0"
                                            }
                                        )
                              }
                            , Effect.none
                            )

                    else if available < quote.total then
                        toParent
                            ( { model
                                | withdrawState =
                                    WithdrawError
                                        (Translations.insufficientBalanceText [ props.browserEnv.translations ]
                                            { needed = String.fromInt quote.total
                                            , available = String.fromInt available
                                            }
                                        )
                              }
                            , Effect.none
                            )

                    else
                        let
                            requestId =
                                model.nextRequestId
                        in
                        toParent
                            ( { model
                                | withdrawState = WithdrawPaying requestId
                                , nextRequestId = requestId + 1
                                , pendingProofEventIds = eventIds
                              }
                            , Effect.sendCmd
                                (Ports.meltCashuToLightning
                                    { requestId = requestId
                                    , mintUrl = quote.mintUrl
                                    , invoice = quote.invoice
                                    , proofs = List.map CashuWallet.encodeProof proofs
                                    }
                                )
                            )

                _ ->
                    ( props.toModel props.model, Effect.none )

        ResetWithdraw ->
            toParent
                ( { model
                    | withdrawState = WithdrawIdle
                    , pendingProofEventIds = []
                  }
                , Effect.none
                )

        ReceivedPortMessage message ->
            updateWithPortMessage props.userPubKey model message
                |> toParent


updateWithPortMessage : PubKey -> Internal -> IncomingMessage -> ( Internal, Effect Msg )
updateWithPortMessage userPubKey model message =
    case message.messageType of
        "cashuMeltQuote" ->
            case Decode.decodeValue quoteDecoder message.value of
                Ok quote ->
                    if matchesCheckingRequestId model.withdrawState quote.requestId then
                        ( { model | withdrawState = WithdrawQuoteReady quote }
                        , Effect.none
                        )

                    else
                        ( model, Effect.none )

                Err _ ->
                    ( model, Effect.none )

        "cashuMelted" ->
            case Decode.decodeValue meltedDecoder message.value of
                Ok melted ->
                    if matchesPayingRequestId model.withdrawState melted.requestId then
                        let
                            tokenEvt =
                                CashuWallet.tokenEvent userPubKey melted.mintUrl melted.keepProofs model.pendingProofEventIds

                            historyEvt =
                                CashuWallet.historyEvent userPubKey
                                    { direction = CashuWallet.HistoryOut
                                    , amount = melted.amount
                                    , nutzapEventId = Nothing
                                    , counterpartPubKey = ""
                                    , createdTokenEventId = Nothing
                                    }
                        in
                        ( { model
                            | withdrawState = WithdrawSuccess melted.amount
                            , pendingProofEventIds = []
                            , invoiceDraft = ""
                          }
                        , Effect.batch
                            [ Shared.Msg.AddCashuBalance -melted.total
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

        "cashuMeltFailed" ->
            case Decode.decodeValue failedDecoder message.value of
                Ok failed ->
                    if
                        matchesCheckingRequestId model.withdrawState failed.requestId
                            || matchesPayingRequestId model.withdrawState failed.requestId
                    then
                        ( { model
                            | withdrawState = WithdrawError failed.reason
                            , pendingProofEventIds = []
                          }
                        , Effect.none
                        )

                    else
                        ( model, Effect.none )

                Err _ ->
                    ( model, Effect.none )

        _ ->
            ( model, Effect.none )


view : CashuWithdrawDialog msg -> Html msg
view (Settings settings) =
    let
        (Model model) =
            settings.model

        translations =
            settings.browserEnv.translations

        balance =
            Nostr.getCashuBalance settings.nostr

        openButton =
            Button.new
                { label = Translations.dialogTitle [ translations ]
                , onClick = Just (settings.toMsg OpenDialog)
                , theme = settings.theme
                }
                |> Button.withTypeSecondary
                |> Button.withDisabled (balance <= 0 || List.isEmpty settings.mints)
                |> Button.view
    in
    div
        [ css [ Tw.flex, Tw.flex_col, Tw.gap_2 ]
        , Attr.attribute "data-test" "ecash-withdraw"
        ]
        [ openButton
        , if model.open then
            viewDialog settings model
                |> Html.map settings.toMsg

          else
            emptyHtml
        ]


viewDialog : { a | browserEnv : BrowserEnv, theme : Theme, mints : List String, nostr : Nostr.Model } -> Internal -> Html Msg
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


viewDialogBody : { a | browserEnv : BrowserEnv, theme : Theme, mints : List String, nostr : Nostr.Model } -> Internal -> Html Msg
viewDialogBody settings model =
    let
        translations =
            settings.browserEnv.translations

        styles =
            stylesForTheme settings.theme

        selectedMintUrl =
            selectedMint model settings.mints

        mintBalance =
            selectedMintUrl
                |> Maybe.map
                    (\mintUrl ->
                        Nostr.getCashuProofsForMint settings.nostr mintUrl
                            |> Tuple.first
                            |> List.map .amount
                            |> List.sum
                    )
                |> Maybe.withDefault 0

        invoiceOk =
            normalizeInvoice model.invoiceDraft /= Nothing
    in
    case model.withdrawState of
        WithdrawCheckingQuote _ ->
            centeredStatus settings
                (Translations.checkingQuoteButtonTitle [ translations ])
                Nothing

        WithdrawPaying _ ->
            centeredStatus settings
                (Translations.payingInvoiceButtonTitle [ translations ])
                Nothing

        WithdrawSuccess amount ->
            div
                [ css [ Tw.flex, Tw.flex_col, Tw.gap_3, Tw.items_center, Tw.py_2 ] ]
                [ p
                    (styles.colorStyleGrayscaleText ++ styles.textStyleBody)
                    [ text <|
                        Translations.successText [ translations ]
                            { amount = String.fromInt amount }
                    ]
                , Button.new
                    { label = Translations.closeButtonTitle [ translations ]
                    , onClick = Just CloseDialog
                    , theme = settings.theme
                    }
                    |> Button.withTypePrimary
                    |> Button.view
                ]

        WithdrawError reason ->
            div
                [ css [ Tw.flex, Tw.flex_col, Tw.gap_3, Tw.items_center, Tw.py_2 ] ]
                [ p
                    (styles.colorStyleGrayscaleText ++ styles.textStyleBody)
                    [ text reason ]
                , Button.new
                    { label = Translations.tryAgainButtonTitle [ translations ]
                    , onClick = Just ResetWithdraw
                    , theme = settings.theme
                    }
                    |> Button.withTypeSecondary
                    |> Button.view
                ]

        WithdrawQuoteReady quote ->
            div
                [ css [ Tw.flex, Tw.flex_col, Tw.gap_3, Tw.min_w_64 ] ]
                [ quoteRow styles
                    (Translations.quoteAmountLabel [ translations ])
                    (String.fromInt quote.amount ++ " sats")
                , quoteRow styles
                    (Translations.quoteFeeLabel [ translations ])
                    (String.fromInt quote.feeReserve ++ " sats")
                , quoteRow styles
                    (Translations.quoteTotalLabel [ translations ])
                    (String.fromInt quote.total ++ " sats")
                , p
                    (styles.colorStyleGrayscaleMuted ++ styles.textStyleBody)
                    [ text (Translations.balanceLabel [ translations ] ++ ": " ++ String.fromInt mintBalance ++ " sats") ]
                , Button.new
                    { label = Translations.payInvoiceButtonTitle [ translations ]
                    , onClick = Just ConfirmPay
                    , theme = settings.theme
                    }
                    |> Button.withTypePrimary
                    |> Button.withDisabled (mintBalance < quote.total)
                    |> Button.view
                , Button.new
                    { label = Translations.cancelButtonTitle [ translations ]
                    , onClick = Just ResetWithdraw
                    , theme = settings.theme
                    }
                    |> Button.withTypeSecondary
                    |> Button.view
                ]

        WithdrawIdle ->
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
                                    let
                                        bal =
                                            Nostr.getCashuProofsForMint settings.nostr mint
                                                |> Tuple.first
                                                |> List.map .amount
                                                |> List.sum
                                    in
                                    option
                                        [ Attr.value mint
                                        , Attr.selected (Just mint == selectedMintUrl)
                                        ]
                                        [ text (mint ++ " (" ++ String.fromInt bal ++ " sats)") ]
                                )
                                settings.mints
                            )
                        ]

                  else
                    p
                        (styles.colorStyleGrayscaleMuted ++ styles.textStyleBody)
                        [ text (Translations.balanceLabel [ translations ] ++ ": " ++ String.fromInt mintBalance ++ " sats") ]
                , div
                    [ css [ Tw.flex, Tw.flex_col, Tw.gap_1 ] ]
                    [ p
                        (styles.colorStyleGrayscaleText ++ styles.textStyleBody)
                        [ text <| Translations.invoiceLabel [ translations ] ]
                    , textarea
                        (styles.colorStyleBackground
                            ++ styles.colorStyleGrayscaleText
                            ++ [ Attr.placeholder <| Translations.invoicePlaceholder [ translations ]
                               , Attr.value model.invoiceDraft
                               , Attr.spellcheck False
                               , Events.onInput UpdateInvoice
                               , Attr.rows 3
                               , css
                                    [ Tw.rounded_md
                                    , Tw.border_2
                                    , Tw.px_3
                                    , Tw.py_2
                                    , Tw.w_full
                                    , Tw.font_mono
                                    , Tw.text_sm
                                    ]
                               ]
                        )
                        []
                    ]
                , Button.new
                    { label = Translations.checkQuoteButtonTitle [ translations ]
                    , onClick =
                        if invoiceOk && selectedMintUrl /= Nothing then
                            Just CheckQuote

                        else
                            Nothing
                    , theme = settings.theme
                    }
                    |> Button.withTypePrimary
                    |> Button.withDisabled (not invoiceOk || selectedMintUrl == Nothing)
                    |> Button.view
                ]


centeredStatus : { a | theme : Theme, browserEnv : BrowserEnv } -> String -> Maybe Msg -> Html Msg
centeredStatus settings label maybeCancel =
    let
        styles =
            stylesForTheme settings.theme
    in
    div
        [ css [ Tw.flex, Tw.flex_col, Tw.gap_3, Tw.items_center, Tw.py_2 ] ]
        [ p
            (styles.colorStyleGrayscaleText ++ styles.textStyleBody)
            [ text label ]
        , case maybeCancel of
            Just msg ->
                Button.new
                    { label = Translations.cancelButtonTitle [ settings.browserEnv.translations ]
                    , onClick = Just msg
                    , theme = settings.theme
                    }
                    |> Button.withTypeSecondary
                    |> Button.view

            Nothing ->
                emptyHtml
        ]


quoteRow : Styles Msg -> String -> String -> Html Msg
quoteRow styles label value =
    div
        [ css [ Tw.flex, Tw.flex_row, Tw.justify_between, Tw.gap_4, Tw.w_full ] ]
        [ p (styles.colorStyleGrayscaleText ++ styles.textStyleBody) [ text label ]
        , p (styles.colorStyleGrayscaleTitle ++ styles.textStyleBody) [ text value ]
        ]



-- HELPERS


preferredMint : Nostr.Model -> List String -> Maybe String -> Maybe String
preferredMint nostr mints current =
    let
        withBalance =
            mints
                |> List.filter
                    (\mintUrl ->
                        Nostr.getCashuProofsForMint nostr mintUrl
                            |> Tuple.first
                            |> List.map .amount
                            |> List.sum
                            |> (\bal -> bal > 0)
                    )

        fallback =
            case List.head withBalance of
                Just mintUrl ->
                    Just mintUrl

                Nothing ->
                    List.head mints
    in
    case current of
        Just selected ->
            if List.member selected withBalance then
                Just selected

            else
                fallback

        Nothing ->
            fallback


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


normalizeInvoice : String -> Maybe String
normalizeInvoice draft =
    let
        trimmed =
            String.trim draft
                |> String.toLower
    in
    if String.startsWith "lightning:" trimmed then
        normalizeInvoice (String.dropLeft 10 trimmed)

    else if String.startsWith "lnbc" trimmed || String.startsWith "lntb" trimmed || String.startsWith "lnbcrt" trimmed then
        Just (String.trim draft)

    else
        Nothing


matchesCheckingRequestId : WithdrawState -> Int -> Bool
matchesCheckingRequestId state requestId =
    case state of
        WithdrawCheckingQuote id ->
            id == requestId

        _ ->
            False


matchesPayingRequestId : WithdrawState -> Int -> Bool
matchesPayingRequestId state requestId =
    case state of
        WithdrawPaying id ->
            id == requestId

        _ ->
            False


quoteDecoder : Decode.Decoder QuotePreview
quoteDecoder =
    Decode.succeed QuotePreview
        |> DecodePipeline.required "requestId" Decode.int
        |> DecodePipeline.required "mintUrl" Decode.string
        |> DecodePipeline.required "invoice" Decode.string
        |> DecodePipeline.required "amount" Decode.int
        |> DecodePipeline.required "feeReserve" Decode.int
        |> DecodePipeline.required "total" Decode.int


type alias Melted =
    { requestId : Int
    , mintUrl : String
    , amount : Int
    , feeReserve : Int
    , total : Int
    , keepProofs : List CashuWallet.CashuProof
    }


meltedDecoder : Decode.Decoder Melted
meltedDecoder =
    Decode.succeed Melted
        |> DecodePipeline.required "requestId" Decode.int
        |> DecodePipeline.required "mintUrl" Decode.string
        |> DecodePipeline.required "amount" Decode.int
        |> DecodePipeline.required "feeReserve" Decode.int
        |> DecodePipeline.required "total" Decode.int
        |> DecodePipeline.required "keepProofs" (Decode.list cashuProofDecoder)


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
