module Nostr.Zaps exposing (..)

import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as DecodePipeline
import Url


type alias ZapReceipt =
    { id : String
    , address : Maybe String
    , event : Maybe String
    , bolt11 : String
    , preimage : Maybe String
    , recipient : Maybe String
    , amount : Maybe Int
    }



{-
   {
   "id": "93a65bdce4e4d0ba1f5042fde8e8311781cb5eee13b30e19e4b701a1c0ca6b57",
   "recipient": "ec42c765418b3db9c85abff3a88f4a3bbe57535eebbdc54522041fa5328c0600",
   "address": "30023:ec42c765418b3db9c85abff3a88f4a3bbe57535eebbdc54522041fa5328c0600:1707912490439",
   "pubkeySender": "6b0a60cff3eca5a2b2505ccb3f7133d8422045cbef40f3d2c6189fb0b952e7d4",
   "bolt11": "lnbc210n1pjaqca7pp59mrnpu6chr5j3q763wqjqprd5kdqrxqt5x8elr49y7k52gd55lushp5855evqmzhzmv9geeu2pgqc46wdhhnmhg4a6yv77mcau4y08085gscqzzsxqyz5vqsp5zktsuxysy7ffnye2caajk07g8lpwk8geg9f00h5g6ve30s98dfms9qyyssqv86ejdsap4gu3x3ej9mjy4qtuyxws8pxuxh30fr9q9nh6xkf37fx3q858lxkwpyge5udqf35z34gm6ut3w86xh7yafa7aqrguy770wsqy5dke6",
   "preimage": "9887b5e519332ae71991fd5d0d315e6ab24d92979a54ac29c0fd4d74394ecc16",
   "amount": "21000"
   }
-}


type alias Lud16 =
    { user : String
    , domain : String
    }


type alias PayRequest =
    { callback : String
    , maxSendable : Int
    , minSendable : Int
    , metadata : String
    , commentAllowed : Int
    , tag : String
    , allowsNostr : Bool
    , nostrPubkey : String
    }


payRequestDecoder : Decoder PayRequest
payRequestDecoder =
    Decode.succeed PayRequest
        |> DecodePipeline.required "callback" Decode.string
        |> DecodePipeline.required "maxSendable" Decode.int
        |> DecodePipeline.required "minSendable" Decode.int
        |> DecodePipeline.required "metadata" Decode.string
        |> DecodePipeline.required "commentAllowed" Decode.int
        |> DecodePipeline.required "tag" Decode.string
        |> DecodePipeline.required "allowsNostr" Decode.bool
        |> DecodePipeline.required "nostrPubkey" Decode.string


nostrZapReceiptDecoder : Decoder ZapReceipt
nostrZapReceiptDecoder =
    Decode.succeed ZapReceipt
        |> DecodePipeline.required "id" Decode.string
        |> DecodePipeline.optional "address" (Decode.maybe Decode.string) Nothing
        |> DecodePipeline.optional "event" (Decode.maybe Decode.string) Nothing
        |> DecodePipeline.required "bolt11" Decode.string
        |> DecodePipeline.optional "preimage" (Decode.maybe Decode.string) Nothing
        |> DecodePipeline.optional "recipient" (Decode.maybe Decode.string) Nothing
        |> DecodePipeline.optional "amount" (Decode.maybe stringNumberDecoder) Nothing


stringNumberDecoder : Decoder Int
stringNumberDecoder =
    Decode.string
        |> Decode.map (String.toInt >> Maybe.withDefault 0)


fetchPayRequest : (Result Http.Error PayRequest -> msg) -> Lud16 -> Cmd msg
fetchPayRequest toMsg lud16 =
    Http.request
        { method = "GET"
        , headers =
            [ Http.header "Accept" "application/json"
            ]
        , url = "https://" ++ lud16.domain ++ "/.well-known/lnurlp/" ++ lud16.user
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg payRequestDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


type alias Invoice =
    { pr : String
    }


invoiceDecoder : Decoder Invoice
invoiceDecoder =
    Decode.succeed Invoice
        |> DecodePipeline.required "pr" Decode.string


{-| Request a BOLT11 invoice from an LNURL-pay callback.
Amount is in millisatoshis.
On failure, the `Err` string is a human-readable provider/network message.
-}
fetchInvoice :
    (Result String Invoice -> msg)
    -> String
    -> Int
    -> Maybe String
    -> Maybe String
    -> Cmd msg
fetchInvoice toMsg callbackUrl amountMsats maybeComment maybeNostrEventJson =
    let
        queryParts =
            [ Just ("amount=" ++ String.fromInt amountMsats)
            , maybeComment
                |> Maybe.andThen
                    (\comment ->
                        if String.trim comment == "" then
                            Nothing

                        else
                            Just ("comment=" ++ Url.percentEncode comment)
                    )
            , maybeNostrEventJson
                |> Maybe.map (\json -> "nostr=" ++ Url.percentEncode json)
            ]
                |> List.filterMap identity

        separator =
            if String.contains "?" callbackUrl then
                "&"

            else
                "?"

        url =
            callbackUrl ++ separator ++ String.join "&" queryParts
    in
    Http.request
        { method = "GET"
        , headers =
            [ Http.header "Accept" "application/json"
            ]
        , url = url
        , body = Http.emptyBody
        , expect = expectInvoice toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


expectInvoice : (Result String Invoice -> msg) -> Http.Expect msg
expectInvoice toMsg =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err ("Bad URL: " ++ url)

                Http.Timeout_ ->
                    Err "Network timeout"

                Http.NetworkError_ ->
                    Err "Network error"

                Http.BadStatus_ metadata body ->
                    Err (invoiceErrorMessage body metadata.statusCode)

                Http.GoodStatus_ _ body ->
                    case Decode.decodeString invoiceResponseDecoder body of
                        Ok (Ok invoice) ->
                            Ok invoice

                        Ok (Err reason) ->
                            Err reason

                        Err _ ->
                            Err "Could not read invoice response"


invoiceResponseDecoder : Decoder (Result String Invoice)
invoiceResponseDecoder =
    Decode.oneOf
        [ Decode.map Ok invoiceDecoder
        , Decode.map Err providerErrorDecoder
        ]


providerErrorDecoder : Decoder String
providerErrorDecoder =
    Decode.oneOf
        [ Decode.field "message" Decode.string
        , Decode.field "reason" Decode.string
        , Decode.succeed "Invoice request failed"
        ]


invoiceErrorMessage : String -> Int -> String
invoiceErrorMessage body statusCode =
    case Decode.decodeString providerErrorDecoder body of
        Ok message ->
            message

        Err _ ->
            "Invoice request failed (HTTP " ++ String.fromInt statusCode ++ ")"

