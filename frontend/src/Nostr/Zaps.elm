module Nostr.Zaps exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as DecodePipeline
import Time

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