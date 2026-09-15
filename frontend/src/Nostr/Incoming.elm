module Nostr.Incoming exposing
    ( Effect(..)
    , AuthorData
    , authorDecoder
    , decode
    )

{-| Decode port incoming messages into typed effects.
Applying effects to the Nostr model stays in `Nostr`.
-}

import Json.Decode as Decode
import Json.Decode.Pipeline as DecodePipeline
import Nostr.Event exposing (Event, Kind, decodeEvent, informationForKind, numberForKind)
import Nostr.External as External
import Nostr.Nip05 as Nip05 exposing (Nip05)
import Nostr.Profile exposing (PubkeyProfile, pubkeyProfileDecoder)
import Nostr.Relay exposing (RelayState(..), relayUrlDecoder)
import Nostr.Request exposing (RequestId)
import Nostr.Types exposing (IncomingMessage, PubKey)
import Nostr.Zaps as Zaps exposing (ZapReceipt)


type alias AuthorData =
    { pubKey : PubKey
    , nip05 : Nip05
    }


type Effect
    = SetPoolState RelayState
    | NoOp
    | SetRelayStatus String RelayState
    | AppendError String
    | GotProfiles (List PubkeyProfile)
    | GotZapReceipts (List ZapReceipt)
    | GotAuthors (List AuthorData)
    | GotEvents RequestId Kind (List Event)
    | EventsComplete RequestId


authorDecoder : Decode.Decoder AuthorData
authorDecoder =
    Decode.succeed AuthorData
        |> DecodePipeline.required "pubkey" Decode.string
        |> DecodePipeline.required "nip-05" Nip05.nip05StringDecoder


decode : IncomingMessage -> Effect
decode message =
    case message.messageType of
        "connecting" ->
            SetPoolState RelayConnecting

        "connected" ->
            SetPoolState RelayConnected

        "relay:notice" ->
            NoOp

        "relay:connected" ->
            decodeRelayStatus RelayConnected message.value

        "relay:ready" ->
            decodeRelayStatus RelayReady message.value

        "relay:disconnected" ->
            decodeRelayStatus RelayDisconnected message.value

        "profiles" ->
            case Decode.decodeValue (Decode.list pubkeyProfileDecoder) message.value of
                Ok pubkeyProfiles ->
                    GotProfiles pubkeyProfiles

                Err error ->
                    AppendError (Decode.errorToString error)

        "zap_receipts" ->
            case Decode.decodeValue (Decode.list Zaps.nostrZapReceiptDecoder) message.value of
                Ok zapReceipts ->
                    GotZapReceipts zapReceipts

                Err error ->
                    AppendError (Decode.errorToString error)

        "authors" ->
            case Decode.decodeValue (Decode.list authorDecoder) message.value of
                Ok authorsData ->
                    GotAuthors authorsData

                Err error ->
                    AppendError (Decode.errorToString error)

        "events" ->
            decodeEvents message.value

        "eventsComplete" ->
            case External.decodeRequestId message.value of
                Ok requestId ->
                    EventsComplete requestId

                Err error ->
                    AppendError (Decode.errorToString error)

        "error" ->
            case External.decodeReason message.value of
                Ok error ->
                    AppendError error

                Err error ->
                    AppendError (Decode.errorToString error)

        _ ->
            NoOp


decodeRelayStatus : RelayState -> Decode.Value -> Effect
decodeRelayStatus state value =
    case Decode.decodeValue relayUrlDecoder value of
        Ok relayUrlWithoutProtocol ->
            SetRelayStatus relayUrlWithoutProtocol state

        Err error ->
            AppendError (Decode.errorToString error)


decodeEvents : Decode.Value -> Effect
decodeEvents value =
    case ( External.decodeRequestId value, External.decodeEventsKind value ) of
        ( Ok requestId, Ok kind ) ->
            case Decode.decodeValue (Decode.field "events" (Decode.list decodeEvent)) value of
                Ok events ->
                    GotEvents requestId kind events

                Err errorDecodingEvents ->
                    let
                        kindDesc =
                            kind
                                |> informationForKind
                                |> .description
                    in
                    AppendError
                        ("Error decoding events of kind "
                            ++ String.fromInt (numberForKind kind)
                            ++ " ("
                            ++ kindDesc
                            ++ ") - request ID "
                            ++ String.fromInt requestId
                            ++ ": "
                            ++ Decode.errorToString errorDecodingEvents
                        )

        _ ->
            AppendError "Error decoding request ID or kind"
