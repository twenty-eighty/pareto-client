module Nostr.Types exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode

type alias EventId = String

type alias PubKey = String

type alias RelayUrl = String

type RelayRole
    = ReadRelay
    | WriteRelay
    | ReadWriteRelay

type alias OutgoingCommand =
    { command : String
    , value : Encode.Value
    }

type alias IncomingMessage =
    { messageType : String
    , value : Encode.Value
    }

relayRoleFromString : String -> RelayRole
relayRoleFromString role =
    case role of
        "read" ->
            ReadRelay

        "write" ->
            WriteRelay

        _ ->
            ReadWriteRelay

relayRoleToString : RelayRole -> Maybe String
relayRoleToString role =
    case role of
        ReadRelay ->
            Just "read"

        WriteRelay ->
            Just "write"

        ReadWriteRelay ->
            Nothing


decodeRelayRole : Decode.Decoder RelayRole
decodeRelayRole =
    Decode.string
    |> Decode.andThen (\roleString ->
            relayRoleFromString roleString
            |> Decode.succeed
        )