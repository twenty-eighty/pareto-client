module Nostr.Nip44 exposing
    ( calcPaddedLen
    , pad
    , unpad
    , decodePayload
    , hmacAad
    , getConversationKey
    , getMessageKeys
    , encrypt
    , decrypt
    )

import Basics exposing (floor, logBase, (//))
import Bytes exposing (Bytes)
import Bytes.Decode exposing (decode)
import Bytes.Encode exposing (encode)
import Crypto exposing (hmacSha256, sha256, chacha20)
import Utf8 exposing (fromString, toString)
import Maybe exposing (withDefault)
import Debug exposing (log)

calcPaddedLen : Int -> Int
calcPaddedLen unpaddedLen =
    let
        nextPower =
            2 ^ (floor (logBase 2 (toFloat (unpaddedLen - 1))) + 1)

        chunk =
            if nextPower <= 256 then
                32
            else
                nextPower // 8
    in
    if unpaddedLen <= 32 then
        32
    else
        chunk * (floor ((unpaddedLen - 1) // chunk) + 1)

pad : String -> Result String Bytes
pad plaintext =
    let
        unpadded = fromString plaintext
        unpaddedLen = String.length plaintext
        paddedLen = calcPaddedLen unpaddedLen
        prefix = Bytes.Encode.unsignedInt16Be unpaddedLen
        suffix = Bytes.Encode.repeat (paddedLen - unpaddedLen) 0
    in
    if unpaddedLen < 1 || unpaddedLen > 65535 then
        Err "Invalid plaintext length"
    else
        Ok (Bytes.Encode.sequence [ prefix, Bytes.Encode.string plaintext, suffix ])

unpad : Bytes -> Result String String
unpad padded =
    case Bytes.Decode.decode Bytes.Decode.unsignedInt16Be padded of
        Err _ ->
            Err "Invalid padding"

        Ok (unpaddedLen, rest) ->
            let
                unpadded = Bytes.Decode.bytes unpaddedLen rest
            in
            if unpaddedLen == 0 || Bytes.length rest /= unpaddedLen + 2 then
                Err "Invalid padding"
            else
                Ok (Bytes.Decode.string unpadded)

decodePayload : Bytes -> Result String (Bytes, Bytes, Bytes)
decodePayload payload =
    let
        plen = Bytes.length payload
    in
    if plen == 0 || Bytes.get 0 payload == Just (toByte '#') then
        Err "Unknown version"
    else if plen < 132 || plen > 87472 then
        Err "Invalid payload size"
    else
        let
            vers = Bytes.get 0 payload
            nonce = Bytes.slice 1 33 payload
            ciphertext = Bytes.slice 33 (plen - 32) payload
            mac = Bytes.slice (plen - 32) plen payload
        in
        case vers of
            Just 2 ->
                Ok (nonce, ciphertext, mac)

            _ ->
                Err "Unknown version"

hmacAad : Bytes -> Bytes -> Bytes -> Result String Bytes
hmacAad key message aad =
    if Bytes.length aad /= 32 then
        Err "AAD must be 32 bytes"
    else
        Ok (hmacSha256 key (Bytes.concat [ aad, message ]))

getConversationKey : Bytes -> Bytes -> Result String Bytes
getConversationKey privateKeyA publicKeyB =
    case secp256k1Ecdh privateKeyA publicKeyB of
        Nothing ->
            Err "Invalid ECDH"

        Just sharedX ->
            Ok (hkdfExtract (Utf8.fromString "nip44-v2") sharedX)

getMessageKeys : Bytes -> Bytes -> Result String (Bytes, Bytes, Bytes)
getMessageKeys conversationKey nonce =
    if Bytes.length conversationKey /= 32 then
        Err "Invalid conversation key length"
    else if Bytes.length nonce /= 32 then
        Err "Invalid nonce length"
    else
        let
            keys = hkdfExpand conversationKey nonce 76
            chachaKey = Bytes.slice 0 32 keys
            chachaNonce = Bytes.slice 32 44 keys
            hmacKey = Bytes.slice 44 76 keys
        in
        Ok (chachaKey, chachaNonce, hmacKey)

encrypt : String -> Bytes -> Bytes -> Result String Bytes
encrypt plaintext conversationKey nonce =
    case getMessageKeys conversationKey nonce of
        Err err ->
            Err err

        Ok (chachaKey, chachaNonce, hmacKey) ->
            case pad plaintext of
                Err err ->
                    Err err

                Ok padded ->
                    let
                        ciphertext = chacha20 chachaKey chachaNonce padded
                        mac = hmacAad hmacKey ciphertext nonce
                    in
                    case mac of
                        Err err ->
                            Err err

                        Ok validMac ->
                            Ok (Bytes.concat [ Bytes.Encode.uint8 2, nonce, ciphertext, validMac ])

decrypt : Bytes -> Bytes -> Result String String
decrypt payload conversationKey =
    case decodePayload payload of
        Err err ->
            Err err

        Ok (nonce, ciphertext, mac) ->
            case getMessageKeys conversationKey nonce of
                Err err ->
                    Err err

                Ok (chachaKey, chachaNonce, hmacKey) ->
                    let
                        calculatedMac = hmacAad hmacKey ciphertext nonce
                    in
                    if calculatedMac /= Ok mac then
                        Err "Invalid MAC"
                    else
                        let
                            paddedPlaintext = chacha20 chachaKey chachaNonce ciphertext
                        in
                        unpad paddedPlaintext
