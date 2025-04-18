module Nostr.Nip19 exposing (NAddrData, NEventData, NIP19Type(..), decode, encode)

import Bitwise exposing (or, shiftLeftBy, shiftRightBy)
import Maybe exposing (withDefault)
import Nostr.Bech32 as Bech32
import String exposing (fromList)



-- Define the type that covers all possible NIP-19 types


type NIP19Type
    = Npub String
    | Nsec String
    | Note String
    | NProfile NProfileData
    | NEvent NEventData
    | NAddr NAddrData
    | NRelay String
    | Unknown String



-- Define types for each NIP-19 entity


type alias NProfileData =
    { pubKey : String
    , relays : List String
    }


type alias NEventData =
    { id : String
    , author : Maybe String
    , kind : Maybe Int
    , relays : List String
    }


type alias NAddrData =
    { identifier : String
    , pubKey : String
    , kind : Int
    , relays : List String
    }



-- Define the TLV type for structured data


type alias TLV =
    { type_ : Int
    , length : Int
    , value : List Int
    }



-- Decode a NIP-19 string based on the Bech32 output


decode : String -> Result String NIP19Type
decode input =
    case Bech32.decode input of
        Nothing ->
            Err "Bech32 decoding failed"

        Just ( prefix, data, _ ) ->
            case Bech32.convertBits data 5 8 False of
                Nothing ->
                    Err "Failed to convert data bits"

                Just dataBytes ->
                    case prefix of
                        "npub" ->
                            Ok (Npub (bytesToHex dataBytes))

                        "nsec" ->
                            Ok (Nsec (bytesToHex dataBytes))

                        "note" ->
                            Ok (Note (bytesToHex dataBytes))

                        "nprofile" ->
                            decodeNProfile dataBytes

                        "nevent" ->
                            decodeNEvent dataBytes

                        "naddr" ->
                            decodeNAddr dataBytes

                        "nrelay" ->
                            Ok (NRelay (bytesToAsciiString dataBytes))

                        _ ->
                            Err "Unknown NIP-19 prefix"



-- Decode nprofile data


decodeNProfile : List Int -> Result String NIP19Type
decodeNProfile dataBytes =
    case parseTLV dataBytes of
        Ok tlvs ->
            case extractNProfileData tlvs of
                Ok profileData ->
                    Ok (NProfile profileData)

                Err err ->
                    Err err

        Err err ->
            Err ("Failed to decode TLV data: " ++ err)



-- Decode nevent data


decodeNEvent : List Int -> Result String NIP19Type
decodeNEvent dataBytes =
    case parseTLV dataBytes of
        Ok tlvs ->
            case extractNEventData tlvs of
                Ok eventData ->
                    Ok (NEvent eventData)

                Err err ->
                    Err err

        Err err ->
            Err ("Failed to decode TLV data: " ++ err)



-- Decode naddr data


decodeNAddr : List Int -> Result String NIP19Type
decodeNAddr dataBytes =
    case parseTLV dataBytes of
        Ok tlvs ->
            case extractNAddrData tlvs of
                Ok addrData ->
                    Ok (NAddr addrData)

                Err err ->
                    Err err

        Err err ->
            Err ("Failed to decode TLV data: " ++ err)



-- Extract NProfileData from TLVs


extractNProfileData : List TLV -> Result String NProfileData
extractNProfileData tlvs =
    let
        -- Type 0: special (pubKey)
        pubKeyTLV =
            List.filter (\tlv -> tlv.type_ == 0) tlvs
                |> List.head

        -- Type 1: relay (may be multiple)
        relayTLVs =
            List.filter (\tlv -> tlv.type_ == 1) tlvs

        relays =
            relayTLVs
                |> List.map (\tlv -> bytesToAsciiString tlv.value)
    in
    case pubKeyTLV of
        Just pkTlv ->
            let
                pubKey =
                    bytesToHex pkTlv.value
            in
            Ok { pubKey = pubKey, relays = relays }

        Nothing ->
            Err "Missing required pubkey TLV for nprofile"



-- Extract NEventData from TLVs


extractNEventData : List TLV -> Result String NEventData
extractNEventData tlvs =
    let
        -- Type 0: special (event id)
        idTLV =
            List.filter (\tlv -> tlv.type_ == 0) tlvs
                |> List.head

        -- Type 2: author (optional)
        authorTLV =
            List.filter (\tlv -> tlv.type_ == 2) tlvs
                |> List.head

        -- Type 3: kind (optional)
        kindTLV =
            List.filter (\tlv -> tlv.type_ == 3) tlvs
                |> List.head

        -- Type 1: relay (may be multiple)
        relayTLVs =
            List.filter (\tlv -> tlv.type_ == 1) tlvs

        relays =
            relayTLVs
                |> List.map (\tlv -> bytesToAsciiString tlv.value)

        author =
            authorTLV
                |> Maybe.map (\tlv -> bytesToHex tlv.value)

        kind =
            kindTLV
                |> Maybe.map (\tlv -> bytesToInt tlv.value)
    in
    case idTLV of
        Just idTlv ->
            let
                id =
                    bytesToHex idTlv.value
            in
            Ok { id = id, author = author, kind = kind, relays = relays }

        Nothing ->
            Err "Missing required event id TLV for nevent"



-- Extract NAddrData from TLVs


extractNAddrData : List TLV -> Result String NAddrData
extractNAddrData tlvs =
    let
        -- Type 0: special (identifier)
        identifierTLV =
            List.filter (\tlv -> tlv.type_ == 0) tlvs
                |> List.head

        -- Type 2: author (pubkey)
        pubKeyTLV =
            List.filter (\tlv -> tlv.type_ == 2) tlvs
                |> List.head

        -- Type 3: kind
        kindTLV =
            List.filter (\tlv -> tlv.type_ == 3) tlvs
                |> List.head

        -- Type 1: relay (may be multiple)
        relayTLVs =
            List.filter (\tlv -> tlv.type_ == 1) tlvs

        relays =
            relayTLVs
                |> List.map (\tlv -> bytesToAsciiString tlv.value)
    in
    case ( identifierTLV, pubKeyTLV, kindTLV ) of
        ( Just idTlv, Just pkTlv, Just kindTlv ) ->
            let
                identifier =
                    bytesToAsciiString idTlv.value

                pubKey =
                    bytesToHex pkTlv.value

                kind =
                    bytesToInt kindTlv.value
            in
            Ok { identifier = identifier, pubKey = pubKey, kind = kind, relays = relays }

        _ ->
            Err "Missing required TLV(s) for naddr"



-- Helper function to decode bytes to hex string


bytesToHex : List Int -> String
bytesToHex bytes =
    bytes
        |> List.map byteToHex
        |> String.join ""


byteToHex : Int -> String
byteToHex byte =
    let
        hexChars =
            "0123456789abcdef"

        highNibble =
            byte // 16

        lowNibble =
            modBy 16 byte

        highChar =
            String.fromChar (String.uncons (String.dropLeft highNibble hexChars) |> Maybe.map Tuple.first |> withDefault '0')

        lowChar =
            String.fromChar (String.uncons (String.dropLeft lowNibble hexChars) |> Maybe.map Tuple.first |> withDefault '0')
    in
    highChar ++ lowChar



-- Convert bytes to ASCII string


bytesToAsciiString : List Int -> String
bytesToAsciiString bytes =
    bytes
        |> List.map Char.fromCode
        |> fromList



-- Convert list of bytes to Int (big-endian)


bytesToInt : List Int -> Int
bytesToInt bytes =
    bytes
        |> List.foldl (\byte acc -> shiftLeftBy 8 acc |> or byte) 0



-- Parse TLV formatted data


parseTLV : List Int -> Result String (List TLV)
parseTLV rawData =
    let
        parseTlvHelp remaining tlvs =
            case remaining of
                [] ->
                    Ok (List.reverse tlvs)

                typeByte :: lengthByte :: rest ->
                    if List.length rest < lengthByte then
                        Err "TLV length mismatch"

                    else
                        let
                            ( value, remainingData ) =
                                splitAt lengthByte rest

                            tlv =
                                { type_ = typeByte, length = lengthByte, value = value }
                        in
                        parseTlvHelp remainingData (tlv :: tlvs)

                _ ->
                    Err "TLV header too short"
    in
    parseTlvHelp rawData []


splitAt : Int -> List a -> ( List a, List a )
splitAt n list =
    if n <= 0 then
        ( [], list )

    else
        splitAtHelper n list []


splitAtHelper : Int -> List a -> List a -> ( List a, List a )
splitAtHelper n remaining acc =
    case remaining of
        [] ->
            ( List.reverse acc, [] )

        x :: xs ->
            if n == 1 then
                ( List.reverse (x :: acc), xs )

            else
                splitAtHelper (n - 1) xs (x :: acc)



-- Encode a NIP19Type entity into a NIP-19 string


encode : NIP19Type -> Result String String
encode entity =
    case entity of
        Npub pubKeyHex ->
            encodeSimple "npub" pubKeyHex

        Nsec secKeyHex ->
            encodeSimple "nsec" secKeyHex

        Note noteIdHex ->
            encodeSimple "note" noteIdHex

        NProfile profileData ->
            encodeNProfile profileData

        NEvent eventData ->
            encodeNEvent eventData

        NAddr addrData ->
            encodeNAddr addrData

        NRelay relayUrl ->
            encodeSimple "nrelay" (asciiStringToHex relayUrl)

        Unknown _ ->
            Err "Cannot encode unknown NIP-19 entity"



-- Helper function to encode simple entities


encodeSimple : String -> String -> Result String String
encodeSimple prefix hexData =
    case hexToBytes hexData of
        Just dataBytes ->
            case Bech32.convertBits dataBytes 8 5 True of
                Just data5Bit ->
                    Ok (Bech32.encode prefix data5Bit Bech32.BECH32)

                Nothing ->
                    Err "Failed to convert data bits to 5-bit"

        Nothing ->
            Err "Invalid hex data"



-- Encode nprofile data


encodeNProfile : NProfileData -> Result String String
encodeNProfile profileData =
    let
        pubKeyBytes =
            hexToBytes profileData.pubKey

        relayTLVs =
            profileData.relays
                |> List.map
                    (\relayUrl ->
                        let
                            value =
                                asciiStringToBytes relayUrl
                        in
                        { type_ = 1, length = List.length value, value = value }
                    )

        pubKeyTLV =
            pubKeyBytes
                |> Maybe.map
                    (\bytes ->
                        { type_ = 0, length = List.length bytes, value = bytes }
                    )
    in
    case pubKeyTLV of
        Just pkTlv ->
            let
                tlvs =
                    pkTlv :: relayTLVs

                dataBytes =
                    tlvsToBytes tlvs
            in
            case Bech32.convertBits dataBytes 8 5 True of
                Just data5Bit ->
                    Ok (Bech32.encode "nprofile" data5Bit Bech32.BECH32)

                Nothing ->
                    Err "Failed to convert data bits to 5-bit"

        Nothing ->
            Err "Invalid pubkey hex data"



-- Encode nevent data


encodeNEvent : NEventData -> Result String String
encodeNEvent eventData =
    let
        idBytes =
            hexToBytes eventData.id

        authorTLV =
            eventData.author
                |> Maybe.andThen
                    (\authorHex ->
                        hexToBytes authorHex
                            |> Maybe.map
                                (\bytes ->
                                    { type_ = 2, length = List.length bytes, value = bytes }
                                )
                    )

        kindTLV =
            eventData.kind
                |> Maybe.map
                    (\kindInt ->
                        let
                            bytes =
                                intToBytes kindInt 4
                        in
                        { type_ = 3, length = List.length bytes, value = bytes }
                    )

        relayTLVs =
            eventData.relays
                |> List.map
                    (\relayUrl ->
                        let
                            value =
                                asciiStringToBytes relayUrl
                        in
                        { type_ = 1, length = List.length value, value = value }
                    )

        idTLV =
            idBytes
                |> Maybe.map
                    (\bytes ->
                        { type_ = 0, length = List.length bytes, value = bytes }
                    )
    in
    case idTLV of
        Just idTlv ->
            let
                tlvs =
                    idTlv
                        :: maybeToList authorTLV
                        ++ maybeToList kindTLV
                        ++ relayTLVs

                dataBytes =
                    tlvsToBytes tlvs
            in
            case Bech32.convertBits dataBytes 8 5 True of
                Just data5Bit ->
                    Ok (Bech32.encode "nevent" data5Bit Bech32.BECH32)

                Nothing ->
                    Err "Failed to convert data bits to 5-bit"

        Nothing ->
            Err "Invalid event id hex data"



-- Encode naddr data


encodeNAddr : NAddrData -> Result String String
encodeNAddr addrData =
    let
        identifierBytes =
            asciiStringToBytes addrData.identifier

        pubKeyBytes =
            hexToBytes addrData.pubKey

        kindBytes =
            intToBytes addrData.kind 4

        identifierTLV =
            { type_ = 0, length = List.length identifierBytes, value = identifierBytes }

        pubKeyTLV =
            pubKeyBytes
                |> Maybe.map
                    (\bytes ->
                        { type_ = 2, length = List.length bytes, value = bytes }
                    )

        kindTLV =
            { type_ = 3, length = List.length kindBytes, value = kindBytes }

        relayTLVs =
            addrData.relays
                |> List.map
                    (\relayUrl ->
                        let
                            value =
                                asciiStringToBytes relayUrl
                        in
                        { type_ = 1, length = List.length value, value = value }
                    )

        tlvs =
            identifierTLV
                :: maybeToList pubKeyTLV
                ++ [ kindTLV ]
                ++ relayTLVs

        dataBytes =
            tlvsToBytes tlvs
    in
    case Bech32.convertBits dataBytes 8 5 True of
        Just data5Bit ->
            Ok (Bech32.encode "naddr" data5Bit Bech32.BECH32)

        Nothing ->
            Err "Failed to convert data bits to 5-bit"



-- Convert TLVs to bytes


tlvsToBytes : List TLV -> List Int
tlvsToBytes tlvs =
    List.concatMap (\tlv -> [ tlv.type_, tlv.length ] ++ tlv.value) tlvs



-- Helper function to convert hex string to bytes


hexToBytes : String -> Maybe (List Int)
hexToBytes hexStr =
    let
        hexChars =
            String.toList hexStr

        toByte highChar lowChar =
            let
                highNibble =
                    hexCharToInt highChar

                lowNibble =
                    hexCharToInt lowChar
            in
            case ( highNibble, lowNibble ) of
                ( Just hi, Just lo ) ->
                    Just ((hi * 16) + lo)

                _ ->
                    Nothing

        processChars chars acc =
            case chars of
                highChar :: lowChar :: rest ->
                    toByte highChar lowChar
                        |> Maybe.andThen (\byte -> processChars rest (byte :: acc))

                [] ->
                    Just (List.reverse acc)

                _ ->
                    -- Odd number of characters
                    Nothing
    in
    processChars hexChars []



-- Convert an ASCII string to bytes


asciiStringToBytes : String -> List Int
asciiStringToBytes str =
    str
        |> String.toList
        |> List.map Char.toCode



-- Convert an integer to bytes (big-endian)


intToBytes : Int -> Int -> List Int
intToBytes value numBytes =
    if numBytes <= 0 then
        []

    else
        let
            byte =
                shiftRightBy ((numBytes - 1) * 8) value |> modBy 256
        in
        byte :: intToBytes value (numBytes - 1)



-- Helper function to convert a hex character to an integer


hexCharToInt : Char -> Maybe Int
hexCharToInt c =
    case c of
        '0' ->
            Just 0

        '1' ->
            Just 1

        '2' ->
            Just 2

        '3' ->
            Just 3

        '4' ->
            Just 4

        '5' ->
            Just 5

        '6' ->
            Just 6

        '7' ->
            Just 7

        '8' ->
            Just 8

        '9' ->
            Just 9

        'a' ->
            Just 10

        'b' ->
            Just 11

        'c' ->
            Just 12

        'd' ->
            Just 13

        'e' ->
            Just 14

        'f' ->
            Just 15

        'A' ->
            Just 10

        'B' ->
            Just 11

        'C' ->
            Just 12

        'D' ->
            Just 13

        'E' ->
            Just 14

        'F' ->
            Just 15

        _ ->
            Nothing



-- Helper function to convert an ASCII string to hex string


asciiStringToHex : String -> String
asciiStringToHex str =
    str
        |> asciiStringToBytes
        |> bytesToHex


maybeToList : Maybe a -> List a
maybeToList maybeVal =
    case maybeVal of
        Just val ->
            [ val ]

        Nothing ->
            []
