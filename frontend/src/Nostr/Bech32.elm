module Nostr.Bech32 exposing (Encoding(..), convertBits, decode, encode)

import Bitwise as BW
import Char
import List exposing (foldl)
import String
import UInt64 exposing (UInt64, and, fromInt, isZero, shiftLeftBy, shiftRightZfBy, toString, xor)



-- Define the encoding types


type Encoding
    = BECH32
    | BECH32M



-- Constants


separator : Char
separator =
    '1'


charset : List String
charset =
    [ "q"
    , "p"
    , "z"
    , "r"
    , "y"
    , "9"
    , "x"
    , "8"
    , "g"
    , "f"
    , "2"
    , "t"
    , "v"
    , "d"
    , "w"
    , "0"
    , "s"
    , "3"
    , "j"
    , "n"
    , "5"
    , "4"
    , "k"
    , "h"
    , "c"
    , "e"
    , "6"
    , "m"
    , "u"
    , "a"
    , "7"
    , "l"
    ]


bech32mConst : UInt64
bech32mConst =
    UInt64.fromInt 0x2BC830A3


bech32Const : UInt64
bech32Const =
    UInt64.fromInt 1



-- Helper functions


getAt : Int -> List a -> Maybe a
getAt index list =
    if index < 0 then
        Nothing

    else
        list |> List.drop index |> List.head


findIndex : (a -> Bool) -> List a -> Maybe Int
findIndex predicate list =
    findIndexHelper predicate list 0


findIndexHelper : (a -> Bool) -> List a -> Int -> Maybe Int
findIndexHelper predicate list index =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if predicate x then
                Just index

            else
                findIndexHelper predicate xs (index + 1)


dropRight : Int -> List a -> List a
dropRight n list =
    let
        len =
            List.length list
    in
    if n >= len then
        []

    else
        List.take (len - n) list


lastIndexOf : String -> String -> Maybe Int
lastIndexOf substring string =
    let
        indices =
            String.indexes substring string
    in
    case List.reverse indices of
        i :: _ ->
            Just i

        [] ->
            Nothing



-- Encode function


encode : String -> List Int -> Encoding -> String
encode hrp dataList spec =
    let
        checksummed =
            dataList ++ createChecksum spec hrp dataList

        checksummedStr =
            String.join "" (List.map getCharsetChar checksummed)
    in
    hrp ++ String.fromChar separator ++ checksummedStr


getCharsetChar : Int -> String
getCharsetChar i =
    case getAt i charset of
        Just c ->
            c

        Nothing ->
            "?"


createChecksum : Encoding -> String -> List Int -> List Int
createChecksum encoding hrp dataPart =
    let
        values =
            expandHrp hrp ++ dataPart ++ List.repeat 6 0

        const : UInt64
        const =
            if encoding == BECH32M then
                UInt64.fromInt 0x2BC830A3

            else
                UInt64.fromInt 1

        polymodValue : UInt64
        polymodValue =
            UInt64.xor (polymod values) const

        checksumInts =
            List.map
                (\i ->
                    UInt64.and
                        (UInt64.shiftRightZfBy (5 * (5 - i)) polymodValue)
                        (UInt64.fromInt 31)
                        |> UInt64.toInt31
                        |> Maybe.withDefault 0
                )
                (List.range 0 5)
    in
    checksumInts


expandHrp : String -> List Int
expandHrp hrp =
    let
        hrpChars =
            String.toList hrp

        hrp1 =
            List.map (\c -> Char.toCode c // 32) hrpChars

        hrp2 =
            List.map (\c -> modBy 32 (Char.toCode c)) hrpChars
    in
    hrp1 ++ [ 0 ] ++ hrp2


polymod : List Int -> UInt64
polymod values =
    let
        generator : List UInt64
        generator =
            [ UInt64.fromInt 0x3B6A57B2
            , UInt64.fromInt 0x26508E6D
            , UInt64.fromInt 0x1EA119FA
            , UInt64.fromInt 0x3D4233DD
            , UInt64.fromInt 0x2A1462B3
            ]

        step : Int -> UInt64 -> UInt64
        step v chk =
            let
                b : UInt64
                b =
                    UInt64.shiftRightZfBy 25 chk

                chk1 : UInt64
                chk1 =
                    UInt64.xor
                        (UInt64.shiftLeftBy 5 (UInt64.and chk (UInt64.fromInt 0x01FFFFFF)))
                        (UInt64.fromInt v)

                chkNew : UInt64
                chkNew =
                    List.foldl
                        (\( i, g ) acc ->
                            if not (UInt64.isZero (UInt64.and b (UInt64.shiftLeftBy i UInt64.one))) then
                                UInt64.xor acc g

                            else
                                acc
                        )
                        chk1
                        (List.indexedMap Tuple.pair generator)
            in
            chkNew
    in
    List.foldl step (UInt64.fromInt 1) values



-- Decode function


decode : String -> Maybe ( String, List Int, Encoding )
decode bech =
    let
        invalidChar =
            String.toList bech |> List.any (\c -> Char.toCode c < 33 || Char.toCode c > 126)

        allLower =
            bech == String.toLower bech

        allUpper =
            bech == String.toUpper bech

        bechLower =
            String.toLower bech

        pos =
            lastIndexOf (String.fromChar separator) bechLower

        maxLength =
            1000
    in
    if invalidChar then
        Nothing

    else if not allLower && not allUpper then
        Nothing

    else
        case pos of
            Nothing ->
                Nothing

            Just posIndex ->
                let
                    bechLength =
                        String.length bechLower
                in
                if posIndex + 7 > bechLength then
                    Nothing

                else if bechLength > maxLength then
                    Nothing

                else
                    let
                        dataPart =
                            String.slice (posIndex + 1) bechLength bechLower

                        validData =
                            String.toList dataPart |> List.all (\c -> List.member (String.fromChar c) charset)
                    in
                    if not validData then
                        Nothing

                    else
                        let
                            hrp =
                                String.slice 0 posIndex bechLower

                            dataChars =
                                String.toList dataPart

                            dataInts =
                                List.map (\c -> findIndex ((==) (String.fromChar c)) charset |> Maybe.withDefault -1) dataChars

                            maybeEncoding =
                                verifyChecksum hrp dataInts
                        in
                        case maybeEncoding of
                            Just encoding ->
                                let
                                    dataWithoutChecksum =
                                        dropRight 6 dataInts
                                in
                                Just ( hrp, dataWithoutChecksum, encoding )

                            Nothing ->
                                Nothing


verifyChecksum : String -> List Int -> Maybe Encoding
verifyChecksum hrp dataList =
    let
        polymodInput =
            expandHrp hrp ++ dataList

        polymodResult : UInt64
        polymodResult =
            polymod polymodInput
    in
    if equal polymodResult bech32Const then
        Just BECH32

    else if equal polymodResult bech32mConst then
        Just BECH32M

    else
        Nothing



-- Convert bits function


convertBits : List Int -> Int -> Int -> Bool -> Maybe (List Int)
convertBits dataList fromBits toBits padding =
    let
        maxv =
            BW.shiftLeftBy toBits 1 - 1

        maxAcc =
            BW.shiftLeftBy (fromBits + toBits - 1) 1 - 1

        processBits bitsValue accValue resultValue =
            if bitsValue >= toBits then
                let
                    bitsNew =
                        bitsValue - toBits

                    resNew =
                        resultValue ++ [ BW.and (BW.shiftRightBy bitsNew accValue) maxv ]
                in
                processBits bitsNew accValue resNew

            else
                ( bitsValue, accValue, resultValue )

        convert acc bits result remainingData =
            case remainingData of
                [] ->
                    if padding then
                        if bits /= 0 then
                            let
                                result1 =
                                    result ++ [ BW.and (BW.shiftLeftBy (toBits - bits) acc) maxv ]
                            in
                            Just result1

                        else
                            Just result

                    else if bits >= fromBits || BW.and (BW.shiftLeftBy (toBits - bits) acc) maxv /= 0 then
                        Nothing

                    else
                        Just result

                v :: vs ->
                    if v < 0 || BW.shiftRightBy fromBits v /= 0 then
                        Nothing

                    else
                        let
                            acc1 =
                                BW.and (BW.or (BW.shiftLeftBy fromBits acc) v) maxAcc

                            bits1 =
                                bits + fromBits

                            ( bits2, acc2, result2 ) =
                                processBits bits1 acc1 result
                        in
                        convert acc2 bits2 result2 vs
    in
    convert 0 0 [] dataList


equal : UInt64 -> UInt64 -> Bool
equal a b =
    UInt64.compare a b == EQ
