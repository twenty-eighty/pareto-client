module HmacSha256 exposing (hex)

{-| HMAC-SHA256 over UTF-8 strings, hex-encoded lowercase.
-}

import Bitwise
import Bytes
import Bytes.Decode as Decode
import Bytes.Encode as Encode
import SHA256


hex : String -> String -> String
hex key message =
    let
        blockSize =
            64

        keyBytes =
            padKey blockSize (utf8Bytes key)

        inner =
            SHA256.fromByteValues (List.map (Bitwise.xor 0x36) keyBytes ++ utf8Bytes message)
                |> SHA256.toByteValues
    in
    SHA256.fromByteValues (List.map (Bitwise.xor 0x5C) keyBytes ++ inner)
        |> SHA256.toHex


padKey : Int -> List Int -> List Int
padKey blockSize keyBytes =
    let
        hashed =
            if List.length keyBytes > blockSize then
                SHA256.fromByteValues keyBytes
                    |> SHA256.toByteValues

            else
                keyBytes
    in
    hashed ++ List.repeat (blockSize - List.length hashed) 0


utf8Bytes : String -> List Int
utf8Bytes str =
    let
        bytes =
            Encode.encode (Encode.string str)
    in
    Decode.decode (Decode.loop ( Bytes.width bytes, [] ) utf8Step) bytes
        |> Maybe.withDefault []


utf8Step : ( Int, List Int ) -> Decode.Decoder (Decode.Step ( Int, List Int ) (List Int))
utf8Step ( remaining, acc ) =
    if remaining <= 0 then
        Decode.succeed (Decode.Done (List.reverse acc))

    else
        Decode.unsignedInt8
            |> Decode.map (\b -> Decode.Loop ( remaining - 1, b :: acc ))
