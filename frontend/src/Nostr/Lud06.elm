module Nostr.Lud06 exposing (..)

import Nostr.Bech32 as Bech32
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Url exposing (Url)


type alias Lud06 = String


parseLud06 : Lud06 -> Result String Url
parseLud06 lud06String =
    Bech32.decode lud06String 
        |> Maybe.map (\( prefix, data, _ ) ->
            case Bech32.convertBits data 5 8 False of
                Just dataBytes ->
                    case prefix of
                        "lnurl" ->
                            let
                                urlString = dataBytes |> List.map Char.fromCode |> String.fromList
                            in
                            case Url.fromString urlString of
                                Just url ->
                                    Ok url

                                Nothing ->
                                    Err ("Invalid URL: " ++ urlString)

                        _ ->
                            Err "Invalid prefix"

                Nothing ->
                    Err "Failed to convert data bits"
        )
        |> Maybe.withDefault (Err ("Failed to decode lud06 (Bech32): " ++ lud06String))




lud06StringDecoder : Decoder Lud06
lud06StringDecoder =
    Decode.string
        |> Decode.andThen
            (\lud06String ->
                case parseLud06 lud06String of
                    Ok _ ->
                        Decode.succeed lud06String

                    Err error ->
                        Decode.fail <| "Error parsing lud06: " ++ lud06String ++ " " ++ error
            )
