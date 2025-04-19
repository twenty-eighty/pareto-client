module Nostr.Lud16 exposing (..)

import Email
import Json.Decode as Decode exposing (Decoder)


type alias Lud16 =
    { user : String
    , domain : String
    }


type alias Lud16String =
    String


parseLud16 : String -> Maybe Lud16
parseLud16 lud16String =
    case Email.parse (String.trim lud16String) of
        Ok { local, domain } ->
            Just { user = local, domain = domain }

        Err _ ->
            Nothing


lud16ToString : Lud16 -> Lud16String
lud16ToString lud16 =
    lud16.user ++ "@" ++ lud16.domain


lud16ToDisplayString : Lud16 -> Lud16String
lud16ToDisplayString lud16 =
    case lud16.user of
        "_" ->
            -- special case according to NIP-05
            lud16.domain

        _ ->
            lud16ToString lud16


lud16StringDecoder : Decoder Lud16
lud16StringDecoder =
    Decode.string
        |> Decode.andThen
            (\lud16String ->
                parseLud16 lud16String
                    |> Maybe.map Decode.succeed
                    |> Maybe.withDefault (Decode.fail <| "Error parsing lud16: " ++ lud16String)
            )
