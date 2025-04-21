module Nostr.Lud16 exposing (..)

import Http
import Email
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required, optional)
import Url exposing (Url)


type alias Lud16 =
    { user : String
    , domain : String
    }

type alias LightningPaymentData =
    { allowsNostr : Maybe Bool
    , callback : Url
    , commentAllowed : Maybe Int
    , maxSendable : Int
    , metadata : String
    , minSendable : Int
    , nostrPubkey : Maybe String
    , tag : String
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

requestLightningPaymentData : (Result Http.Error LightningPaymentData -> msg) -> Lud16 -> Cmd msg
requestLightningPaymentData responseMsg lud16 =
    Http.get
        { url = "https://" ++ lud16.domain ++ "/.well-known/lnurlp/" ++ lud16.user
        , expect = Http.expectJson responseMsg lightningPaymentDataDecoder
        }

lightningPaymentDataDecoder : Decoder LightningPaymentData
lightningPaymentDataDecoder =
    Decode.succeed LightningPaymentData
        |> optional "allowsNostr" (Decode.maybe Decode.bool) Nothing
        |> required "callback" urlDecoder
        |> optional "commentAllowed" (Decode.maybe Decode.int) Nothing
        |> required "maxSendable" Decode.int
        |> required "metadata" Decode.string
        |> required "minSendable" Decode.int
        |> optional "nostrPubkey" (Decode.maybe Decode.string) Nothing
        |> required "tag" Decode.string

urlDecoder : Decoder Url
urlDecoder =
    Decode.string
        |> Decode.andThen
            (\urlString ->
                (Url.fromString urlString)
                    |> Maybe.map
                        (\url ->
                                Decode.succeed url
                        )
                    |> Maybe.withDefault (Decode.fail <| "Error parsing URL: " ++ urlString)
            )