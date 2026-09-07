module Portal exposing (..)

import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (optional, required)
import Nostr.Lud16 exposing (Lud16, lud16StringDecoder)
import Nostr.Nip05 exposing (Nip05, nip05StringDecoder, nip05ToString)
import Nostr.Types exposing (PubKey)
import Pareto



-- This module handles API calls to the Nostr Portal server of the Pareto project


type alias PortalCheckResponse =
    { email : Bool
    , username : Maybe String
    , nip05 : Maybe Nip05
    , lud16 : Maybe Lud16
    }


{-| Authenticated profile from `GET /api/users/me` (NIP-98).
-}
type alias MeProfile =
    { name : Maybe String
    , postalAddress : Maybe String
    }


loadUserDataByPubKey : (PubKey -> Result Http.Error PortalCheckResponse -> msg) -> PubKey -> Cmd msg
loadUserDataByPubKey msg pubKey =
    Http.get
        { url = Pareto.newsletterAuthorCheckEndpointPubKey ++ "/" ++ pubKey
        , expect = Http.expectJson (msg pubKey) decodeNewsletterCheckResponse
        }


loadUserDataByNip05 : (Nip05 -> Result Http.Error PortalCheckResponse -> msg) -> Nip05 -> Cmd msg
loadUserDataByNip05 msg nip05 =
    Http.get
        { url = Pareto.newsletterAuthorCheckEndpointNip05 ++ "/" ++ nip05ToString nip05
        , expect = Http.expectJson (msg nip05) decodeNewsletterCheckResponse
        }


{-| `GET /api/users/me` with a NIP-98 Authorization header produced in JS.
-}
loadMeProfile : (Result Http.Error MeProfile -> msg) -> String -> String -> Cmd msg
loadMeProfile msg authApiBaseUrl authHeader =
    Http.request
        { method = "GET"
        , headers =
            [ Http.header "Authorization" authHeader
            , Http.header "Accept" "application/json"
            ]
        , url = meEndpoint authApiBaseUrl
        , body = Http.emptyBody
        , expect = Http.expectJson msg decodeMeProfile
        , timeout = Nothing
        , tracker = Nothing
        }


meEndpoint : String -> String
meEndpoint authApiBaseUrl =
    stripTrailingSlash authApiBaseUrl ++ "/api/users/me"


stripTrailingSlash : String -> String
stripTrailingSlash value =
    if String.endsWith "/" value then
        String.dropRight 1 value

    else
        value


decodeNewsletterCheckResponse : Decode.Decoder PortalCheckResponse
decodeNewsletterCheckResponse =
    Decode.succeed PortalCheckResponse
        |> required "email" Decode.bool
        |> optional "username" (Decode.map Just Decode.string) Nothing
        |> optional "nip05" (Decode.map Just nip05StringDecoder) Nothing
        |> optional "lud16" (Decode.map Just lud16StringDecoder) Nothing


decodeMeProfile : Decode.Decoder MeProfile
decodeMeProfile =
    Decode.map2 MeProfile
        (optionalTrimmedString "name")
        (optionalTrimmedString "postal_address")


optionalTrimmedString : String -> Decode.Decoder (Maybe String)
optionalTrimmedString fieldName =
    Decode.oneOf
        [ Decode.field fieldName (Decode.nullable Decode.string)
        , Decode.succeed Nothing
        ]
        |> Decode.map (Maybe.andThen trimToMaybe)


trimToMaybe : String -> Maybe String
trimToMaybe value =
    case String.trim value of
        "" ->
            Nothing

        trimmed ->
            Just trimmed
