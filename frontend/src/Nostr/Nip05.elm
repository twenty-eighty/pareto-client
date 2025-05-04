module Nostr.Nip05 exposing (..)

import Dict exposing (Dict)
import Email
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as DecodePipeline
import Nostr.Relay as Relay
import Nostr.Types exposing (RelayUrl)


type alias Nip05 =
    { user : String
    , domain : String
    }


type alias Nip05Data =
    { names : Dict String String
    , relays : Maybe (Dict String (List RelayUrl))
    }


type alias Nip05String =
    String


parseNip05 : String -> Maybe Nip05
parseNip05 nip05String =
    case Email.parse (String.trim nip05String) of
        Ok { local, domain } ->
            Just { user = String.toLower local, domain = domain }

        Err _ ->
            Nothing


nip05ToString : Nip05 -> Nip05String
nip05ToString nip05 =
    nip05.user ++ "@" ++ nip05.domain


nip05ToDisplayString : Nip05 -> Nip05String
nip05ToDisplayString nip05 =
    case nip05.user of
        "_" ->
            -- special case according to NIP-05
            nip05.domain

        _ ->
            nip05ToString nip05


nip05Decoder : Decoder Nip05Data
nip05Decoder =
    Decode.succeed Nip05Data
        |> DecodePipeline.required "names" nip05NamesDecoder
        |> DecodePipeline.optional "relays" (Decode.maybe nip05RelaysDecoder) Nothing


nip05NamesDecoder : Decoder (Dict String String)
nip05NamesDecoder =
    Decode.dict Decode.string


nip05RelaysDecoder : Decoder (Dict String (List RelayUrl))
nip05RelaysDecoder =
    Decode.dict (Decode.list Relay.relayUrlDecoder)


nip05StringDecoder : Decoder Nip05
nip05StringDecoder =
    Decode.string
        |> Decode.andThen
            (\nip05String ->
                parseNip05 nip05String
                    |> Maybe.map Decode.succeed
                    |> Maybe.withDefault (Decode.fail <| "Error parsing nip05: " ++ nip05String)
            )



-- tests with checking NIP-05 data via proxy (on pareto.space) have not lead to more successful validations
-- (didn't do exact statistics but still saw many failed validations)


fetchNip05Info : (Result Http.Error Nip05Data -> msg) -> Nip05 -> Cmd msg
fetchNip05Info toMsg nip05 =
    --fetchNip05InfoViaProxy toMsg nip05
    fetchNip05InfoDirectly toMsg nip05


fetchNip05InfoDirectly : (Result Http.Error Nip05Data -> msg) -> Nip05 -> Cmd msg
fetchNip05InfoDirectly toMsg nip05 =
    Http.request
        { method = "GET"
        , headers =
            [ Http.header "Accept" "application/nostr+json"
            ]
        , url = "https://" ++ nip05.domain ++ "/.well-known/nostr.json?name=" ++ nip05.user
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg nip05Decoder
        , timeout = Nothing
        , tracker = Nothing
        }


fetchNip05InfoViaProxy : (Result Http.Error Nip05Data -> msg) -> Nip05 -> Cmd msg
fetchNip05InfoViaProxy toMsg nip05 =
    Http.request
        { method = "GET"
        , headers =
            [ Http.header "Accept" "application/nostr+json"
            ]

        -- , url = "http://localhost:4000/api/nip05/validate?handle=" ++ nip05ToString nip05
        , url = "https://pareto.space/api/nip05/validate?handle=" ++ nip05ToString nip05
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg nip05Decoder
        , timeout = Nothing
        , tracker = Nothing
        }
