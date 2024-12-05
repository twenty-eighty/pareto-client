module Nostr.Nip05 exposing (..)

import Dict exposing (Dict)
import Email
import UInt64 exposing (add)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as DecodePipeline
import Http
import Nostr.Types exposing (PubKey)

type alias Nip05 =
    { user : String
    , domain : String
    }

type alias Nip05Data =
    { names : Dict String String
    , relays : Maybe (Dict String (List String))
    }

type alias Nip05String = String

parseNip05 : String -> Maybe Nip05
parseNip05 nip05String =
    case Email.parse nip05String of
        Ok {local, domain} ->
            Just { user = local, domain = domain}

        Err _ ->
            Nothing

nip05ToString : Nip05 -> Nip05String
nip05ToString nip05 =
    case nip05.user of
        "_" ->
            nip05.domain

        user ->
            user ++ "@" ++ nip05.domain

nip05Decoder : Decoder Nip05Data
nip05Decoder =
    Decode.succeed Nip05Data
    |> DecodePipeline.required "names" nip05NamesDecoder
    |> DecodePipeline.optional "relays" (Decode.maybe nip05RelaysDecoder) Nothing

nip05NamesDecoder : Decoder (Dict String String)
nip05NamesDecoder =
    Decode.dict Decode.string

nip05RelaysDecoder : Decoder (Dict String (List String))
nip05RelaysDecoder =
    Decode.dict (Decode.list Decode.string)


nip05StringDecoder : Decoder Nip05
nip05StringDecoder =
    Decode.string
    |> Decode.andThen (\nip05String ->
        parseNip05 nip05String
        |> Maybe.map Decode.succeed
        |> Maybe.withDefault (Decode.fail <| "Error parsing nip05: " ++ nip05String)
        )

fetchNip05Info : (Result Http.Error Nip05Data -> msg) -> Nip05 -> Cmd msg
fetchNip05Info toMsg nip05 =
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


