module Nostr.Nip11 exposing (..)

import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Time

type alias Nip11Info =
    { name : Maybe String
    , description : Maybe String
    , pubkey : Maybe String
    , contact : Maybe String
    , supportedNips : Maybe (List Int)
    , software : Maybe String
    , version : Maybe String
    , icon : Maybe String
    , limitation : Maybe Nip11Limitations
    , paymentsUrl : Maybe String
    }


type alias Nip11Fees =
    { amount : Int
    , unit : String
    , period : Int
    }

type alias Nip11Limitations =
    { authRequired : Maybe Bool
    , createdAtLowerLimit : Maybe Time.Posix
    , createdAtUpperLimit : Maybe Time.Posix
    , maxContentLength : Maybe Int
    , maxEventTags : Maybe Int
    , maxFilters : Maybe Int
    , maxLimit : Maybe Int
    , maxMessageLength : Maybe Int
    , maxSubitLength : Maybe Int
    , maxSubscriptions : Maybe Int
    , minPowDifficulty : Maybe Int
    , paymentRequired : Maybe Bool
    , restrictedWrites : Maybe Bool
    }

type RelayFeature
    = RelayAuth
    | RelayBlog
    | RelayChat
    | RelayDelete
    | RelaySearch


nip11Decoder : Decode.Decoder Nip11Info
nip11Decoder =
    Decode.succeed Nip11Info
        |> Pipeline.optional "name" (Decode.map Just Decode.string) Nothing
        |> Pipeline.optional "description" (Decode.map Just Decode.string) Nothing
        |> Pipeline.optional "pubkey" (Decode.map Just Decode.string) Nothing
        |> Pipeline.optional "contact" (Decode.map Just Decode.string) Nothing
        |> Pipeline.optional "supported_nips" (Decode.map Just (Decode.list Decode.int)) Nothing
        |> Pipeline.optional "software" (Decode.map Just Decode.string) Nothing
        |> Pipeline.optional "version" (Decode.map Just Decode.string) Nothing
        |> Pipeline.optional "icon" (Decode.map Just Decode.string) Nothing
        |> Pipeline.optional "limitation" (Decode.map Just nip11LimitationsDecoder) Nothing
        |> Pipeline.optional "payments_url" (Decode.map Just Decode.string) Nothing


nip11LimitationsDecoder : Decode.Decoder Nip11Limitations
nip11LimitationsDecoder =
    Decode.succeed Nip11Limitations
        |> Pipeline.optional "auth_required" (Decode.map Just Decode.bool) Nothing
        |> Pipeline.optional "created_at_lower_limit" (Decode.map Just decodeUnixTime) Nothing
        |> Pipeline.optional "created_at_upper_limit" (Decode.map Just decodeUnixTime) Nothing
        |> Pipeline.optional "max_content_length" (Decode.map Just Decode.int) Nothing
        |> Pipeline.optional "max_event_tags" (Decode.map Just Decode.int) Nothing
        |> Pipeline.optional "max_filters" (Decode.map Just Decode.int) Nothing
        |> Pipeline.optional "max_limit" (Decode.map Just Decode.int) Nothing
        |> Pipeline.optional "max_message_length" (Decode.map Just Decode.int) Nothing
        |> Pipeline.optional "max_subid_length" (Decode.map Just Decode.int) Nothing
        |> Pipeline.optional "max_subscriptions" (Decode.map Just Decode.int) Nothing
        |> Pipeline.optional "min_pow_difficulty" (Decode.map Just Decode.int) Nothing
        |> Pipeline.optional "payment_required" (Decode.map Just Decode.bool) Nothing
        |> Pipeline.optional "restricted_writes" (Decode.map Just Decode.bool) Nothing


decodeUnixTime : Decode.Decoder Time.Posix
decodeUnixTime =
    Decode.int
        |> Decode.map (\unixTime -> Time.millisToPosix (unixTime * 1000))



relayFeatures : Nip11Info -> List RelayFeature
relayFeatures nip11Info =
    case nip11Info.supportedNips of
        Just supportedNips ->
            List.filterMap relayFeatureFromNip supportedNips 

        Nothing ->
            []


relayFeatureFromNip : Int -> Maybe RelayFeature
relayFeatureFromNip nip =
    case nip of
        28 ->
            Just RelayChat

        40 ->
            Just RelayDelete
        
        _ ->
            Nothing

fetchNip11 : (Result Http.Error Nip11Info -> msg) -> String -> Cmd msg
fetchNip11 toMsg urlWithoutProtocol =
     Http.request
        { method = "GET"
        , headers =
            [ Http.header "Accept" "application/nostr+json"
            ]
        , url = "https://" ++ urlWithoutProtocol -- ++ "/.well-known/nostr.json"
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg nip11Decoder
        , timeout = Nothing
        , tracker = Nothing
        }

