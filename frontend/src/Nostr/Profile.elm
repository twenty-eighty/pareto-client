module Nostr.Profile exposing (..)

import Dict exposing (Dict)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as DecodePipeline
import Nostr.Event exposing (Event, Tag(..))
import Nostr.Nip05 exposing (Nip05, nip05StringDecoder)
import Nostr.Types exposing (PubKey)
import Time
import Nostr.Nip05 as Nip05

type Author
    = AuthorPubkey PubKey
    | AuthorProfile Profile

type alias Profile =
    { nip05 : Maybe Nip05
    , lud16 : Maybe String
    , name : Maybe String
    , displayName : Maybe String
    , about : Maybe String
    , picture : Maybe String
    , banner : Maybe String
    , website : Maybe String
    , npub : Maybe String
    , createdAt : Maybe Time.Posix
    , pubKey : PubKey
    , valid : ProfileValidation
    }

type ProfileValidation
    = ValidationUnknown
    | ValidationNoNip05InProfile
    | ValidationNameMissing
    | ValidationNotMatchingPubKey
    | ValidationNetworkError Http.Error
    | ValidationSucceeded

emptyProfile : String -> Profile
emptyProfile pubKey =
    { nip05 = Nothing
    , lud16 = Nothing
    , name = Nothing
    , displayName = Nothing
    , about = Nothing
    , picture = Nothing
    , banner = Nothing
    , website = Nothing
    , pubKey = pubKey
    , npub = Nothing
    , createdAt = Nothing
    , valid = ValidationUnknown

    }

profileFromEvent : Event -> Maybe Profile
profileFromEvent event =
    Decode.decodeString nostrProfileDecoder event.content
    |> Result.toMaybe
    |> Maybe.map (\profile ->
        if profile.nip05 == Nothing then
            { profile | pubKey = event.pubKey, valid = ValidationNoNip05InProfile }
        else
            { profile | pubKey = event.pubKey, valid = ValidationUnknown }
        )

{-
{
    "name": "NathanDay",
    "nip05": "nathan@btcmap.org",
    "about": "Tag maps and stay humble.\n\nBuilding @btcmap.",
    "lud16": "nathanday@getalby.com",
    "display_name": "Nathan Day",
    "picture": "https://m.primal.net/JwCz.png",
    "banner": "https://m.primal.net/HMAv.jpg",
    "website": "nathan.day.ag",
    "displayName": "Nathan Day",
    "pubkey": "c4f5e7a75a8ce3683d529cff06368439c529e5243c6b125ba68789198856cac7",
    "npub": "npub1cn670f663n3ks02jnnlsvd5y88zjnefy8343ykaxs7y3nzzketrsrjwt8a",
    "created_at": 1723382003
}

-}

type alias PubkeyProfile =
    { pubKey : String
    , profile : Profile
    }

pubkeyProfileDecoder : Decoder PubkeyProfile
pubkeyProfileDecoder =
    Decode.succeed PubkeyProfile
    |> DecodePipeline.required "pubkey" Decode.string
    |> DecodePipeline.required "profile" nostrProfileDecoder

nostrProfileDecoder : Decoder Profile
nostrProfileDecoder =
    Decode.succeed Profile
    |> DecodePipeline.optional "nip05" (Decode.maybe nip05StringDecoder) Nothing
    |> DecodePipeline.optional "lud16" (Decode.maybe Decode.string) Nothing
    |> DecodePipeline.optional "name" (Decode.maybe Decode.string) Nothing
    |> DecodePipeline.optional "display_name" (Decode.maybe Decode.string) Nothing
    |> DecodePipeline.optional "about" (Decode.maybe Decode.string) Nothing
    |> DecodePipeline.optional "picture" (Decode.maybe Decode.string) Nothing
    |> DecodePipeline.optional "banner" (Decode.maybe Decode.string) Nothing
    |> DecodePipeline.optional "website" (Decode.maybe Decode.string) Nothing
    |> DecodePipeline.optional "npub" (Decode.maybe Decode.string) Nothing
    |> DecodePipeline.optional "created_at" (Decode.maybe decodeUnixTime) Nothing
    |> DecodePipeline.hardcoded ""
    |> DecodePipeline.hardcoded ValidationUnknown


decodeUnixTime : Decoder Time.Posix
decodeUnixTime =
    Decode.int 
        |> Decode.map (\timeInt -> Time.millisToPosix (timeInt)
        )

{-
{
    "created_at": 1725050484,
    "profileEvent": "{\"created_at\":1725050484,\"content\":\"{\\\"nip05\\\":\\\"synalysis@synalysis.com\\\",\\\"lud16\\\":\\\"synalysis@npub.cash\\\",\\\"picture\\\":\\\"https://image.nostr.build/a4630d4bd50ed9ce2f24c6cde9de8b0097806134c3b89ec9be698e98eb5bb0b2.jpg\\\",\\\"name\\\":\\\"synalysis\\\",\\\"about\\\":\\\"Software for binary file analysis\\\",\\\"banner\\\":\\\"https://synalysis.com/banner.png\\\"}\",\"tags\":[],\"kind\":0,\"pubkey\":\"8127df93d8453767aa11e74206f48aeea30d3d65a383c98d243b031fc7446afb\",\"id\":\"72340a0731be736735b8f8d08c31d1b4db0e925a4c31fff6dd674dd9a0cbeb1d\",\"sig\":\"67f37ed6a952f8f15f225afc8d3f5fca540501e6a63c37b12067caf081b7a32b65b954d73e6d2bc898e95d850ae47537c2d1e685b1354d8c72aef150cfaf1af1\"}",
    "nip05": "synalysis@synalysis.com",
    "lud16": "synalysis@npub.cash",
    "image": "https://image.nostr.build/a4630d4bd50ed9ce2f24c6cde9de8b0097806134c3b89ec9be698e98eb5bb0b2.jpg",
    "name": "synalysis",
    "about": "Software for binary file analysis",
    "banner": "https://synalysis.com/banner.png"
}
-}

profileDisplayName : PubKey -> Profile -> String
profileDisplayName pubKey profile =
    case (profile.displayName, profile.name, profile.nip05) of
        (Just displayName, _, _) ->
            displayName
        (Nothing, Just name, _) ->
            name
        (Nothing, Nothing, Just nip05) ->
            Nip05.nip05ToString nip05
        (Nothing, Nothing, Nothing) ->
            pubKey
