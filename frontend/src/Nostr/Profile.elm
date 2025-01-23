module Nostr.Profile exposing (..)

import Dict exposing (Dict)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as DecodePipeline
import Json.Encode as Encode
import Nostr.Event exposing (Event, Identity, Tag(..))
import Nostr.Nip05 as Nip05 exposing (Nip05, nip05StringDecoder)
import Nostr.Shared
import Nostr.Types exposing (PubKey)
import Time


type Author
    = AuthorPubkey PubKey
    | AuthorProfile Profile ProfileValidation


type alias Profile =
    { nip05 : Maybe Nip05
    , lud16 : Maybe String
    , name : Maybe String
    , displayName : Maybe String
    , about : Maybe String
    , picture : Maybe String
    , banner : Maybe String
    , website : Maybe String
    , bot : Maybe Bool
    , npub : Maybe String
    , createdAt : Maybe Time.Posix
    , pubKey : PubKey
    , identities : List Identity
    }


type ProfileValidation
    = ValidationUnknown
    | ValidationPending
    | ValidationNameMissing
    | ValidationNotMatchingPubKey
    | ValidationNetworkError Http.Error
    | ValidationSucceeded


profileToJson : Profile -> String
profileToJson profile =
    []
        |> appendStringToEncode "name" profile.name
        |> appendStringToEncode "displayName" profile.displayName
        |> appendStringToEncode "nip05" (Maybe.map Nip05.nip05ToString profile.nip05)
        |> appendStringToEncode "lud16" profile.lud16
        |> appendStringToEncode "about" profile.about
        |> appendStringToEncode "picture" profile.picture
        |> appendStringToEncode "banner" profile.banner
        |> appendStringToEncode "website" profile.website
        |> appendBoolToEncode "bot" profile.bot
        |> Encode.object
        |> Encode.encode 0


appendStringToEncode : String -> Maybe String -> List ( String, Encode.Value ) -> List ( String, Encode.Value )
appendStringToEncode key maybeValue elements =
    case maybeValue of
        Just value ->
            elements ++ [ ( key, Encode.string value ) ]

        Nothing ->
            elements


appendBoolToEncode : String -> Maybe Bool -> List ( String, Encode.Value ) -> List ( String, Encode.Value )
appendBoolToEncode key maybeValue elements =
    case maybeValue of
        Just value ->
            ( key, Encode.bool value ) :: elements

        Nothing ->
            elements


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
    , bot = Nothing
    , pubKey = pubKey
    , npub = Nothing
    , createdAt = Nothing
    , identities = []
    }


profileFromEvent : Event -> Maybe Profile
profileFromEvent event =
    Decode.decodeString nostrProfileDecoder event.content
        |> Result.toMaybe
        |> Maybe.map
            (\profile ->
                if profile.nip05 == Nothing then
                    { profile | pubKey = event.pubKey }

                else
                    { profile | pubKey = event.pubKey }
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
        |> DecodePipeline.optional "picture" (Decode.maybe httpsUrlDecoder) Nothing
        |> DecodePipeline.optional "banner" (Decode.maybe httpsUrlDecoder) Nothing
        |> DecodePipeline.optional "website" (Decode.maybe httpsUrlDecoder) Nothing
        |> DecodePipeline.optional "bot" (Decode.maybe Decode.bool) Nothing
        |> DecodePipeline.optional "npub" (Decode.maybe Decode.string) Nothing
        |> DecodePipeline.optional "created_at" (Decode.maybe decodeUnixTime) Nothing
        |> DecodePipeline.hardcoded ""
        |> DecodePipeline.hardcoded []



-- by upgrading HTTP to HTTPS links we avoid
-- being displayed as unsafe site


httpsUrlDecoder : Decode.Decoder String
httpsUrlDecoder =
    Decode.string
        |> Decode.andThen
            (\url ->
                url
                    |> Nostr.Shared.ensureHttps
                    |> Decode.succeed
            )


decodeUnixTime : Decoder Time.Posix
decodeUnixTime =
    Decode.int
        |> Decode.map
            (\timeInt -> Time.millisToPosix timeInt)
