module Nostr.Reactions exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as DecodePipeline
import Nostr.Event exposing (Kind, kindDecoder)
import Nostr.Types exposing (PubKey, EventId)

type alias Interactions =
    { zaps : Maybe Int
    , highlights : Maybe Int
    , reactions : Maybe Int
    , reposts : Maybe Int
    , notes : Maybe Int
    , bookmarks : Maybe Int
    }

type alias Reaction =
    { content : String
    , id : EventId
    , pubKey : PubKey
    , noteIdReactedTo : Maybe EventId
    , pubKeyReactedTo : Maybe PubKey
    , kindReactedTo : Maybe Kind
    , coordinatesReactedTo : Maybe String
    }

nostrReactionDecoder : Decoder Reaction
nostrReactionDecoder =
    Decode.succeed Reaction
    |> DecodePipeline.required "content" Decode.string
    |> DecodePipeline.required "id" Decode.string
    |> DecodePipeline.required "pubkey" Decode.string
    |> DecodePipeline.optional "noteid-reactedto" (Decode.maybe Decode.string) Nothing
    |> DecodePipeline.optional "pubkey-reactedto" (Decode.maybe Decode.string) Nothing
    |> DecodePipeline.optional "kind-reactedto" (Decode.maybe kindDecoder) Nothing
    |> DecodePipeline.optional "coordinates-reactedto" (Decode.maybe Decode.string) Nothing
