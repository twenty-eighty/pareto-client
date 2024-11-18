module Nostr.FollowList exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as DecodePipeline
import Nostr.Event exposing (Event, Tag(..))
import Nostr.Types exposing (PubKey)


type alias PubKeyFollowList =
    { pubKey : PubKey
    , following : List Following
    }

type alias Following =
    { pubKey : PubKey
    , relay : Maybe String
    , petname : Maybe String
    }


followListFromEvent : Event -> PubKeyFollowList
followListFromEvent event =
    let
        followList =
            event.tags
            |> List.foldl (\tag res ->
                case tag of 
                    PublicKeyTag pubKey relay petname ->
                        { res | following = res.following ++ [{pubKey = pubKey, relay = relay, petname = petname}] }

                    _ ->
                        res
                    )
                { pubKey = event.pubKey
                , following = []
                }
    in
    followList


nostrPubKeyFollowListDecoder : Decoder PubKeyFollowList
nostrPubKeyFollowListDecoder =
    Decode.succeed PubKeyFollowList
    |> DecodePipeline.required "pubkey" Decode.string
    |> DecodePipeline.required "following" (Decode.list nostrFollowingDecoder)

nostrFollowingDecoder : Decoder Following
nostrFollowingDecoder =
    Decode.succeed Following
    |> DecodePipeline.required "pubkey" Decode.string
    |> DecodePipeline.optional "relay" (Decode.maybe Decode.string) Nothing
    |> DecodePipeline.optional "petname" (Decode.maybe Decode.string) Nothing
