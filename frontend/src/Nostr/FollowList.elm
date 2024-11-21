module Nostr.FollowList exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as DecodePipeline
import Nostr.Event exposing (Event, Tag(..))
import Nostr.Types exposing (PubKey)


type alias PubKeyFollowList =
    { pubKey : PubKey
    , following : List Following
    }

type Following
    = FollowingPubKey
        { pubKey : PubKey
        , relay : Maybe String
        , petname : Maybe String
        }
    | FollowingHashtag String


followingPubKey : Following -> Maybe PubKey
followingPubKey following =
    case following of
        FollowingPubKey { pubKey }  ->
            Just pubKey

        FollowingHashtag _ ->
            Nothing

followListFromEvent : Event -> PubKeyFollowList
followListFromEvent event =
    let
        followList =
            event.tags
            |> List.foldl (\tag res ->
                case tag of 
                    PublicKeyTag pubKey relay petname ->
                        { res | following = res.following ++ [FollowingPubKey {pubKey = pubKey, relay = relay, petname = petname}] }

                    HashTag hashtag ->
                        { res | following = res.following ++ [FollowingHashtag hashtag ] }

                    _ ->
                        res
                    )
                { pubKey = event.pubKey
                , following = []
                }
    in
    followList