module Nostr.FollowList exposing (..)

import Nostr.Event exposing (Event, Kind(..), Tag(..), emptyEvent)
import Nostr.Types exposing (Following(..), PubKey)


type alias PubKeyFollowList =
    { pubKey : PubKey
    , following : List Following
    }


emptyFollowList : List Following
emptyFollowList =
    []


followListWithPubKey : List Following -> PubKey -> List Following
followListWithPubKey followList pubKey =
    let
        listContainsPubKey =
            followList
                |> List.filter
                    (\following ->
                        case followingPubKey following of
                            Just pubKeyFollowing ->
                                pubKeyFollowing == pubKey

                            _ ->
                                False
                    )
                |> List.isEmpty
                |> not
    in
    -- don't duplicate entry
    if not listContainsPubKey then
        followList ++ [ FollowingPubKey { pubKey = pubKey, relay = Nothing, petname = Nothing } ]

    else
        followList


followListWithoutPubKey : List Following -> PubKey -> List Following
followListWithoutPubKey followList pubKey =
    followList
        |> List.filter
            (\following ->
                case followingPubKey following of
                    Just pubKeyFollowing ->
                        pubKeyFollowing /= pubKey

                    Nothing ->
                        True
            )


followingPubKey : Following -> Maybe PubKey
followingPubKey following =
    case following of
        FollowingPubKey { pubKey } ->
            Just pubKey

        FollowingHashtag _ ->
            Nothing


followListFromEvent : Event -> PubKeyFollowList
followListFromEvent event =
    event.tags
        |> List.foldl
            (\tag res ->
                case tag of
                    PublicKeyTag pubKey relay petname ->
                        { res | following = res.following ++ [ FollowingPubKey { pubKey = pubKey, relay = relay, petname = petname } ] }

                    HashTag hashtag ->
                        { res | following = res.following ++ [ FollowingHashtag hashtag ] }

                    _ ->
                        res
            )
            { pubKey = event.pubKey
            , following = []
            }


followListEvent : PubKey -> List Following -> Event
followListEvent pubKey list =
    let
        event =
            emptyEvent pubKey KindFollows
    in
    { event
        | tags =
            []
                |> addFollowsTags list
    }


addFollowsTags : List Following -> List Tag -> List Tag
addFollowsTags followsList tags =
    followsList
        |> List.map followsTag
        |> List.append tags


followsTag : Following -> Tag
followsTag following =
    case following of
        FollowingPubKey { pubKey, relay, petname } ->
            PublicKeyTag pubKey relay petname

        FollowingHashtag hashtag ->
            HashTag hashtag


pubKeyIsFollower : PubKey -> List Following -> Bool
pubKeyIsFollower userPubKey followsList =
    followsList
        |> List.filter
            (\follows ->
                case follows of
                    FollowingPubKey { pubKey } ->
                        userPubKey == pubKey

                    _ ->
                        False
            )
        |> (not << List.isEmpty)
