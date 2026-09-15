module Nostr.FollowSet exposing (..)

import Dict exposing (Dict)
import Nostr.Event exposing (Event, Kind(..), Tag(..))
import Nostr.Relay as Relay
import Nostr.Types exposing (Following(..), PubKey)


type alias FollowSet =
    { identifier : String
    , title : Maybe String
    , image : Maybe String
    , description : Maybe String
    , followList : List Following
    }



{- -}


followSetFromEvent : Event -> Maybe ( PubKey, FollowSet )
followSetFromEvent event =
    let
        followSet =
            event.tags
                |> List.foldl
                    (\tag res ->
                        case tag of
                            AboutTag description ->
                                { res | description = Just description }

                            DescriptionTag description ->
                                { res | description = Just description }

                            EventDelegationTag identifier ->
                                { res | identifier = Just identifier }

                            ImageTag url _ ->
                                { res | image = Just url }

                            NameTag title ->
                                { res | title = Just title }

                            PublicKeyTag pubKey relay petname ->
                                { res | followList = res.followList ++ [ FollowingPubKey { pubKey = pubKey, relay = Maybe.map Relay.toWire relay, petname = petname } ] }

                            HashTag hashtag ->
                                { res | followList = res.followList ++ [ FollowingHashtag hashtag ] }

                            TitleTag title ->
                                { res | title = Just title }

                            _ ->
                                res
                    )
                    { identifier = Nothing
                    , title = Nothing
                    , image = Nothing
                    , description = Nothing
                    , followList = []
                    }
    in
    Maybe.map
        (\identifier ->
            ( event.pubKey
            , { identifier = identifier
              , title = followSet.title
              , image = followSet.image
              , description = followSet.description
              , followList = followSet.followList
              }
            )
        )
        followSet.identifier


ingest : Dict PubKey (Dict String FollowSet) -> List Event -> Dict PubKey (Dict String FollowSet)
ingest dict events =
    events
        |> List.filterMap followSetFromEvent
        |> List.foldl
            (\( pubKey, followSet ) acc ->
                case Dict.get pubKey acc of
                    Just followSetDict ->
                        Dict.insert pubKey (Dict.insert followSet.identifier followSet followSetDict) acc

                    Nothing ->
                        Dict.insert pubKey (Dict.singleton followSet.identifier followSet) acc
            )
            dict
