module Nostr.FollowSet exposing (..)

import Nostr.Event exposing (Event, Kind(..), Tag(..))
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
                                { res | followList = res.followList ++ [ FollowingPubKey { pubKey = pubKey, relay = relay, petname = petname } ] }

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
