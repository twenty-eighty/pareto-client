module Nostr.CommunityList exposing (..)

import Nostr.Event exposing (Event, Kind(..), Tag(..))
import Nostr.Types exposing (PubKey)


type alias CommunityReference =
    { identifier : String
    , pubKey : String
    }



{-
   ["EVENT","+authors-kinds-0gcop",
       {"content":""
       ,"created_at":1725831874
       ,"id":"e86a22bbed058d659c0d361b9a9623b50ceb7b623ade1a252317f49ac62e4c8d"
       ,"kind":10004
       ,"pubkey":"8127df93d8453767aa11e74206f48aeea30d3d65a383c98d243b031fc7446afb"
       ,"sig":"624dc5f345e5db810569c1ff68428217589a15b97b30434104f73c05c16a423c3ce989c2c6fc08fa586cafae165adb6c0fb11cce5e558c766b51e7211631080b"
       ,"tags":[
           ["a","34550:8127df93d8453767aa11e74206f48aeea30d3d65a383c98d243b031fc7446afb:Pareto"]
           ]
       }
-}


communityListFromEvent : Event -> ( PubKey, List CommunityReference )
communityListFromEvent event =
    let
        communityList =
            event.tags
                |> List.filterMap
                    (\tag ->
                        case tag of
                            AddressTag ( KindCommunityDefinition, pubKey, identifier ) _ ->
                                Just { identifier = identifier, pubKey = pubKey }

                            _ ->
                                Nothing
                    )
    in
    ( event.pubKey, communityList )
