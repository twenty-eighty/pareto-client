module Nostr.Reactions exposing (..)

import Nostr.Event exposing (AddressComponents, Event, Kind, Tag(..))
import Nostr.Types exposing (EventId, PubKey)



-- NIP-25 / kind 7


type alias Interactions =
    { zaps : Maybe Int
    , highlights : Maybe Int
    , reactions : Maybe Int
    , reposts : Maybe Int
    , notes : Maybe Int
    , bookmarks : Maybe Int
    , isBookmarked : Bool
    , reaction : Maybe Reaction
    }


type alias Reaction =
    { content : String
    , id : EventId
    , pubKey : PubKey
    , noteIdReactedTo : Maybe EventId
    , pubKeyReactedTo : Maybe PubKey
    , kindReactedTo : Maybe Kind
    , addressComponentsReactedTo : Maybe AddressComponents
    }


reactionFromEvent : Event -> Reaction
reactionFromEvent event =
    event.tags
        |> List.foldl
            (\tag acc ->
                case tag of
                    AddressTag addressComponents ->
                        { acc | addressComponentsReactedTo = Just addressComponents }

                    EventIdTag eventId _ ->
                        { acc | noteIdReactedTo = Just eventId }

                    KindTag kind ->
                        { acc | kindReactedTo = Just kind }

                    PublicKeyTag pubKey _ _ ->
                        { acc | pubKeyReactedTo = Just pubKey }

                    _ ->
                        acc
            )
            (emptyReaction event.content event.id event.pubKey)


emptyReaction : String -> EventId -> PubKey -> Reaction
emptyReaction content eventId pubKey =
    { content = content
    , id = eventId
    , pubKey = pubKey
    , noteIdReactedTo = Nothing
    , pubKeyReactedTo = Nothing
    , kindReactedTo = Nothing
    , addressComponentsReactedTo = Nothing
    }



{-
   ["EVENT",{"created_at":1735141143
   ,"content":"+"
   ,"tags":[
       ["a","30023:cff1720e77bb068f0ebbd389dcd50822dd1ac8d2ac0b0f5f0800ae9e15c7e2b2:1734615116706"],
       ["p","cff1720e77bb068f0ebbd389dcd50822dd1ac8d2ac0b0f5f0800ae9e15c7e2b2"]
       ]
   ,"kind":7
   ,"pubkey":"0f4795bf31824a414148daf1b589bb8138fb0a03963f984c84462e40a8365abe"
   ,"id":"4fdfff5156f1a3d47cbb0283fb3728bc26ce27b2f8f3bf5cc7c6f4962ca97247"
   ,"sig":"dc3579a077b6ce13df67125452ae981c0b841145130d79c3117c7cca2eb54cd8d3aa263c7fab1075c6cf3897258544f8258662d16de17b745bbdc797a9c7565e"
   }]

   ["EVENT",{"created_at":1735141185
   ,"content":"This reaction will be deleted!"
   ,"tags":
       [["e","4fdfff5156f1a3d47cbb0283fb3728bc26ce27b2f8f3bf5cc7c6f4962ca97247"]]
   ,"kind":5
   ,"pubkey":"0f4795bf31824a414148daf1b589bb8138fb0a03963f984c84462e40a8365abe"
   ,"id":"f6af082b601fdb6cb8fbde694992e11a95557f89999323617e97b9c7a19f5868"
   ,"sig":"0728ea946bca18dbc1b285f70bee582a58c05a4c60769f9421dec67dca747b6b35547ee040fab0738dc4d567e647a0bc497c73e87536871f66a39d0eb274bfea"}]

-}
