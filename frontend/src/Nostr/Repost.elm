module Nostr.Repost exposing (..)

import Nostr.Event exposing (Event, Tag(..))
import Nostr.Types exposing (PubKey, EventId)


type alias Repost =
    { pubKey : PubKey
    , repostedEvent : EventId
    , repostedPubKey : Maybe PubKey
    }
{-
-}

repostFromEvent : Event -> Maybe Repost
repostFromEvent event =
    let
        repost =
            event.tags
            |> List.foldl (\tag res ->
                case tag of 
                    EventIdTag eventId ->
                        { res | repostedEvent = Just eventId }

                    PublicKeyTag repostedPubKey _ _ ->
                        { res | repostedPubKey = Just repostedPubKey }

                    _ ->
                        res
                    )
                { repostedEvent = Nothing
                , repostedPubKey = Nothing
                }
    in
    repost.repostedEvent
    |> Maybe.map (\repostedEvent ->
            { pubKey = event.pubKey, repostedEvent = repostedEvent, repostedPubKey = repost.repostedPubKey }
        ) 
