module Nostr.RelayList exposing (..)

import Nostr.Event exposing (Event, Tag(..))
import Nostr.Types exposing (PubKey)

-- NIP-51
-- this is intended for kinds 10006 (blocked relays), 10007 (search relays), 10050 (DM relays)

relayListFromEvent : Event -> (PubKey, List String)
relayListFromEvent event =
    let
        relayList =
            event.tags
            |> List.foldl (\tag acc ->
                case tag of 
                    RelayTag url ->
                        acc ++ [ url ]

                    _ ->
                        acc
                    )
                []
    in
    ( event.pubKey, relayList )
