module Nostr.RelayList exposing (..)

import Dict exposing (Dict)
import Nostr.Event exposing (Event, Tag(..))
import Nostr.Relay exposing (hostWithoutProtocol)
import Nostr.Types exposing (PubKey, RelayUrl)
import Set



-- NIP-51
-- this is intended for kinds 10006 (blocked relays), 10007 (search relays), 10050 (DM relays)


type alias SearchIngestResult =
    { searchRelayLists : Dict PubKey (List RelayUrl)
    , unknownRelays : List String
    }


type alias PrivateIngestResult =
    { privateRelayLists : Dict PubKey (List RelayUrl)
    , unknownRelays : List String
    }


ingestSearchRelays :
    Dict PubKey (List RelayUrl)
    -> Dict String a
    -> List Event
    -> SearchIngestResult
ingestSearchRelays searchRelayLists relays events =
    let
        result =
            ingestRelayLists searchRelayLists relays events
    in
    { searchRelayLists = result.relayLists
    , unknownRelays = result.unknownRelays
    }


ingestPrivateRelays :
    Dict PubKey (List RelayUrl)
    -> Dict String a
    -> List Event
    -> PrivateIngestResult
ingestPrivateRelays privateRelayLists relays events =
    let
        result =
            ingestRelayLists privateRelayLists relays events
    in
    { privateRelayLists = result.relayLists
    , unknownRelays = result.unknownRelays
    }


ingestRelayLists :
    Dict PubKey (List RelayUrl)
    -> Dict String a
    -> List Event
    -> { relayLists : Dict PubKey (List RelayUrl), unknownRelays : List String }
ingestRelayLists existingRelayLists relays events =
    let
        lists =
            events
                |> List.map relayListFromEvent

        relayListDict =
            lists
                |> List.foldl
                    (\( pubKey, relayList ) dict ->
                        Dict.insert pubKey (withUniqueEntries relayList) dict
                    )
                    existingRelayLists

        unknownRelays =
            lists
                |> List.concatMap (\( _, urls ) -> urls)
                |> List.map hostWithoutProtocol
                |> List.filter (\relay -> not (Dict.member relay relays))
                |> Set.fromList
                |> Set.toList
    in
    { relayLists = relayListDict
    , unknownRelays = unknownRelays
    }


withUniqueEntries : List RelayUrl -> List RelayUrl
withUniqueEntries relayList =
    relayList
        |> Set.fromList
        |> Set.toList


relayListFromEvent : Event -> ( PubKey, List RelayUrl )
relayListFromEvent event =
    let
        relayList =
            event.tags
                |> List.foldl
                    (\tag acc ->
                        case tag of
                            RelayTag url ->
                                acc ++ [ url ]

                            _ ->
                                acc
                    )
                    []
    in
    ( event.pubKey, relayList )
