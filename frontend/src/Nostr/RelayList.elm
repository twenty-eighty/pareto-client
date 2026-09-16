module Nostr.RelayList exposing
    ( PrivateIngestResult
    , SearchIngestResult
    , eventWithPrivateRelayList
    , ingestPrivateRelays
    , ingestSearchRelays
    , withUniqueEntries
    )

import Dict exposing (Dict)
import Nostr.Event exposing (Event, Kind(..), Tag(..), emptyEvent)
import Nostr.Relay as Relay exposing (RelayUrl)
import Nostr.Types exposing (PubKey)
import Time



-- NIP-51
-- this is intended for kinds 10006 (blocked relays), 10007 (search relays), 10050 (DM relays)


type alias SearchIngestResult =
    { searchRelayLists : Dict PubKey (List RelayUrl)
    , unknownRelays : List RelayUrl
    }


type alias PrivateIngestResult =
    { privateRelayLists : Dict PubKey (List RelayUrl)
    , unknownRelays : List RelayUrl
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
    -> { relayLists : Dict PubKey (List RelayUrl), unknownRelays : List RelayUrl }
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
                |> List.filter (\url -> not (Dict.member (Relay.toKey url) relays))
                |> withUniqueEntries
    in
    { relayLists = relayListDict
    , unknownRelays = unknownRelays
    }


withUniqueEntries : List RelayUrl -> List RelayUrl
withUniqueEntries relayList =
    relayList
        |> List.map (\url -> ( Relay.toKey url, url ))
        |> Dict.fromList
        |> Dict.values


{-| Build an unsigned kind 10013 event. JS encrypts tags into content before publish.
-}
eventWithPrivateRelayList : PubKey -> List RelayUrl -> Event
eventWithPrivateRelayList pubKey relays =
    let
        event =
            emptyEvent pubKey KindPrivateRelayList
    in
    { event
        | createdAt = Time.millisToPosix 0
        , content = ""
        , tags =
            relays
                |> List.map (\url -> RelayTag (Relay.toWire url))
    }


relayListFromEvent : Event -> ( PubKey, List RelayUrl )
relayListFromEvent event =
    let
        relayList =
            event.tags
                |> List.foldl
                    (\tag acc ->
                        case tag of
                            RelayTag url ->
                                acc ++ [ Relay.fromString url ]

                            _ ->
                                acc
                    )
                    []
    in
    ( event.pubKey, relayList )
