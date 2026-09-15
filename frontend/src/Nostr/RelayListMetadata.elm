module Nostr.RelayListMetadata exposing (..)

import Dict exposing (Dict)
import Nostr.Event exposing (Event, Kind(..), Tag(..))
import Nostr.Relay exposing (Relay, RelayState(..), hostWithoutProtocol)
import Nostr.Types exposing (PubKey, RelayRole(..))
import Set
import Time


type alias RelayMetadata =
    { url : String
    , role : RelayRole
    }


type alias IngestResult =
    { relayMetadataLists : Dict PubKey (List RelayMetadata)
    , relays : Dict String Relay
    , unknownRelays : List String
    }


{-| Decode events and merge into relay metadata lists, stubbing unknown relays.
-}
ingest :
    Dict PubKey (List RelayMetadata)
    -> Dict String Relay
    -> List Event
    -> IngestResult
ingest relayMetadataLists relays events =
    let
        relayLists =
            events
                |> List.map relayMetadataListFromEvent

        relayListDict =
            relayLists
                |> List.foldl
                    (\( pubKey, relayList ) dict ->
                        Dict.insert pubKey (withUniqueEntries relayList) dict
                    )
                    relayMetadataLists

        unknownRelays =
            relayLists
                |> List.concatMap (\( _, relayMetadataList ) -> relayMetadataList)
                |> List.map (\{ url } -> hostWithoutProtocol url)
                |> List.filter (\relay -> not (Dict.member relay relays))
                |> Set.fromList
                |> Set.toList

        updatedRelays =
            stubUnknownRelays unknownRelays relays
    in
    { relayMetadataLists = relayListDict
    , relays = updatedRelays
    , unknownRelays = unknownRelays
    }


withUniqueEntries : List RelayMetadata -> List RelayMetadata
withUniqueEntries relayList =
    relayList
        |> List.map (\relayMetadata -> ( relayMetadata.url, relayMetadata.role ))
        |> Dict.fromList
        |> Dict.toList
        |> List.map (\( url, role ) -> { url = hostWithoutProtocol url, role = role })


stubUnknownRelays : List String -> Dict String Relay -> Dict String Relay
stubUnknownRelays unknownRelays relays =
    unknownRelays
        |> List.foldl
            (\unknownRelay acc ->
                Dict.insert unknownRelay { nip11 = Nothing, state = RelayStateUnknown, urlWithoutProtocol = unknownRelay } acc
            )
            relays



-- a relay can be either read, write or read/write
-- this function consolidates a list and additional relays


extendRelayList : List RelayMetadata -> List RelayMetadata -> List RelayMetadata
extendRelayList additionalRelays relayList =
    additionalRelays
        |> List.foldl
            (\additionalRelay acc ->
                extendEntryInList acc additionalRelay
            )
            relayList


extendEntryInList : List RelayMetadata -> RelayMetadata -> List RelayMetadata
extendEntryInList relayList additionalRelay =
    let
        ( extendedList, wasExtended ) =
            relayList
                |> List.foldl
                    (\listRelay ( listAcc, extendedAcc ) ->
                        if listRelay.url == additionalRelay.url then
                            ( { listRelay | role = combinedRole listRelay.role additionalRelay.role } :: listAcc, True )

                        else
                            ( listRelay :: listAcc, extendedAcc )
                    )
                    ( [], False )
    in
    if wasExtended then
        extendedList

    else
        additionalRelay :: relayList


removeFromRelayList : RelayMetadata -> List RelayMetadata -> List RelayMetadata
removeFromRelayList relayToRemove relayList =
    relayList
        |> List.foldl
            (\listRelay listAcc ->
                if listRelay.url == relayToRemove.url then
                    case ( listRelay.role, relayToRemove.role ) of
                        ( ReadRelay, ReadWriteRelay ) ->
                            listAcc

                        ( WriteRelay, ReadWriteRelay ) ->
                            listAcc

                        ( ReadWriteRelay, ReadWriteRelay ) ->
                            listAcc

                        ( ReadRelay, ReadRelay ) ->
                            listAcc

                        ( WriteRelay, WriteRelay ) ->
                            listAcc

                        ( ReadRelay, WriteRelay ) ->
                            listRelay :: listAcc

                        ( WriteRelay, ReadRelay ) ->
                            listRelay :: listAcc

                        ( ReadWriteRelay, ReadRelay ) ->
                            { listRelay | role = WriteRelay } :: listAcc

                        ( ReadWriteRelay, WriteRelay ) ->
                            { listRelay | role = ReadRelay } :: listAcc

                else
                    listRelay :: listAcc
            )
            []


combinedRole : RelayRole -> RelayRole -> RelayRole
combinedRole role1 role2 =
    case ( role1, role2 ) of
        ( ReadRelay, ReadRelay ) ->
            ReadRelay

        ( ReadRelay, WriteRelay ) ->
            ReadWriteRelay

        ( WriteRelay, ReadRelay ) ->
            ReadWriteRelay

        ( WriteRelay, WriteRelay ) ->
            WriteRelay

        ( ReadWriteRelay, _ ) ->
            ReadWriteRelay

        ( _, ReadWriteRelay ) ->
            ReadWriteRelay


addUrlTags : List RelayMetadata -> List Tag -> List Tag
addUrlTags relays tags =
    let
        relayTags =
            relays
                |> List.map
                    (\relay ->
                        UrlTag relay.url relay.role
                    )
    in
    tags ++ relayTags


eventWithRelayList : PubKey -> List RelayMetadata -> Event
eventWithRelayList pubKey relays =
    { pubKey = pubKey
    , createdAt = Time.millisToPosix 0 -- will be set when signing
    , kind = KindRelayListMetadata
    , tags =
        []
            |> addUrlTags relays
    , content = ""
    , id = ""
    , sig = Nothing
    , relays = Nothing
    }


relayMetadataListFromEvent : Event -> ( PubKey, List RelayMetadata )
relayMetadataListFromEvent event =
    let
        relayList =
            event.tags
                |> List.filterMap
                    (\tag ->
                        case tag of
                            UrlTag url role ->
                                Just { url = url, role = role }

                            _ ->
                                Nothing
                    )
    in
    ( event.pubKey, relayList )
