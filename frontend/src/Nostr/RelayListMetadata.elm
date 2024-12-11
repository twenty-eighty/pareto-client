module Nostr.RelayListMetadata exposing (..)

import Dict exposing (Dict)
import Nostr.Event exposing (Event, Kind(..), Tag(..), TagReference, parseAddress)
import Nostr.Types exposing (PubKey, RelayRole(..), relayRoleFromString)
import Time


type alias RelayMetadata =
    { url : String
    , role : RelayRole
    }


-- a relay can be either read, write or read/write
-- this function consolidates a list and additional relays
extendRelayList : List RelayMetadata -> List RelayMetadata -> List RelayMetadata
extendRelayList additionalRelays relayList =
    additionalRelays
    |> List.foldl (\additionalRelay acc ->
            extendEntryInList acc additionalRelay
        ) relayList

extendEntryInList : List RelayMetadata -> RelayMetadata -> List RelayMetadata
extendEntryInList relayList additionalRelay =
    let
        (extendedList, wasExtended) =
            relayList
            |> List.foldl (\listRelay (listAcc, extendedAcc) ->
                if listRelay.url == additionalRelay.url then
                    ({ listRelay | role = combinedRole listRelay.role additionalRelay.role } :: listAcc, True)
                else
                    (listRelay :: listAcc, extendedAcc)
                ) ([], False)
    in
    if wasExtended then
        extendedList
    else
        additionalRelay :: relayList

combinedRole : RelayRole -> RelayRole -> RelayRole
combinedRole role1 role2 =
    case (role1, role2) of
        (ReadRelay, ReadRelay) ->
            ReadRelay

        (ReadRelay, WriteRelay) ->
            ReadWriteRelay

        (WriteRelay, ReadRelay) ->
            ReadWriteRelay

        (WriteRelay, WriteRelay) ->
            WriteRelay

        (ReadWriteRelay, _) ->
            ReadWriteRelay

        (_, ReadWriteRelay) ->
            ReadWriteRelay

addUrlTags : List RelayMetadata -> List Tag -> List Tag
addUrlTags relays tags =
    let
        relayTags =
            relays
            |> List.map (\relay ->
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
        [ ]
        |> addUrlTags relays
    , content = ""
    , id = ""
    , sig = Nothing
    , relay = Nothing
    }


relayListFromEvent : Event -> (PubKey, List RelayMetadata)
relayListFromEvent event =
    let
        relayList =
            event.tags
            |> List.filterMap (\tag ->
                case tag of 
                    UrlTag url role ->
                        Just { url = url, role = role }

                    _ ->
                        Nothing
                    )
    in
    (event.pubKey, relayList)

