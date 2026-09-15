module Nostr.RelayAccess exposing
    ( pairMetadata
    , filterRead
    , filterWrite
    , withSearchCapability
    , urlsWithoutProtocol
    , resolveUrls
    , nip65ForPubKey
    , combinedForPubKey
    , searchUrls
    )

{-| Resolve relay metadata / URL lists against the live relay dict.
-}

import Dict exposing (Dict)
import Nostr.Relay exposing (Relay, RelayState(..), hostWithoutProtocol)
import Nostr.RelayListMetadata exposing (RelayMetadata, withUniqueEntries)
import Nostr.Types exposing (PubKey, RelayRole(..), RelayUrl)


pairMetadata : List RelayMetadata -> Dict String Relay -> List ( RelayRole, Relay )
pairMetadata relayList relays =
    relayList
        |> withUniqueEntries
        |> List.filterMap
            (\{ role, url } ->
                Maybe.map
                    (\relay -> ( role, relay ))
                    (Dict.get url relays)
            )


filterRead : List ( RelayRole, Relay ) -> List Relay
filterRead paired =
    paired
        |> List.filterMap
            (\( role, relay ) ->
                if role == ReadRelay || role == ReadWriteRelay then
                    Just relay

                else
                    Nothing
            )


filterWrite : List ( RelayRole, Relay ) -> List Relay
filterWrite paired =
    paired
        |> List.filterMap
            (\( role, relay ) ->
                if role == WriteRelay || role == ReadWriteRelay then
                    Just relay

                else
                    Nothing
            )


withSearchCapability : Dict String Relay -> List RelayUrl
withSearchCapability relays =
    relays
        |> Dict.values
        |> List.filterMap
            (\relay ->
                relay.nip11
                    |> Maybe.andThen
                        (\nip11 ->
                            nip11.supportedNips
                                |> Maybe.andThen
                                    (\supportedNips ->
                                        if List.member 50 supportedNips then
                                            Just relay.urlWithoutProtocol

                                        else
                                            Nothing
                                    )
                        )
            )


urlsWithoutProtocol : List Relay -> List String
urlsWithoutProtocol relays =
    List.map .urlWithoutProtocol relays


{-| Build Relay records for URL strings, filling unknown hosts with stubs.
-}
resolveUrls : Dict String Relay -> List RelayUrl -> List Relay
resolveUrls relays relayUrls =
    relayUrls
        |> List.map
            (\relayUrl ->
                let
                    host =
                        hostWithoutProtocol relayUrl
                in
                Dict.get host relays
                    |> Maybe.withDefault { urlWithoutProtocol = host, state = RelayStateUnknown, nip11 = Nothing }
            )


nip65ForPubKey : Dict PubKey (List RelayMetadata) -> Dict String Relay -> PubKey -> List ( RelayRole, Relay )
nip65ForPubKey relayMetadataLists relays pubKey =
    Dict.get pubKey relayMetadataLists
        |> Maybe.map (\relayList -> pairMetadata relayList relays)
        |> Maybe.withDefault []


{-| Merge NIP-05 hint relays, optional extra URLs (e.g. test relays), and NIP-65 metadata.
-}
combinedForPubKey :
    Dict PubKey (List RelayUrl)
    -> Dict PubKey (List RelayMetadata)
    -> Dict String Relay
    -> List RelayUrl
    -> PubKey
    -> List ( RelayRole, Relay )
combinedForPubKey relaysForPubKey relayMetadataLists relays extraUrls pubKey =
    let
        fromNip05 =
            Dict.get pubKey relaysForPubKey
                |> Maybe.withDefault []

        fromNip65 =
            Dict.get pubKey relayMetadataLists
                |> Maybe.withDefault []

        combined =
            (fromNip05 ++ extraUrls)
                |> List.map (\relayUrl -> { role = ReadWriteRelay, url = relayUrl })
                |> List.append fromNip65
    in
    pairMetadata combined relays


searchUrls : Dict String Relay -> List RelayUrl -> List RelayUrl
searchUrls relays fallback =
    case withSearchCapability relays of
        [] ->
            -- Before NIP-11 data arrives, fall back to configured search relays.
            fallback

        capabilityUrls ->
            capabilityUrls
                |> List.map (\urlWithoutProtocol -> "wss://" ++ urlWithoutProtocol)
