module Nostr.Profiles exposing
    ( ingest
    , ingestPubkeyProfiles
    , addNip05Mappings
    )

{-| Profile store updates. Orchestration (NIP-05 fetches, related kinds) stays in `Nostr`.
-}

import Dict exposing (Dict)
import Nostr.Event exposing (Event)
import Nostr.Nip05 exposing (Nip05String)
import Nostr.Nip05Cache as Nip05Cache
import Nostr.Profile exposing (Profile, PubkeyProfile, profileFromEvent)
import Nostr.Types exposing (PubKey)


ingest :
    Dict PubKey Profile
    -> List Event
    -> ( Dict PubKey Profile, List Profile )
ingest profiles events =
    let
        decoded =
            events
                |> List.filterMap profileFromEvent

        updated =
            decoded
                |> List.foldl
                    (\profile dict ->
                        Dict.insert profile.pubKey profile dict
                    )
                    profiles
    in
    ( updated, decoded )


ingestPubkeyProfiles :
    Dict PubKey Profile
    -> List PubkeyProfile
    -> ( Dict PubKey Profile, List Profile )
ingestPubkeyProfiles profiles pubkeyProfiles =
    let
        updated =
            pubkeyProfiles
                |> List.foldl
                    (\{ pubKey, profile } dict ->
                        Dict.insert pubKey profile dict
                    )
                    profiles

        decoded =
            List.map .profile pubkeyProfiles
    in
    ( updated, decoded )


addNip05Mappings : Dict Nip05String PubKey -> List Profile -> Dict Nip05String PubKey
addNip05Mappings dict profiles =
    profiles
        |> List.filterMap
            (\profile ->
                profile.nip05
                    |> Maybe.map (\nip05 -> ( Nip05Cache.lookupKey nip05, profile.pubKey ))
            )
        |> List.foldl
            (\( key, pubKey ) acc ->
                Dict.insert key pubKey acc
            )
            dict
