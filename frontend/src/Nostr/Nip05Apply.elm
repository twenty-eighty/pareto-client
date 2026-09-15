module Nostr.Nip05Apply exposing
    ( profileUses
    , relaysForResolvedPubKey
    , maybeRelaysForResolvedPubKey
    , unknownRelayHosts
    , validationAfterFetch
    , applyValidationStatus
    , insertPubKey
    , mergeAuthorPubKeys
    )

{-| Pure NIP-05 apply/validate helpers against store fields.
Orchestration (HTTP, follow-up requests, Cmds) stays in `Nostr`.
-}

import Dict exposing (Dict)
import Nostr.Nip05 as Nip05 exposing (Nip05, Nip05String)
import Nostr.Nip05Cache as Nip05Cache
import Nostr.Profile exposing (Profile, ProfileValidation(..))
import Nostr.Relay as Relay exposing (Relay, RelayUrl)
import Nostr.Types exposing (PubKey)


profileUses : Dict PubKey Profile -> PubKey -> Nip05 -> Bool
profileUses profiles pubKey nip05 =
    profiles
        |> Dict.get pubKey
        |> Maybe.andThen .nip05
        |> Maybe.map (Nip05Cache.equal nip05)
        |> Maybe.withDefault False


relaysForResolvedPubKey : Nip05.Nip05Data -> Maybe PubKey -> List RelayUrl
relaysForResolvedPubKey nip05Data maybePubKey =
    maybeRelaysForResolvedPubKey nip05Data maybePubKey
        |> Maybe.withDefault []


maybeRelaysForResolvedPubKey : Nip05.Nip05Data -> Maybe PubKey -> Maybe (List RelayUrl)
maybeRelaysForResolvedPubKey nip05Data maybePubKey =
    Maybe.map2
        (\pubKey relaysDict ->
            Dict.get pubKey relaysDict
        )
        maybePubKey
        nip05Data.relays
        |> Maybe.andThen identity


unknownRelayHosts : Dict String Relay -> List RelayUrl -> List RelayUrl
unknownRelayHosts relays relayUrls =
    relayUrls
        |> List.filter (\relayUrl -> not <| Dict.member (Relay.toKey relayUrl) relays)


{-| Decide which pubkey to update and with which validation status after a NIP-05 fetch.
-}
validationAfterFetch :
    Dict PubKey Profile
    -> Nip05
    -> Nip05.Nip05Data
    -> Maybe ( PubKey, ProfileValidation )
validationAfterFetch profiles nip05 nip05Data =
    let
        pubKeyInNip05Data =
            Nip05Cache.pubKeyFromNames nip05 nip05Data

        loadedProfile =
            pubKeyInNip05Data
                |> Maybe.andThen (\pubKey -> Dict.get pubKey profiles)
    in
    case ( pubKeyInNip05Data, loadedProfile ) of
        ( Just pubKey, Just profile ) ->
            if pubKey == profile.pubKey then
                Just ( profile.pubKey, ValidationSucceeded )

            else
                Just ( profile.pubKey, ValidationNotMatchingPubKey )

        ( Just pubKey, Nothing ) ->
            Just ( pubKey, ValidationPending )

        ( Nothing, _ ) ->
            Nothing


applyValidationStatus :
    { a
        | profiles : Dict PubKey Profile
        , profileValidations : Dict PubKey ProfileValidation
        , pubKeyByNip05 : Dict Nip05String PubKey
    }
    -> PubKey
    -> ProfileValidation
    ->
        { a
            | profiles : Dict PubKey Profile
            , profileValidations : Dict PubKey ProfileValidation
            , pubKeyByNip05 : Dict Nip05String PubKey
        }
applyValidationStatus model pubKey valid =
    let
        maybeProfile =
            Dict.get pubKey model.profiles

        updatedNip05Dict =
            maybeProfile
                |> Maybe.andThen .nip05
                |> Maybe.map
                    (\nip05 ->
                        Dict.insert (Nip05Cache.lookupKey nip05) pubKey model.pubKeyByNip05
                    )
                |> Maybe.withDefault model.pubKeyByNip05
    in
    { model
        | profileValidations = Dict.insert pubKey valid model.profileValidations
        , pubKeyByNip05 = updatedNip05Dict
    }


insertPubKey :
    { a | pubKeyByNip05 : Dict Nip05String PubKey }
    -> Nip05
    -> PubKey
    -> { a | pubKeyByNip05 : Dict Nip05String PubKey }
insertPubKey model nip05 pubKey =
    { model | pubKeyByNip05 = Dict.insert (Nip05Cache.lookupKey nip05) pubKey model.pubKeyByNip05 }


mergeAuthorPubKeys :
    Dict Nip05String PubKey
    -> List { a | nip05 : Nip05, pubKey : PubKey }
    -> Dict Nip05String PubKey
mergeAuthorPubKeys existing authors =
    authors
        |> List.map (\author -> ( Nip05Cache.lookupKey author.nip05, author.pubKey ))
        |> Dict.fromList
        |> (\incoming -> Dict.union incoming existing)
