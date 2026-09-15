module Nostr.Nip05Cache exposing
    ( Nip05RequestTarget(..)
    , Nip05CacheEntry(..)
    , CheckDecision(..)
    , FetchDecision(..)
    , ContentRequestDecision(..)
    , lookupKey
    , equal
    , pubKeyFromNames
    , validationForPubKey
    , cacheEntryIsFresh
    , addWaiter
    , decideCheck
    , decideFetch
    , decideContentRequest
    , pendingTimeoutMillis
    , successCacheTtlMillis
    , errorCacheTtlMillis
    )

{-| NIP-05 request targeting and result cache.
Orchestration that mutates the Nostr model stays in `Nostr`.
-}

import Dict exposing (Dict)
import Http
import Nostr.ContentRequest exposing (ContentRequestState(..))
import Nostr.Nip05 as Nip05 exposing (Nip05, nip05ToString)
import Nostr.Profile exposing (ProfileValidation(..))
import Nostr.Relay exposing (RelayUrl)
import Nostr.Request exposing (RequestData(..), RequestId)
import Nostr.Types exposing (PubKey)
import Time exposing (Posix)


type Nip05RequestTarget
    = Nip05ForPubKey PubKey
    | Nip05ForRequest RequestId


type Nip05CacheEntry
    = Nip05Pending Posix (List Nip05RequestTarget)
    | Nip05Cached Posix (Result Http.Error Nip05.Nip05Data)


type CheckDecision
    = JoinPending Posix (List Nip05RequestTarget)
    | StartFetch (List Nip05RequestTarget)
    | UseCached (Result Http.Error Nip05.Nip05Data)


type FetchDecision
    = DeliverToWaiters (List Nip05RequestTarget) (Result Http.Error Nip05.Nip05Data)
    | IgnoreStale


type ContentRequestDecision
    = FailMissingPubKey
    | KeepWaitingForArticle
    | SettleReady
    | LeaveUnchanged


lookupKey : Nip05 -> String
lookupKey nip05 =
    String.toLower (nip05ToString nip05)


equal : Nip05 -> Nip05 -> Bool
equal left right =
    lookupKey left == lookupKey right


{-| Validation status and usable relays when checking a profile pubkey against NIP-05 data.
-}
validationForPubKey : PubKey -> Maybe PubKey -> List RelayUrl -> ( ProfileValidation, List RelayUrl )
validationForPubKey expectedPubKey maybePubKeyInNip05 relays =
    maybePubKeyInNip05
        |> Maybe.map
            (\pubKeyInNip05 ->
                if pubKeyInNip05 == expectedPubKey then
                    ( ValidationSucceeded, relays )

                else
                    ( ValidationNotMatchingPubKey, [] )
            )
        |> Maybe.withDefault ( ValidationNameMissing, [] )


pubKeyFromNames : Nip05 -> Nip05.Nip05Data -> Maybe PubKey
pubKeyFromNames nip05 nip05Data =
    case Dict.get nip05.user nip05Data.names of
        Just pubKey ->
            Just pubKey

        Nothing ->
            nip05Data.names
                |> Dict.toList
                |> List.filterMap
                    (\( name, pubKey ) ->
                        if String.toLower name == String.toLower nip05.user then
                            Just pubKey

                        else
                            Nothing
                    )
                |> List.head


pendingTimeoutMillis : Int
pendingTimeoutMillis =
    30 * 1000


successCacheTtlMillis : Int
successCacheTtlMillis =
    60 * 60 * 1000


errorCacheTtlMillis : Int
errorCacheTtlMillis =
    60 * 1000


cacheEntryIsFresh : Posix -> Nip05CacheEntry -> Bool
cacheEntryIsFresh now cacheEntry =
    let
        ( cachedAt, ttl ) =
            case cacheEntry of
                Nip05Pending requestedAt _ ->
                    ( requestedAt, pendingTimeoutMillis )

                Nip05Cached fetchedAt (Ok _) ->
                    ( fetchedAt, successCacheTtlMillis )

                Nip05Cached fetchedAt (Err _) ->
                    ( fetchedAt, errorCacheTtlMillis )

        age =
            Time.posixToMillis now - Time.posixToMillis cachedAt
    in
    age >= 0 && age < ttl


addWaiter : Nip05RequestTarget -> List Nip05RequestTarget -> List Nip05RequestTarget
addWaiter target waiters =
    if List.member target waiters then
        waiters

    else
        target :: waiters


decideCheck : Dict String Nip05CacheEntry -> Nip05 -> Nip05RequestTarget -> Posix -> CheckDecision
decideCheck cache nip05 target now =
    let
        cacheKey =
            nip05ToString nip05
    in
    case Dict.get cacheKey cache of
        Just ((Nip05Pending requestedAt waiters) as cacheEntry) ->
            if cacheEntryIsFresh now cacheEntry then
                JoinPending requestedAt (addWaiter target waiters)

            else
                StartFetch (addWaiter target waiters)

        Just ((Nip05Cached _ result) as cacheEntry) ->
            if cacheEntryIsFresh now cacheEntry then
                UseCached result

            else
                StartFetch [ target ]

        Nothing ->
            StartFetch [ target ]


decideFetch : Dict String Nip05CacheEntry -> Nip05 -> Posix -> Result Http.Error Nip05.Nip05Data -> FetchDecision
decideFetch cache nip05 requestedAt result =
    let
        cacheKey =
            nip05ToString nip05
    in
    case Dict.get cacheKey cache of
        Just (Nip05Pending currentRequestedAt waiters) ->
            if Time.posixToMillis currentRequestedAt == Time.posixToMillis requestedAt then
                DeliverToWaiters waiters result

            else
                IgnoreStale

        _ ->
            IgnoreStale


decideContentRequest : Maybe ContentRequestState -> Maybe PubKey -> List RequestData -> ContentRequestDecision
decideContentRequest maybeState maybePubKey followUps =
    case ( maybeState, maybePubKey, followUps ) of
        ( Just WaitingForNip05, Nothing, _ ) ->
            FailMissingPubKey

        ( Just WaitingForNip05, Just _, datas ) ->
            let
                waitingForArticleFetch =
                    List.any
                        (\requestData ->
                            case requestData of
                                RequestArticle _ _ ->
                                    True

                                _ ->
                                    False
                        )
                        datas
            in
            if waitingForArticleFetch then
                KeepWaitingForArticle

            else
                SettleReady

        _ ->
            LeaveUnchanged
