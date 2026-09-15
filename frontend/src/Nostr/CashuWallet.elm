module Nostr.CashuWallet exposing
    ( CashuProof
    , CashuTokenEvent
    , CashuWallet
    , HistoryDirection(..)
    , NutzapMintRecommendation
    , defaultMintUrl
    , historyEvent
    , ingestTokens
    , mintRecommendationEvent
    , mintRecommendationFromEvent
    , redeemedNutzapIdsFromEvents
    , tokenEvent
    , tokenEventFromDecrypted
    , totalBalance
    , walletEvent
    , walletFromDecryptedEvent
    )

{-| NIP-60 Cashu wallet + token events and NIP-61 mint recommendation (kind 10019).
-}

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as DecodePipeline
import Json.Encode as Encode
import Nostr.Event exposing (Event, EventTagMarker(..), Kind(..), Tag(..), emptyEvent)
import Nostr.Relay as Relay exposing (RelayUrl)
import Nostr.Types exposing (EventId, PubKey)
import Set exposing (Set)
import Time


defaultMintUrl : String
defaultMintUrl =
    "https://stablenut.umint.cash"


type alias CashuWallet =
    { pubKey : PubKey
    , privkey : String
    , mints : List String
    , eventId : Maybe EventId
    }


type alias CashuProof =
    { id : String
    , amount : Int
    , secret : String
    , c : String
    }


type alias CashuTokenEvent =
    { id : EventId
    , pubKey : PubKey
    , mint : String
    , unit : String
    , proofs : List CashuProof
    , deleted : List EventId
    , createdAt : Time.Posix
    }


type alias NutzapMintRecommendation =
    { pubKey : PubKey
    , relays : List String
    , mints : List String
    , p2pkPubkey : Maybe String
    }


type HistoryDirection
    = HistoryIn
    | HistoryOut


walletFromDecryptedEvent : Event -> Maybe CashuWallet
walletFromDecryptedEvent event =
    case Decode.decodeString walletTagsDecoder event.content of
        Ok { privkey, mints } ->
            Just
                { pubKey = event.pubKey
                , privkey = privkey
                , mints = mints
                , eventId = Just event.id
                }

        Err _ ->
            Nothing


type alias WalletTags =
    { privkey : String
    , mints : List String
    }


walletTagsDecoder : Decoder WalletTags
walletTagsDecoder =
    Decode.list (Decode.list Decode.string)
        |> Decode.andThen
            (\rows ->
                let
                    privkey =
                        rows
                            |> List.filterMap
                                (\row ->
                                    case row of
                                        [ "privkey", value ] ->
                                            Just value

                                        _ ->
                                            Nothing
                                )
                            |> List.head

                    mints =
                        rows
                            |> List.filterMap
                                (\row ->
                                    case row of
                                        "mint" :: url :: _ ->
                                            Just url

                                        _ ->
                                            Nothing
                                )
                in
                case privkey of
                    Just pk ->
                        Decode.succeed { privkey = pk, mints = mints }

                    Nothing ->
                        Decode.fail "missing privkey"
            )


{-| Build unsigned kind 17375. Content is plaintext JSON tags; interop encrypts before publish.
-}
walletEvent : PubKey -> String -> List String -> Event
walletEvent ownerPubKey privkey mints =
    let
        event =
            emptyEvent ownerPubKey KindCashuWalletEvent

        tagsJson =
            Encode.encode 0
                (Encode.list (Encode.list Encode.string)
                    (( "privkey", privkey )
                        :: List.map (\mint -> ( "mint", mint )) mints
                        |> List.map (\( k, v ) -> [ k, v ])
                    )
                )
    in
    { event | content = tagsJson, tags = [] }


tokenEventFromDecrypted : Event -> Maybe CashuTokenEvent
tokenEventFromDecrypted event =
    case Decode.decodeString tokenPayloadDecoder event.content of
        Ok payload ->
            Just
                { id = event.id
                , pubKey = event.pubKey
                , mint = payload.mint
                , unit = payload.unit
                , proofs = payload.proofs
                , deleted = payload.deleted
                , createdAt = event.createdAt
                }

        Err _ ->
            Nothing


type alias TokenPayload =
    { mint : String
    , unit : String
    , proofs : List CashuProof
    , deleted : List EventId
    }


tokenPayloadDecoder : Decoder TokenPayload
tokenPayloadDecoder =
    Decode.succeed TokenPayload
        |> DecodePipeline.required "mint" Decode.string
        |> DecodePipeline.optional "unit" Decode.string "sat"
        |> DecodePipeline.required "proofs" (Decode.list proofDecoder)
        |> DecodePipeline.optional "del" (Decode.list Decode.string) []


proofDecoder : Decoder CashuProof
proofDecoder =
    Decode.succeed CashuProof
        |> DecodePipeline.required "id" Decode.string
        |> DecodePipeline.required "amount" Decode.int
        |> DecodePipeline.required "secret" Decode.string
        |> DecodePipeline.required "C" Decode.string


encodeProof : CashuProof -> Encode.Value
encodeProof proof =
    Encode.object
        [ ( "id", Encode.string proof.id )
        , ( "amount", Encode.int proof.amount )
        , ( "secret", Encode.string proof.secret )
        , ( "C", Encode.string proof.c )
        ]


{-| Build unsigned kind 7375. Content is plaintext JSON; interop encrypts before publish.
-}
tokenEvent : PubKey -> String -> List CashuProof -> List EventId -> Event
tokenEvent ownerPubKey mint proofs deleted =
    let
        event =
            emptyEvent ownerPubKey KindCashuWalletTokens

        payload =
            Encode.object
                [ ( "mint", Encode.string mint )
                , ( "unit", Encode.string "sat" )
                , ( "proofs", Encode.list encodeProof proofs )
                , ( "del", Encode.list Encode.string deleted )
                ]
    in
    { event | content = Encode.encode 0 payload, tags = [] }


mintRecommendationFromEvent : Event -> NutzapMintRecommendation
mintRecommendationFromEvent event =
    let
        relays =
            event.tags
                |> List.filterMap
                    (\tag ->
                        case tag of
                            RelayTag url ->
                                Just url

                            GenericTag ("relay" :: url :: _) ->
                                Just url

                            _ ->
                                Nothing
                    )

        mints =
            event.tags
                |> List.filterMap
                    (\tag ->
                        case tag of
                            GenericTag ("mint" :: url :: _) ->
                                Just url

                            _ ->
                                Nothing
                    )

        p2pkPubkey =
            event.tags
                |> List.filterMap
                    (\tag ->
                        case tag of
                            GenericTag ("pubkey" :: pk :: _) ->
                                Just pk

                            _ ->
                                Nothing
                    )
                |> List.head
    in
    { pubKey = event.pubKey
    , relays = relays
    , mints = mints
    , p2pkPubkey = p2pkPubkey
    }


mintRecommendationEvent : PubKey -> List RelayUrl -> List String -> Maybe String -> Event
mintRecommendationEvent ownerPubKey relays mints maybeP2pk =
    let
        event =
            emptyEvent ownerPubKey KindNutzapMintRecommendation

        relayTags =
            List.map (\url -> RelayTag (Relay.toWire url)) relays

        mintTags =
            List.map (\url -> GenericTag [ "mint", url, "sat" ]) mints

        pubkeyTags =
            case maybeP2pk of
                Just pk ->
                    [ GenericTag [ "pubkey", pk ] ]

                Nothing ->
                    []
    in
    { event | tags = relayTags ++ mintTags ++ pubkeyTags, content = "" }


{-| Build unsigned kind 7376. Content is plaintext history tags JSON; interop encrypts.
Public tags mark the redeemed nutzap.
-}
historyEvent :
    PubKey
    ->
        { direction : HistoryDirection
        , amount : Int
        , nutzapEventId : EventId
        , senderPubKey : PubKey
        , createdTokenEventId : Maybe EventId
        }
    -> Event
historyEvent ownerPubKey data =
    let
        event =
            emptyEvent ownerPubKey KindCashuWalletHistory

        directionValue =
            case data.direction of
                HistoryIn ->
                    "in"

                HistoryOut ->
                    "out"

        contentTags =
            [ [ "direction", directionValue ]
            , [ "amount", String.fromInt data.amount ]
            , [ "unit", "sat" ]
            ]
                ++ (case data.createdTokenEventId of
                        Just tokenId ->
                            [ [ "e", tokenId, "", "created" ] ]

                        Nothing ->
                            []
                   )

        publicTags =
            [ EventIdTag data.nutzapEventId Nothing (Just EventTagRedeemedMarker) Nothing
            , PublicKeyTag data.senderPubKey Nothing Nothing
            ]
    in
    { event
        | content = Encode.encode 0 (Encode.list (Encode.list Encode.string) contentTags)
        , tags = publicTags
    }


redeemedNutzapIdsFromEvents : List Event -> Set EventId
redeemedNutzapIdsFromEvents events =
    events
        |> List.concatMap
            (\event ->
                event.tags
                    |> List.filterMap
                        (\tag ->
                            case tag of
                                EventIdTag eventId _ (Just EventTagRedeemedMarker) _ ->
                                    Just eventId

                                GenericTag ("e" :: eventId :: _ :: "redeemed" :: _) ->
                                    Just eventId

                                _ ->
                                    Nothing
                        )
            )
        |> Set.fromList


totalBalance : Dict EventId CashuTokenEvent -> Int
totalBalance tokens =
    tokens
        |> Dict.values
        |> List.concatMap .proofs
        |> List.map .amount
        |> List.sum


ingestTokens : Dict EventId CashuTokenEvent -> List CashuTokenEvent -> Dict EventId CashuTokenEvent
ingestTokens existing tokens =
    let
        withNew =
            List.foldl (\token dict -> Dict.insert token.id token dict) existing tokens

        deletedIds =
            tokens
                |> List.concatMap .deleted
                |> Set.fromList
    in
    Dict.filter (\id _ -> not (Set.member id deletedIds)) withNew
