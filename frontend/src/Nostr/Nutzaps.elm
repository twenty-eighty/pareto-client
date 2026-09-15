module Nostr.Nutzaps exposing
    ( Nutzap
    , ingest
    , nutzapFromEvent
    , totalAmount
    )

{-| NIP-61 nutzap (kind 9321) parsing and address/event indexing.
-}

import Dict exposing (Dict)
import Json.Decode as Decode
import Nostr.Event exposing (Event, Tag(..), buildAddress)
import Nostr.Types exposing (EventId, PubKey)
import Time


type alias Nutzap =
    { id : EventId
    , pubKey : PubKey
    , content : String
    , createdAt : Time.Posix
    , recipient : Maybe PubKey
    , mintUrl : Maybe String
    , unit : String
    , proofs : List Decode.Value
    , address : Maybe String
    , event : Maybe EventId
    , amount : Int
    }


nutzapFromEvent : Event -> Maybe Nutzap
nutzapFromEvent event =
    let
        recipient =
            event.tags
                |> List.filterMap
                    (\tag ->
                        case tag of
                            PublicKeyTag pk _ _ ->
                                Just pk

                            _ ->
                                Nothing
                    )
                |> List.head

        mintUrl =
            event.tags
                |> List.filterMap
                    (\tag ->
                        case tag of
                            GenericTag ("u" :: url :: _) ->
                                Just url

                            UrlTag url _ ->
                                Just url

                            _ ->
                                Nothing
                    )
                |> List.head

        unit =
            event.tags
                |> List.filterMap
                    (\tag ->
                        case tag of
                            GenericTag ("unit" :: value :: _) ->
                                Just value

                            _ ->
                                Nothing
                    )
                |> List.head
                |> Maybe.withDefault "sat"

        proofValues =
            event.tags
                |> List.filterMap
                    (\tag ->
                        case tag of
                            GenericTag ("proof" :: json :: _) ->
                                case Decode.decodeString Decode.value json of
                                    Ok value ->
                                        Just value

                                    Err _ ->
                                        Nothing

                            _ ->
                                Nothing
                    )

        address =
            event.tags
                |> List.filterMap
                    (\tag ->
                        case tag of
                            AddressTag components _ _ ->
                                Just (buildAddress components)

                            _ ->
                                Nothing
                    )
                |> List.head

        referencedEvent =
            event.tags
                |> List.filterMap
                    (\tag ->
                        case tag of
                            EventIdTag eventId _ _ _ ->
                                Just eventId

                            _ ->
                                Nothing
                    )
                |> List.head

        amount =
            proofValues
                |> List.filterMap
                    (\value ->
                        Decode.decodeValue (Decode.field "amount" Decode.int) value
                            |> Result.toMaybe
                    )
                |> List.sum
    in
    if List.isEmpty proofValues then
        Nothing

    else
        Just
            { id = event.id
            , pubKey = event.pubKey
            , content = event.content
            , createdAt = event.createdAt
            , recipient = recipient
            , mintUrl = mintUrl
            , unit = unit
            , proofs = proofValues
            , address = address
            , event = referencedEvent
            , amount = amount
            }


ingest :
    { nutzapsAddress : Dict String (Dict String Nutzap)
    , nutzapsEvents : Dict String (Dict String Nutzap)
    }
    -> List Nutzap
    ->
        { nutzapsAddress : Dict String (Dict String Nutzap)
        , nutzapsEvents : Dict String (Dict String Nutzap)
        }
ingest store nutzaps =
    let
        forAddresses =
            nutzaps
                |> List.filterMap
                    (\nutzap ->
                        nutzap.address
                            |> Maybe.map (\address -> ( address, nutzap ))
                    )
                |> List.foldl addToDict store.nutzapsAddress

        forEvents =
            nutzaps
                |> List.filterMap
                    (\nutzap ->
                        nutzap.event
                            |> Maybe.map (\eventId -> ( eventId, nutzap ))
                    )
                |> List.foldl addToDict store.nutzapsEvents
    in
    { nutzapsAddress = forAddresses
    , nutzapsEvents = forEvents
    }


addToDict : ( String, Nutzap ) -> Dict String (Dict String Nutzap) -> Dict String (Dict String Nutzap)
addToDict ( key, nutzap ) nutzapDict =
    let
        updated =
            Dict.get key nutzapDict
                |> Maybe.map (Dict.insert nutzap.id nutzap)
                |> Maybe.withDefault (Dict.singleton nutzap.id nutzap)
    in
    Dict.insert key updated nutzapDict


totalAmount : Dict String Nutzap -> Int
totalAmount nutzaps =
    nutzaps
        |> Dict.values
        |> List.map .amount
        |> List.sum
