module Nostr.DeletionRequests exposing (ingest)

{-| Deletion-request store updates.
-}

import Dict exposing (Dict)
import Nostr.DeletionRequest exposing (deletionRequestFromEvent)
import Nostr.Event exposing (Event)
import Nostr.Types exposing (Address, EventId, PubKey)
import Set exposing (Set)


ingest :
    { deletedAddresses : Set Address
    , deletedEvents : Dict EventId (Set PubKey)
    }
    -> List Event
    ->
        { deletedAddresses : Set Address
        , deletedEvents : Dict EventId (Set PubKey)
        }
ingest store events =
    let
        ( deletedAddresses, deletedEvents ) =
            events
                |> List.map deletionRequestFromEvent
                |> List.foldl
                    (\deletionRequest ( accAddresses, accEvents ) ->
                        ( Set.union accAddresses deletionRequest.addresses
                        , updateDeletedEvents accEvents deletionRequest.eventIds deletionRequest.pubKey
                        )
                    )
                    ( store.deletedAddresses, store.deletedEvents )
    in
    { deletedAddresses = deletedAddresses
    , deletedEvents = deletedEvents
    }


updateDeletedEvents : Dict EventId (Set PubKey) -> Set EventId -> PubKey -> Dict EventId (Set PubKey)
updateDeletedEvents dict eventIds pubKey =
    eventIds
        |> Set.foldl
            (\eventId acc ->
                Dict.update eventId
                    (\setToUpdate ->
                        setToUpdate
                            |> Maybe.map (Set.insert pubKey)
                            |> Maybe.withDefault (Set.singleton pubKey)
                            |> Just
                    )
                    acc
            )
            dict
