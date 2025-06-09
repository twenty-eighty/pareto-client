module Nostr.DeletionRequest exposing (..)

import Nostr.Event exposing (Event, Kind(..), Tag(..), addAddressTag, addKindTag, buildAddress)
import Nostr.Profile exposing (ProfileValidation(..))
import Nostr.Types exposing (EventId, PubKey)
import Set exposing (Set)
import Time
import Nostr.Event exposing (addEventIdTag)
import Nostr.Event exposing (AddressComponents)



-- NIP-09


type alias DeletionRequest =
    { pubKey : PubKey
    , eventIds : Set EventId
    , kinds : Set Int
    , addresses : Set String
    , reason : String
    }


deletionRequestFromEvent : Event -> DeletionRequest
deletionRequestFromEvent event =
    event.tags
        |> List.foldl
            (\tag acc ->
                case tag of
                    AddressTag ((_, addressPubKey, _) as addressComponents) _ _ ->
                        -- only accept deletion requests a pubkey made for own articles/addresses
                        if addressPubKey == event.pubKey then
                            { acc | addresses = Set.insert (buildAddress addressComponents) acc.addresses }
                        else
                            acc

                    EventIdTag eventId _ _ _ ->
                        -- we don't know at this point if the creator of the deletion event also created the event to be deleted
                        { acc | eventIds = Set.insert eventId acc.eventIds }

                    KindTag kind ->
                        { acc | kinds = Set.insert (Nostr.Event.numberForKind kind) acc.kinds }

                    _ ->
                        acc
            )
            (emptyDeletionRequest event.pubKey)

emptyDeletionRequest : PubKey -> DeletionRequest
emptyDeletionRequest pubKey =
    { pubKey = pubKey
    , eventIds = Set.empty
    , addresses = Set.empty
    , reason = ""
    , kinds = Set.empty
    }


draftDeletionEvent : PubKey -> Time.Posix -> EventId -> String -> Maybe AddressComponents -> Event
draftDeletionEvent pubKey createdAt eventId content maybeAddressComponents =
    let
        addAddress =
            case maybeAddressComponents of
                Just ((_, articlePubKey, _) as addressComponents) ->
                    if articlePubKey == pubKey then
                        addAddressTag addressComponents Nothing
                    else
                        identity

                Nothing ->
                    identity
    in
    { pubKey = pubKey
    , createdAt = createdAt
    , kind = KindEventDeletionRequest
    , tags =
        []
            |> addAddress
            |> addEventIdTag eventId Nothing Nothing Nothing
            |> addKindTag KindDraftLongFormContent
            |> addKindTag KindDraft
    , content = content
    , id = ""
    , sig = Nothing
    , relays = Nothing
    }
