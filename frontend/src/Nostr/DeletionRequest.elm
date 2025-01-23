module Nostr.DeletionRequest exposing (..)

import BrowserEnv exposing (BrowserEnv)
import Dict exposing (Dict)
import Nostr.Event exposing (Event, Kind(..), Tag(..), addAddressTag, addKindTag, buildAddress)
import Nostr.Profile exposing (Profile, ProfileValidation(..))
import Nostr.Types exposing (EventId, PubKey, RelayUrl)
import Set exposing (Set)
import Time



-- NIP-09


type alias DeletionRequest =
    { eventIds : Set EventId
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
                    AddressTag addressComponents ->
                        { acc | addresses = Set.insert (buildAddress addressComponents) acc.addresses }

                    EventIdTag eventId ->
                        { acc | eventIds = Set.insert eventId acc.eventIds }

                    KindTag kind ->
                        { acc | kinds = Set.insert (Nostr.Event.numberForKind kind) acc.kinds }

                    _ ->
                        acc
            )
            { eventIds = Set.empty, addresses = Set.empty, reason = event.content, kinds = Set.empty }


draftDeletionEvent : PubKey -> Time.Posix -> EventId -> String -> Maybe String -> Event
draftDeletionEvent pubKey createdAt draftEventId content maybeIdentifier =
    let
        addIdentifer =
            case maybeIdentifier of
                Just identifier ->
                    addAddressTag ( KindDraftLongFormContent, pubKey, identifier )

                Nothing ->
                    identity
    in
    { pubKey = pubKey
    , createdAt = createdAt
    , kind = KindEventDeletionRequest
    , tags =
        []
            |> addIdentifer
            |> addKindTag KindDraftLongFormContent
            |> addKindTag KindDraft
    , content = content
    , id = ""
    , sig = Nothing
    , relay = Nothing
    }
