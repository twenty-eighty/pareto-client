module Nostr.Nip10 exposing (..)

import Nostr.Event exposing (AddressComponents, Event, EventTagMarker(..), Tag(..), TagReference(..))
import Nostr.Types exposing (EventId, PubKey, RelayUrl)
import Time exposing (Posix)



type alias TextNote =
    { pubKey : PubKey
    , eventId : EventId
    , createdAt : Posix
    , subject : Maybe String
    , citedEvents : List CitedEventRef
    , rootAddressComponents : Maybe AddressComponents
    , rootEventId : Maybe EventId
    , rootPubKey : Maybe PubKey
    , rootRelay : Maybe RelayUrl
    , parentAddressComponents : Maybe AddressComponents
    , parentEventId : Maybe EventId
    , parentPubKey : Maybe PubKey
    , parentRelay : Maybe RelayUrl
    , eventIdReferences : List (EventId, Maybe RelayUrl)
    , pubKeyReferences : List PubKey
    , content : String
    }



type alias CitedEvent =
    { eventId : EventId
    , createdAt : Posix
    , citedEvents : List EventId
    , relayUrl : Maybe RelayUrl
    , pubKey : Maybe PubKey
    }

type CitedEventRef
    = CitedEventReference EventId (Maybe RelayUrl) (Maybe PubKey)
    | CitedAddressReference AddressComponents (Maybe RelayUrl) (Maybe PubKey)

emptyTextNote : PubKey -> EventId -> TextNote
emptyTextNote pubKey eventId =
    { pubKey = pubKey
    , eventId = eventId
    , createdAt = Time.millisToPosix 0
    , subject = Nothing
    , citedEvents = []
    , rootAddressComponents = Nothing
    , rootEventId = Nothing
    , rootPubKey = Nothing
    , rootRelay = Nothing
    , parentAddressComponents = Nothing
    , parentEventId = Nothing
    , parentPubKey = Nothing
    , parentRelay = Nothing
    , eventIdReferences = []
    , pubKeyReferences = []
    , content = ""
    }

textNoteFromEvent : Event -> TextNote
textNoteFromEvent event =
    let
        initialTextNote =
            emptyTextNote event.pubKey event.id
    in
    event.tags
        |> List.foldl
            (\tag acc ->
                case tag of
                    AddressTag address maybeRelayUrl maybeMarker ->
                        case maybeMarker of
                            Just EventTagRootMarker ->
                                { acc | rootAddressComponents = Just address, rootRelay = maybeRelayUrl }

                            Just EventTagReplyMarker ->
                                { acc | parentAddressComponents = Just address, parentRelay = maybeRelayUrl }

                            _ ->
                                acc

                    EventIdTag eventId maybeRelayUrl maybeMarker maybePubKey ->
                        case maybeMarker of
                            Just EventTagRootMarker ->
                                { acc | rootEventId = Just eventId, rootRelay = maybeRelayUrl, rootPubKey = maybePubKey }

                            Just EventTagReplyMarker ->
                                { acc | parentEventId = Just eventId, parentRelay = maybeRelayUrl, parentPubKey = maybePubKey }

                            _ ->
                                { acc | eventIdReferences = (eventId, maybeRelayUrl) :: acc.eventIdReferences }

                    QuotedEventTag eventIdOrAddress maybeRelayUrl maybePubKey ->
                        case Nostr.Event.parseAddress eventIdOrAddress of
                            Just address ->
                                { acc | citedEvents = CitedAddressReference address maybeRelayUrl maybePubKey :: acc.citedEvents }

                            Nothing ->
                                { acc | citedEvents = CitedEventReference eventIdOrAddress maybeRelayUrl maybePubKey :: acc.citedEvents } 

                    SubjectTag subject ->
                        { acc | subject = Just subject }

                    _ ->
                        acc
            )
            { initialTextNote | content = event.content, createdAt = event.createdAt }


tagReference : TextNote -> TagReference
tagReference textNote =
    TagReferenceEventId textNote.eventId