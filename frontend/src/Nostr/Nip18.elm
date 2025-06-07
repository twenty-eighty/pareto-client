module Nostr.Nip18 exposing (..)

import Nostr.Article exposing (Article, addressComponentsForArticle)
import Nostr.Event exposing (AddressComponents, Event, Kind(..), Tag(..), addAddressTags, emptyEvent)
import Nostr.Types exposing (EventId, PubKey, RelayUrl)
import Set


type alias Repost =
    { pubKey : PubKey
    , repostedAddress : Maybe ( AddressComponents, Maybe RelayUrl )
    , repostedEvent : Maybe ( EventId, Maybe RelayUrl )
    , repostedPubKey : Maybe ( PubKey, Maybe RelayUrl )
    }


emptyRepost : PubKey -> Repost
emptyRepost pubKey =
    { pubKey = pubKey
    , repostedAddress = Nothing
    , repostedEvent = Nothing
    , repostedPubKey = Nothing
    }


repostFromEvent : Event -> Repost
repostFromEvent event =
    event.tags
        |> List.foldl
            (\tag res ->
                case tag of
                    AddressTag address maybeRelayUrl _ ->
                        { res | repostedAddress = Just ( address, maybeRelayUrl ) }

                    EventIdTag eventId maybeRelayUrl _ _ ->
                        { res | repostedEvent = Just ( eventId, maybeRelayUrl ) }

                    PublicKeyTag repostedPubKey maybeRelayUrl _ ->
                        { res | repostedPubKey = Just ( repostedPubKey, maybeRelayUrl ) }

                    _ ->
                        res
            )
            (emptyRepost event.pubKey)


articleRepostEvent : PubKey -> Article -> Event
articleRepostEvent pubKey article =
    let
        firstRelay =
            article.relays
                |> Set.toList
                |> List.head
    in
    repostEvent pubKey article.id article.author article.kind (addressComponentsForArticle article) firstRelay


repostEvent : PubKey -> EventId -> PubKey -> Kind -> Maybe AddressComponents -> Maybe RelayUrl -> Event
repostEvent pubKey eventId author kind maybeAddressComponents maybeRelayUrl =
    let
        event =
            emptyEvent pubKey KindGenericRepost
    in
    { event
        | tags =
            [ EventIdTag eventId maybeRelayUrl Nothing Nothing
            , PublicKeyTag author Nothing Nothing
            , KindTag kind
            ]
                |> addAddressTags (maybeAddressComponents |> Maybe.map List.singleton |> Maybe.withDefault []) maybeRelayUrl
    }
