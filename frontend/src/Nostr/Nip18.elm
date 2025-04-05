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
                    AddressTag address maybeRelayUrl ->
                        { res | repostedAddress = Just ( address, maybeRelayUrl ) }

                    EventIdTag eventId maybeRelayUrl ->
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
        event =
            emptyEvent pubKey KindGenericRepost

        firstRelay =
            article.relays
                |> Set.toList
                |> List.head
    in
    { event
        | tags =
            [ EventIdTag article.id firstRelay
            , PublicKeyTag article.author Nothing Nothing
            , KindTag article.kind
            ]
                |> addAddressTags (addressComponentsForArticle article |> Maybe.map List.singleton |> Maybe.withDefault []) firstRelay
    }
