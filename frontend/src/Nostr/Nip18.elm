module Nostr.Nip18 exposing (..)

import Nostr.Article exposing (Article, addressComponentsForArticle)
import Nostr.Event exposing (AddressComponents, Event, Kind(..), Tag(..), addAddressTags, emptyEvent)
import Nostr.Types exposing (EventId, PubKey)
import Set


type alias Repost =
    { pubKey : PubKey
    , repostedAddress : Maybe AddressComponents
    , repostedEvent : Maybe EventId
    , repostedPubKey : Maybe PubKey
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
                    AddressTag address ->
                        { res | repostedAddress = Just address }

                    EventIdTag eventId _ ->
                        { res | repostedEvent = Just eventId }

                    PublicKeyTag repostedPubKey _ _ ->
                        { res | repostedPubKey = Just repostedPubKey }

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
                |> addAddressTags (addressComponentsForArticle article |> Maybe.map List.singleton |> Maybe.withDefault [])
    }
