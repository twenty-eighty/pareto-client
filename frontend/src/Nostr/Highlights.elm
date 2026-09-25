module Nostr.Highlights exposing
    ( Highlight
    , HighlightItem
    , byAuthor
    , countForAddress
    , forAddress
    , forAuthorArticles
    , fromEvent
    , highlightEvent
    , ingest
    )

{-| NIP-84 kind 9802 highlights: parse, store, and build publish events.

Queries omit a highlight when its author has published a NIP-09 deletion request for it.

-}

import Dict exposing (Dict)
import Nostr.Article exposing (Article, addressComponentsForArticle)
import Nostr.Event exposing (AddressComponents, Event, Kind(..), Tag(..), addAddressTags, addEventIdTag, addKindTag, buildAddress, emptyEvent)
import Nostr.Types exposing (Address, EventId, PubKey)
import Set exposing (Set)
import Time exposing (Posix)


type alias Highlight =
    { id : EventId
    , pubKey : PubKey
    , createdAt : Posix
    , content : String
    , context : Maybe String
    , addressComponents : Maybe AddressComponents
    , eventIdReferenced : Maybe EventId
    , authorPubKey : Maybe PubKey
    }


type alias HighlightItem =
    { highlight : Highlight
    , article : Article
    }


type alias Store a =
    { a
        | highlightsByAddress : Dict Address (Dict EventId Highlight)
    }


type alias Visible a =
    { a
        | highlightsByAddress : Dict Address (Dict EventId Highlight)
        , deletedEvents : Dict EventId (Set PubKey)
    }


fromEvent : Event -> Highlight
fromEvent event =
    event.tags
        |> List.foldl
            (\tag acc ->
                case tag of
                    AddressTag addressComponents _ _ ->
                        { acc | addressComponents = Just addressComponents }

                    EventIdTag eventId _ _ _ ->
                        { acc | eventIdReferenced = Just eventId }

                    PublicKeyTag pubKey _ _ ->
                        { acc
                            | authorPubKey =
                                case acc.authorPubKey of
                                    Just _ ->
                                        acc.authorPubKey

                                    Nothing ->
                                        Just pubKey
                        }

                    GenericTag ("context" :: contextText :: _) ->
                        { acc | context = Just contextText }

                    _ ->
                        acc
            )
            (emptyHighlight event)


emptyHighlight : Event -> Highlight
emptyHighlight event =
    { id = event.id
    , pubKey = event.pubKey
    , createdAt = event.createdAt
    , content = event.content
    , context = Nothing
    , addressComponents = Nothing
    , eventIdReferenced = Nothing
    , authorPubKey = Nothing
    }


{-| Build a NIP-84 highlight for an article selection.
-}
highlightEvent :
    PubKey
    ->
        { content : String
        , context : Maybe String
        , articleEventId : EventId
        , articleAuthor : PubKey
        , addressComponents : AddressComponents
        , articleKind : Kind
        }
    -> Event
highlightEvent userPubKey params =
    let
        event =
            emptyEvent userPubKey KindHighlights

        tags =
            [ GenericTag [ "p", params.articleAuthor, "", "author" ]
            ]
                |> addEventIdTag params.articleEventId Nothing Nothing Nothing
                |> addAddressTags [ params.addressComponents ] Nothing
                |> addKindTag params.articleKind
                |> (\existing ->
                        case params.context of
                            Just contextText ->
                                if String.isEmpty (String.trim contextText) then
                                    existing

                                else
                                    GenericTag [ "context", contextText ] :: existing

                            Nothing ->
                                existing
                   )
    in
    { event
        | content = params.content
        , tags = tags
    }


ingest :
    { highlightsByAddress : Dict Address (Dict EventId Highlight) }
    -> List Event
    -> { highlightsByAddress : Dict Address (Dict EventId Highlight) }
ingest store events =
    { highlightsByAddress =
        events
            |> List.map fromEvent
            |> List.foldl insertByAddress store.highlightsByAddress
    }


forAddress : Visible a -> AddressComponents -> List Highlight
forAddress store addressComponents =
    Dict.get (buildAddress addressComponents) store.highlightsByAddress
        |> Maybe.map Dict.values
        |> Maybe.withDefault []
        |> List.filter (isNotDeleted store)
        |> List.sortBy (\h -> Time.posixToMillis h.createdAt)
        |> List.reverse


countForAddress : Visible a -> AddressComponents -> Maybe Int
countForAddress store addressComponents =
    Dict.get (buildAddress addressComponents) store.highlightsByAddress
        |> Maybe.map
            (\byId ->
                byId
                    |> Dict.values
                    |> List.filter (isNotDeleted store)
                    |> List.length
            )


byAuthor : Visible a -> PubKey -> List Highlight
byAuthor store pubKey =
    store.highlightsByAddress
        |> Dict.values
        |> List.concatMap Dict.values
        |> List.filter (\highlight -> highlight.pubKey == pubKey)
        |> List.filter (isNotDeleted store)
        |> List.sortBy (\highlight -> Time.posixToMillis highlight.createdAt)
        |> List.reverse


forAuthorArticles : Visible a -> PubKey -> List Article -> List HighlightItem
forAuthorArticles store authorPubKey articles =
    articles
        |> List.concatMap (itemsForArticle store authorPubKey)
        |> List.sortBy (\item -> Time.posixToMillis item.highlight.createdAt)
        |> List.reverse


itemsForArticle : Visible a -> PubKey -> Article -> List HighlightItem
itemsForArticle store authorPubKey article =
    case addressComponentsForArticle article of
        Nothing ->
            []

        Just addressComponents ->
            forAddress store addressComponents
                |> List.filter (\highlight -> highlight.pubKey /= authorPubKey)
                |> List.map (\highlight -> { highlight = highlight, article = article })


isNotDeleted : Visible a -> Highlight -> Bool
isNotDeleted store highlight =
    Dict.get highlight.id store.deletedEvents
        |> Maybe.map (Set.member highlight.pubKey)
        |> Maybe.withDefault False
        |> not


insertByAddress : Highlight -> Dict Address (Dict EventId Highlight) -> Dict Address (Dict EventId Highlight)
insertByAddress highlight acc =
    case highlight.addressComponents of
        Just addressComponents ->
            let
                address =
                    buildAddress addressComponents
            in
            case Dict.get address acc of
                Just byId ->
                    Dict.insert address (Dict.insert highlight.id highlight byId) acc

                Nothing ->
                    Dict.insert address (Dict.singleton highlight.id highlight) acc

        Nothing ->
            acc
