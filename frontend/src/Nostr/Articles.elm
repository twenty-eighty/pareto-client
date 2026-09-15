module Nostr.Articles exposing
    ( get
    , getWithId
    , getWithIdentifier
    , getDraftWithIdentifier
    , getDraftWithId
    , getForAddressComponents
    , getForNip19
    , publishedByDate
    , forAuthor
    , filterWithIdentifier
    , ingestPublished
    , ingestDrafts
    , sortByDate
    , isNotDeleted
    )

{-| Article lookups and store updates against the shared Nostr store.
Uses extensible records for lookups so this module does not depend on `Nostr`.
-}

import Dict exposing (Dict)
import Nostr.Article exposing (Article, addressComponentsForArticle, addressForArticle, articleFromEvent, publishedTime)
import Nostr.Event exposing (AddressComponents, Event, Kind(..), buildAddress, kindFromNumber)
import Nostr.Nip19 exposing (NIP19Type(..))
import Nostr.Types exposing (Address, EventId, PubKey, RelayUrl)
import Set exposing (Set)
import Time


type alias PublishedFields =
    { articlesByAddress : Dict Address Article
    , articlesByAuthor : Dict PubKey (List Article)
    , articlesByDate : List Article
    , articlesById : Dict EventId Article
    }


type alias DraftFields =
    { articleDraftsByDate : List Article
    , articleDraftsById : Dict EventId Article
    , articleDraftRelays : Dict EventId (Set RelayUrl)
    }


type alias PublishedIngest =
    { articlesByAddress : Dict Address Article
    , articlesByAuthor : Dict PubKey (List Article)
    , articlesByDate : List Article
    , articlesById : Dict EventId Article
    , articles : List Article
    , errors : List String
    }


type alias DraftIngest =
    { articleDraftsByDate : List Article
    , articleDraftsById : Dict EventId Article
    , articleDraftRelays : Dict EventId (Set RelayUrl)
    , articles : List Article
    , errors : List String
    }


get : { a | articlesByAddress : Dict Address Article } -> AddressComponents -> Maybe Article
get model addressComponents =
    Dict.get (buildAddress addressComponents) model.articlesByAddress


getWithId : { a | articlesById : Dict EventId Article } -> EventId -> Maybe Article
getWithId model eventId =
    Dict.get eventId model.articlesById


getWithIdentifier : { a | articlesByAuthor : Dict PubKey (List Article) } -> PubKey -> String -> Maybe Article
getWithIdentifier model pubKey identifier =
    model.articlesByAuthor
        |> Dict.get pubKey
        |> Maybe.andThen (filterWithIdentifier identifier)


filterWithIdentifier : String -> List Article -> Maybe Article
filterWithIdentifier identifier articles =
    articles
        |> List.filter (\article -> article.identifier == Just identifier)
        |> List.head


getDraftWithIdentifier : { a | articleDraftsByDate : List Article } -> PubKey -> String -> Maybe Article
getDraftWithIdentifier model pubKey identifier =
    model.articleDraftsByDate
        |> List.filter
            (\article ->
                article.author
                    == pubKey
                    && article.identifier
                    == Just identifier
            )
        |> List.head


getDraftWithId : { a | articleDraftsById : Dict EventId Article } -> EventId -> Maybe Article
getDraftWithId model id =
    Dict.get id model.articleDraftsById


getForAddressComponents :
    { a
        | articlesByAuthor : Dict PubKey (List Article)
        , articleDraftsByDate : List Article
    }
    -> AddressComponents
    -> Maybe Article
getForAddressComponents model addressComponents =
    case addressComponents of
        ( KindLongFormContent, pubKey, identifier ) ->
            getWithIdentifier model pubKey identifier

        ( KindDraftLongFormContent, pubKey, identifier ) ->
            getDraftWithIdentifier model pubKey identifier

        _ ->
            Nothing


publishedByDate :
    { a
        | articlesByDate : List Article
        , deletedEvents : Dict EventId (Set PubKey)
        , deletedAddresses : Set Address
    }
    -> List Article
publishedByDate model =
    model.articlesByDate
        |> List.filter (isNotDeleted model)


forAuthor :
    { a
        | articlesByAuthor : Dict PubKey (List Article)
        , deletedEvents : Dict EventId (Set PubKey)
        , deletedAddresses : Set Address
    }
    -> PubKey
    -> List Article
forAuthor model pubKey =
    model.articlesByAuthor
        |> Dict.get pubKey
        |> Maybe.withDefault []
        |> List.filter (isNotDeleted model)
        |> sortByDate


getForNip19 :
    { a
        | articlesByAuthor : Dict PubKey (List Article)
        , articlesById : Dict EventId Article
        , articleDraftsByDate : List Article
        , articleDraftsById : Dict EventId Article
    }
    -> NIP19Type
    -> Maybe Article
getForNip19 model nip19 =
    case nip19 of
        NAddr { identifier, kind, pubKey } ->
            case kindFromNumber kind of
                KindLongFormContent ->
                    getWithIdentifier model pubKey identifier

                KindDraftLongFormContent ->
                    getDraftWithIdentifier model pubKey identifier

                _ ->
                    Nothing

        NEvent { id, kind } ->
            case kind of
                Just kindNum ->
                    case kindFromNumber kindNum of
                        KindLongFormContent ->
                            getWithId model id

                        KindDraftLongFormContent ->
                            getDraftWithId model id

                        _ ->
                            Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


{-| Decode and merge published long-form events into the article indexes.
-}
ingestPublished : PublishedFields -> List Event -> PublishedIngest
ingestPublished store events =
    let
        ( articles, errors ) =
            decodeArticles events

        articlesByDate =
            store.articlesByDate
                ++ articles
                |> List.map
                    (\article ->
                        ( Maybe.withDefault "" (addressForArticle article), article )
                    )
                |> Dict.fromList
                |> Dict.values
                |> sortByDate

        articlesByAddress =
            articles
                |> List.foldl
                    (\article dict ->
                        case addressForArticle article of
                            Just address ->
                                Dict.insert address article dict

                            Nothing ->
                                dict
                    )
                    store.articlesByAddress

        articlesByAuthor =
            articles
                |> List.foldl
                    (\article dict ->
                        case Dict.get article.author dict of
                            Just articleList ->
                                Dict.insert article.author (appendToList articleList article) dict

                            Nothing ->
                                Dict.insert article.author [ article ] dict
                    )
                    store.articlesByAuthor

        articlesById =
            articles
                |> List.foldl
                    (\article dict ->
                        Dict.insert article.id article dict
                    )
                    store.articlesById
    in
    { articlesByAddress = articlesByAddress
    , articlesByAuthor = articlesByAuthor
    , articlesByDate = articlesByDate
    , articlesById = articlesById
    , articles = articles
    , errors = errors
    }


{-| Decode and merge draft long-form events into the draft indexes.
-}
ingestDrafts : DraftFields -> List Event -> DraftIngest
ingestDrafts store events =
    let
        ( articles, errors ) =
            decodeArticles events

        articleDraftRelays =
            articles
                |> List.foldl
                    (\article acc ->
                        case ( article.relays, Dict.get article.id acc ) of
                            ( relayUrls, Just relaySet ) ->
                                Dict.insert article.id (Set.union relayUrls relaySet) acc

                            ( relayUrls, Nothing ) ->
                                if not (Set.isEmpty relayUrls) then
                                    Dict.insert article.id relayUrls acc

                                else
                                    acc
                    )
                    store.articleDraftRelays

        articleDraftsByDate =
            store.articleDraftsByDate
                ++ articles
                |> List.map
                    (\article ->
                        ( Maybe.withDefault "" article.identifier, article )
                    )
                |> Dict.fromList
                |> Dict.values
                |> List.sortBy
                    (\article ->
                        article.publishedAt
                            |> Maybe.map (\publishedAt -> Time.posixToMillis publishedAt * -1)
                            |> Maybe.withDefault 0
                    )

        articleDraftsById =
            articles
                |> List.foldl
                    (\article dict ->
                        Dict.insert article.id article dict
                    )
                    store.articleDraftsById
    in
    { articleDraftsByDate = articleDraftsByDate
    , articleDraftsById = articleDraftsById
    , articleDraftRelays = articleDraftRelays
    , articles = articles
    , errors = errors
    }


decodeArticles : List Event -> ( List Article, List String )
decodeArticles events =
    events
        |> List.map articleFromEvent
        |> List.foldl
            (\decodingResult ( articleAcc, errors ) ->
                case decodingResult of
                    Ok article ->
                        ( article :: articleAcc, errors )

                    Err decodingErrors ->
                        ( articleAcc, decodingErrors ++ errors )
            )
            ( [], [] )


sortByDate : List Article -> List Article
sortByDate articles =
    articles
        |> List.sortBy
            (\article ->
                publishedTime article.createdAt article.publishedAt
                    |> Time.posixToMillis
                    |> (*) -1
            )


isNotDeleted :
    { a
        | deletedEvents : Dict EventId (Set PubKey)
        , deletedAddresses : Set Address
    }
    -> Article
    -> Bool
isNotDeleted model article =
    let
        articleEventIdDeleted =
            Dict.get article.id model.deletedEvents
                |> Maybe.map (Set.member article.author)
                |> Maybe.withDefault False
    in
    not (articleEventIdDeleted || Set.member (addressForArticle article |> Maybe.withDefault "") model.deletedAddresses)


appendToList : List Article -> Article -> List Article
appendToList articleList article =
    let
        addressComponents =
            addressComponentsForArticle article

        articleIsInList =
            articleList
                |> List.filter
                    (\articleInList ->
                        addressComponents == addressComponentsForArticle articleInList
                    )
                |> List.isEmpty
                |> not
    in
    if articleIsInList then
        articleList

    else
        articleList ++ [ article ]
