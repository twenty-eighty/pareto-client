module Nostr.Request exposing (..)

import Nostr.Event exposing (EventFilter, Kind(..), emptyEventFilter)
import Nostr.Nip05 exposing (Nip05)
import Nostr.Types exposing (PubKey, RelayUrl)
import Time exposing (Posix)



-- one request can lead to subsequent requests for related kinds
-- for each RequestData is tracked if it is sent already


type alias Request =
    { id : RequestId
    , relatedKinds : List Kind
    , states : List RequestState
    , description : String
    }


type RequestState
    = RequestCreated RequestData
    | RequestSent RequestData


type alias RequestId =
    Int


type RequestData
    = RequestArticle (Maybe (List RelayUrl)) EventFilter
    | RequestArticles (List EventFilter)
    | RequestArticlesFeed Bool (List EventFilter)
    | RequestArticleDrafts (List EventFilter)
    | RequestBookmarks EventFilter
    | RequestCommunity (Maybe (List RelayUrl)) EventFilter
    | RequestDeletionRequests EventFilter
    | RequestFollowSets EventFilter
    | RequestFutureArticles (List EventFilter)
    | RequestMediaServerLists EventFilter
    | RequestNip05AndArticle Nip05 String
    | RequestPicturesFeed (List EventFilter)
    | RequestProfile (Maybe (List RelayUrl)) EventFilter
    | RequestProfileByNip05 Nip05
    | RequestReactions EventFilter
    | RequestRelayLists EventFilter
    | RequestSubscribers EventFilter
    | RequestUserData EventFilter
    | RequestBlossomAuth String String HttpRequestMethod
    | RequestNip98Auth String String String HttpRequestMethod
    | RequestSearchResults (List EventFilter)
    | RequestShortNote (Maybe (List RelayUrl)) EventFilter


type HttpRequestMethod
    = GetRequest
    | DeleteRequest Int
    | PatchRequest Int String
    | PostRequest Int String
    | PutRequest Int String


relatedKindsForRequest : Maybe Request -> List Kind
relatedKindsForRequest maybeRequest =
    maybeRequest
        |> Maybe.map .relatedKinds
        |> Maybe.withDefault []


relaysOfRequest : Request -> Maybe (List RelayUrl)
relaysOfRequest request =
    let
        maybeData =
            List.head request.states
                |> Maybe.andThen
                    (\state ->
                        case state of
                            RequestCreated requestData ->
                                Just requestData

                            RequestSent requestData ->
                                Just requestData
                    )
    in
    maybeData
        |> Maybe.andThen
            (\data ->
                case data of
                    RequestArticle (Just relayList) _ ->
                        Just relayList

                    RequestArticle Nothing _ ->
                        Nothing

                    RequestArticles _ ->
                        Nothing

                    RequestArticlesFeed _ _ ->
                        Nothing

                    RequestArticleDrafts _ ->
                        Nothing

                    RequestBookmarks _ ->
                        Nothing

                    RequestCommunity (Just relayList) _ ->
                        Just relayList

                    RequestCommunity Nothing _ ->
                        Nothing

                    RequestDeletionRequests _ ->
                        Nothing

                    RequestFollowSets _ ->
                        Nothing

                    RequestFutureArticles _ ->
                        Nothing

                    RequestMediaServerLists _ ->
                        Nothing

                    RequestNip05AndArticle _ _ ->
                        Nothing

                    RequestPicturesFeed _ ->
                        Nothing

                    RequestProfile (Just relayList) _ ->
                        Just relayList

                    RequestProfile Nothing _ ->
                        Nothing

                    RequestProfileByNip05 _ ->
                        Nothing

                    RequestReactions _ ->
                        Nothing

                    RequestRelayLists _ ->
                        Nothing

                    RequestSubscribers _ ->
                        Nothing

                    RequestUserData _ ->
                        Nothing

                    RequestBlossomAuth _ _ _ ->
                        Nothing

                    RequestNip98Auth _ _ _ _ ->
                        Nothing

                    RequestSearchResults _ ->
                        Nothing

                    RequestShortNote relayList _ ->
                        relayList
            )


requestDataOfState : RequestState -> RequestData
requestDataOfState state =
    case state of
        RequestCreated data ->
            data

        RequestSent data ->
            data


shouldRequestArticleDetails : Request -> Bool
shouldRequestArticleDetails request =
    List.any
        (\state ->
            case requestDataOfState state of
                RequestArticle _ _ ->
                    True

                RequestNip05AndArticle _ _ ->
                    True

                _ ->
                    False
        )
        request.states


identifierFromNip05ArticleRequest : Request -> Maybe String
identifierFromNip05ArticleRequest request =
    request.states
        |> List.filterMap
            (\state ->
                case state of
                    RequestCreated (RequestNip05AndArticle _ identifier) ->
                        Just identifier

                    RequestSent (RequestNip05AndArticle _ identifier) ->
                        Just identifier

                    _ ->
                        Nothing
            )
        |> List.head


eventFiltersWithUntil : List EventFilter -> Maybe Posix -> List EventFilter
eventFiltersWithUntil eventFilters maybeUntil =
    eventFilters
        |> List.map (\eventFilter -> { eventFilter | until = maybeUntil })


{-| Kinds fetched when a user logs in (profile + lists + relays + servers).
-}
userDataKinds : List Kind
userDataKinds =
    [ KindUserMetadata
    , KindBlockedRelaysList
    , KindBookmarkList
    , KindBookmarkSets
    , KindCommunitiesList
    , KindFileStorageServerList
    , KindFollows
    , KindFollowSets
    , KindMuteList
    , KindRelayListMetadata
    , KindRelayListForDMs
    , KindRelaySets
    , KindSearchRelaysList
    , KindUserServerList
    ]


userDataFilter : PubKey -> EventFilter
userDataFilter pubKey =
    { emptyEventFilter
        | authors = Just [ pubKey ]
        , kinds = Just userDataKinds
    }
