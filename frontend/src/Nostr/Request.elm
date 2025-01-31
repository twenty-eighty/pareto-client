module Nostr.Request exposing (..)

import Nostr.Event exposing (EventFilter, Kind)
import Nostr.Nip05 exposing (Nip05)
import Nostr.Types exposing (RelayUrl)



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
    | RequestArticles EventFilter
    | RequestArticlesFeed EventFilter
    | RequestArticleDrafts EventFilter
    | RequestBookmarks EventFilter
    | RequestCommunity (Maybe (List RelayUrl)) EventFilter
    | RequestDeletionRequests EventFilter
    | RequestFollowSets EventFilter
    | RequestMediaServerLists EventFilter
    | RequestNip05AndArticle Nip05 String
    | RequestProfile (Maybe (List RelayUrl)) EventFilter
    | RequestProfileByNip05 Nip05
    | RequestReactions EventFilter
    | RequestRelayLists EventFilter
    | RequestSubscribers EventFilter
    | RequestUserData EventFilter
    | RequestBlossomAuth String String HttpRequestMethod
    | RequestNip98Auth String String HttpRequestMethod
    | RequestSearchResults (List EventFilter)
    | RequestShortNote EventFilter


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

                    RequestArticlesFeed _ ->
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

                    RequestMediaServerLists _ ->
                        Nothing

                    RequestNip05AndArticle _ _ ->
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

                    RequestNip98Auth _ _ _ ->
                        Nothing

                    RequestSearchResults _ ->
                        Nothing

                    RequestShortNote _ ->
                        Nothing
            )
