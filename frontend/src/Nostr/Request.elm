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

type alias RequestId = Int

type RequestData
    = RequestArticle (Maybe (List RelayUrl)) EventFilter
    | RequestArticles EventFilter
    | RequestArticlesFeed EventFilter
    | RequestArticleDrafts EventFilter
    | RequestBookmarks EventFilter
    | RequestCommunity (Maybe (List RelayUrl)) EventFilter
    | RequestDeletionRequests EventFilter
    | RequestFollowSets EventFilter
    | RequestNip05AndArticle Nip05 String
    | RequestProfile  (Maybe (List RelayUrl)) EventFilter
    | RequestProfileByNip05 Nip05
    | RequestReactions EventFilter
    | RequestUserData EventFilter
    | RequestBlossomAuth String String HttpRequestMethod
    | RequestNip98Auth String String HttpRequestMethod
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
            case List.head request.states of
                Just (RequestCreated requestData) ->
                    Just requestData

                Just (RequestSent requestData) ->
                    Just requestData
                
                Nothing ->
                    Nothing
    in
    case maybeData of
        Just (RequestArticle (Just relayList) _) ->
            Just relayList

        Just (RequestArticle Nothing _) ->
            Nothing

        Just (RequestArticles _) ->
            Nothing

        Just (RequestArticlesFeed _) ->
            Nothing

        Just (RequestArticleDrafts _) ->
            Nothing

        Just (RequestBookmarks _) ->
            Nothing

        Just (RequestCommunity (Just relayList) _) ->
            Just relayList

        Just (RequestCommunity Nothing _) ->
            Nothing

        Just (RequestDeletionRequests _) ->
            Nothing

        Just (RequestFollowSets _) ->
            Nothing

        Just (RequestNip05AndArticle _ _) ->
            Nothing

        Just (RequestProfile (Just relayList) _) ->
            Just relayList

        Just (RequestProfile Nothing _) ->
            Nothing

        Just (RequestProfileByNip05 _) ->
            Nothing

        Just (RequestReactions _) ->
            Nothing

        Just (RequestUserData _) ->
            Nothing

        Just (RequestBlossomAuth _ _ _) ->
            Nothing

        Just (RequestNip98Auth _ _ _) ->
            Nothing

        Just (RequestShortNote _) ->
            Nothing

        Nothing ->
            Nothing