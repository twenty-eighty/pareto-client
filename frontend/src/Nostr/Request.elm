module Nostr.Request exposing (..)

import Nostr.Event exposing (EventFilter, Kind)
import Nostr.Nip05 exposing (Nip05)
import Nostr.Types exposing (RelayUrl)

-- one request can lead to subsequent requests for related kinds
-- for each RequestData is tracked if it is sent already
type alias Request =
    { id : RequestId
    , relatedKinds : List Kind
    , value : List RequestState
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

