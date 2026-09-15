module Nostr.Query exposing
    ( ContentLoadPhase(..)
    , ContentQueryStatus(..)
    , statusFrom
    , contentFromStatus
    )

{-| Page-facing status for a single-content query.
-}

import Dict exposing (Dict)
import Nostr.ContentRequest as ContentRequest exposing (ContentRequestState(..))
import Nostr.Request exposing (RequestId)


type ContentLoadPhase
    = ResolvingAuthor
    | FetchingContent


type ContentQueryStatus a
    = ContentQueryLoading ContentLoadPhase
    | ContentQueryReady a
    | ContentQueryNotFound
    | ContentQueryFailed String


{-| Resolve status from cached content (if any) and an optional in-flight request.
-}
statusFrom : Dict RequestId ContentRequestState -> Maybe a -> Maybe RequestId -> ContentQueryStatus a
statusFrom contentRequestStates maybeContent maybeRequestId =
    case maybeContent of
        Just content ->
            ContentQueryReady content

        Nothing ->
            case maybeRequestId of
                Nothing ->
                    ContentQueryLoading FetchingContent

                Just requestId ->
                    case ContentRequest.get requestId contentRequestStates of
                        Just (ContentRequestFailed reason) ->
                            ContentQueryFailed reason

                        Just ContentRequestSettled ->
                            ContentQueryNotFound

                        Just WaitingForNip05 ->
                            ContentQueryLoading ResolvingAuthor

                        Just WaitingForContent ->
                            ContentQueryLoading FetchingContent

                        Nothing ->
                            ContentQueryLoading FetchingContent


contentFromStatus : ContentQueryStatus a -> Maybe a
contentFromStatus status =
    case status of
        ContentQueryReady content ->
            Just content

        _ ->
            Nothing
