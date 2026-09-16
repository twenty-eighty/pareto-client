module Nostr.ContentRequest exposing
    ( ContentRequestState(..)
    , track
    , settle
    , fail
    , get
    , settlesOnKind
    )

{-| Lifecycle of a single-content fetch (article, note, or picture post).
-}

import Dict exposing (Dict)
import Nostr.Event exposing (Kind(..))
import Nostr.Request exposing (RequestId)


type ContentRequestState
    = WaitingForNip05
    | WaitingForContent
    | ContentRequestSettled
    | ContentRequestFailed String


get : RequestId -> Dict RequestId ContentRequestState -> Maybe ContentRequestState
get =
    Dict.get


track : RequestId -> ContentRequestState -> Dict RequestId ContentRequestState -> Dict RequestId ContentRequestState
track requestId state states =
    Dict.insert requestId state states


settle : RequestId -> Dict RequestId ContentRequestState -> Dict RequestId ContentRequestState
settle requestId states =
    case Dict.get requestId states of
        Just (ContentRequestFailed _) ->
            states

        Just _ ->
            track requestId ContentRequestSettled states

        Nothing ->
            states


fail : RequestId -> String -> Dict RequestId ContentRequestState -> Dict RequestId ContentRequestState
fail requestId reason states =
    case Dict.get requestId states of
        Just _ ->
            track requestId (ContentRequestFailed reason) states

        Nothing ->
            states


{-| Whether arriving events of this kind complete a WaitingForContent request.
-}
settlesOnKind : Kind -> Bool
settlesOnKind kind =
    case kind of
        KindLongFormContent ->
            True

        KindDraftLongFormContent ->
            True

        KindShortTextNote ->
            True

        KindPicture ->
            True

        _ ->
            False
