module Nostr.FileStorageServerList exposing (..)

import Dict exposing (Dict)
import Nostr.Event exposing (Event, Tag(..))
import Nostr.Types exposing (PubKey)


fileStorageServerListFromEvent : Event -> ( PubKey, List String )
fileStorageServerListFromEvent event =
    let
        fileStorageServerList =
            event.tags
                |> List.foldl
                    (\tag serverList ->
                        case tag of
                            ServerTag url ->
                                serverList ++ [ url ]

                            _ ->
                                serverList
                    )
                    []
    in
    ( event.pubKey, fileStorageServerList )


ingest : Dict PubKey (List String) -> List Event -> Dict PubKey (List String)
ingest dict events =
    events
        |> List.map fileStorageServerListFromEvent
        |> List.foldl
            (\( pubKey, fileStorageServerList ) acc ->
                Dict.insert pubKey fileStorageServerList acc
            )
            dict
