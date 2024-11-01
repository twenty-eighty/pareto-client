module Nostr.FileStorageServerList exposing (..)

import Nostr.Event exposing (Event, Tag(..))
import Nostr.Types exposing (PubKey)


fileStorageServerListFromEvent : Event -> (PubKey, List String)
fileStorageServerListFromEvent event =
    let
        fileStorageServerList =
            event.tags
            |> List.foldl (\tag serverList ->
                case tag of 
                    ServerTag url ->
                        serverList ++ [ url ]

                    _ ->
                        serverList
                    )
                []
    in
    (event.pubKey, fileStorageServerList )
