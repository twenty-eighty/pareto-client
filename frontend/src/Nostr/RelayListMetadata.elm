module Nostr.RelayListMetadata exposing (..)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Nostr.Event exposing (Event, Kind(..), Tag(..), TagReference, parseAddress)
import Nostr.Types exposing (PubKey)


type alias RelayMetadata =
    { url : String
    , role : RelayRole
    }

type RelayRole
    = ReadRelay
    | WriteRelay
    | ReadWriteRelay

{-
-}

relayListFromEvent : Event -> (PubKey, List RelayMetadata)
relayListFromEvent event =
    let
        relayList =
            event.tags
            |> List.filterMap (\tag ->
                case tag of 
                    UrlTag url role ->
                        Just { url = url, role = roleFromString role }

                    _ ->
                        Nothing
                    )
    in
    (event.pubKey, relayList)


roleFromString : Maybe String -> RelayRole
roleFromString maybeRole =
    case maybeRole of
        Just "read" ->
            ReadRelay

        Just "write" ->
            WriteRelay

        _ ->
            ReadWriteRelay