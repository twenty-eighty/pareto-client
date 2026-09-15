module Nostr.Relay exposing
    ( Protocol(..)
    , Relay
    , RelayState(..)
    , RelayUrl
    , applyNip11Result
    , displayName
    , fromString
    , host
    , hostAndPathOfUrl
    , hostPort
    , iconUrl
    , initFromUrls
    , parse
    , protocol
    , relayUrlDecoder
    , toHttp
    , toKey
    , toWire
    , updateRelayNip11
    , updateRelayStatus
    )

{-| Relay connection state and opaque relay URLs.

`RelayUrl` stores protocol separately from host so dict identity / equality can
ignore protocol (`toKey` / `host`), while wire/HTTP encodings still know `ws` vs `wss`.
-}

import Dict exposing (Dict)
import Http
import Json.Decode as Decode
import Nostr.Nip11 exposing (Nip11Info)
import Url


type Protocol
    = Ws
    | Wss


{-| Normalized relay address: protocol + host[:port][/path], no trailing slash.
-}
type RelayUrl
    = RelayUrl Protocol String


type alias Relay =
    { url : RelayUrl
    , state : RelayState
    , nip11 : Maybe Nip11Info
    }


type RelayState
    = RelayStateUnknown
    | RelayStateNip11RequestFailed Http.Error
    | RelayDisconnected
    | RelayConnecting
    | RelayConnected
    | RelayReady


protocol : RelayUrl -> Protocol
protocol (RelayUrl p _) =
    p


{-| Host (and optional port/path) without protocol — for display and protocol-independent compares.
-}
host : RelayUrl -> String
host (RelayUrl _ h) =
    h


{-| Alias for `host` (UI naming).
-}
hostPort : RelayUrl -> String
hostPort =
    host


{-| Dict / Set key — protocol-independent.
-}
toKey : RelayUrl -> String
toKey =
    host


toWire : RelayUrl -> String
toWire (RelayUrl p h) =
    protocolToPrefix p ++ h


toHttp : RelayUrl -> String
toHttp (RelayUrl p h) =
    case p of
        Ws ->
            -- Local relays (Citrine) typically have no TLS.
            "http://" ++ h

        Wss ->
            "https://" ++ h


{-| Parse a user or wire string. Bare hosts default to `wss://`.
-}
parse : String -> Maybe RelayUrl
parse raw =
    let
        trimmed =
            String.trim raw
    in
    if trimmed == "" then
        Nothing

    else
        Just (fromString trimmed)


{-| Parse with `wss://` default for bare hosts. Always succeeds for non-empty-ish input.
-}
fromString : String -> RelayUrl
fromString raw =
    let
        trimmed =
            String.trim raw
                |> stripTrailingSlash

        ( proto, rest ) =
            if String.startsWith "wss://" trimmed then
                ( Wss, String.dropLeft 6 trimmed )

            else if String.startsWith "ws://" trimmed then
                ( Ws, String.dropLeft 5 trimmed )

            else if String.startsWith "https://" trimmed then
                ( Wss, String.dropLeft 8 trimmed )

            else if String.startsWith "http://" trimmed then
                ( Ws, String.dropLeft 7 trimmed )

            else
                ( Wss, trimmed )
    in
    RelayUrl proto (stripTrailingSlash rest)


protocolToPrefix : Protocol -> String
protocolToPrefix p =
    case p of
        Ws ->
            "ws://"

        Wss ->
            "wss://"


stripTrailingSlash : String -> String
stripTrailingSlash s =
    if String.endsWith "/" s then
        String.dropRight 1 s

    else
        s


displayName : Relay -> String
displayName relay =
    relay.nip11
        |> Maybe.andThen .name
        |> Maybe.withDefault (host relay.url)


iconUrl : Relay -> String
iconUrl relay =
    relay.nip11
        |> Maybe.andThen .icon
        |> Maybe.withDefault (toHttp relay.url ++ "/favicon.ico")


relayUrlDecoder : Decode.Decoder RelayUrl
relayUrlDecoder =
    Decode.field "url" Decode.string
        |> Decode.map fromString


updateRelayStatus : RelayUrl -> RelayState -> Dict String Relay -> Dict String Relay
updateRelayStatus relayUrl state relayDict =
    let
        key =
            toKey relayUrl
    in
    case Dict.get key relayDict of
        Just relay ->
            Dict.insert key { relay | url = relayUrl, state = state } relayDict

        Nothing ->
            Dict.insert
                key
                { url = relayUrl
                , state = state
                , nip11 = Nothing
                }
                relayDict


initFromUrls : List RelayUrl -> Dict String Relay
initFromUrls relayUrls =
    relayUrls
        |> List.map
            (\url ->
                ( toKey url
                , { url = url
                  , state = RelayStateUnknown
                  , nip11 = Nothing
                  }
                )
            )
        |> Dict.fromList


applyNip11Result : RelayUrl -> Result Http.Error Nip11Info -> Dict String Relay -> Dict String Relay
applyNip11Result relayUrl result relays =
    let
        key =
            toKey relayUrl
    in
    case result of
        Ok info ->
            let
                updatedRelay =
                    Dict.get key relays
                        |> Maybe.map (\relay -> { relay | nip11 = Just info })
                        |> Maybe.withDefault
                            { url = relayUrl
                            , state = RelayStateUnknown
                            , nip11 = Just info
                            }
            in
            Dict.insert key updatedRelay relays

        Err err ->
            let
                updatedRelay =
                    Dict.get key relays
                        |> Maybe.map (\relay -> { relay | state = RelayStateNip11RequestFailed err })
                        |> Maybe.withDefault
                            { url = relayUrl
                            , state = RelayStateNip11RequestFailed err
                            , nip11 = Nothing
                            }
            in
            Dict.insert key updatedRelay relays


updateRelayNip11 : RelayUrl -> Nip11Info -> List Relay -> List Relay
updateRelayNip11 relayUrl info relays =
    List.map
        (\relay ->
            if toKey relay.url == toKey relayUrl then
                { relay | nip11 = Just info }

            else
                relay
        )
        relays


hostAndPathOfUrl : String -> ( Maybe String, Maybe String )
hostAndPathOfUrl urlString =
    case Url.fromString urlString of
        Just url ->
            ( Just url.host, Just url.path )

        Nothing ->
            ( Nothing, Nothing )
