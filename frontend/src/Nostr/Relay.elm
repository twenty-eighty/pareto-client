module Nostr.Relay exposing (..)

import Nostr.Nip11 exposing (Nip11Info)
import Url

type alias Relay =
    { urlWithoutProtocol : String
    , state : RelayState
    , nip11 : Maybe Nip11Info
    }

type RelayState
    = RelayStateUnknown
    | RelayDisconnected
    | RelayConnecting
    | RelayConnected
    | RelayReady

icon : Relay -> String
icon relay =
    relay.nip11
    |> Maybe.andThen .icon
    |> Maybe.withDefault ("https://" ++ relay.urlWithoutProtocol ++ "/favicon.ico")

websocketUrl : String -> String
websocketUrl urlWithoutProtocol =
    "wss://" ++ urlWithoutProtocol

updateRelayStatus : String -> RelayState -> List Relay -> List Relay
updateRelayStatus relayUrlWithoutProtocol state relayList =
    relayList
    |> List.map (\relay ->
        if relayUrlWithoutProtocol == relay.urlWithoutProtocol then
            { relay | state = state }
        else
            relay
        )

updateRelayNip11 : String -> Nip11Info -> List Relay -> List Relay
updateRelayNip11 urlWithoutProtocol info relays =
    List.map
        (\relay ->
            if relay.urlWithoutProtocol == urlWithoutProtocol then
                { relay | nip11 = Just info }
            else
                relay
        )
        relays
 
hostAndPathOfUrl : String -> (Maybe String, Maybe String )
hostAndPathOfUrl urlString =
    case Url.fromString urlString of
        Just url ->
            (Just url.host, Just url.path)

        Nothing ->
            (Nothing, Nothing)

