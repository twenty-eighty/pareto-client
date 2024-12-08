module Nostr.Relay exposing (..)

import Nostr.Nip11 exposing (Nip11Info)
import Url
import Dict exposing (Dict)

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

displayName : Relay -> String
displayName relay =
    relay.nip11
    |> Maybe.andThen .name
    |> Maybe.withDefault relay.urlWithoutProtocol


icon : Relay -> String
icon relay =
    relay.nip11
    |> Maybe.andThen .icon
    |> Maybe.withDefault ("https://" ++ relay.urlWithoutProtocol ++ "/favicon.ico")

websocketUrl : String -> String
websocketUrl urlWithoutProtocol =
    "wss://" ++ urlWithoutProtocol

updateRelayStatus : String -> RelayState -> Dict String Relay -> Dict String Relay
updateRelayStatus relayUrlWithoutProtocol state relayDict =
    Dict.update relayUrlWithoutProtocol
        (\maybeRelay ->
            maybeRelay
            |> Maybe.map (\relay -> { relay | state = state })
        ) relayDict

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
 
hostWithoutProtocol : String -> String
hostWithoutProtocol url =
    let
        urlWithoutProtocol =
            if String.startsWith "wss://" url then
                String.dropLeft 6 url
            else
                url

    in
    if String.endsWith "/" urlWithoutProtocol then
        String.dropRight 1 urlWithoutProtocol
    else
        urlWithoutProtocol

hostAndPathOfUrl : String -> (Maybe String, Maybe String )
hostAndPathOfUrl urlString =
    case Url.fromString urlString of
        Just url ->
            (Just url.host, Just url.path)

        Nothing ->
            (Nothing, Nothing)

