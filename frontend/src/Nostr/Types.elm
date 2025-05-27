module Nostr.Types exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode


type alias EventId =
    String


type alias PubKey =
    String


type alias RelayUrl =
    String


type alias ServerUrl =
    String


type alias Address =
    String


type LoginStatus
    = LoggedOut
    | LoggedInUnknown
    | LoggedIn PubKey LoginMethod


type LoginMethod
    = LoginMethodConnect
    | LoginMethodExtension
    | LoginMethodLocal
    | LoginMethodOther String
    | LoginMethodReadOnly


type RelayRole
    = ReadRelay
    | WriteRelay
    | ReadWriteRelay


type Following
    = FollowingPubKey
        { pubKey : PubKey
        , relay : Maybe String
        , petname : Maybe String
        }
    | FollowingHashtag String


type alias OutgoingCommand =
    { command : String
    , value : Encode.Value
    }


type alias IncomingMessage =
    { messageType : String
    , value : Encode.Value
    }


loggedInPubKey : LoginStatus -> Maybe PubKey
loggedInPubKey loginStatus =
    case loginStatus of
        LoggedIn pubKey _ ->
            Just pubKey

        _ ->
            Nothing



-- return a pubkey if the user can sign events


loggedInSigningPubKey : LoginStatus -> Maybe PubKey
loggedInSigningPubKey loginStatus =
    case loginStatus of
        LoggedIn _ LoginMethodReadOnly ->
            Nothing

        LoggedIn pubKey _ ->
            Just pubKey

        _ ->
            Nothing



-- check if user can sign events


signingPubKeyAvailable : LoginStatus -> Bool
signingPubKeyAvailable loginStatus =
    loggedInSigningPubKey loginStatus 
    |> Maybe.map (\_ -> True)
    |> Maybe.withDefault False

relayRoleFromString : String -> RelayRole
relayRoleFromString role =
    case role of
        "read" ->
            ReadRelay

        "write" ->
            WriteRelay

        _ ->
            ReadWriteRelay


relayRoleToString : RelayRole -> Maybe String
relayRoleToString role =
    case role of
        ReadRelay ->
            Just "read"

        WriteRelay ->
            Just "write"

        ReadWriteRelay ->
            Nothing


decodeRelayRole : Decode.Decoder RelayRole
decodeRelayRole =
    Decode.string
        |> Decode.andThen
            (\roleString ->
                relayRoleFromString roleString
                    |> Decode.succeed
            )
