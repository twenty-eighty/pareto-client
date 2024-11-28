module Shared exposing
    ( Flags, decoder
    , Model, Msg
    , init, update, subscriptions
    , loggedIn
    )

{-|

@docs Flags, decoder
@docs Model, Msg
@docs init, update, subscriptions

-}

import BrowserEnv exposing (BrowserEnv)
import Effect exposing (Effect)
import Json.Decode
import Json.Decode as Decode
import Nostr
import Nostr.Event exposing (Kind(..), emptyEventFilter)
import Nostr.Profile exposing (Profile)
import Nostr.Request exposing (Request, RequestData(..))
import Nostr.Types exposing (PubKey, IncomingMessage)
import Pareto
import Ports
import Route exposing (Route)
import Route.Path
import Shared.Model exposing (LoginStatus(..))
import Shared.Msg exposing (Msg(..))
import Shared.Model exposing (ClientRole(..))
import Ui.Styles exposing (Theme(..))

type alias Model = Shared.Model.Model


defaultRelays : List String
defaultRelays =
    [
     "nostr.pareto.space/"
--    , "team-relay.pareto.space"
    , "pareto.nostr1.com/"
--    , "relay.snort.social"
--  , "relay.damus.io"
    , "nos.lol"
--  , "offchain.pub"
--  , "relay.damus.io"
--  , "nostr.wine"
    ]

-- FLAGS

type alias Flags =
    { darkMode : Bool
    , isLoggedIn : Bool
    , locale : String
    , sharingAvailable : Bool
    }


-- Define a decoder for the 'isLoggedIn' field
decoder : Json.Decode.Decoder Flags
decoder =
    Json.Decode.map4 Flags
        (Json.Decode.field "darkMode" Json.Decode.bool)
        (Json.Decode.field "isLoggedIn" Json.Decode.bool)
        (Json.Decode.field "locale" Json.Decode.string)
        (Json.Decode.field "sharingAvailable" Json.Decode.bool)

-- INIT


init : Result Json.Decode.Error Flags -> Route () -> ( Model, Effect Msg )
init flagsResult route =
    case flagsResult of
        Ok flags ->
            let
                (loginStatus, effect) =
                    if flags.isLoggedIn then
                        (Shared.Model.LoggedInUnknown, Effect.sendCmd <| Ports.requestUser)
                    else
                        (Shared.Model.LoggedOut, Effect.none )

                (browserEnv, browserEnvCmd) =
                    BrowserEnv.init
                        { backendUrl = ""
                        , darkMode = flags.darkMode
                        , frontendUrl = ""
                        , locale = flags.locale
                        , sharingAvailable = flags.sharingAvailable
                        }

                (nostrInit, nostrInitCmd) =
                    Nostr.init portHooks defaultRelays

                -- request bookmark list of Pareto creators
                -- as well as bookmark sets for different purposes
                (nostr, nostrRequestCmd) =
                    { emptyEventFilter | authors = Just [ Pareto.authorsKey ], kinds = Just [ KindFollows, KindFollowSets ] }
                    |> RequestFollowSets
                    |> Nostr.createRequest nostrInit "Follow list/sets of Pareto user" []
                    |> Nostr.doRequest nostrInit

            in
            ( { loginStatus = loginStatus
              , browserEnv = browserEnv
              , nostr = nostr
              , role = ClientConsumer
              , theme = ReferenceStyleTheme
              }
            , Effect.batch
                [ effect
                , Effect.sendCmd <| Cmd.map Shared.Msg.BrowserEnvMsg browserEnvCmd
                , Effect.sendCmd <| Cmd.map Shared.Msg.NostrMsg nostrInitCmd
                , Effect.sendCmd <| Cmd.map Shared.Msg.NostrMsg nostrRequestCmd
                ]
            )

        Err _ ->
            let
                (browserEnv, _) =
                    BrowserEnv.init
                        { backendUrl = ""
                        , darkMode = False
                        , frontendUrl = ""
                        , locale = ""
                        , sharingAvailable = False
                        }
            in
            
            ( { loginStatus = Shared.Model.LoggedOut
              , browserEnv = browserEnv
              , nostr = Nostr.empty
              , role = ClientConsumer
              , theme = ReferenceStyleTheme
              }
            , Effect.none
            )

portHooks : Nostr.Hooks
portHooks =
    { connect = Ports.connect
    , requestEvents = Ports.requestEvents
    , receiveMessage = Ports.receiveMessage
    , requestBlossomListAuth = Ports.requestBlossomListAuth
    , requestNip96Auth = Ports.requestNip96Auth
    , sendEvent = Ports.sendEvent
    }



-- UPDATE


type alias Msg =
    Shared.Msg.Msg



update : Route () -> Msg -> Model -> ( Model, Effect Msg )
update route msg model =
    case msg of
        TriggerLogin ->
            ( model
            , Effect.sendCmd <| Ports.requestUser
            )

        ReceivedPortMessage portMessage -> 
            updateWithPortMessage model portMessage

        BrowserEnvMsg browserEnvMsg ->
            let
                (newBrowserEnv, browserEnvCmd) =
                    BrowserEnv.update browserEnvMsg model.browserEnv
            in
            ( { model | browserEnv = newBrowserEnv }
            , Effect.sendCmd <| Cmd.map Shared.Msg.BrowserEnvMsg browserEnvCmd)


        NostrMsg nostrMsg ->
            let
                (newNostr, nostrCmd) =
                    Nostr.update nostrMsg model.nostr
            in
            ( { model | nostr = newNostr }
            , Effect.sendCmd <| Cmd.map Shared.Msg.NostrMsg nostrCmd
            )

        RequestNostrEvents request ->
            let
                (newNostr, nostrCmd) =
                    Nostr.doRequest model.nostr request
            in
            ( { model | nostr = newNostr }
            , Effect.sendCmd <| Cmd.map Shared.Msg.NostrMsg nostrCmd
            )

        
        SendNostrEvent sendRequest ->
            let
                (newNostr, nostrCmd) =
                    Nostr.send model.nostr sendRequest
            in
            ( { model | nostr = newNostr }
            , Effect.sendCmd <| Cmd.map Shared.Msg.NostrMsg nostrCmd
            )

        SwitchClientRole ->
            if model.role == ClientConsumer then
                ( { model | role = ClientCreator }, Effect.pushRoutePath Route.Path.Posts )
            else
                ( { model | role = ClientConsumer }, Effect.pushRoutePath Route.Path.Home_ )

updateWithPortMessage : Model -> IncomingMessage -> ( Model, Effect Msg )
updateWithPortMessage model portMessage =
    case portMessage.messageType of
        "user" ->
            updateWithUserValue model portMessage.value
        
        _ -> 
            ( model, Effect.none )

updateWithUserValue : Model -> Decode.Value -> ( Model, Effect Msg )
updateWithUserValue model value =
    case Json.Decode.decodeValue pubkeyDecoder value of
        Ok pubKey ->
            let
                (nostr, cmd) =
                    Nostr.requestUserData model.nostr pubKey
            in

            ( { model | loginStatus = Shared.Model.LoggedIn pubKey, nostr = nostr }
            , Effect.sendCmd (Cmd.map Shared.Msg.NostrMsg cmd)
            )

        Err error ->
            ( model , Effect.none )

loggedIn : Model -> Bool
loggedIn model =
    case model.loginStatus of
        LoggedOut ->
            False

        LoggedInUnknown ->
            False

        LoggedIn _ ->
            True

        

pubkeyDecoder : Json.Decode.Decoder PubKey
pubkeyDecoder =
    Json.Decode.at ["pubKey"] Json.Decode.string

-- SUBSCRIPTIONS

subscriptions : Route () -> Model -> Sub Msg
subscriptions route model =
    Sub.batch
        [ Ports.receiveMessage Shared.Msg.ReceivedPortMessage
        , Sub.map Shared.Msg.BrowserEnvMsg (BrowserEnv.subscriptions model.browserEnv)
        , Sub.map Shared.Msg.NostrMsg (Nostr.subscriptions model.nostr)
        ]
