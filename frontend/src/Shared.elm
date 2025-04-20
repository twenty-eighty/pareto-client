module Shared exposing
    ( Flags, decoder
    , Model, Msg
    , init, update, subscriptions
    , loggedIn, loggedInPubKey, loggedInSigningPubKey, signingPubKeyAvailable
    )

{-|

@docs Flags, decoder
@docs Model, Msg
@docs init, update, subscriptions

-}

import BrowserEnv
import Effect exposing (Effect)
import Json.Decode
import Nostr
import Nostr.ConfigCheck as ConfigCheck
import Nostr.Event exposing (Kind(..), emptyEventFilter)
import Nostr.External
import Nostr.Request exposing (RequestData(..))
import Nostr.Types exposing (IncomingMessage, PubKey)
import Pareto
import Ports
import Process
import Route exposing (Route)
import Route.Path
import Shared.Model exposing (ClientRole(..), LoginMethod(..), LoginStatus(..))
import Shared.Msg exposing (Msg(..))
import Task
import Ui.Styles exposing (Theme(..))


type alias Model =
    Shared.Model.Model



-- FLAGS


type alias Flags =
    { darkMode : Bool
    , environment : Maybe String
    , locale : String
    , nativeSharingAvailable : Bool
    , testMode : Bool
    }


decoder : Json.Decode.Decoder Flags
decoder =
    Json.Decode.map5 Flags
        (Json.Decode.field "darkMode" Json.Decode.bool)
        (Json.Decode.field "environment" (Json.Decode.maybe Json.Decode.string))
        (Json.Decode.field "locale" Json.Decode.string)
        (Json.Decode.field "nativeSharingAvailable" Json.Decode.bool)
        (Json.Decode.field "testMode" Json.Decode.bool)



-- INIT


init : Result Json.Decode.Error Flags -> Route () -> ( Model, Effect Msg )
init flagsResult _ =
    case flagsResult of
        Ok flags ->
            let
                ( browserEnv, browserEnvCmd ) =
                    BrowserEnv.init
                        { backendUrl = ""
                        , darkMode = flags.darkMode
                        , environment = flags.environment
                        , frontendUrl = ""
                        , locale = flags.locale
                        , nativeSharingAvailable = flags.nativeSharingAvailable
                        , testMode = flags.testMode
                        }

                nostrTestMode =
                    if flags.testMode then
                        Nostr.TestModeEnabled

                    else
                        Nostr.TestModeOff

                ( nostrInit, nostrInitCmd ) =
                    Nostr.init portHooks nostrTestMode Pareto.defaultRelays

                -- request bookmark list of Pareto creators
                -- as well as bookmark sets for different purposes
                ( nostr, nostrRequestCmd ) =
                    { emptyEventFilter | authors = Just [ Pareto.authorsKey, Pareto.rssAuthorsKey, Pareto.betaTestKey, Pareto.editorKey ], kinds = Just [ KindFollows, KindFollowSets ] }
                        |> RequestFollowSets
                        |> Nostr.createRequest nostrInit "Follow list/sets of Pareto user" []
                        |> Nostr.doRequest nostrInit
            in
            ( { loginStatus = Shared.Model.LoggedInUnknown
              , browserEnv = browserEnv
              , configCheck = ConfigCheck.init
              , nostr = nostr
              , role = ClientReader
              , theme = ParetoTheme
              }
            , Effect.batch
                [ Effect.sendCmd <| Cmd.map Shared.Msg.BrowserEnvMsg browserEnvCmd
                , Effect.sendCmd <| Cmd.map Shared.Msg.NostrMsg nostrInitCmd
                , Effect.sendCmd <| Cmd.map Shared.Msg.NostrMsg nostrRequestCmd
                ]
            )

        Err _ ->
            let
                ( browserEnv, _ ) =
                    BrowserEnv.init
                        { backendUrl = ""
                        , darkMode = False
                        , environment = Nothing
                        , frontendUrl = ""
                        , locale = ""
                        , nativeSharingAvailable = False
                        , testMode = False
                        }
            in
            ( { loginStatus = Shared.Model.LoggedOut
              , browserEnv = browserEnv
              , configCheck = ConfigCheck.init
              , nostr = Nostr.empty
              , role = ClientReader
              , theme = ParetoTheme
              }
            , Effect.none
            )


portHooks : Nostr.External.Hooks msg
portHooks =
    { connect = Ports.connect
    , requestEvents = Ports.requestEvents
    , receiveMessage = Ports.receiveMessage
    , requestBlossomAuth = Ports.requestBlossomAuth
    , requestNip96Auth = Ports.requestNip96Auth
    , searchEvents = Ports.searchEvents
    , sendEvent = Ports.sendEvent
    }



-- UPDATE


type alias Msg =
    Shared.Msg.Msg


update : Route () -> Msg -> Model -> ( Model, Effect Msg )
update _ msg model =
    case msg of
        TriggerLogin ->
            ( model, Effect.sendCmd <| Ports.loginSignUp )

        ReceivedPortMessage portMessage ->
            updateWithPortMessage model portMessage

        BrowserEnvMsg browserEnvMsg ->
            let
                ( newBrowserEnv, browserEnvCmd ) =
                    BrowserEnv.update browserEnvMsg model.browserEnv
            in
            ( { model | browserEnv = newBrowserEnv }
            , Effect.sendCmd <| Cmd.map Shared.Msg.BrowserEnvMsg browserEnvCmd
            )

        NostrMsg nostrMsg ->
            let
                ( newNostr, nostrCmd ) =
                    Nostr.update nostrMsg model.nostr
            in
            ( { model | nostr = newNostr }
            , Effect.sendCmd <| Cmd.map Shared.Msg.NostrMsg nostrCmd
            )

        RequestNostrEvents request ->
            let
                ( newNostr, nostrCmd ) =
                    Nostr.doRequest model.nostr request
            in
            ( { model | nostr = newNostr }
            , Effect.sendCmd <| Cmd.map Shared.Msg.NostrMsg nostrCmd
            )

        ResetArticles ->
            let
                newNostr =
                    Nostr.resetArticles model.nostr
            in
            ( { model | nostr = newNostr }
            , Effect.none
            )

        SendNostrEvent sendRequest ->
            let
                ( newNostr, nostrCmd ) =
                    Nostr.send model.nostr sendRequest
            in
            ( { model | nostr = newNostr }
            , Effect.sendCmd <| Cmd.map Shared.Msg.NostrMsg nostrCmd
            )

        SetClientRole changePath clientRole ->
            let
                newPath =
                    if model.role == ClientReader then
                        Route.Path.Posts

                    else
                        Route.Path.Read
            in
            ( { model | role = clientRole }
            , if changePath then
                Effect.pushRoutePath newPath

              else
                Effect.none
            )

        SetTestMode testMode ->
            let
                ( browserEnv, cmd ) =
                    BrowserEnv.setTestMode model.browserEnv testMode
            in
            ( { model | browserEnv = browserEnv }
            , cmd
                |> Effect.sendCmd
            )

        CheckConfiguration _ ->
            let
                ( configCheck, checkCmd ) =
                    loggedInSigningPubKey model.loginStatus
                        |> Maybe.map (ConfigCheck.performChecks model.nostr)
                        |> Maybe.withDefault ( model.configCheck, Cmd.none )
            in
            ( { model | configCheck = configCheck }
            , checkCmd
                |> Cmd.map ConfigCheckMsg
                |> Effect.sendCmd
            )

        ConfigCheckMsg configCheckMsg ->
            let
                ( configCheck, checkCmd ) =
                    ConfigCheck.update configCheckMsg model.configCheck
            in
            ( { model | configCheck = configCheck }
            , checkCmd
                |> Cmd.map ConfigCheckMsg
                |> Effect.sendCmd
            )

        LoadUserDataByPubKey pubKey ->
            ( model
            , Nostr.loadUserDataByPubKey model.nostr pubKey
                |> Cmd.map NostrMsg
                |> Effect.sendCmd
            )

        LoadUserDataByNip05 nip05 ->
            ( model
            , Nostr.loadUserDataByNip05 model.nostr nip05
                |> Cmd.map NostrMsg
                |> Effect.sendCmd
            )


updateWithPortMessage : Model -> IncomingMessage -> ( Model, Effect Msg )
updateWithPortMessage model portMessage =
    case portMessage.messageType of
        "user" ->
            updateWithUserValue model portMessage.value

        "loggedOut" ->
            ( { model | loginStatus = Shared.Model.LoggedOut }, Effect.none )

        _ ->
            ( model, Effect.none )


updateWithUserValue : Model -> Json.Decode.Value -> ( Model, Effect Msg )
updateWithUserValue model value =
    case
        ( Json.Decode.decodeValue pubkeyDecoder value
        , Json.Decode.decodeValue loginMethodDecoder value
        , model.loginStatus
        )
    of
        ( Ok pubKeyNew, Ok loginMethod, Shared.Model.LoggedIn pubKeyLoggedIn _ ) ->
            let
                ( nostr, cmdNostr ) =
                    if pubKeyNew /= pubKeyLoggedIn then
                        Nostr.requestUserData model.nostr pubKeyNew

                    else
                        -- ignore messages that don't change user
                        ( model.nostr, Cmd.none )

                startConfigCheckCmd =
                    if Nostr.isEditor model.nostr pubKeyNew then
                        -- trigger configuration check for Pareto users/authors
                        Process.sleep (5 * 1000.0)
                            |> Task.perform CheckConfiguration

                    else
                        -- ignore messages that don't change user
                        Cmd.none
            in
            ( { model
                | loginStatus = Shared.Model.LoggedIn pubKeyNew loginMethod
                , nostr = nostr
              }
            , [ cmdNostr
                    |> Cmd.map Shared.Msg.NostrMsg

              -- check if user sends newsletters
              , Nostr.loadUserDataByPubKey model.nostr pubKeyNew
                    |> Cmd.map Shared.Msg.NostrMsg
              , startConfigCheckCmd
              ]
                |> Cmd.batch
                |> Effect.sendCmd
            )

        ( Ok pubKeyNew, Ok loginMethod, _ ) ->
            let
                ( nostr, cmd ) =
                    Nostr.requestUserData model.nostr pubKeyNew
            in
            ( { model | loginStatus = Shared.Model.LoggedIn pubKeyNew loginMethod, nostr = nostr }
            , [ cmd

              -- check if user sends newsletters
              , Nostr.loadUserDataByPubKey model.nostr pubKeyNew
              ]
                |> Cmd.batch
                |> Cmd.map Shared.Msg.NostrMsg
                |> Effect.sendCmd
            )

        ( _, _, _ ) ->
            ( model, Effect.none )


loggedIn : Model -> Bool
loggedIn model =
    case model.loginStatus of
        LoggedOut ->
            False

        LoggedInUnknown ->
            False

        LoggedIn _ _ ->
            True


loggedInPubKey : Shared.Model.LoginStatus -> Maybe PubKey
loggedInPubKey loginStatus =
    case loginStatus of
        Shared.Model.LoggedIn pubKey _ ->
            Just pubKey

        _ ->
            Nothing



-- return a pubkey if the user can sign events


loggedInSigningPubKey : Shared.Model.LoginStatus -> Maybe PubKey
loggedInSigningPubKey loginStatus =
    case loginStatus of
        Shared.Model.LoggedIn _ LoginMethodReadOnly ->
            Nothing

        Shared.Model.LoggedIn pubKey _ ->
            Just pubKey

        _ ->
            Nothing



-- check if user can sign events


signingPubKeyAvailable : Shared.Model.LoginStatus -> Bool
signingPubKeyAvailable loginStatus =
    case loginStatus of
        Shared.Model.LoggedIn _ LoginMethodReadOnly ->
            False

        Shared.Model.LoggedIn _ _ ->
            True

        _ ->
            False


pubkeyDecoder : Json.Decode.Decoder PubKey
pubkeyDecoder =
    Json.Decode.field "pubKey" Json.Decode.string


loginMethodDecoder : Json.Decode.Decoder Shared.Model.LoginMethod
loginMethodDecoder =
    Json.Decode.field "method" Json.Decode.string
        |> Json.Decode.andThen
            (\method ->
                case String.toLower method of
                    "connect" ->
                        Json.Decode.succeed Shared.Model.LoginMethodConnect

                    "extension" ->
                        Json.Decode.succeed Shared.Model.LoginMethodExtension

                    "local" ->
                        Json.Decode.succeed Shared.Model.LoginMethodLocal

                    "readonly" ->
                        Json.Decode.succeed Shared.Model.LoginMethodReadOnly

                    other ->
                        Json.Decode.succeed (Shared.Model.LoginMethodOther other)
            )



-- SUBSCRIPTIONS


subscriptions : Route () -> Model -> Sub Msg
subscriptions _ model =
    Sub.batch
        [ Ports.receiveMessage Shared.Msg.ReceivedPortMessage
        , Sub.map Shared.Msg.BrowserEnvMsg (BrowserEnv.subscriptions model.browserEnv)
        , Sub.map Shared.Msg.NostrMsg (Nostr.subscriptions model.nostr)
        ]
