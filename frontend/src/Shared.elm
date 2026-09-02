module Shared exposing
    ( Flags, decoder
    , Model, Msg
    , init, update, subscriptions
    , contentId, createFollowersEffect, loggedIn
    )

{-|

@docs Flags, decoder
@docs Model, Msg
@docs init, update, subscriptions

-}

import Browser.Dom
import BrowserEnv
import Components.AlertTimerMessage as AlertTimerMessage
import Components.AuthDialog as AuthDialog
import Effect exposing (Effect)
import Json.Decode
import Nostr
import Nostr.ConfigCheck as ConfigCheck
import Nostr.Event exposing (Kind(..), TagReference(..), emptyEventFilter)
import Nostr.External
import Nostr.Profile exposing (emptyProfile, eventFromProfile)
import Nostr.RelayListMetadata exposing (RelayMetadata, eventWithRelayList)
import Nostr.Request exposing (RequestData(..))
import Nostr.Send exposing (SendRequest(..))
import Nostr.Types exposing (IncomingMessage, LoginMethod(..), LoginStatus(..), PubKey, RelayRole(..), loggedInPubKey, loggedInSigningPubKey)
import Pareto
import Ports
import Process
import Route exposing (Route)
import Route.Path
import Shared.Model exposing (ClientRole(..))
import Shared.Msg exposing (Msg(..))
import Task
import Ui.Styles exposing (Theme(..))


type alias Model =
    Shared.Model.Model


contentId : String
contentId =
    "content-container"


-- FLAGS


type alias Flags =
    { darkMode : Bool
    , environment : Maybe String
    , imageCachingServer : String
    , imageCacheKey : String
    , locale : String
    , nativeSharingAvailable : Bool
    , testMode : Bool
    , authApiBaseUrl : String
    }


decoder : Json.Decode.Decoder Flags
decoder =
    Json.Decode.map8 Flags
        (Json.Decode.field "darkMode" Json.Decode.bool)
        (Json.Decode.field "environment" (Json.Decode.maybe Json.Decode.string))
        (Json.Decode.field "imageCachingServer" Json.Decode.string)
        (Json.Decode.oneOf
            [ Json.Decode.field "imageCacheKey" Json.Decode.string
            , Json.Decode.succeed ""
            ]
        )
        (Json.Decode.field "locale" Json.Decode.string)
        (Json.Decode.field "nativeSharingAvailable" Json.Decode.bool)
        (Json.Decode.field "testMode" Json.Decode.bool)
        (Json.Decode.field "authApiBaseUrl" Json.Decode.string)



-- INIT


init : Result Json.Decode.Error Flags -> Route () -> ( Model, Effect Msg )
init flagsResult _ =
    case flagsResult of
        Ok flags ->
            let
                ( browserEnv, browserEnvCmd ) =
                    BrowserEnv.init
                        { authApiBaseUrl = flags.authApiBaseUrl
                        , backendUrl = ""
                        , darkMode = flags.darkMode
                        , environment = flags.environment
                        , frontendUrl = ""
                        , imageCachingServer = flags.imageCachingServer
                        , imageCacheKey = flags.imageCacheKey
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
                    Nostr.init portHooks browserEnv.environment nostrTestMode Pareto.defaultRelays

                -- request bookmark list of Pareto creators
                -- as well as bookmark sets for different purposes
                ( nostr, nostrRequestCmd ) =
                    { emptyEventFilter | authors = Just [ Pareto.authorsKey, Pareto.rssAuthorsKey, Pareto.betaTestKey, Pareto.editorKey ], kinds = Just [ KindFollows, KindFollowSets, KindMuteList ] }
                        |> RequestFollowSets
                        |> Nostr.createRequest nostrInit "Follow list/sets of Pareto user" []
                        |> Nostr.doRequest nostrInit
            in
            ( { loginStatus = LoggedInUnknown
              , browserEnv = browserEnv
              , configCheck = ConfigCheck.init
              , nostr = nostr
              , role = ClientReader
              , theme = ParetoTheme
              , alertTimerMessage = AlertTimerMessage.init
              , authDialog = AuthDialog.init
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
                        { authApiBaseUrl = Pareto.authApiBaseUrl
                        , backendUrl = ""
                        , darkMode = False
                        , environment = Nothing
                        , frontendUrl = ""
                        , imageCachingServer = ""
                        , imageCacheKey = ""
                        , locale = ""
                        , nativeSharingAvailable = False
                        , testMode = False
                        }
            in
            ( { loginStatus = LoggedOut
              , browserEnv = browserEnv
              , configCheck = ConfigCheck.init
              , nostr = Nostr.empty
              , role = ClientReader
              , theme = ParetoTheme
              , alertTimerMessage = AlertTimerMessage.init
              , authDialog = AuthDialog.init
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
update route msg model =
    case msg of
        TriggerLogin ->
            ( { model | authDialog = AuthDialog.open model.authDialog }
            , Effect.sendCmd Ports.listIdentities
            )

        TriggerEmailLogin ->
            ( { model | authDialog = AuthDialog.openEmailLogin model.authDialog }
            , Effect.sendCmd Ports.listIdentities
            )

        AuthDialogMsg authDialogMsg ->
            let
                ( authDialog, cmd ) =
                    AuthDialog.update model.browserEnv authDialogMsg model.authDialog
            in
            ( { model | authDialog = authDialog }
            , Effect.sendCmd (Cmd.map Shared.Msg.AuthDialogMsg cmd)
            )

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

        DelayedCheckConfiguration ->
            ( model
            , Process.sleep 1000.0
                |> Task.perform CheckConfiguration
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

        ShowAlert alert ->
            update route (AlertSent (AlertTimerMessage.AddMessage alert 2000)) model

        AlertSent innerMsg ->
            let
                ( newModel, alertTimerMessageCmd ) =
                    AlertTimerMessage.update
                        { msg = innerMsg
                        , model = model.alertTimerMessage
                        , toModel = \alertTimerMessage -> { model | alertTimerMessage = alertTimerMessage }
                        , toMsg = AlertSent
                        }
            in
            ( newModel
            , alertTimerMessageCmd
                |> Effect.sendCmd
            )

        ScrollContentToTop ->
            ( model, Effect.sendCmd <| Task.attempt DomError (Browser.Dom.setViewportOf contentId 0 0) )

        DomError _ ->
            ( model, Effect.none )

        ChangeLocale locale ->
            update route (BrowserEnvMsg (BrowserEnv.UpdateLocale locale)) model


updateWithPortMessage : Model -> IncomingMessage -> ( Model, Effect Msg )
updateWithPortMessage model portMessage =
    let
        ( authDialog, authCmd ) =
            AuthDialog.update model.browserEnv (AuthDialog.PortMsg portMessage) model.authDialog

        modelWithAuth =
            { model | authDialog = authDialog }

        authEffect =
            Effect.sendCmd (Cmd.map Shared.Msg.AuthDialogMsg authCmd)
    in
    case portMessage.messageType of
        "user" ->
            let
                ( updatedModel, userEffect ) =
                    updateWithUserValue modelWithAuth portMessage.value
            in
            ( updatedModel, Effect.batch [ authEffect, userEffect ] )

        "loggedOut" ->
            ( { modelWithAuth | loginStatus = LoggedOut }
            , authEffect
            )

        "identities" ->
            let
                pubKeys =
                    AuthDialog.identityPubKeys modelWithAuth.authDialog

                ( nostr, profileCmd ) =
                    if List.isEmpty pubKeys then
                        ( modelWithAuth.nostr, Cmd.none )

                    else
                        { emptyEventFilter
                            | authors = Just pubKeys
                            , kinds = Just [ KindUserMetadata ]
                        }
                            |> RequestProfile Nothing
                            |> Nostr.createRequest modelWithAuth.nostr "Auth dialog identity profiles" []
                            |> Nostr.doRequest modelWithAuth.nostr
            in
            ( { modelWithAuth | nostr = nostr }
            , Effect.batch
                [ authEffect
                , Effect.sendCmd (Cmd.map Shared.Msg.NostrMsg profileCmd)
                ]
            )

        _ ->
            ( modelWithAuth, authEffect )


updateWithUserValue : Model -> Json.Decode.Value -> ( Model, Effect Msg )
updateWithUserValue model value =
    case
        ( Json.Decode.decodeValue pubkeyDecoder value
        , Json.Decode.decodeValue loginMethodDecoder value
        , model.loginStatus
        )
    of
        ( Ok pubKeyNew, Ok loginMethod, LoggedIn pubKeyLoggedIn _ ) ->
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
                        -- don't check for non-Pareto users
                        Cmd.none

                bootstrapEffect =
                    bootstrapEmailAccountEffect model.nostr pubKeyNew value
            in
            ( { model
                | loginStatus = LoggedIn pubKeyNew loginMethod
                , nostr = nostr
              }
            , Effect.batch
                [ [ cmdNostr
                        |> Cmd.map Shared.Msg.NostrMsg

                  -- check if user sends newsletters
                  , Nostr.loadUserDataByPubKey model.nostr pubKeyNew
                        |> Cmd.map Shared.Msg.NostrMsg
                  , startConfigCheckCmd
                  ]
                    |> Cmd.batch
                    |> Effect.sendCmd
                , bootstrapEffect
                ]
            )

        ( Ok pubKeyNew, Ok loginMethod, _ ) ->
            let
                ( nostr, cmd ) =
                    Nostr.requestUserData model.nostr pubKeyNew

                bootstrapEffect =
                    bootstrapEmailAccountEffect model.nostr pubKeyNew value
            in
            ( { model | loginStatus = LoggedIn pubKeyNew loginMethod, nostr = nostr }
            , Effect.batch
                [ [ cmd

                  -- check if user sends newsletters
                  , Nostr.loadUserDataByPubKey model.nostr pubKeyNew
                  ]
                    |> Cmd.batch
                    |> Cmd.map Shared.Msg.NostrMsg
                    |> Effect.sendCmd
                , bootstrapEffect
                ]
            )

        ( _, _, _ ) ->
            ( model, Effect.none )


bootstrapEmailAccountEffect : Nostr.Model -> PubKey -> Json.Decode.Value -> Effect Msg
bootstrapEmailAccountEffect nostr pubKey value =
    case Json.Decode.decodeValue bootstrapDecoder value of
        Ok { bootstrap, displayName } ->
            if not bootstrap then
                Effect.none

            else
                let
                    profile =
                        let
                            base =
                                emptyProfile pubKey
                        in
                        { base
                            | displayName = displayName
                            , name = displayName
                        }

                    relays : List RelayMetadata
                    relays =
                        List.map (\url -> { url = url, role = WriteRelay }) Pareto.recommendedOutboxRelays
                            ++ List.map (\url -> { url = url, role = ReadRelay }) Pareto.recommendedInboxRelays

                    relaysWithProtocol =
                        List.map (\relay -> { relay | url = "wss://" ++ relay.url }) relays

                    writeRelayUrls =
                        relays
                            |> List.filterMap
                                (\relay ->
                                    if relay.role == WriteRelay || relay.role == ReadWriteRelay then
                                        Just relay.url

                                    else
                                        Nothing
                                )

                    profileRelays =
                        Nostr.getWriteRelayUrlsForPubKey nostr pubKey
                in
                Effect.batch
                    [ eventFromProfile pubKey profile
                        |> SendProfile profileRelays
                        |> Shared.Msg.SendNostrEvent
                        |> Effect.sendSharedMsg
                    , eventWithRelayList pubKey relaysWithProtocol
                        |> SendRelayList writeRelayUrls
                        |> Shared.Msg.SendNostrEvent
                        |> Effect.sendSharedMsg
                    , SendFollowList pubKey Nostr.paretoAuthorsFollowList
                        |> Shared.Msg.SendNostrEvent
                        |> Effect.sendSharedMsg
                    , Effect.sendCmd (Ports.markBootstrapDone pubKey)
                    ]

        Err _ ->
            Effect.none


type alias BootstrapPayload =
    { bootstrap : Bool
    , displayName : Maybe String
    }


bootstrapDecoder : Json.Decode.Decoder BootstrapPayload
bootstrapDecoder =
    Json.Decode.map2 BootstrapPayload
        (Json.Decode.map (Maybe.withDefault False) (Json.Decode.maybe (Json.Decode.field "bootstrap" Json.Decode.bool)))
        (Json.Decode.maybe (Json.Decode.field "displayName" Json.Decode.string)
            |> Json.Decode.map (Maybe.andThen blankToNothing)
        )


blankToNothing : String -> Maybe String
blankToNothing value =
    case String.trim value of
        "" ->
            Nothing

        trimmed ->
            Just trimmed


createFollowersEffect : Nostr.Model -> Maybe PubKey -> Effect msg
createFollowersEffect nostr maybePubKey =
    let
        buildRequestEffect pk =
            { emptyEventFilter | kinds = Just [ KindFollows ], authors = Nothing, tagReferences = Just [ TagReferencePubKey pk ], limit = Nothing }
                |> RequestFollowSets
                |> Nostr.createRequest nostr "Followers of user" []
                |> Shared.Msg.RequestNostrEvents
                |> Effect.sendSharedMsg
    in
    maybePubKey
        |> Maybe.map buildRequestEffect
        |> Maybe.withDefault Effect.none


loggedIn : Model -> Bool
loggedIn model =
    loggedInPubKey model.loginStatus
    |> Maybe.map (\_ -> True)
    |> Maybe.withDefault False



pubkeyDecoder : Json.Decode.Decoder PubKey
pubkeyDecoder =
    Json.Decode.field "pubKey" Json.Decode.string


loginMethodDecoder : Json.Decode.Decoder LoginMethod
loginMethodDecoder =
    Json.Decode.field "method" Json.Decode.string
        |> Json.Decode.andThen
            (\method ->
                case String.toLower method of
                    "connect" ->
                        Json.Decode.succeed LoginMethodConnect

                    "extension" ->
                        Json.Decode.succeed LoginMethodExtension

                    "local" ->
                        Json.Decode.succeed LoginMethodLocal

                    "readonly" ->
                        Json.Decode.succeed LoginMethodReadOnly

                    other ->
                        Json.Decode.succeed (LoginMethodOther other)
            )



-- SUBSCRIPTIONS


subscriptions : Route () -> Model -> Sub Msg
subscriptions _ model =
    Sub.batch
        [ Ports.receiveMessage Shared.Msg.ReceivedPortMessage
        , Sub.map Shared.Msg.BrowserEnvMsg (BrowserEnv.subscriptions model.browserEnv)
        , Sub.map Shared.Msg.NostrMsg (Nostr.subscriptions model.nostr)
        ]
