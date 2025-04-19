module Nostr.ConfigCheck exposing (..)

import Http
import I18Next
import Nostr
import Nostr.Event exposing (Event, Kind(..), Tag(..))
import Nostr.Types exposing (PubKey, RelayUrl, ServerUrl)
import Pareto
import Translations.ConfigCheck as Translations
import Nostr.Relay exposing (RelayState(..))


type alias Model =
    { issues : List Issue
    }


type IssueType
    = ProfileIssue
    | MediaServerIssue
    | RelayIssue


type Issue
    = OutboxRelaysMissingIssue (List RelayUrl)
    | OfflineRelays (List RelayUrl)
    | MediaServersMissing (List ServerUrl)
    | UnreliableMediaServers (List ServerUrl)
    | ProfileMissing
    | ProfileNameMissing
    | ProfileDisplayNameMissing
    | ProfileAboutMissing
    | ProfileNip05Missing
    | ProfileNip05Invalid
    | ProfileAvatarMissing
    | ProfileBannerMissing
    | ProfileLud06Configured
    | ProfileLud16Missing
    | ProfileLud16InvalidForm
    | ProfileLud16Offline
    | ProfileLud16InvalidResponse
    | TestIssue


type alias IssueText =
    { message : String
    , explanation : String
    , solution : String
    }


type alias PerformLocalCheckFunction =
    Nostr.Model -> PubKey -> Maybe Issue


type alias PerformRemoteCheckFunction =
    Nostr.Model -> PubKey -> Maybe (Cmd Msg)


type Msg
    = NoOp


init : Model
init =
    { issues = []
    }


relayIssues : Model -> List Issue
relayIssues model =
    model.issues
        |> List.filter (\issue -> issueType issue == RelayIssue)


mediaServerIssues : Model -> List Issue
mediaServerIssues model =
    model.issues
        |> List.filter (\issue -> issueType issue == MediaServerIssue)


profileIssues : Model -> List Issue
profileIssues model =
    model.issues
        |> List.filter (\issue -> issueType issue == ProfileIssue)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


performChecks : Nostr.Model -> PubKey -> ( Model, Cmd Msg )
performChecks nostr pubKey =
    let
        localCheckResults =
            localCheckFunctions
                |> List.filterMap (\checkFunction -> checkFunction nostr pubKey)

        remoteCheckCmd =
            remoteCheckFunctions
                |> List.filterMap (\checkFunction -> checkFunction nostr pubKey)
                |> Cmd.batch
    in
    ( { issues = localCheckResults
      }
    , remoteCheckCmd
    )


issueText : I18Next.Translations -> Issue -> IssueText
issueText translations issue =
    case issue of
        OutboxRelaysMissingIssue relayUrls ->
            { message = Translations.outboxRelaysMissingText [ translations ]
            , explanation = Translations.outboxRelaysMissingExplanation [ translations ] { relays = relayUrls |> String.join ", " }
            , solution = ""
            }

        OfflineRelays relayUrls ->
            { message = Translations.relaysOfflineText [ translations ]
            , explanation = Translations.relaysOfflineExplanation [ translations ] { relays = relayUrls |> String.join ", " }
            , solution = ""
            }

        MediaServersMissing serverUrls ->
            { message = Translations.mediaServerMissingText [ translations ]
            , explanation = Translations.mediaServerMissingExplanation [ translations ] { servers = serverUrls |> String.join ", " }
            , solution = ""
            }

        UnreliableMediaServers serverUrls ->
            { message = Translations.unreliableMediaServersText [ translations ]
            , explanation = Translations.unreliableMediaServersExplanation [ translations ] { servers = serverUrls |> String.join ", " }
            , solution = ""
            }

        ProfileMissing ->
            { message = Translations.profileMissingText [ translations ]
            , explanation = Translations.profileMissingExplanation [ translations ]
            , solution = ""
            }

        ProfileNameMissing ->
            { message = Translations.profileNameMissingText [ translations ]
            , explanation = Translations.profileNameMissingExplanation [ translations ]
            , solution = ""
            }

        ProfileDisplayNameMissing ->
            { message = Translations.profileDisplayNameMissingText [ translations ]
            , explanation = Translations.profileDisplayNameMissingExplanation [ translations ]
            , solution = ""
            }

        ProfileAboutMissing ->
            { message = Translations.profileAboutTextMissingText [ translations ]
            , explanation = Translations.profileAboutTextMissingExplanation [ translations ]
            , solution = ""
            }

        ProfileNip05Missing ->
            { message = Translations.profileNip05MissingText [ translations ]
            , explanation = Translations.profileNip05MissingExplanation [ translations ]
            , solution = ""
            }

        ProfileNip05Invalid ->
            { message = Translations.profileNip05InvalidText [ translations ]
            , explanation = Translations.profileNip05InvalidExplanation [ translations ]
            , solution = ""
            }

        ProfileAvatarMissing ->
            { message = Translations.profileAvatarMissingText [ translations ]
            , explanation = Translations.profileAvatarMissingExplanation [ translations ]
            , solution = ""
            }

        ProfileBannerMissing ->
            { message = Translations.profileBannerMissingText [ translations ]
            , explanation = Translations.profileBannerMissingExplanation [ translations ]
            , solution = ""
            }

        ProfileLud06Configured ->
            { message = Translations.profileLud06ConfiguredText [ translations ]
            , explanation = Translations.profileLud06ConfiguredExplanation [ translations ]
            , solution = ""
            }

        ProfileLud16Missing ->
            { message = Translations.profileLud16MissingText [ translations ]
            , explanation = Translations.profileLud16MissingExplanation [ translations ]
            , solution = ""
            }

        ProfileLud16InvalidForm ->
            { message = Translations.profileLud16InvalidText [ translations ]
            , explanation = Translations.profileLud16InvalidExplanation [ translations ]
            , solution = ""
            }

        ProfileLud16Offline ->
            { message = Translations.profileLud16OfflineText [ translations ]
            , explanation = Translations.profileLud16OfflineExplanation [ translations ]
            , solution = ""
            }

        ProfileLud16InvalidResponse ->
            { message = Translations.profileLud16InvalidResponseText [ translations ]
            , explanation = Translations.profileLud16InvalidResponseExplanation [ translations ]
            , solution = ""
            }

        TestIssue ->
            { message = "Test message"
            , explanation = "Test explanation"
            , solution = "Test solution"
            }


issueType : Issue -> IssueType
issueType issue =
    case issue of
        OutboxRelaysMissingIssue _ ->
            RelayIssue

        OfflineRelays _ ->
            RelayIssue

        MediaServersMissing _ ->
            MediaServerIssue

        UnreliableMediaServers _ ->
            MediaServerIssue

        ProfileMissing ->
            ProfileIssue

        ProfileNameMissing ->
            ProfileIssue

        ProfileDisplayNameMissing ->
            ProfileIssue

        ProfileAboutMissing ->
            ProfileIssue

        ProfileNip05Missing ->
            ProfileIssue

        ProfileNip05Invalid ->
            ProfileIssue

        ProfileAvatarMissing ->
            ProfileIssue

        ProfileBannerMissing ->
            ProfileIssue

        ProfileLud06Configured ->
            ProfileIssue

        ProfileLud16Missing ->
            ProfileIssue

        ProfileLud16InvalidForm ->
            ProfileIssue

        ProfileLud16Offline ->
            ProfileIssue

        ProfileLud16InvalidResponse ->
            ProfileIssue

        TestIssue ->
            ProfileIssue


localCheckFunctions : List PerformLocalCheckFunction
localCheckFunctions =
    [ checkMissingOutboxRelays
    , checkOfflineRelays
    , checkMissingMediaServers
    , checkUnreliableMediaServers
    , checkMissingProfile
    , checkMissingProfileName
    , checkMissingNip05
    , checkMissingProfileAvatar
    , checkMissingProfileBanner

    -- , checkLocalTestFunction
    ]


remoteCheckFunctions : List PerformRemoteCheckFunction
remoteCheckFunctions =
    [ dummyRemoteCheckFunction
    ]


checkLocalTestFunction : PerformLocalCheckFunction
checkLocalTestFunction _ _ =
    Just TestIssue



-- media server checks


checkMissingMediaServers : PerformLocalCheckFunction
checkMissingMediaServers nostr pubKey =
    let
        nip96Servers =
            Nostr.getNip96Servers nostr pubKey

        missingNip96Servers =
            Pareto.defaultNip96ServersAuthors
                |> List.filter (\serverUrl -> not <| List.member serverUrl nip96Servers)
    in
    if List.length missingNip96Servers > 0 then
        Just (MediaServersMissing missingNip96Servers)

    else
        Nothing


checkUnreliableMediaServers : PerformLocalCheckFunction
checkUnreliableMediaServers nostr pubKey =
    let
        nip96Servers =
            Nostr.getNip96Servers nostr pubKey

        unreliableMediaServers =
            [ "https://void.cat"
            ]
                |> List.filter (\serverUrl -> List.member serverUrl nip96Servers)
    in
    if List.length unreliableMediaServers > 0 then
        Just (UnreliableMediaServers unreliableMediaServers)

    else
        Nothing


-- relay checks


checkMissingOutboxRelays : PerformLocalCheckFunction
checkMissingOutboxRelays nostr pubKey =
    let
        outboxRelays =
            Nostr.getWriteRelayUrlsForPubKey nostr pubKey

        missingOutboxRelays =
            Pareto.paretoOutboxRelays
                |> List.filter (\relayUrl -> not <| List.member relayUrl outboxRelays)
    in
    if List.length missingOutboxRelays > 0 then
        Just (OutboxRelaysMissingIssue missingOutboxRelays)

    else
        Nothing



checkOfflineRelays : PerformLocalCheckFunction
checkOfflineRelays nostr pubKey =
    let
        offlineRelays =
            Nostr.getRelaysForPubKey nostr pubKey
            |> List.filterMap (\(_, relay) ->
                case relay.state of
                    Nostr.Relay.RelayStateNip11RequestFailed _ ->
                        Just relay.urlWithoutProtocol

                    _ ->
                        Nothing
                )

    in
    if List.length offlineRelays > 0 then
        Just (OfflineRelays offlineRelays)

    else
        Nothing



-- profile checks


checkMissingProfile : PerformLocalCheckFunction
checkMissingProfile nostr pubKey =
    if Nostr.getProfile nostr pubKey == Nothing then
        Just ProfileMissing

    else
        Nothing


checkMissingProfileName : PerformLocalCheckFunction
checkMissingProfileName nostr pubKey =
    Nostr.getProfile nostr pubKey
        |> Maybe.andThen
            (\profile ->
                if profile.name == Nothing then
                    Just ProfileNameMissing

                else
                    Nothing
            )


checkMissingNip05 : PerformLocalCheckFunction
checkMissingNip05 nostr pubKey =
    Nostr.getProfile nostr pubKey
        |> Maybe.andThen
            (\profile ->
                if profile.nip05 == Nothing then
                    Just ProfileNip05Missing

                else
                    Nothing
            )


checkMissingProfileAvatar : PerformLocalCheckFunction
checkMissingProfileAvatar nostr pubKey =
    Nostr.getProfile nostr pubKey
        |> Maybe.andThen
            (\profile ->
                if profile.picture == Nothing then
                    Just ProfileAvatarMissing

                else
                    Nothing
            )


checkMissingProfileBanner : PerformLocalCheckFunction
checkMissingProfileBanner nostr pubKey =
    Nostr.getProfile nostr pubKey
        |> Maybe.andThen
            (\profile ->
                if profile.banner == Nothing then
                    Just ProfileBannerMissing

                else
                    Nothing
            )


dummyLocalCheckFunction : PerformLocalCheckFunction
dummyLocalCheckFunction nostr pubKey =
    Nothing


dummyRemoteCheckFunction : PerformRemoteCheckFunction
dummyRemoteCheckFunction nostr pubKey =
    Nothing
