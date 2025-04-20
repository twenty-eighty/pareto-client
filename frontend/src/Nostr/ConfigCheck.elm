module Nostr.ConfigCheck exposing (..)

import Http
import I18Next
import Nostr
import Nostr.Bech32 as Bech32
import Nostr.Event exposing (Kind(..), Tag(..))
import Nostr.Lud06 as Lud06
import Nostr.Lud16 as Lud16
import Nostr.Nip05 as Nip05 exposing (Nip05)
import Nostr.Types exposing (PubKey, RelayUrl, ServerUrl)
import Pareto
import Translations.ConfigCheck as Translations
import Nostr.Relay exposing (RelayState(..))
import Nostr.Shared exposing (httpErrorToString)
import Nostr.Profile exposing (ProfileValidation(..))
import Url exposing (Url)

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
    | ProfileNip05Missing (Maybe Nip05)
    | ProfileNip05NameMissing (Maybe Nip05)
    | ProfileNip05NotMatchingPubKey (Maybe Nip05)
    | ProfileNip05NetworkError Http.Error (Maybe Nip05)
    | ProfileAvatarMissing
    | ProfileAvatarNotUrl
    | ProfileAvatarError 
    | ProfileBannerMissing
    | ProfileBannerNotUrl
    | ProfileBannerError 
    | ProfileLud06Configured
    | ProfileLud06FormatError String
    | ProfileLud16Missing
    | ProfileLud16Whitespace
    | ProfileLud16InvalidForm
    | ProfileLud16BitcoinAddress
    | ProfileLud16Offline Http.Error
    | ProfileLud16CallbackOffline Http.Error
    | ProfileLud16InvalidResponse String


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
    | ReceivedLightningPaymentData (Result Http.Error Lud16.LightningPaymentData)
    | ReceivedLightningCallbackResponse (Result Http.Error ())


init : Model
init =
    { issues = []
    }


getIssues : Model -> List Issue
getIssues model =
    model.issues


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

        ReceivedLightningPaymentData (Ok lightningPaymentData) ->
            ( model
            , httpHeadRequest lightningPaymentData.callback ReceivedLightningCallbackResponse
             )

        ReceivedLightningPaymentData (Err (Http.BadBody error)) ->
            ( { model | issues = model.issues ++ [ ProfileLud16InvalidResponse error] }, Cmd.none )

        ReceivedLightningPaymentData (Err error) ->
            ( { model | issues = model.issues ++ [ ProfileLud16Offline error ] }, Cmd.none )

        ReceivedLightningCallbackResponse (Ok _) ->
            ( model, Cmd.none )

        ReceivedLightningCallbackResponse (Err error) ->
            ( { model | issues = model.issues ++ [ ProfileLud16CallbackOffline error ] }, Cmd.none )


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

        ProfileNip05Missing maybeNip05 ->
            { message = Translations.profileNip05MissingText [ translations ]
            , explanation = Translations.profileNip05MissingExplanation [ translations ]
            , solution =
                case maybeNip05 of
                    Just nip05 ->
                        Translations.profileNip05MissingSolution [ translations ] { nip05 = nip05 |> Nip05.nip05ToString }

                    Nothing ->
                        ""
            }

        ProfileNip05NameMissing maybeNip05 ->
            { message = Translations.profileNip05NameMissingText [ translations ]
            , explanation = Translations.profileNip05NameMissingExplanation [ translations ]
            , solution =
                case maybeNip05 of
                    Just nip05 ->
                        Translations.profileNip05NameMissingSolution [ translations ] { nip05 = nip05 |> Nip05.nip05ToString }

                    Nothing ->
                        ""
            }

        ProfileNip05NotMatchingPubKey maybeNip05 ->
            { message = Translations.profileNip05NotMatchingPubkeyText [ translations ]
            , explanation = Translations.profileNip05NotMatchingPubkeyExplanation [ translations ]
            , solution =
                case maybeNip05 of
                    Just nip05 ->
                        Translations.profileNip05NotMatchingPubkeySolution [ translations ] { nip05 = nip05 |> Nip05.nip05ToString }

                    Nothing ->
                        ""
            }

        ProfileNip05NetworkError httpError maybeNip05 ->
            { message = Translations.profileNip05NetworkErrorText [ translations ]
            , explanation = Translations.profileNip05NetworkErrorExplanation [ translations ] { error = httpErrorToString httpError }
            , solution =
                case maybeNip05 of
                    Just nip05 ->
                        Translations.profileNip05NetworkErrorSolution [ translations ] { nip05 = nip05 |> Nip05.nip05ToString }

                    Nothing ->
                        ""
            }

        ProfileAvatarMissing ->
            { message = Translations.profileAvatarMissingText [ translations ]
            , explanation = Translations.profileAvatarMissingExplanation [ translations ]
            , solution = ""
            }

        ProfileAvatarNotUrl ->
            { message = Translations.profileAvatarNotUrlText [ translations ]
            , explanation = Translations.profileAvatarNotUrlExplanation [ translations ]
            , solution = ""
            }

        ProfileBannerMissing ->
            { message = Translations.profileBannerMissingText [ translations ]
            , explanation = Translations.profileBannerMissingExplanation [ translations ]
            , solution = ""
            }

        ProfileBannerNotUrl ->
            { message = Translations.profileBannerNotUrlText [ translations ]
            , explanation = Translations.profileBannerNotUrlExplanation [ translations ]
            , solution = ""
            }

        ProfileLud06Configured ->
            { message = Translations.profileLud06ConfiguredText [ translations ]
            , explanation = Translations.profileLud06ConfiguredExplanation [ translations ]
            , solution = ""
            }

        ProfileLud06FormatError error ->
            { message = Translations.profileLud06FormatErrorText [ translations ]
            , explanation = Translations.profileLud06FormatErrorExplanation [ translations ] { error = error }
            , solution = ""
            }

        ProfileLud16Missing ->
            { message = Translations.profileLud16MissingText [ translations ]
            , explanation = Translations.profileLud16MissingExplanation [ translations ]
            , solution = ""
            }

        ProfileLud16Whitespace ->
            { message = Translations.profileLud16WhitespaceText [ translations ]
            , explanation = Translations.profileLud16WhitespaceExplanation [ translations ]
            , solution = ""
            }

        ProfileLud16InvalidForm ->
            { message = Translations.profileLud16InvalidText [ translations ]
            , explanation = Translations.profileLud16InvalidExplanation [ translations ]
            , solution = Translations.profileLud16InvalidSolution [ translations ]
            }

        ProfileLud16BitcoinAddress ->
            { message = Translations.profileLud16BitcoinAddressText [ translations ]
            , explanation = Translations.profileLud16BitcoinAddressExplanation [ translations ]
            , solution = Translations.profileLud16BitcoinAddressSolution [ translations ]
            }

        ProfileLud16Offline error ->
            { message = Translations.profileLud16OfflineText [ translations ]
            , explanation = Translations.profileLud16OfflineExplanation [ translations ] { error = httpErrorToString error }
            , solution = ""
            }

        ProfileLud16CallbackOffline error ->
            { message = Translations.profileLud16CallbackOfflineText [ translations ]
            , explanation = Translations.profileLud16CallbackOfflineExplanation [ translations ] { error = httpErrorToString error }
            , solution = ""
            }

        ProfileLud16InvalidResponse error ->
            { message = Translations.profileLud16InvalidResponseText [ translations ]
            , explanation = Translations.profileLud16InvalidResponseExplanation [ translations ] { error = error }
            , solution = ""
            }

        ProfileAvatarError ->
            { message = Translations.profileAvatarErrorText [ translations ]
            , explanation = Translations.profileAvatarErrorExplanation [ translations ]
            , solution = ""
            }

        ProfileBannerError ->
            { message = Translations.profileBannerErrorText [ translations ]
            , explanation = Translations.profileBannerErrorExplanation [ translations ]
            , solution = ""
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

        ProfileNip05Missing _ ->
            ProfileIssue

        ProfileNip05NameMissing _ ->
            ProfileIssue

        ProfileNip05NotMatchingPubKey _ ->
            ProfileIssue

        ProfileNip05NetworkError _ _ ->
            ProfileIssue

        ProfileAvatarMissing ->
            ProfileIssue

        ProfileAvatarNotUrl ->
            ProfileIssue

        ProfileBannerMissing ->
            ProfileIssue

        ProfileBannerNotUrl ->
            ProfileIssue

        ProfileLud06Configured ->
            ProfileIssue

        ProfileLud06FormatError _ ->
            ProfileIssue

        ProfileLud16Missing ->
            ProfileIssue

        ProfileLud16Whitespace ->
            ProfileIssue

        ProfileLud16InvalidForm ->
            ProfileIssue

        ProfileLud16BitcoinAddress ->
            ProfileIssue

        ProfileLud16Offline _ ->
            ProfileIssue

        ProfileLud16CallbackOffline _ ->
            ProfileIssue

        ProfileLud16InvalidResponse _ ->
            ProfileIssue

        ProfileAvatarError ->
            ProfileIssue

        ProfileBannerError ->
            ProfileIssue


localCheckFunctions : List PerformLocalCheckFunction
localCheckFunctions =
    [ checkMissingOutboxRelays
    , checkOfflineRelays
    , checkMissingMediaServers
    , checkUnreliableMediaServers
    , checkMissingProfile
    , checkMissingProfileName
    , checkMissingProfileDisplayName
    , checkMissingProfileAbout
    , checkMissingProfileNip05
    , checkInvalidProfileNip05
    , checkProfileAvatarStatic
    , checkProfileBannerStatic
    -- , checkLud06Configured
    , checkLud06Format
    , checkMissingLud16
    , checkMalformedLud16
    ]


remoteCheckFunctions : List PerformRemoteCheckFunction
remoteCheckFunctions =
    [ checkLud16Response
    ]



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


checkMissingProfileDisplayName : PerformLocalCheckFunction
checkMissingProfileDisplayName nostr pubKey =
    Nostr.getProfile nostr pubKey
        |> Maybe.andThen
            (\profile ->
                if profile.displayName == Nothing then
                    Just ProfileDisplayNameMissing

                else
                    Nothing
            )


checkMissingProfileAbout : PerformLocalCheckFunction
checkMissingProfileAbout nostr pubKey =
    Nostr.getProfile nostr pubKey
        |> Maybe.andThen
            (\profile ->
                if profile.about == Nothing then
                    Just ProfileAboutMissing

                else
                    Nothing
            )


checkMissingProfileNip05 : PerformLocalCheckFunction
checkMissingProfileNip05 nostr pubKey =
    Nostr.getProfile nostr pubKey
        |> Maybe.andThen
            (\profile ->
                let
                    portalUserInfo =
                        Nostr.getPortalUserInfo nostr pubKey
                in
                if profile.nip05 == Nothing then
                    Just (ProfileNip05Missing (portalUserInfo |> Maybe.andThen .nip05))

                else
                    Nothing
            )


checkInvalidProfileNip05 : PerformLocalCheckFunction
checkInvalidProfileNip05 nostr pubKey =
    case Nostr.getProfileValidationStatus nostr pubKey of
        Just ValidationNameMissing ->
            Just (ProfileNip05NameMissing (Nostr.getPortalUserInfo nostr pubKey |> Maybe.andThen .nip05))

        Just ValidationNotMatchingPubKey ->
            Just (ProfileNip05NotMatchingPubKey (Nostr.getPortalUserInfo nostr pubKey |> Maybe.andThen .nip05))

        Just (ValidationNetworkError httpError) ->
            let
                portalUserInfo =
                    Nostr.getPortalUserInfo nostr pubKey
            in
            Just (ProfileNip05NetworkError httpError (portalUserInfo |> Maybe.andThen .nip05))

        _ ->
            Nothing




checkProfileAvatarStatic : PerformLocalCheckFunction
checkProfileAvatarStatic nostr pubKey =
    Nostr.getProfile nostr pubKey
        |> Maybe.andThen
            (\profile ->
                case profile.picture of
                    Just pictureUrl ->
                        if Url.fromString pictureUrl == Nothing then
                            Just ProfileAvatarNotUrl

                        else
                            Nothing

                    Nothing ->
                        Just ProfileAvatarNotUrl
            )


checkProfileBannerStatic : PerformLocalCheckFunction
checkProfileBannerStatic nostr pubKey =
    Nostr.getProfile nostr pubKey
        |> Maybe.andThen
            (\profile ->
                case profile.banner of
                    Just bannerUrl ->
                        if Url.fromString bannerUrl == Nothing then
                            Just ProfileBannerNotUrl

                        else
                            Nothing

                    Nothing ->
                        Just ProfileBannerMissing
            )


checkLud06Configured : PerformLocalCheckFunction
checkLud06Configured nostr pubKey =
    Nostr.getProfile nostr pubKey
        |> Maybe.andThen
            (\profile ->
                if profile.lud06 /= Nothing then
                    Just ProfileLud06Configured

                else
                    Nothing
            )


checkLud06Format : PerformLocalCheckFunction
checkLud06Format nostr pubKey =
    Nostr.getProfile nostr pubKey
        |> Maybe.andThen
            (\profile ->
                profile.lud06
                |> Maybe.andThen (\lud06 ->
                    case Lud06.parseLud06 lud06 of
                        Ok _ ->
                            Nothing

                        Err error ->
                            Just (ProfileLud06FormatError error)
                )
            )


checkMissingLud16 : PerformLocalCheckFunction
checkMissingLud16 nostr pubKey =
    Nostr.getProfile nostr pubKey
        |> Maybe.andThen
            (\profile ->
                case profile.lud16 of
                    Just lud16 ->
                        if String.trim lud16 /= lud16 then
                            Just ProfileLud16Whitespace

                        else
                            Nothing

                    Nothing ->
                        Just ProfileLud16Missing
            )

checkMalformedLud16 : PerformLocalCheckFunction
checkMalformedLud16 nostr pubKey =
    Nostr.getProfile nostr pubKey
        |> Maybe.andThen
            (\profile ->
                profile.lud16
                |> Maybe.andThen (\lud16 ->
                        if String.startsWith "bc1" lud16 then
                            Just ProfileLud16BitcoinAddress
                        else if Lud16.parseLud16 lud16 == Nothing then
                            Just ProfileLud16InvalidForm

                        else
                            Nothing
                )
            )


checkLud16Response : PerformRemoteCheckFunction
checkLud16Response nostr pubKey =
    Nostr.getProfile nostr pubKey
        |> Maybe.andThen
            (\profile ->
                profile.lud16
                |> Maybe.andThen (\lud16String ->
                        Lud16.parseLud16 lud16String 
                            |> Maybe.map (\lud16 ->
                                Lud16.requestLightningPaymentData ReceivedLightningPaymentData lud16
                            )
                )
            )
        

httpHeadRequest : Url -> (Result Http.Error () -> Msg) -> Cmd Msg
httpHeadRequest url onResult =
    Http.request
        { method = "HEAD"
        ,url = Url.toString url
        , expect = Http.expectWhatever onResult
        , headers = []
        , body = Http.emptyBody
        , timeout = Nothing
                        , tracker = Nothing
                        }
