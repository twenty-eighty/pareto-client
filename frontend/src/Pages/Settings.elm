module Pages.Settings exposing (Model, Msg, page)

import Auth
import Nostr.ConfigCheck as ConfigCheck
import Nostr.External
import Ports
import BrowserEnv exposing (BrowserEnv)
import Components.Button as Button
import Components.Categories as Categories
import Components.EntryField as EntryField
import Components.Icon as Icon
import Components.MediaSelector as MediaSelector
import Css
import Dict
import Effect exposing (Effect)
import FeatherIcons
import Html.Styled as Html exposing (Html, datalist, div, h3, input, option, p, text)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events as Events exposing (..)
import I18Next
import Json.Decode as Decode
import Layouts
import Layouts.Sidebar
import Nostr
import Nostr.Blossom exposing (eventWithBlossomServerList)
import Nostr.ConfigCheck as ConfigCheck
import Nostr.Event exposing (Kind(..), emptyEventFilter)
import Nostr.Lud16 as Lud16
import Nostr.Nip05 as Nip05
import Nostr.Nip96 as Nip96 exposing (eventWithNip96ServerList)
import Nostr.Profile exposing (Profile, ProfileValidation(..), emptyProfile, eventFromProfile, profilesEqual, profileFromEvent)
import Nostr.Relay as Relay exposing (Relay, RelayState(..), hostWithoutProtocol)
import Nostr.RelayListMetadata exposing (RelayMetadata, eventWithRelayList, extendRelayList, removeFromRelayList)
import Nostr.Request exposing (RequestData(..))
import Nostr.Send exposing (SendRequest(..), SendRequestId)
import Nostr.Types exposing (IncomingMessage, PubKey, RelayRole(..), RelayUrl, ServerUrl)
import Page exposing (Page)
import Pareto
import Route exposing (Route)
import Route.Path
import Shared
import Shared.Msg
import Tailwind.Breakpoints as Bp
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw
import Translations.Settings as Translations
import Ui.Profile exposing (FollowType(..))
import Ui.Relay exposing (viewRelayImage)
import Ui.Shared exposing (countBadge, emptyHtml, viewConfigIssues)
import Ui.Styles exposing (Theme(..), darkMode, stylesForTheme)
import Url
import View exposing (View)


page : Auth.User -> Shared.Model -> Route () -> Page Model Msg
page user shared route =
    Page.new
        { init = init user shared route
        , update = update user shared
        , subscriptions = subscriptions
        , view = view user shared
        }
        |> Page.withLayout (toLayout shared)


toLayout : Shared.Model -> Model -> Layouts.Layout Msg
toLayout shared model =
    let
        configCheckIssues =
            { profileIssues = profileIssues model shared.configCheck
            , relaysIssues = ConfigCheck.relayIssues shared.configCheck
            , mediaServersIssues = ConfigCheck.mediaServerIssues shared.configCheck
            }

        topPart =
            Categories.new
                { model = model.categories
                , toMsg = CategoriesSent
                , onSelect = CategorySelected
                , equals = (==)
                , image = \_ -> Nothing
                , categories = availableCategories shared.browserEnv.translations configCheckIssues
                , browserEnv = shared.browserEnv
                , theme = shared.theme
                }
                |> Categories.view
    in
    Layouts.Sidebar.new
        { theme = shared.theme
        }
        |> Layouts.Sidebar.withTopPart topPart Categories.heightString
        |> Layouts.Sidebar



-- INIT


type alias Model =
    { categories : Categories.Model Category
    , data : DataModel
    , path : Route.Path.Path
    }


type DataModel
    = RelaysData RelaysModel
    | MediaServersData MediaServersModel
    | ProfileData ProfileModel


type alias RelaysModel =
    { outboxRelay : Maybe String
    , inboxRelay : Maybe String
    , searchRelay : Maybe String
    , state : RelayListState
    }

type RelayListState
    = RelayListStateEditing
    | RelayListStateSaving SendRequestId



type alias MediaServersModel =
    { nip96Server : Maybe String
    , blossomServer : Maybe String
    , state : MediaServerState
    }

type MediaServerState
    = MediaServerStateEditing
    | MediaServerStateSavingNip96 SendRequestId
    | MediaServerStateSavingBlossom SendRequestId


type alias ProfileModel =
    { nip05 : String
    , lud06 : String
    , lud16 : String
    , name : String
    , displayName : String
    , about : String
    , picture : String
    , banner : String
    , website : String
    , bot : Bool
    , savedProfile : Maybe Profile
    , mediaSelector : MediaSelector.Model
    , state : EditState
    , pictureIssue : Maybe ConfigCheck.Issue
    , bannerIssue : Maybe ConfigCheck.Issue
    }

type EditState
    = EditStateEditing
    | EditStateSaving SendRequestId


type ImageUploadType
    = ImagePicture
    | ImageBanner


profileModelFromProfile : Auth.User -> Shared.Model -> Profile -> ( ProfileModel, Effect Msg )
profileModelFromProfile user shared profile =
    let
        ( mediaSelector, mediaSelectorEffect ) =
            MediaSelector.init
                { selected = Nothing
                , toMsg = MediaSelectorSent
                , blossomServers = Nostr.getBlossomServers shared.nostr user.pubKey
                , nip96Servers = Nostr.getNip96Servers shared.nostr user.pubKey
                , displayType = MediaSelector.DisplayModalDialog False
                }
    in
    ( { nip05 = profile.nip05 |> Maybe.map Nip05.nip05ToString |> Maybe.withDefault ""
      , lud06 = profile.lud06 |> Maybe.withDefault ""
      , lud16 = profile.lud16 |> Maybe.withDefault ""
      , name = profile.name |> Maybe.withDefault ""
      , displayName = profile.displayName |> Maybe.withDefault ""
      , about = profile.about |> Maybe.withDefault ""
      , picture = profile.picture |> Maybe.withDefault ""
      , banner = profile.banner |> Maybe.withDefault ""
      , website = profile.website |> Maybe.withDefault ""
      , bot = profile.bot |> Maybe.withDefault False
      , savedProfile = Just profile
      , mediaSelector = mediaSelector
      , state = EditStateEditing
      , pictureIssue = Nothing
      , bannerIssue = Nothing
      }
    , mediaSelectorEffect
    )


profileFromProfileModel : PubKey -> ProfileModel -> Profile
profileFromProfileModel pubKey profileModel =
    { nip05 = Nip05.parseNip05 profileModel.nip05
    , lud06 = stringToMaybe profileModel.lud06
    , lud16 = stringToMaybe profileModel.lud16
    , name = stringToMaybe profileModel.name
    , displayName = stringToMaybe profileModel.displayName
    , about = stringToMaybe profileModel.about
    , picture = stringToMaybe profileModel.picture
    , banner = stringToMaybe profileModel.banner
    , website = stringToMaybe profileModel.website
    , bot = boolToMaybe profileModel.bot
    , npub = Nothing
    , createdAt = Nothing
    , pubKey = pubKey
    , identities = []
    , relays = []
    }


stringToMaybe : String -> Maybe String
stringToMaybe value =
    if value /= "" then
        Just value

    else
        Nothing


boolToMaybe : Bool -> Maybe Bool
boolToMaybe value =
    if value then
        Just value

    else
        Nothing


emptyRelaysModel : RelaysModel
emptyRelaysModel =
    { outboxRelay = Nothing
    , inboxRelay = Nothing
    , searchRelay = Nothing
    , state = RelayListStateEditing
    }


emptyMediaServersModel : MediaServersModel
emptyMediaServersModel =
    { nip96Server = Nothing
    , blossomServer = Nothing
    , state = MediaServerStateEditing
    }


emptyProfileModel : Auth.User -> Shared.Model -> ( ProfileModel, Effect Msg )
emptyProfileModel user shared =
    let
        ( mediaSelector, mediaSelectorEffect ) =
            MediaSelector.init
                { selected = Nothing
                , toMsg = MediaSelectorSent
                , blossomServers = Nostr.getBlossomServers shared.nostr user.pubKey
                , nip96Servers = Nostr.getNip96Servers shared.nostr user.pubKey
                , displayType = MediaSelector.DisplayModalDialog False
                }
    in
    ( { nip05 = ""
      , lud06 = ""
      , lud16 = ""
      , name = ""
      , displayName = ""
      , about = ""
      , picture = ""
      , banner = ""
      , website = ""
      , bot = False
      , savedProfile = Nothing
      , mediaSelector = mediaSelector
      , state = EditStateEditing
      , pictureIssue = Nothing
      , bannerIssue = Nothing
      }
    , mediaSelectorEffect
    )


type Category
    = Relays
    | MediaServers
    | Profile


availableCategories : I18Next.Translations -> ConfigCheckIssues -> List (Categories.CategoryData Category)
availableCategories translations configCheckIssues =
    let
        relaysIssuesCount =
            configCheckIssues.relaysIssues
                |> List.length

        relaysIssuesSuffix =
            if relaysIssuesCount > 0 then
                "\u{00A0}" ++ countBadge relaysIssuesCount 

            else
                ""

        mediaServersIssuesCount =
            configCheckIssues.mediaServersIssues
                |> List.length

        mediaServersIssuesSuffix =
            if mediaServersIssuesCount > 0 then
                "\u{00A0}" ++ countBadge mediaServersIssuesCount

            else
                ""
                
        profileIssuesCount =
            configCheckIssues.profileIssues
                |> List.length

        profileIssuesSuffix =
            if profileIssuesCount > 0 then
                "\u{00A0}" ++ countBadge profileIssuesCount

            else
                ""
                
    in
    [ { category = Relays
      , title = Translations.relaysCategory [ translations ] ++ relaysIssuesSuffix
      }
    , { category = MediaServers
      , title = Translations.mediaServersCategory [ translations ] ++ mediaServersIssuesSuffix
      }
    , { category = Profile
      , title = Translations.profileCategory [ translations ] ++ profileIssuesSuffix
      }
    ]


init : Auth.User -> Shared.Model -> Route () -> () -> ( Model, Effect Msg )
init user shared route () =
    let
        category =
            Dict.get categoryParamName route.query
                |> Maybe.andThen categoryFromString
                |> Maybe.withDefault Relays
    in
    updateModelWithCategory
        user
        shared
        { categories = Categories.init { selected = category }
        , data = RelaysData emptyRelaysModel
        , path = route.path
        }
        category


categoryParamName : String
categoryParamName =
    "category"


stringFromCategory : Category -> String
stringFromCategory category =
    case category of
        Relays ->
            "relays"

        MediaServers ->
            "media_servers"

        Profile ->
            "profile"


categoryFromString : String -> Maybe Category
categoryFromString categoryString =
    case categoryString of
        "relays" ->
            Just Relays

        "media_servers" ->
            Just MediaServers

        "profile" ->
            Just Profile

        _ ->
            Nothing



-- UPDATE


type Msg
    = CategorySelected Category
    | CategoriesSent (Categories.Msg Category Msg)
    | UpdateRelayModel RelaysModel
    | AddOutboxRelay PubKey RelayUrl
    | AddInboxRelay PubKey RelayUrl
    | AddSearchRelay PubKey RelayUrl
    | AddDefaultOutboxRelays (List RelayUrl)
    | AddDefaultInboxRelays (List RelayUrl)
    | RemoveRelay PubKey RelayRole RelayUrl
    | UpdateMediaServerModel MediaServersModel
    | AddNip96MediaServer PubKey ServerUrl
    | RemoveNip96MediaServer PubKey ServerUrl
    | AddDefaultNip96MediaServers PubKey (List ServerUrl)
    | AddBlossomMediaServer PubKey ServerUrl
    | RemoveBlossomMediaServer PubKey ServerUrl
    | UpdateProfileModel ProfileModel
    | OpenImageSelection ImageUploadType
    | MediaSelectorSent (MediaSelector.Msg Msg)
    | ImageSelected MediaSelector.UploadedFile
    | SaveProfile Profile
    | CreateProfile
    | ReceivedPortMessage IncomingMessage
    | PictureLoaded Bool
    | BannerLoaded Bool


update : Auth.User -> Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update user shared msg model =
    case msg of
        CategorySelected category ->
            updateModelWithCategory user shared model category

        CategoriesSent innerMsg ->
            Categories.update
                { msg = innerMsg
                , model = model.categories
                , toModel = \categories -> { model | categories = categories }
                , toMsg = CategoriesSent
                }

        UpdateRelayModel relaysModel ->
            ( { model | data = RelaysData relaysModel }, Effect.none )

        AddOutboxRelay pubKey relayUrl ->
            ( { model | data = RelaysData { emptyRelaysModel | state = RelayListStateSaving (Nostr.getLastSendRequestId shared.nostr) } }
            , Nostr.getRelayListForPubKey shared.nostr pubKey
                |> extendRelayList (relayListWithRole [ relayUrl ] WriteRelay)
                |> sendRelayListCmd pubKey
            )

        AddInboxRelay pubKey relayUrl ->
            ( { model | data = RelaysData { emptyRelaysModel | state = RelayListStateSaving (Nostr.getLastSendRequestId shared.nostr) } }
            , Nostr.getRelayListForPubKey shared.nostr pubKey
                |> extendRelayList (relayListWithRole [ relayUrl ] ReadRelay)
                |> sendRelayListCmd pubKey
            )

        AddSearchRelay _ _ ->
            ( model, Effect.none )

        AddDefaultOutboxRelays relayUrls ->
            ( { model | data = RelaysData { emptyRelaysModel | state = RelayListStateSaving (Nostr.getLastSendRequestId shared.nostr) } }
            , Nostr.getRelayListForPubKey shared.nostr user.pubKey
                |> extendRelayList (relayListWithRole relayUrls WriteRelay)
                |> sendRelayListCmd user.pubKey
            )

        AddDefaultInboxRelays relayUrls ->
            ( { model | data = RelaysData { emptyRelaysModel | state = RelayListStateSaving (Nostr.getLastSendRequestId shared.nostr) } }
            , Nostr.getRelayListForPubKey shared.nostr user.pubKey
                |> extendRelayList (relayListWithRole relayUrls ReadRelay)
                |> sendRelayListCmd user.pubKey
            )

        RemoveRelay pubKey relayRole relayUrl ->
            ( { model | data = RelaysData { emptyRelaysModel | state = RelayListStateSaving (Nostr.getLastSendRequestId shared.nostr) } }
            , Nostr.getRelayListForPubKey shared.nostr pubKey
                |> removeFromRelayList { url = relayUrl, role = relayRole }
                |> sendRelayListCmd pubKey
            )

        UpdateMediaServerModel mediaServersModel ->
            ( { model | data = MediaServersData mediaServersModel }, Effect.none )

        AddNip96MediaServer pubKey mediaServer ->
            ( { model | data = MediaServersData { emptyMediaServersModel | state = MediaServerStateSavingNip96 (Nostr.getLastSendRequestId shared.nostr) } }
            , Nostr.getNip96Servers shared.nostr pubKey
                |> extendMediaServerList mediaServer
                |> sendNip96MediaServerListCmd shared.browserEnv pubKey
            )

        RemoveNip96MediaServer pubKey mediaServer ->
            ( { model | data = MediaServersData { emptyMediaServersModel | state = MediaServerStateSavingNip96 (Nostr.getLastSendRequestId shared.nostr) } }
            , Nostr.getNip96Servers shared.nostr pubKey
                |> removeMediaServerFromList mediaServer
                |> sendNip96MediaServerListCmd shared.browserEnv pubKey
            )

        AddDefaultNip96MediaServers pubKey mediaServers ->
            ( { model | data = MediaServersData { emptyMediaServersModel | state = MediaServerStateSavingNip96 (Nostr.getLastSendRequestId shared.nostr) } }
            , mediaServers
                |> sendNip96MediaServerListCmd shared.browserEnv pubKey
            )

        AddBlossomMediaServer pubKey mediaServer ->
            ( { model | data = MediaServersData { emptyMediaServersModel | state = MediaServerStateSavingBlossom (Nostr.getLastSendRequestId shared.nostr) } }
            , Nostr.getBlossomServers shared.nostr pubKey
                |> extendMediaServerList mediaServer
                |> sendBlossomMediaServerListCmd shared.browserEnv pubKey
            )

        RemoveBlossomMediaServer pubKey mediaServer ->
            ( { model | data = MediaServersData { emptyMediaServersModel | state = MediaServerStateSavingBlossom (Nostr.getLastSendRequestId shared.nostr) } }
            , Nostr.getBlossomServers shared.nostr pubKey
                |> removeMediaServerFromList mediaServer
                |> sendBlossomMediaServerListCmd shared.browserEnv pubKey
            )

        UpdateProfileModel profileModel ->
            ( { model | data = ProfileData profileModel }, Effect.none )

        OpenImageSelection imageUploadType ->
            case model.data of
                ProfileData profileModel ->
                    let
                        mediaType =
                            case imageUploadType of
                                ImagePicture ->
                                    Nip96.MediaTypeAvatar

                                ImageBanner ->
                                    Nip96.MediaTypeBanner
                    in
                    ( { model
                        | data =
                            ProfileData
                                { profileModel
                                    | mediaSelector =
                                        profileModel.mediaSelector
                                            |> MediaSelector.withMediaType mediaType
                                            |> MediaSelector.show
                                }
                      }
                    , Effect.none
                    )

                _ ->
                    ( model, Effect.none )

        MediaSelectorSent innerMsg ->
            case model.data of
                ProfileData profileModel ->
                    MediaSelector.update
                        { pubKey = user.pubKey
                        , nostr = shared.nostr
                        , msg = innerMsg
                        , model = profileModel.mediaSelector
                        , toModel =
                            \mediaSelector ->
                                let
                                    data =
                                        ProfileData { profileModel | mediaSelector = mediaSelector }
                                in
                                { model | data = data }
                        , toMsg = MediaSelectorSent
                        , browserEnv = shared.browserEnv
                        }

                _ ->
                    ( model, Effect.none )

        ImageSelected uploadedFile ->
            case model.data of
                ProfileData profileModel ->
                    let
                        url =
                            case uploadedFile of
                                MediaSelector.BlossomFile blobDescriptor ->
                                    blobDescriptor.url

                                MediaSelector.Nip96File fileMetadata ->
                                    fileMetadata.url |> Maybe.withDefault ""

                        data =
                            -- it's not ideal to misuse the media type to know which of the
                            -- profile images is to be selected but since we want to enforce
                            -- the media type when uploading it's convenient
                            case MediaSelector.getMediaType profileModel.mediaSelector of
                                Just Nip96.MediaTypeAvatar ->
                                    ProfileData { profileModel | picture = url }

                                Just Nip96.MediaTypeBanner ->
                                    ProfileData { profileModel | banner = url }

                                Nothing ->
                                    -- this case shouldn't be used
                                    ProfileData profileModel
                    in
                    ( { model | data = data }, Effect.none )

                _ ->
                    ( model, Effect.none )

        SaveProfile profile ->
            case model.data of
                ProfileData profileModel ->
                    ( { model | data = ProfileData { profileModel | state = EditStateSaving (Nostr.getLastSendRequestId shared.nostr) } }
                    , eventFromProfile profile.pubKey profile
                        |> SendProfile (Nostr.getWriteRelayUrlsForPubKey shared.nostr profile.pubKey)
                        |> Shared.Msg.SendNostrEvent
                        |> Effect.sendSharedMsg
                    )

                _ ->
                    ( model, Effect.none )

        CreateProfile ->
            case model.data of
                ProfileData profileModel ->
                    let
                        portalUserData =
                            Nostr.getPortalUserInfo shared.nostr user.pubKey
                    in
                    ( { model
                        | data =
                            ProfileData
                                { profileModel
                                  -- preset new profile with data received from portal server
                                    | name = portalUserData |> Maybe.andThen .username |> Maybe.withDefault ""
                                    , nip05 = portalUserData |> Maybe.andThen .nip05 |> Maybe.map Nip05.nip05ToString |> Maybe.withDefault ""
                                    , lud16 = portalUserData |> Maybe.andThen .lud16 |> Maybe.map Lud16.lud16ToString |> Maybe.withDefault ""
                                    , savedProfile = Just <| emptyProfile user.pubKey
                                }
                      }
                    , Effect.none
                    )

                _ ->
                    ( model, Effect.none )

        ReceivedPortMessage message ->
            updateWithPortMessage model message

        PictureLoaded isLoaded ->
            case model.data of
                ProfileData profileModel ->
                    let
                        pictureIssue =
                            if isLoaded then
                                Nothing
                            else
                                Just ConfigCheck.ProfileAvatarError
                    in
                    ( { model | data = ProfileData { profileModel | pictureIssue = pictureIssue } }, Effect.none )

                _ ->
                    ( model, Effect.none )

        BannerLoaded isLoaded ->
            case model.data of
                ProfileData profileModel ->
                    let
                        bannerIssue =
                            if isLoaded then
                                Nothing
                            else
                                Just ConfigCheck.ProfileBannerError
                    in
                    ( { model | data = ProfileData { profileModel | bannerIssue = bannerIssue } }, Effect.none )

                _ ->
                    ( model, Effect.none )

updateWithPortMessage : Model -> IncomingMessage -> ( Model, Effect Msg )
updateWithPortMessage model message =
    case message.messageType of
        "published" ->
            case (model.data, Nostr.External.decodeSendId message.value, Nostr.External.decodeEvent message.value) of
                ( RelaysData relaysModel, Ok incomingSendId, _ ) ->
                    case relaysModel.state of
                        RelayListStateSaving sendRequestId ->
                            if sendRequestId == incomingSendId then
                                ( { model
                                    | data = RelaysData
                                        { relaysModel
                                        | state = RelayListStateEditing
                                        }
                                  }
                                  -- check configuration again after saving relays
                                , Effect.sendSharedMsg Shared.Msg.DelayedCheckConfiguration
                                )

                            else
                                ( model, Effect.none )

                        _ ->
                            ( model, Effect.none )

                ( ProfileData profileModel, Ok incomingSendId, Ok event ) ->
                    case (profileModel.state, profileFromEvent event) of
                        ( EditStateSaving sendRequestId, Just profile ) ->
                            if sendRequestId == incomingSendId then
                                ( { model
                                    | data = ProfileData
                                        { profileModel
                                        | state = EditStateEditing
                                        , savedProfile = Just profile
                                        }
                                  }
                                  -- check configuration again after saving profile
                                , Effect.sendSharedMsg Shared.Msg.DelayedCheckConfiguration
                                )

                            else
                                ( model, Effect.none )

                        _ ->
                            ( model, Effect.none )

                ( MediaServersData mediaServersModel, Ok incomingSendId, _ ) ->
                    case mediaServersModel.state of
                        MediaServerStateSavingNip96 sendRequestId ->
                            if sendRequestId == incomingSendId then
                                ( { model
                                    | data = MediaServersData
                                        { mediaServersModel
                                        | state = MediaServerStateEditing
                                        }
                                  }
                                  -- check configuration again after saving media server
                                , Effect.sendSharedMsg Shared.Msg.DelayedCheckConfiguration
                                )

                            else
                                ( model, Effect.none )

                        MediaServerStateSavingBlossom sendRequestId ->
                            if sendRequestId == incomingSendId then
                                ( { model
                                    | data = MediaServersData
                                        { mediaServersModel
                                        | state = MediaServerStateEditing
                                        }
                                  }
                                  -- check configuration again after saving media server
                                , Effect.sendSharedMsg Shared.Msg.DelayedCheckConfiguration
                                )

                            else
                                ( model, Effect.none )

                        _ ->
                            ( model, Effect.none )
                _ ->
                    ( model, Effect.none )

        _ ->
            ( model, Effect.none )


extendMediaServerList : ServerUrl -> List ServerUrl -> List ServerUrl
extendMediaServerList mediaServer mediaServers =
    if List.member mediaServer mediaServers then
        mediaServers

    else
        mediaServers ++ [ mediaServer ]


removeMediaServerFromList : ServerUrl -> List ServerUrl -> List ServerUrl
removeMediaServerFromList mediaServer mediaServers =
    mediaServers
        |> List.filter (\serverInList -> serverInList /= mediaServer)


sendNip96MediaServerListCmd : BrowserEnv -> PubKey -> List ServerUrl -> Effect msg
sendNip96MediaServerListCmd browserEnv pubKey mediaServers =
    eventWithNip96ServerList browserEnv pubKey mediaServers
        |> SendFileStorageServerList []
        |> Shared.Msg.SendNostrEvent
        |> Effect.sendSharedMsg


sendBlossomMediaServerListCmd : BrowserEnv -> PubKey -> List ServerUrl -> Effect msg
sendBlossomMediaServerListCmd browserEnv pubKey mediaServers =
    eventWithBlossomServerList browserEnv pubKey mediaServers
        |> SendFileStorageServerList []
        |> Shared.Msg.SendNostrEvent
        |> Effect.sendSharedMsg


relayListWithRole : List RelayUrl -> RelayRole -> List RelayMetadata
relayListWithRole relayUrls role =
    relayUrls
        |> List.map
            (\relayUrl ->
                { url = hostWithoutProtocol relayUrl, role = role }
            )


sendRelayListCmd : PubKey -> List RelayMetadata -> Effect msg
sendRelayListCmd pubKey relays =
    let
        relaysWithProtocol =
            relays
                |> List.map
                    (\relay ->
                        { relay | url = "wss://" ++ relay.url }
                    )

        relayUrls =
            relays
                |> List.filterMap
                    (\relay ->
                        if relay.role == WriteRelay || relay.role == ReadWriteRelay then
                            Just relay.url

                        else
                            Nothing
                    )
    in
    eventWithRelayList pubKey relaysWithProtocol
        |> SendRelayList relayUrls
        |> Shared.Msg.SendNostrEvent
        |> Effect.sendSharedMsg


updateModelWithCategory : Auth.User -> Shared.Model -> Model -> Category -> ( Model, Effect Msg )
updateModelWithCategory user shared model category =
    let
        ( newModel, effect ) =
            case category of
                Relays ->
                    ( { model | data = RelaysData emptyRelaysModel }
                    , RequestRelayLists { emptyEventFilter | kinds = Just [ KindRelayListMetadata, KindBlockedRelaysList, KindSearchRelaysList, KindPrivateRelayList, KindRelayListForDMs ], authors = Just [ user.pubKey ] }
                        |> Nostr.createRequest shared.nostr "Relay lists of user" []
                        |> Shared.Msg.RequestNostrEvents
                        |> Effect.sendSharedMsg
                    )

                MediaServers ->
                    ( { model | data = MediaServersData emptyMediaServersModel }
                    , RequestMediaServerLists { emptyEventFilter | kinds = Just [ KindUserServerList, KindFileStorageServerList ], authors = Just [ user.pubKey ] }
                        |> Nostr.createRequest shared.nostr "Media server lists of user" []
                        |> Shared.Msg.RequestNostrEvents
                        |> Effect.sendSharedMsg
                    )

                Profile ->
                    let
                        ( profileModel, profileEffect ) =
                            case Nostr.getProfile shared.nostr user.pubKey of
                                Just profile ->
                                    profileModelFromProfile user shared profile

                                Nothing ->
                                    emptyProfileModel user shared
                    in
                    ( { model | data = ProfileData profileModel }
                    , Effect.batch
                        [ profileEffect
                        , Shared.Msg.LoadUserDataByPubKey user.pubKey
                            |> Effect.sendSharedMsg
                        ]
                    )
    in
    ( newModel
    , Effect.batch
        [ Effect.replaceRoute { path = model.path, query = Dict.singleton categoryParamName (stringFromCategory category), hash = Nothing }
        , effect
        ]
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.receiveMessage ReceivedPortMessage
        , case model.data of
            ProfileData profileModel ->
                Sub.map MediaSelectorSent (MediaSelector.subscribe profileModel.mediaSelector)

            _ ->
                Sub.none
        ]



-- VIEW


view : Auth.User -> Shared.Model -> Model -> View Msg
view user shared model =
    let
        configCheckIssues =
            { profileIssues = profileIssues model shared.configCheck
            , relaysIssues = ConfigCheck.relayIssues shared.configCheck
            , mediaServersIssues = ConfigCheck.mediaServerIssues shared.configCheck
            }
    in
    { title = Translations.pageTitle [ shared.browserEnv.translations ]
    , body =
        [ div
            [ css
                [ Tw.mx_10
                , Tw.mb_4
                ]
            ]
            [ viewCategory shared configCheckIssues model user
            ]
        ]
    }

profileIssues : Model -> ConfigCheck.Model -> List ConfigCheck.Issue
profileIssues model configCheck =
    let
        imageIssues =
            case model.data of
                ProfileData profileModel ->
                    let
                        pictureIssue =
                            case profileModel.pictureIssue of
                                Just issue ->
                                    [ issue ]

                                Nothing ->
                                    []

                        bannerIssue =
                            case profileModel.bannerIssue of
                                Just issue ->
                                    [ issue ]

                                Nothing ->
                                    []
                    in
                    pictureIssue ++ bannerIssue

                _ ->
                    []
    in
    imageIssues ++ ConfigCheck.profileIssues configCheck

type alias ConfigCheckIssues =
    { profileIssues : List ConfigCheck.Issue
    , relaysIssues : List ConfigCheck.Issue
    , mediaServersIssues : List ConfigCheck.Issue
    }

viewCategory : Shared.Model -> ConfigCheckIssues -> Model -> Auth.User -> Html Msg
viewCategory shared configCheckIssues model user =
    case ( Categories.selected model.categories, model.data ) of
        ( Relays, RelaysData relaysModel ) ->
            viewRelays shared configCheckIssues.relaysIssues user relaysModel

        ( MediaServers, MediaServersData mediaServersModel ) ->
            viewMediaServers shared configCheckIssues.mediaServersIssues user mediaServersModel

        ( Profile, ProfileData profileModel ) ->
            viewProfile shared configCheckIssues.profileIssues user profileModel

        _ ->
            emptyHtml


type alias Suggestions =
    { identifier : String
    , suggestions : List String
    }


viewRelays : Shared.Model -> List ConfigCheck.Issue -> Auth.User -> RelaysModel -> Html Msg
viewRelays shared configCheckIssues user relaysModel =
    {-
       searchRelays =
           Nostr.getSearchRelaysForPubKey shared.nostr user.pubKey

       searchRelaySuggestions =
           { identifier = "search-relay-suggestions"
           , suggestions =
               missingRelays inboxRelays Pareto.defaultSearchRelays
           }
    -}
    div
        [ css
            [ Tw.flex
            , Tw.flex_col
            , Tw.gap_2
            ]
        ]
        [ viewConfigIssues shared.browserEnv (Translations.relayIssuesTitle [ shared.browserEnv.translations ]) configCheckIssues
        , outboxRelaySection shared user relaysModel
        , inboxRelaySection shared user relaysModel

        -- , viewRelayList searchRelays
        -- , addRelayBox shared.theme shared.browserEnv.translations relaysModel.searchRelay (updateRelayModelSearch relaysModel) (AddSearchRelay user.pubKey)
        ]


outboxRelaySection : Shared.Model -> Auth.User -> RelaysModel -> Html Msg
outboxRelaySection shared user relaysModel =
    let
        styles =
            stylesForTheme shared.theme

        outboxRelays =
            Nostr.getNip65WriteRelaysForPubKey shared.nostr user.pubKey

        suggestedOutboxRelays =
            suggestedRelays shared user.pubKey WriteRelay

        outboxRelaySuggestions =
            { identifier = "outbox-relay-suggestions"
            , suggestions =
                missingRelays outboxRelays suggestedOutboxRelays
            }

        readOnly =
            Shared.signingPubKeyAvailable shared.loginStatus
                |> not

        saving =
            case relaysModel.state of
                RelayListStateSaving _ ->
                    True

                _ ->
                    False
    in
    if shared.browserEnv.testMode == BrowserEnv.TestModeEnabled then
        div
            [ css
                [ Tw.italic
                ]
            ]
            [ text <| Translations.outboxRelaysTestModeInformation [ shared.browserEnv.translations ] ]

    else
        div []
            [ h3
                (styles.colorStyleGrayscaleTitle ++ styles.textStyleH3)
                [ text <| Translations.outboxSectionTitle [ shared.browserEnv.translations ] ]
            , p [] [ text <| Translations.outboxRelaysDescription [ shared.browserEnv.translations ] ]
            , viewRelayList shared.theme shared.browserEnv.translations readOnly (AddDefaultOutboxRelays suggestedOutboxRelays) (RemoveRelay user.pubKey WriteRelay) outboxRelays
            , if not readOnly then
                addRelayBox shared.theme shared.browserEnv.translations relaysModel.outboxRelay outboxRelaySuggestions (updateRelayModelOutbox relaysModel) (AddOutboxRelay user.pubKey) saving

              else
                emptyHtml
            ]


inboxRelaySection : Shared.Model -> Auth.User -> RelaysModel -> Html Msg
inboxRelaySection shared user relaysModel =
    let
        styles =
            stylesForTheme shared.theme

        inboxRelays =
            Nostr.getNip65ReadRelaysForPubKey shared.nostr user.pubKey

        suggestedInboxRelays =
            suggestedRelays shared user.pubKey ReadRelay

        inboxRelaySuggestions =
            { identifier = "inbox-relay-suggestions"
            , suggestions =
                missingRelays inboxRelays suggestedInboxRelays
            }

        readOnly =
            Shared.signingPubKeyAvailable shared.loginStatus
                |> not

        saving =
            case relaysModel.state of
                RelayListStateSaving _ ->
                    True

                _ ->
                    False
    in
    div []
        [ h3
            (styles.colorStyleGrayscaleTitle
                ++ styles.textStyleH3
                ++ [ css [ Tw.mt_3 ] ]
            )
            [ text <| Translations.inboxSectionTitle [ shared.browserEnv.translations ] ]
        , p [] [ text <| Translations.inboxRelaysDescription [ shared.browserEnv.translations ] ]
        , viewRelayList shared.theme shared.browserEnv.translations readOnly (AddDefaultInboxRelays suggestedInboxRelays) (RemoveRelay user.pubKey ReadRelay) inboxRelays
        , if not readOnly then
            addRelayBox shared.theme shared.browserEnv.translations relaysModel.inboxRelay inboxRelaySuggestions (updateRelayModelInbox relaysModel) (AddInboxRelay user.pubKey) saving

          else
            emptyHtml
        ]



-- users must be whitelisted for Pareto outbox relays


suggestedRelays : Shared.Model -> PubKey -> RelayRole -> List RelayUrl
suggestedRelays shared pubKey role =
    case role of
        WriteRelay ->
            if Nostr.isEditor shared.nostr pubKey then
                Pareto.paretoOutboxRelays ++ Pareto.recommendedOutboxRelays

            else
                Pareto.recommendedOutboxRelays

        ReadRelay ->
            Pareto.recommendedInboxRelays

        ReadWriteRelay ->
            []


missingRelays : List Relay -> List String -> List String
missingRelays addedRelays recommendedRelays =
    recommendedRelays
        |> List.filter
            (\relayUrl ->
                addedRelays
                    |> List.filter
                        (\addedRelay ->
                            addedRelay.urlWithoutProtocol == hostWithoutProtocol relayUrl
                        )
                    |> List.isEmpty
            )


updateRelayModelOutbox : RelaysModel -> Maybe String -> RelaysModel
updateRelayModelOutbox relaysModel value =
    { relaysModel | outboxRelay = value }


updateRelayModelInbox : RelaysModel -> Maybe String -> RelaysModel
updateRelayModelInbox relaysModel value =
    { relaysModel | inboxRelay = value }



--   updateRelayModelSearch : RelaysModel -> Maybe String -> RelaysModel
--   updateRelayModelSearch relaysModel value =
--       { relaysModel | searchRelay = value }


addRelayBox : Theme -> I18Next.Translations -> Maybe String -> Suggestions -> (Maybe String -> RelaysModel) -> (String -> Msg) -> Bool -> Html Msg
addRelayBox theme translations maybeValue suggestions updateRelayFn addRelayMsg saving =
    let
        styles =
            stylesForTheme theme

        showProtocolPrefix =
            maybeValue
                |> Maybe.map (\value -> not <| String.startsWith "wss://" value || String.startsWith "ws://" value)
                |> Maybe.withDefault True
    in
    div
        [ css
            [ Tw.flex
            , Tw.flex_row
            , Tw.gap_2
            ]
        ]
        [ div
            [ css
                [ Tw.flex
                , Tw.flex_row
                , Tw.relative
                , Tw.w_full
                , Bp.sm
                    [ Css.property "width" "400px"
                    ]
                ]
            ]
            [ div
                (styles.colorStyleGrayscaleMuted
                    ++ [ css
                            [ Tw.flex
                            , Tw.absolute
                            , Tw.leading_6
                            , Tw.w_10
                            , Tw.h_10
                            , Tw.items_center
                            , Tw.justify_center
                            , Tw.left_2
                            , Tw.top_0
                            , Tw.pointer_events_none
                            ]
                       ]
                )
                [ if showProtocolPrefix then
                    text "wss://"

                  else
                    text ""
                ]
            , input
                (styles.colorStyleBackground
                    ++ styles.colorStyleGrayscaleText
                    ++ [ Attr.placeholder <| Translations.addRelayPlaceholder [ translations ]
                       , Attr.value (Maybe.withDefault "" maybeValue)
                       , Attr.type_ "url"
                       , Attr.spellcheck False
                       , Attr.list suggestions.identifier
                       , Events.onInput
                            (\relayText ->
                                if relayText /= "" then
                                    UpdateRelayModel <| updateRelayFn (Just <| relayText)

                                else
                                    UpdateRelayModel <| updateRelayFn Nothing
                            )
                       , css
                            [ Tw.appearance_none
                            , Tw.bg_scroll
                            , Tw.bg_clip_border
                            , Tw.rounded_md
                            , Tw.border_2
                            , Tw.box_border
                            , Tw.cursor_text
                            , Tw.block
                            , Tw.ps_14
                            , Tw.pe_16
                            , Tw.pl_14
                            , Tw.pr_16
                            , Tw.h_10
                            , Tw.w_full
                            ]
                       ]
                )
                []
            , relaySuggestionDataList suggestions
            ]
        , Button.new
            { label = Translations.addRelayButtonTitle [ translations ]
            , onClick = Maybe.map addRelayMsg maybeValue
            , theme = theme
            }
            |> Button.withDisabled (not <| relayUrlValid maybeValue)
            |> Button.withIntermediateState saving
            |> Button.view
        ]


relaySuggestionDataList : Suggestions -> Html Msg
relaySuggestionDataList suggestions =
    datalist
        [ Attr.id suggestions.identifier
        ]
        (suggestions.suggestions
            |> List.map
                (\relayUrl ->
                    option [ Attr.value relayUrl ] []
                )
        )


relayUrlValid : Maybe String -> Bool
relayUrlValid maybeRelayUrl =
    case maybeRelayUrl of
        -- TODO: check here for valid
        Just _ ->
            True

        Nothing ->
            False


viewRelayList : Theme -> I18Next.Translations -> Bool -> Msg -> (String -> Msg) -> List Relay -> Html Msg
viewRelayList theme translations readOnly addDefaultRelaysMsg removeMsg relays =
    let
        noRelaysConfigureButton =
            div
                [ css
                    [ Tw.flex
                    , Tw.flex_col
                    , Tw.gap_2
                    , Tw.mb_2
                    ]
                ]
                [ text <| Translations.noRelaysConfiguredText [ translations ]
                , Button.new
                    { label = Translations.addDefaultRelaysButtonTitle [ translations ]
                    , onClick = Just addDefaultRelaysMsg
                    , theme = theme
                    }
                    |> Button.view
                ]
    in
    if List.length relays > 0 then
        div
            [ css
                [ Tw.flex
                , Tw.flex_col
                , Tw.my_2
                , Tw.gap_2
                ]
            ]
            (List.map (viewRelay readOnly removeMsg) relays)

    else if not readOnly then
        noRelaysConfigureButton

    else
        text <| Translations.relayReadOnlyLoginInfo [ translations ]


viewRelay : Bool -> (String -> Msg) -> Relay -> Html Msg
viewRelay readOnly removeMsg relay =
    div
        [ css
            [ Tw.flex
            , Tw.flex_row
            , Tw.items_center
            , Tw.gap_2
            , Tw.p_2
            , Tw.border_b_2
            , Tw.w_80
            , Bp.sm
                [ Tw.w_96
                ]
            ]
        ]
        [ viewRelayConnectionIndicator relay
        , viewRelayImage (Relay.iconUrl relay)
        , div
            [ css
                [ Tw.grow
                , Tw.w_80
                , Bp.sm
                    [ Tw.w_96
                    ]
                ]
            ]
            [ text relay.urlWithoutProtocol
            ]
        , if not readOnly then
            removeRelayButton relay removeMsg

          else
            emptyHtml
        ]


viewRelayConnectionIndicator : Relay -> Html Msg
viewRelayConnectionIndicator relay =
    let
        ( bgColor, borderColor ) =
            case ( relay.nip11, relay.state ) of
                ( Just _, RelayReady ) ->
                    ( Theme.green_500, Theme.blue_500 )

                ( Just _, RelayConnected ) ->
                    ( Theme.green_500, Theme.green_500 )

                ( Just _, RelayConnecting ) ->
                    ( Theme.green_500, Theme.orange_500 )

                ( Just _, RelayDisconnected ) ->
                    ( Theme.green_500, Theme.red_500 )

                -- actually an impossible state - can't have NIP-11 data with failed request
                ( Just _, RelayStateNip11RequestFailed _ ) ->
                    ( Theme.black, Theme.black )

                ( Just _, RelayStateUnknown ) ->
                    ( Theme.green_500, Theme.orange_500 )

                ( Nothing, RelayStateNip11RequestFailed _ ) ->
                    ( Theme.black, Theme.black )

                ( Nothing, _ ) ->
                    ( Theme.red_900, Theme.black )
    in
    div
        [ css
            [ Tw.w_3
            , Tw.h_3
            , Tw.rounded_full
            , Tw.border_2
            , Tw.bg_color bgColor
            , Tw.border_color borderColor
            ]
        ]
        []


removeRelayButton : Relay -> (String -> Msg) -> Html Msg
removeRelayButton relay removeMsg =
    let
        styles =
            stylesForTheme ParetoTheme
    in
    div
        [ css
            [ Tw.cursor_pointer
            , Tw.text_color styles.color3
            , darkMode
                [ Tw.text_color styles.color3DarkMode
                ]
            ]
        , Events.onClick (removeMsg relay.urlWithoutProtocol)
        ]
        [ Icon.FeatherIcon FeatherIcons.delete
            |> Icon.view
        ]


viewMediaServers : Shared.Model -> List ConfigCheck.Issue -> Auth.User -> MediaServersModel -> Html Msg
viewMediaServers shared configCheckIssues user mediaServersModel =
    div
        [ css
            [ Tw.flex
            , Tw.flex_col
            , Tw.gap_8
            ]
        ]
        [ viewConfigIssues shared.browserEnv (Translations.mediaServerIssuesTitle [ shared.browserEnv.translations ]) configCheckIssues
        , nip96ServersSection shared user mediaServersModel
        , blossomServersSection shared user mediaServersModel
        ]


nip96ServersSection : Shared.Model -> Auth.User -> MediaServersModel -> Html Msg
nip96ServersSection shared user mediaServersModel =
    let
        styles =
            stylesForTheme shared.theme

        nip96Servers =
            Nostr.getNip96Servers shared.nostr user.pubKey

        suggestedServers =
            suggestedNip96Servers shared user.pubKey

        nip96ServerSuggestions =
            { identifier = "nip96-server-suggestions"
            , suggestions =
                missingMediaServers nip96Servers suggestedServers
            }

        readOnly =
            Shared.signingPubKeyAvailable shared.loginStatus
                |> not

        saving =
            case mediaServersModel.state of
                MediaServerStateSavingNip96 _ ->
                    True

                _ ->
                    False
    in
    div []
        [ h3
            (styles.colorStyleGrayscaleTitle ++ styles.textStyleH3)
            [ text <| Translations.nip96ServersSectionTitle [ shared.browserEnv.translations ] ]
        , p [] [ text <| Translations.nip96ServersDescription [ shared.browserEnv.translations ] ]
        , viewMediaServersList shared.theme shared.browserEnv.translations readOnly (Just <| AddDefaultNip96MediaServers user.pubKey suggestedServers) (RemoveNip96MediaServer user.pubKey) nip96Servers
        , if not readOnly then
            addMediaServerBox shared.theme shared.browserEnv.translations mediaServersModel.nip96Server nip96ServerSuggestions (updateNip96Server mediaServersModel) (AddNip96MediaServer user.pubKey) saving

          else
            text <| Translations.mediaServerReadOnlyLoginInfo [ shared.browserEnv.translations ]
        ]


suggestedNip96Servers : Shared.Model -> PubKey -> List RelayUrl
suggestedNip96Servers shared pubKey =
    if Nostr.isEditor shared.nostr pubKey then
        Pareto.defaultNip96ServersAuthors

    else
        Pareto.defaultNip96ServersPublic


updateNip96Server : MediaServersModel -> Maybe String -> MediaServersModel
updateNip96Server mediaServersModel value =
    { mediaServersModel | nip96Server = value }


missingMediaServers : List String -> List String -> List String
missingMediaServers addedMediaServers recommendedMediaServers =
    recommendedMediaServers
        |> List.filter
            (\mediaServer ->
                addedMediaServers
                    |> List.filter
                        (\addedMediaServer ->
                            addedMediaServer == mediaServer
                        )
                    |> List.isEmpty
            )


blossomServersSection : Shared.Model -> Auth.User -> MediaServersModel -> Html Msg
blossomServersSection shared user mediaServersModel =
    let
        styles =
            stylesForTheme shared.theme

        blossomServers =
            Nostr.getBlossomServers shared.nostr user.pubKey

        suggestedServers =
            suggestedBlossomServers shared user.pubKey

        blossomServerSuggestions =
            { identifier = "blossom-server-suggestions"
            , suggestions = suggestedServers
            }

        readOnly =
            Shared.signingPubKeyAvailable shared.loginStatus
                |> not

        saving =
            case mediaServersModel.state of
                MediaServerStateSavingBlossom _ ->
                    True

                _ ->
                    False
    in
    div []
        [ h3
            (styles.colorStyleGrayscaleTitle ++ styles.textStyleH3)
            [ text <| Translations.blossomServersSectionTitle [ shared.browserEnv.translations ] ]
        , p [] [ text <| Translations.blossomServersDescription [ shared.browserEnv.translations ] ]
        , viewMediaServersList shared.theme shared.browserEnv.translations readOnly Nothing (RemoveBlossomMediaServer user.pubKey) blossomServers
        , if not readOnly then
            addMediaServerBox shared.theme shared.browserEnv.translations mediaServersModel.blossomServer blossomServerSuggestions (updateBlossomServer mediaServersModel) (AddBlossomMediaServer user.pubKey) saving

          else
            text <| Translations.mediaServerReadOnlyLoginInfo [ shared.browserEnv.translations ]
        ]


suggestedBlossomServers : Shared.Model -> PubKey -> List RelayUrl
suggestedBlossomServers _ _ =
    -- currently we prefer NIP-96
    []


updateBlossomServer : MediaServersModel -> Maybe String -> MediaServersModel
updateBlossomServer mediaServersModel value =
    { mediaServersModel | blossomServer = value }


viewMediaServersList : Theme -> I18Next.Translations -> Bool -> Maybe Msg -> (String -> Msg) -> List String -> Html Msg
viewMediaServersList theme translations readOnly addDefaultMediaServersMsg removeMsg mediaServers =
    let
        noServersConfiguredInfo =
            div
                [ css
                    [ Tw.flex
                    , Tw.flex_col
                    , Tw.gap_2
                    , Tw.mb_2
                    ]
                ]
                [ text <| Translations.noMediaServerConfiguredText [ translations ]
                , Button.new
                    { label = Translations.addDefaultMediaServersButtonTitle [ translations ]
                    , onClick = addDefaultMediaServersMsg
                    , theme = theme
                    }
                    |> Button.withHidden (addDefaultMediaServersMsg == Nothing)
                    |> Button.view
                ]
    in
    if List.length mediaServers > 0 then
        div
            [ css
                [ Tw.flex
                , Tw.flex_col
                , Tw.my_2
                , Tw.gap_2
                ]
            ]
            (List.map (viewMediaServer readOnly removeMsg) mediaServers)

    else if not readOnly then
        noServersConfiguredInfo

    else
        emptyHtml


viewMediaServer : Bool -> (String -> Msg) -> String -> Html Msg
viewMediaServer readOnly removeMsg mediaServer =
    div
        [ css
            [ Tw.flex
            , Tw.flex_row
            , Tw.items_center
            , Tw.gap_2
            , Tw.p_2
            , Tw.border_b_2
            , Tw.w_96
            ]
        ]
        [ div
            [ css
                [ Tw.grow
                , Tw.w_96
                ]
            ]
            [ text mediaServer
            ]
        , if not readOnly then
            removeMediaServerButton mediaServer removeMsg

          else
            emptyHtml
        ]


removeMediaServerButton : String -> (String -> Msg) -> Html Msg
removeMediaServerButton mediaServer removeMsg =
    let
        styles =
            stylesForTheme ParetoTheme
    in
    div
        [ css
            [ Tw.cursor_pointer
            , Tw.text_color styles.color3
            , darkMode
                [ Tw.text_color styles.color3DarkMode
                ]
            ]
        , Events.onClick (removeMsg mediaServer)
        ]
        [ Icon.FeatherIcon FeatherIcons.delete
            |> Icon.view
        ]


addMediaServerBox : Theme -> I18Next.Translations -> Maybe String -> Suggestions -> (Maybe String -> MediaServersModel) -> (String -> Msg) -> Bool -> Html Msg
addMediaServerBox theme translations maybeValue suggestions updateMediaServerFn addMediaServerMsg saving =
    let
        styles =
            stylesForTheme theme

        showProtocolPrefix =
            maybeValue
                |> Maybe.map (\value -> not <| String.startsWith "https://" value || String.startsWith "http://" value)
                |> Maybe.withDefault True

        serverUrlWithProtocol =
            if showProtocolPrefix then
                maybeValue
                    |> Maybe.map (String.append "https://")

            else
                maybeValue
    in
    div
        [ css
            [ Tw.flex
            , Tw.flex_row
            , Tw.gap_2
            ]
        ]
        [ div
            [ css
                [ Tw.flex
                , Tw.flex_row
                , Tw.relative
                , Tw.w_full
                , Bp.sm
                    [ Css.property "width" "400px"
                    ]
                ]
            ]
            [ div
                (styles.colorStyleGrayscaleMuted
                    ++ [ css
                            [ Tw.flex
                            , Tw.absolute
                            , Tw.leading_6
                            , Tw.w_12
                            , Tw.h_10
                            , Tw.items_center
                            , Tw.justify_center
                            , Tw.left_2
                            , Tw.top_0
                            , Tw.pointer_events_none
                            ]
                       ]
                )
                [ if showProtocolPrefix then
                    text "https://"

                  else
                    text ""
                ]
            , input
                (styles.colorStyleBackground
                    ++ styles.colorStyleGrayscaleText
                    ++ [ Attr.placeholder <| Translations.addMediaServerPlaceholder [ translations ]
                       , Attr.value (Maybe.withDefault "" maybeValue)
                       , Attr.type_ "url"
                       , Attr.spellcheck False
                       , Attr.list suggestions.identifier
                       , Events.onInput
                            (\mediaServerText ->
                                if mediaServerText /= "" then
                                    UpdateMediaServerModel <| updateMediaServerFn (Just <| mediaServerText)

                                else
                                    UpdateMediaServerModel <| updateMediaServerFn Nothing
                            )
                       , css
                            [ Tw.appearance_none
                            , Tw.bg_scroll
                            , Tw.bg_clip_border
                            , Tw.rounded_md
                            , Tw.border_2
                            , Tw.box_border
                            , Tw.cursor_text
                            , Tw.block
                            , Tw.ps_16
                            , Tw.pe_16
                            , Tw.pl_16
                            , Tw.pr_16
                            , Tw.h_10
                            , Tw.w_full
                            ]
                       ]
                )
                []
            , relaySuggestionDataList suggestions
            ]
        , Button.new
            { label = Translations.addMediaServerButtonTitle [ translations ]
            , onClick = Maybe.map addMediaServerMsg serverUrlWithProtocol
            , theme = theme
            }
            |> Button.withDisabled (not <| mediaServerUrlValid serverUrlWithProtocol)
            |> Button.withIntermediateState saving
            |> Button.view
        ]


mediaServerUrlValid : Maybe ServerUrl -> Bool
mediaServerUrlValid maybeMediaServerUrl =
    case maybeMediaServerUrl of
        Just url ->
            Url.fromString url /= Nothing

        Nothing ->
            False


viewProfile : Shared.Model -> List ConfigCheck.Issue -> Auth.User -> ProfileModel -> Html Msg
viewProfile shared configCheckIssues user profileModel =
    let
        readOnly =
            Shared.signingPubKeyAvailable shared.loginStatus
                |> not

        viewUserProfile =
            case ( profileModel.savedProfile, readOnly ) of
                ( Just profile, True ) ->
                    Ui.Profile.viewProfile
                        profile
                        { browserEnv = shared.browserEnv
                        , nostr = shared.nostr
                        , loginStatus = shared.loginStatus
                        , following = UnknownFollowing
                        , subscribe = Nothing
                        , theme = shared.theme
                        , validation =
                            Nostr.getProfileValidationStatus shared.nostr user.pubKey
                                |> Maybe.withDefault ValidationUnknown
                        }

                ( Just _, False ) ->
                    viewProfileEditor shared configCheckIssues user profileModel

                ( Nothing, True ) ->
                    Html.text <| Translations.noProfileReadOnlyInformationalText [ shared.browserEnv.translations ]

                ( Nothing, False ) ->
                    div
                        [ css
                            [ Tw.flex
                            , Tw.flex_col
                            , Tw.gap_2
                            ]
                        ]
                        [ Html.text <| Translations.noProfileInformationalText [ shared.browserEnv.translations ]
                        , Button.new
                            { label = Translations.createProfileButtonTitle [ shared.browserEnv.translations ]
                            , onClick = Just CreateProfile
                            , theme = shared.theme
                            }
                            |> Button.view
                        ]
    in
    div
        [ css
            [ Tw.flex
            , Tw.flex_col
            , Tw.gap_2
            ]
        ]
        [ viewUserProfile
        ]


viewProfileEditor : Shared.Model -> List ConfigCheck.Issue -> Auth.User -> ProfileModel -> Html Msg
viewProfileEditor shared configCheckIssues user profileModel =
    let
        portalUserData =
            Nostr.getPortalUserInfo shared.nostr user.pubKey

        profileNotChanged =
            case profileModel.savedProfile of
                Just savedProfile ->
                    profilesEqual savedProfile (profileFromProfileModel user.pubKey profileModel)

                Nothing ->
                    False

        -- only show lud06 field if it was present in loaded profile
        lud06Field =
            profileModel.savedProfile
                |> Maybe.andThen .lud06
                |> Maybe.map (\lud06String ->
                    if lud06String /= "" then
                        EntryField.new
                            { value = profileModel.lud06
                            , onInput = \lud06 -> UpdateProfileModel { profileModel | lud06 = lud06 }
                            , theme = shared.theme
                            }
                            |> EntryField.withLabel (Translations.profileLud06FieldLabel [ shared.browserEnv.translations ])
                            |> EntryField.withPlaceholder (Translations.profileLud06FieldPlaceholder [ shared.browserEnv.translations ])
                            |> EntryField.withType EntryField.FieldTypeText
                            |> EntryField.view
                    else
                        emptyHtml
                )
                |> Maybe.withDefault emptyHtml
    in
    div
        [ css
            [ Tw.flex
            , Tw.flex_col
            , Tw.gap_3
            ]
        ]
        [ viewConfigIssues shared.browserEnv (Translations.profileIssuesTitle [ shared.browserEnv.translations ]) configCheckIssues
        , Button.new
            { label = Translations.profileSaveButtonTitle [ shared.browserEnv.translations ]
            , onClick = Just <| SaveProfile (profileFromProfileModel user.pubKey profileModel)
            , theme = shared.theme
            }
            |> Button.withDisabled (profileNotChanged)
            |> Button.withIntermediateState (profileModel.state /= EditStateEditing)
            |> Button.view
        , div
            [ css
                [ Tw.flex
                , Tw.flex_row
                , Tw.flex_wrap
                , Tw.gap_2
                ]
            ]
            [ EntryField.new
                { value = profileModel.name
                , onInput = \name -> UpdateProfileModel { profileModel | name = name }
                , theme = shared.theme
                }
                |> EntryField.withLabel (Translations.profileNameFieldLabel [ shared.browserEnv.translations ])
                |> EntryField.withPlaceholder (Translations.profileNameFieldPlaceholder [ shared.browserEnv.translations ])
                |> EntryField.withDescription (Translations.profileNameFieldDescription [ shared.browserEnv.translations ])
                |> EntryField.withSuggestions "name" (portalUserData |> Maybe.andThen .username |> Maybe.map List.singleton |> Maybe.withDefault [])
                |> EntryField.view
            , EntryField.new
                { value = profileModel.displayName
                , onInput = \displayName -> UpdateProfileModel { profileModel | displayName = displayName }
                , theme = shared.theme
                }
                |> EntryField.withLabel (Translations.profileDisplayNameFieldLabel [ shared.browserEnv.translations ])
                |> EntryField.withPlaceholder (Translations.profileDisplayNameFieldPlaceholder [ shared.browserEnv.translations ])
                |> EntryField.withDescription (Translations.profileDisplayNameFieldDescription [ shared.browserEnv.translations ])
                |> EntryField.view
            ]
        , EntryField.new
            { value = profileModel.nip05
            , onInput = \nip05 -> UpdateProfileModel { profileModel | nip05 = nip05 }
            , theme = shared.theme
            }
            |> EntryField.withLabel (Translations.profileNip05FieldLabel [ shared.browserEnv.translations ])
            |> EntryField.withDescription (Translations.profileNip05FieldDescription [ shared.browserEnv.translations ])
            |> EntryField.withPlaceholder (Translations.profileNip05FieldPlaceholder [ shared.browserEnv.translations ])
            |> EntryField.withSuggestions "nip05" (portalUserData |> Maybe.andThen .nip05 |> Maybe.map Nip05.nip05ToString |> Maybe.map List.singleton |> Maybe.withDefault [])
            |> EntryField.withType EntryField.FieldTypeEmail
            |> EntryField.view
        , EntryField.new
            { value = profileModel.about
            , onInput = \about -> UpdateProfileModel { profileModel | about = about }
            , theme = shared.theme
            }
            |> EntryField.withLabel (Translations.profileAboutFieldLabel [ shared.browserEnv.translations ])
            |> EntryField.withPlaceholder (Translations.profileAboutFieldPlaceholder [ shared.browserEnv.translations ])
            |> EntryField.withDescription (Translations.profileAboutFieldDescription [ shared.browserEnv.translations ])
            |> EntryField.withRows 2
            |> EntryField.view
        , viewImageSelection shared PictureLoaded ImagePicture profileModel
        , EntryField.new
            { value = profileModel.picture
            , onInput = \picture -> UpdateProfileModel { profileModel | picture = picture }
            , theme = shared.theme
            }
            |> EntryField.withLabel (Translations.profilePictureFieldLabel [ shared.browserEnv.translations ])
            |> EntryField.withPlaceholder (Translations.profilePictureFieldPlaceholder [ shared.browserEnv.translations ])
            |> EntryField.withDescription (Translations.profilePictureFieldDescription [ shared.browserEnv.translations ])
            |> EntryField.withType EntryField.FieldTypeUrl
            |> EntryField.view
        , viewImageSelection shared BannerLoaded ImageBanner profileModel
        , EntryField.new
            { value = profileModel.banner
            , onInput = \banner -> UpdateProfileModel { profileModel | banner = banner }
            , theme = shared.theme
            }
            |> EntryField.withLabel (Translations.profileBannerFieldLabel [ shared.browserEnv.translations ])
            |> EntryField.withPlaceholder (Translations.profileBannerFieldPlaceholder [ shared.browserEnv.translations ])
            |> EntryField.withDescription (Translations.profileBannerFieldDescription [ shared.browserEnv.translations ])
            |> EntryField.withType EntryField.FieldTypeUrl
            |> EntryField.view
        , lud06Field
        , EntryField.new
            { value = profileModel.lud16
            , onInput = \lud16 -> UpdateProfileModel { profileModel | lud16 = lud16 }
            , theme = shared.theme
            }
            |> EntryField.withLabel (Translations.profileLud16FieldLabel [ shared.browserEnv.translations ])
            |> EntryField.withPlaceholder (Translations.profileLud16FieldPlaceholder [ shared.browserEnv.translations ])
            |> EntryField.withDescription (Translations.profileLud16FieldDescription [ shared.browserEnv.translations ])
            |> EntryField.withSuggestions "lud16" (portalUserData |> Maybe.andThen .lud16 |> Maybe.map Lud16.lud16ToString |> Maybe.map List.singleton |> Maybe.withDefault [])
            |> EntryField.withType EntryField.FieldTypeEmail
            |> EntryField.view
        , EntryField.new
            { value = profileModel.website
            , onInput = \website -> UpdateProfileModel { profileModel | website = website }
            , theme = shared.theme
            }
            |> EntryField.withLabel (Translations.profileWebsiteFieldLabel [ shared.browserEnv.translations ])
            |> EntryField.withPlaceholder (Translations.profileWebsiteFieldPlaceholder [ shared.browserEnv.translations ])
            |> EntryField.withType EntryField.FieldTypeUrl
            |> EntryField.view
        , MediaSelector.new
            { model = profileModel.mediaSelector
            , toMsg = MediaSelectorSent
            , onSelected = Just ImageSelected
            , pubKey = user.pubKey
            , browserEnv = shared.browserEnv
            , theme = shared.theme
            }
            |> MediaSelector.view
        ]


viewImageSelection : Shared.Model -> (Bool -> Msg) -> ImageUploadType -> ProfileModel -> Html Msg
viewImageSelection shared onImageLoadedMsg imageUploadType profileModel =
    let
        imageUrl =
            case imageUploadType of
                ImagePicture ->
                    profileModel.picture

                ImageBanner ->
                    profileModel.banner
    in
    div
        [ css
            [ Tw.flex
            , Tw.flex_row
            , Tw.items_center
            , Tw.mt_2
            , Tw.gap_2
            ]
        ]
        [ viewImage onImageLoadedMsg imageUrl
        , Button.new
            { label = Translations.imageSelectionButtonTitle [ shared.browserEnv.translations ]
            , onClick = Just <| OpenImageSelection imageUploadType
            , theme = shared.theme
            }
            |> Button.withTypeSecondary
            |> Button.view
        ]


viewImage : (Bool -> Msg) -> String -> Html Msg
viewImage onImageLoadedMsg imageUrl =
    case Url.fromString imageUrl of
        Just _ ->
            Html.img
                [ Attr.src imageUrl
                , css
                    [ Tw.w_20
                    , Tw.min_h_full
                    , Tw.mt_3
                    ]
                , Events.on "load" (Decode.succeed (onImageLoadedMsg True))
                , Events.on "error" (Decode.succeed (onImageLoadedMsg False))
                ]
                []

        Nothing ->
            emptyHtml
