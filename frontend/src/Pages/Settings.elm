module Pages.Settings exposing (Model, Msg, page)

import Auth
import BrowserEnv exposing (BrowserEnv)
import Components.Button as Button
import Components.CashuReceiveDialog as CashuReceiveDialog
import Components.CashuWithdrawDialog as CashuWithdrawDialog
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
import Json.Decode.Pipeline as DecodePipeline
import Layouts
import Layouts.Sidebar
import Nostr
import Nostr.Blossom exposing (eventWithBlossomServerList)
import Nostr.CashuWallet as CashuWallet
import Nostr.ConfigCheck as ConfigCheck
import Nostr.Event exposing (Kind(..), emptyEventFilter)
import Nostr.External
import Nostr.Lud16 as Lud16
import Nostr.Nip05 as Nip05
import Nostr.Nip96 as Nip96 exposing (eventWithNip96ServerList)
import Nostr.Profile exposing (Profile, ProfileValidation(..), emptyProfile, eventFromProfile, profileDisplayName, profileFromEvent, profilesEqual)
import Nostr.Relay as Relay exposing (Relay, RelayState(..), RelayUrl)
import Nostr.RelayList as RelayList exposing (eventWithBlockedRelayList, eventWithPrivateRelayList, eventWithSearchRelayList)
import Nostr.RelayListMetadata exposing (RelayMetadata, eventWithRelayList, extendRelayList, removeFromRelayList)
import Nostr.Request exposing (RequestData(..))
import Nostr.Send exposing (SendRequest(..), SendRequestId)
import Nostr.Types exposing (Following(..), IncomingMessage, PubKey, RelayRole(..), ServerUrl, signingPubKeyAvailable)
import Page exposing (Page)
import Pareto
import Ports
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
import QRCode
import Svg.Attributes as SvgAttr


page : Auth.User -> Shared.Model -> Route () -> Page Model Msg
page user shared route =
    Page.new
        { init = init user shared route
        , update = update user shared
        , subscriptions = subscriptions
        , view = view user shared
        }
        |> Page.withLayout (toLayout shared)
        |> Page.withOnQueryParameterChanged
            { key = categoryParamName
            , onChange = CategoryQueryChanged
            }


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
                , image = \_ _ -> Nothing
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
    | MutesData MutesModel
    | EcashData EcashModel
    | WalletData WalletModel


type alias WalletModel =
    { uriDraft : String
    , connecting : Bool
    , connected : Bool
    , alias : Maybe String
    , network : Maybe String
    , lud16 : Maybe String
    , balanceSats : Maybe Int
    , walletPubkeyShort : Maybe String
    , error : Maybe String
    , weblnAvailable : Bool
    , canAutoPay : Bool
    , nwaUri : Maybe String
    }


emptyWalletModel : WalletModel
emptyWalletModel =
    { uriDraft = ""
    , connecting = False
    , connected = False
    , alias = Nothing
    , network = Nothing
    , lud16 = Nothing
    , balanceSats = Nothing
    , walletPubkeyShort = Nothing
    , error = Nothing
    , weblnAvailable = False
    , canAutoPay = False
    , nwaUri = Nothing
    }


type alias EcashModel =
    { enabling : Bool
    , mintDraft : Maybe String
    , mints : List String
    , receiveDialog : CashuReceiveDialog.Model
    , withdrawDialog : CashuWithdrawDialog.Model
    }


emptyEcashModel : Shared.Model -> EcashModel
emptyEcashModel shared =
    let
        -- When disabled, do not seed from the stored wallet mint list — that list is
        -- leftover from a previous enable and would undo local removals on reload.
        existingMints =
            if ecashIsEnabled shared then
                case Nostr.getCashuWallet shared.nostr of
                    Just wallet ->
                        wallet.mints

                    Nothing ->
                        Nostr.getNutzapMintRecommendation shared.nostr
                            |> Maybe.map .mints
                            |> Maybe.withDefault []

            else
                []

        mints =
            existingMints
    in
    { enabling = False
    , mintDraft = Nothing
    , mints = mints
    , receiveDialog = CashuReceiveDialog.init mints
    , withdrawDialog = CashuWithdrawDialog.init mints
    }


ecashIsEnabled : Shared.Model -> Bool
ecashIsEnabled shared =
    case ( Nostr.getCashuWallet shared.nostr, Nostr.getNutzapMintRecommendation shared.nostr ) of
        ( Just _, Just rec ) ->
            rec.p2pkPubkey /= Nothing && not (List.isEmpty rec.mints)

        ( Just _, Nothing ) ->
            True

        _ ->
            False

type alias RelaysModel =
    { outboxRelay : Maybe String
    , inboxRelay : Maybe String
    , privateRelay : Maybe String
    , localRelay : Maybe String
    , searchRelay : Maybe String
    , blockedRelay : Maybe String
    , state : RelayListState
    }


type RelayListState
    = RelayListStateEditing
    | RelayListStateSaving SendRequestId


type alias MutesModel =
    { state : MuteListState
    }


type MuteListState
    = MuteListStateEditing
    | MuteListStateSaving SendRequestId


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
    , privateRelay = Nothing
    , localRelay = Nothing
    , searchRelay = Nothing
    , blockedRelay = Nothing
    , state = RelayListStateEditing
    }


emptyMutesModel : MutesModel
emptyMutesModel =
    { state = MuteListStateEditing
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
    | Mutes
    | Wallet
    | Ecash


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
      , testId = "settings-relays"
      }
    , { category = MediaServers
      , title = Translations.mediaServersCategory [ translations ] ++ mediaServersIssuesSuffix
      , testId = "settings-media-servers"
      }
    , { category = Profile
      , title = Translations.profileCategory [ translations ] ++ profileIssuesSuffix
      , testId = "settings-profile"
      }
    , { category = Mutes
      , title = Translations.mutesCategory [ translations ]
      , testId = "settings-mutes"
      }
    , { category = Wallet
      , title = Translations.walletCategory [ translations ]
      , testId = "settings-wallet"
      }
    , { category = Ecash
      , title = Translations.ecashCategory [ translations ]
      , testId = "settings-ecash"
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

        Mutes ->
            "mutes"

        Wallet ->
            "wallet"

        Ecash ->
            "ecash"


categoryFromString : String -> Maybe Category
categoryFromString categoryString =
    case categoryString of
        "relays" ->
            Just Relays

        "media_servers" ->
            Just MediaServers

        "profile" ->
            Just Profile

        "mutes" ->
            Just Mutes

        "wallet" ->
            Just Wallet

        "ecash" ->
            Just Ecash

        _ ->
            Nothing



-- UPDATE


type Msg
    = CategorySelected Category
    | CategoryQueryChanged { from : Maybe String, to : Maybe String }
    | CategoriesSent (Categories.Msg Category Msg)
    | UpdateRelayModel RelaysModel
    | AddOutboxRelay PubKey RelayUrl
    | AddInboxRelay PubKey RelayUrl
    | AddPrivateRelay PubKey RelayUrl
    | AddLocalRelay RelayUrl
    | AddSuggestedLocalRelays (List RelayUrl)
    | RemovePrivateRelay PubKey RelayUrl
    | RemoveLocalRelay RelayUrl
    | AddSearchRelay PubKey RelayUrl
    | RemoveSearchRelay PubKey RelayUrl
    | AddDefaultSearchRelays PubKey (List RelayUrl)
    | AddBlockedRelay PubKey RelayUrl
    | RemoveBlockedRelay PubKey RelayUrl
    | AddDefaultOutboxRelays (List RelayUrl)
    | AddDefaultInboxRelays (List RelayUrl)
    | RemoveRelay PubKey RelayRole RelayUrl
    | UpdateMediaServerModel MediaServersModel
    | AddNip96MediaServer PubKey ServerUrl
    | RemoveNip96MediaServer PubKey ServerUrl
    | AddBlossomMediaServer PubKey ServerUrl
    | RemoveBlossomMediaServer PubKey ServerUrl
    | AddDefaultBlossomMediaServers PubKey (List ServerUrl)
    | UpdateProfileModel ProfileModel
    | OpenImageSelection ImageUploadType
    | MediaSelectorSent (MediaSelector.Msg Msg)
    | ImageSelected MediaSelector.UploadedFile
    | SaveProfile Profile
    | CreateProfile
    | UnmutePubKey PubKey PubKey
    | EnableEcashWallet
    | DisableEcashWallet
    | UpdateEcashMintDraft String
    | AddEcashMint
    | RemoveEcashMint String
    | CashuReceiveDialogSent CashuReceiveDialog.Msg
    | CashuWithdrawDialogSent CashuWithdrawDialog.Msg
    | UpdateNwcUriDraft String
    | ConnectNwc
    | ConnectNwcAlby
    | StartNwaConnect
    | CancelNwaConnect
    | EnableWebln
    | DisconnectNwc
    | ReceivedPortMessage IncomingMessage
    | PictureLoaded Bool
    | BannerLoaded Bool


update : Auth.User -> Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update user shared msg model =
    case msg of
        CategorySelected category ->
            switchToCategory user shared model category

        CategoryQueryChanged { to } ->
            switchToCategory user shared model (categoryFromQuery to)

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

        AddPrivateRelay pubKey relayUrl ->
            let
                updated =
                    Nostr.getPrivateRelayUrls shared.nostr pubKey
                        ++ [ relayUrl ]
                        |> RelayList.withUniqueEntries
            in
            ( { model | data = RelaysData { emptyRelaysModel | state = RelayListStateSaving (Nostr.getLastSendRequestId shared.nostr) } }
            , sendPrivateRelayListCmd pubKey (Nostr.getWriteRelayUrlsForPubKey shared.nostr pubKey) updated
            )

        AddLocalRelay relayUrl ->
            let
                updated =
                    Nostr.getLocalRelayUrls shared.nostr
                        ++ [ relayUrl ]
                        |> RelayList.withUniqueEntries
            in
            ( { model | data = RelaysData emptyRelaysModel }
            , Effect.sendSharedMsg (Shared.Msg.SetLocalRelays updated)
            )

        AddSuggestedLocalRelays relayUrls ->
            let
                updated =
                    Nostr.getLocalRelayUrls shared.nostr
                        ++ relayUrls
                        |> RelayList.withUniqueEntries
            in
            ( { model | data = RelaysData emptyRelaysModel }
            , Effect.sendSharedMsg (Shared.Msg.SetLocalRelays updated)
            )

        RemovePrivateRelay pubKey relayUrl ->
            let
                updated =
                    Nostr.getPrivateRelayUrls shared.nostr pubKey
                        |> List.filter (\url -> Relay.toKey url /= Relay.toKey relayUrl)
            in
            ( { model | data = RelaysData { emptyRelaysModel | state = RelayListStateSaving (Nostr.getLastSendRequestId shared.nostr) } }
            , sendPrivateRelayListCmd pubKey (Nostr.getWriteRelayUrlsForPubKey shared.nostr pubKey) updated
            )

        RemoveLocalRelay relayUrl ->
            let
                updated =
                    Nostr.getLocalRelayUrls shared.nostr
                        |> List.filter (\url -> Relay.toKey url /= Relay.toKey relayUrl)
            in
            ( { model | data = RelaysData emptyRelaysModel }
            , Effect.sendSharedMsg (Shared.Msg.SetLocalRelays updated)
            )

        AddSearchRelay pubKey relayUrl ->
            if Nostr.isBlockedRelay shared.nostr relayUrl then
                ( model, Effect.none )

            else
                let
                    updated =
                        (Nostr.getSearchRelaysForPubKey shared.nostr pubKey |> List.map .url)
                            ++ [ relayUrl ]
                            |> RelayList.withUniqueEntries
                in
                ( { model | data = RelaysData { emptyRelaysModel | state = RelayListStateSaving (Nostr.getLastSendRequestId shared.nostr) } }
                , sendSearchRelayListCmd pubKey (Nostr.getWriteRelayUrlsForPubKey shared.nostr pubKey) updated
                )

        RemoveSearchRelay pubKey relayUrl ->
            let
                updated =
                    (Nostr.getSearchRelaysForPubKey shared.nostr pubKey |> List.map .url)
                        |> List.filter (\url -> Relay.toKey url /= Relay.toKey relayUrl)
            in
            ( { model | data = RelaysData { emptyRelaysModel | state = RelayListStateSaving (Nostr.getLastSendRequestId shared.nostr) } }
            , sendSearchRelayListCmd pubKey (Nostr.getWriteRelayUrlsForPubKey shared.nostr pubKey) updated
            )

        AddDefaultSearchRelays pubKey relayUrls ->
            let
                updated =
                    (Nostr.getSearchRelaysForPubKey shared.nostr pubKey |> List.map .url)
                        ++ relayUrls
                        |> RelayList.withUniqueEntries
                        |> List.filter (\url -> not (Nostr.isBlockedRelay shared.nostr url))
            in
            ( { model | data = RelaysData { emptyRelaysModel | state = RelayListStateSaving (Nostr.getLastSendRequestId shared.nostr) } }
            , sendSearchRelayListCmd pubKey (Nostr.getWriteRelayUrlsForPubKey shared.nostr pubKey) updated
            )

        AddBlockedRelay pubKey relayUrl ->
            let
                updated =
                    publishedBlockedRelays shared.nostr pubKey [ relayUrl ]
            in
            ( { model | data = RelaysData { emptyRelaysModel | state = RelayListStateSaving (Nostr.getLastSendRequestId shared.nostr) } }
            , sendBlockedRelayListCmd pubKey (Nostr.getWriteRelayUrlsForPubKey shared.nostr pubKey) updated
            )

        RemoveBlockedRelay pubKey relayUrl ->
            if List.any (\blocked -> Relay.toKey blocked == Relay.toKey relayUrl) Pareto.blockedRelays then
                ( model, Effect.none )

            else
                let
                    updated =
                        Nostr.getUserBlockedRelayUrls shared.nostr pubKey
                            |> List.filter (\url -> Relay.toKey url /= Relay.toKey relayUrl)
                            |> (\userList -> RelayList.withUniqueEntries (Pareto.blockedRelays ++ userList))
                in
                ( { model | data = RelaysData { emptyRelaysModel | state = RelayListStateSaving (Nostr.getLastSendRequestId shared.nostr) } }
                , sendBlockedRelayListCmd pubKey (Nostr.getWriteRelayUrlsForPubKey shared.nostr pubKey) updated
                )

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
                |> sendNip96MediaServerListCmd shared.browserEnv shared.nostr pubKey
            )

        RemoveNip96MediaServer pubKey mediaServer ->
            ( { model | data = MediaServersData { emptyMediaServersModel | state = MediaServerStateSavingNip96 (Nostr.getLastSendRequestId shared.nostr) } }
            , Nostr.getNip96Servers shared.nostr pubKey
                |> removeMediaServerFromList mediaServer
                |> sendNip96MediaServerListCmd shared.browserEnv shared.nostr pubKey
            )

        AddBlossomMediaServer pubKey mediaServer ->
            ( { model | data = MediaServersData { emptyMediaServersModel | state = MediaServerStateSavingBlossom (Nostr.getLastSendRequestId shared.nostr) } }
            , Nostr.getBlossomServers shared.nostr pubKey
                |> extendMediaServerList mediaServer
                |> sendBlossomMediaServerListCmd shared.browserEnv shared.nostr pubKey
            )

        RemoveBlossomMediaServer pubKey mediaServer ->
            ( { model | data = MediaServersData { emptyMediaServersModel | state = MediaServerStateSavingBlossom (Nostr.getLastSendRequestId shared.nostr) } }
            , Nostr.getBlossomServers shared.nostr pubKey
                |> removeMediaServerFromList mediaServer
                |> sendBlossomMediaServerListCmd shared.browserEnv shared.nostr pubKey
            )

        AddDefaultBlossomMediaServers pubKey mediaServers ->
            ( { model | data = MediaServersData { emptyMediaServersModel | state = MediaServerStateSavingBlossom (Nostr.getLastSendRequestId shared.nostr) } }
            , mediaServers
                |> sendBlossomMediaServerListCmd shared.browserEnv shared.nostr pubKey
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

        UnmutePubKey userPubKey mutedPubKey ->
            ( { model | data = MutesData { state = MuteListStateSaving (Nostr.getLastSendRequestId shared.nostr) } }
            , SendMuteListWithoutPubKey userPubKey mutedPubKey
                |> Shared.Msg.SendNostrEvent
                |> Effect.sendSharedMsg
            )

        EnableEcashWallet ->
            case model.data of
                EcashData ecashModel ->
                    if List.isEmpty ecashModel.mints || ecashModel.enabling then
                        ( model, Effect.none )

                    else
                        let
                            maybeExistingPrivkey =
                                Nostr.getCashuWallet shared.nostr
                                    |> Maybe.map .privkey
                        in
                        ( { model | data = EcashData { ecashModel | enabling = True } }
                        , Effect.sendCmd (Ports.createCashuWallet maybeExistingPrivkey)
                        )

                _ ->
                    ( model, Effect.none )

        DisableEcashWallet ->
            let
                writeRelays =
                    Nostr.getWriteRelayUrlsForPubKey shared.nostr user.pubKey

                disableRec =
                    CashuWallet.mintRecommendationEvent user.pubKey writeRelays [] Nothing

                ( modelAfterCancel, cancelEffect ) =
                    closeEcashReceiveDialog user shared model
            in
            ( modelAfterCancel
            , Effect.batch
                [ cancelEffect
                , disableRec
                    |> SendNutzapMintRecommendation
                    |> Shared.Msg.SendNostrEvent
                    |> Effect.sendSharedMsg
                ]
            )

        UpdateEcashMintDraft draft ->
            case model.data of
                EcashData ecashModel ->
                    ( { model
                        | data =
                            EcashData
                                { ecashModel
                                    | mintDraft =
                                        if String.isEmpty draft then
                                            Nothing

                                        else
                                            Just draft
                                }
                      }
                    , Effect.none
                    )

                _ ->
                    ( model, Effect.none )

        AddEcashMint ->
            case model.data of
                EcashData ecashModel ->
                    case normalizeMintUrl ecashModel.mintDraft of
                        Just mintUrl ->
                            if List.member mintUrl ecashModel.mints then
                                ( { model | data = EcashData { ecashModel | mintDraft = Nothing } }
                                , Effect.none
                                )

                            else
                                let
                                    updatedMints =
                                        ecashModel.mints ++ [ mintUrl ]

                                    updatedModel =
                                        { ecashModel
                                            | mints = updatedMints
                                            , mintDraft = Nothing
                                        }
                                in
                                ( { model | data = EcashData updatedModel }
                                , persistEcashMintsIfWalletExists user shared updatedMints
                                )

                        Nothing ->
                            ( model, Effect.none )

                _ ->
                    ( model, Effect.none )

        RemoveEcashMint mintUrl ->
            case model.data of
                EcashData ecashModel ->
                    let
                        mints =
                            List.filter ((/=) mintUrl) ecashModel.mints

                        updatedModel =
                            { ecashModel | mints = mints }
                    in
                    ( { model | data = EcashData updatedModel }
                    , persistEcashMintsIfWalletExists user shared mints
                    )

                _ ->
                    ( model, Effect.none )

        CashuReceiveDialogSent innerMsg ->
            case model.data of
                EcashData ecashModel ->
                    CashuReceiveDialog.update
                        { msg = innerMsg
                        , model = ecashModel.receiveDialog
                        , toModel =
                            \receiveDialog ->
                                { model | data = EcashData { ecashModel | receiveDialog = receiveDialog } }
                        , toMsg = CashuReceiveDialogSent
                        , mints =
                            if List.isEmpty ecashModel.mints then
                                Nostr.getCashuWallet shared.nostr
                                    |> Maybe.map .mints
                                    |> Maybe.withDefault []

                            else
                                ecashModel.mints
                        , userPubKey = user.pubKey
                        , browserEnv = shared.browserEnv
                        }

                _ ->
                    ( model, Effect.none )

        CashuWithdrawDialogSent innerMsg ->
            case model.data of
                EcashData ecashModel ->
                    CashuWithdrawDialog.update
                        { msg = innerMsg
                        , model = ecashModel.withdrawDialog
                        , toModel =
                            \withdrawDialog ->
                                { model | data = EcashData { ecashModel | withdrawDialog = withdrawDialog } }
                        , toMsg = CashuWithdrawDialogSent
                        , mints =
                            if List.isEmpty ecashModel.mints then
                                Nostr.getCashuWallet shared.nostr
                                    |> Maybe.map .mints
                                    |> Maybe.withDefault []

                            else
                                ecashModel.mints
                        , userPubKey = user.pubKey
                        , browserEnv = shared.browserEnv
                        , nostr = shared.nostr
                        }

                _ ->
                    ( model, Effect.none )

        UpdateNwcUriDraft uri ->
            case model.data of
                WalletData walletModel ->
                    ( { model | data = WalletData { walletModel | uriDraft = uri, error = Nothing } }
                    , Effect.none
                    )

                _ ->
                    ( model, Effect.none )

        ConnectNwc ->
            case model.data of
                WalletData walletModel ->
                    let
                        uri =
                            String.trim walletModel.uriDraft
                    in
                    if String.isEmpty uri then
                        ( { model
                            | data =
                                WalletData
                                    { walletModel
                                        | error = Just (Translations.nwcInvalidUriText [ shared.browserEnv.translations ])
                                    }
                          }
                        , Effect.none
                        )

                    else
                        ( { model
                            | data =
                                WalletData
                                    { walletModel
                                        | connecting = True
                                        , error = Nothing
                                        , nwaUri = Nothing
                                    }
                          }
                        , Effect.batch
                            [ Effect.sendCmd Ports.cancelNwaConnect
                            , Effect.sendCmd (Ports.connectNwc uri)
                            ]
                        )

                _ ->
                    ( model, Effect.none )

        ConnectNwcAlby ->
            case model.data of
                WalletData walletModel ->
                    ( { model
                        | data =
                            WalletData
                                { walletModel
                                    | connecting = True
                                    , error = Nothing
                                    , nwaUri = Nothing
                                }
                      }
                    , Effect.batch
                        [ Effect.sendCmd Ports.cancelNwaConnect
                        , Effect.sendCmd Ports.connectNwcAlby
                        ]
                    )

                _ ->
                    ( model, Effect.none )

        StartNwaConnect ->
            case model.data of
                WalletData walletModel ->
                    ( { model
                        | data =
                            WalletData
                                { walletModel
                                    | connecting = True
                                    , error = Nothing
                                    , nwaUri = Nothing
                                }
                      }
                    , Effect.sendCmd Ports.startNwaConnect
                    )

                _ ->
                    ( model, Effect.none )

        CancelNwaConnect ->
            case model.data of
                WalletData walletModel ->
                    ( { model
                        | data =
                            WalletData
                                { walletModel
                                    | connecting = False
                                    , nwaUri = Nothing
                                    , error = Nothing
                                }
                      }
                    , Effect.sendCmd Ports.cancelNwaConnect
                    )

                _ ->
                    ( model, Effect.none )

        EnableWebln ->
            case model.data of
                WalletData walletModel ->
                    ( { model
                        | data =
                            WalletData
                                { walletModel
                                    | connecting = True
                                    , error = Nothing
                                    , nwaUri = Nothing
                                }
                      }
                    , Effect.batch
                        [ Effect.sendCmd Ports.cancelNwaConnect
                        , Effect.sendCmd Ports.enableWebln
                        ]
                    )

                _ ->
                    ( model, Effect.none )

        DisconnectNwc ->
            case model.data of
                WalletData walletModel ->
                    ( { model
                        | data =
                            WalletData
                                { walletModel
                                    | connected = False
                                    , alias = Nothing
                                    , network = Nothing
                                    , lud16 = Nothing
                                    , balanceSats = Nothing
                                    , walletPubkeyShort = Nothing
                                    , uriDraft = ""
                                    , connecting = False
                                    , error = Nothing
                                    , nwaUri = Nothing
                                    , canAutoPay = walletModel.weblnAvailable
                                }
                      }
                    , Effect.sendCmd Ports.disconnectNwc
                    )

                _ ->
                    ( model, Effect.none )

        ReceivedPortMessage message ->
            case model.data of
                EcashData ecashModel ->
                    if isCashuReceivePortMessage message then
                        CashuReceiveDialog.update
                            { msg = CashuReceiveDialog.ReceivedPortMessage message
                            , model = ecashModel.receiveDialog
                            , toModel =
                                \receiveDialog ->
                                    { model | data = EcashData { ecashModel | receiveDialog = receiveDialog } }
                            , toMsg = CashuReceiveDialogSent
                            , mints = ecashModel.mints
                            , userPubKey = user.pubKey
                            , browserEnv = shared.browserEnv
                            }

                    else if isCashuWithdrawPortMessage message then
                        CashuWithdrawDialog.update
                            { msg = CashuWithdrawDialog.ReceivedPortMessage message
                            , model = ecashModel.withdrawDialog
                            , toModel =
                                \withdrawDialog ->
                                    { model | data = EcashData { ecashModel | withdrawDialog = withdrawDialog } }
                            , toMsg = CashuWithdrawDialogSent
                            , mints = ecashModel.mints
                            , userPubKey = user.pubKey
                            , browserEnv = shared.browserEnv
                            , nostr = shared.nostr
                            }

                    else
                        updateWithPortMessage user shared model message

                WalletData _ ->
                    updateWithPortMessage user shared model message

                _ ->
                    updateWithPortMessage user shared model message

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


closeEcashReceiveDialog : Auth.User -> Shared.Model -> Model -> ( Model, Effect Msg )
closeEcashReceiveDialog user shared model =
    case model.data of
        EcashData ecashModel ->
            let
                ( modelAfterReceive, receiveEffect ) =
                    if CashuReceiveDialog.isOpen ecashModel.receiveDialog then
                        CashuReceiveDialog.update
                            { msg = CashuReceiveDialog.CloseDialog
                            , model = ecashModel.receiveDialog
                            , toModel =
                                \receiveDialog ->
                                    { model | data = EcashData { ecashModel | receiveDialog = receiveDialog } }
                            , toMsg = CashuReceiveDialogSent
                            , mints = ecashModel.mints
                            , userPubKey = user.pubKey
                            , browserEnv = shared.browserEnv
                            }

                    else
                        ( model, Effect.none )

                ecashAfterReceive =
                    case modelAfterReceive.data of
                        EcashData e ->
                            e

                        _ ->
                            ecashModel
            in
            if CashuWithdrawDialog.isOpen ecashAfterReceive.withdrawDialog then
                let
                    ( modelAfterWithdraw, withdrawEffect ) =
                        CashuWithdrawDialog.update
                            { msg = CashuWithdrawDialog.CloseDialog
                            , model = ecashAfterReceive.withdrawDialog
                            , toModel =
                                \withdrawDialog ->
                                    { modelAfterReceive
                                        | data = EcashData { ecashAfterReceive | withdrawDialog = withdrawDialog }
                                    }
                            , toMsg = CashuWithdrawDialogSent
                            , mints = ecashAfterReceive.mints
                            , userPubKey = user.pubKey
                            , browserEnv = shared.browserEnv
                            , nostr = shared.nostr
                            }
                in
                ( modelAfterWithdraw, Effect.batch [ receiveEffect, withdrawEffect ] )

            else
                ( modelAfterReceive, receiveEffect )

        _ ->
            ( model, Effect.none )


isCashuReceivePortMessage : IncomingMessage -> Bool
isCashuReceivePortMessage message =
    List.member message.messageType
        [ "cashuMintInvoice"
        , "cashuMinted"
        , "cashuMintFailed"
        , "cashuMintCancelled"
        ]


isCashuWithdrawPortMessage : IncomingMessage -> Bool
isCashuWithdrawPortMessage message =
    List.member message.messageType
        [ "cashuMeltQuote"
        , "cashuMelted"
        , "cashuMeltFailed"
        ]


{-| Keep kind 17375 mint list in sync while configuring (disabled) so reloads don't restore removed mints.
-}
persistEcashMintsIfWalletExists : Auth.User -> Shared.Model -> List String -> Effect Msg
persistEcashMintsIfWalletExists user shared mints =
    case Nostr.getCashuWallet shared.nostr of
        Just wallet ->
            CashuWallet.walletEvent user.pubKey wallet.privkey mints
                |> SendCashuWallet
                |> Shared.Msg.SendNostrEvent
                |> Effect.sendSharedMsg

        Nothing ->
            Effect.none


updateWithPortMessage : Auth.User -> Shared.Model -> Model -> IncomingMessage -> ( Model, Effect Msg )
updateWithPortMessage user shared model message =
    case message.messageType of
        "nwcStatus" ->
            case ( model.data, Decode.decodeValue nwcStatusDecoder message.value ) of
                ( WalletData walletModel, Ok status ) ->
                    ( { model
                        | data =
                            WalletData
                                { walletModel
                                    | connected = status.connected
                                    , alias = status.alias
                                    , network = status.network
                                    , lud16 = status.lud16
                                    , balanceSats = status.balanceSats
                                    , walletPubkeyShort = status.walletPubkeyShort
                                    , weblnAvailable = status.weblnAvailable
                                    , canAutoPay = status.canAutoPay
                                    , connecting =
                                        if status.connected then
                                            False

                                        else
                                            walletModel.connecting
                                    , nwaUri =
                                        if status.connected then
                                            Nothing

                                        else
                                            walletModel.nwaUri
                                }
                      }
                    , Effect.none
                    )

                _ ->
                    ( model, Effect.none )

        "nwaWaiting" ->
            case ( model.data, Decode.decodeValue nwaWaitingDecoder message.value ) of
                ( WalletData walletModel, Ok waiting ) ->
                    ( { model
                        | data =
                            WalletData
                                { walletModel
                                    | nwaUri =
                                        if waiting.active then
                                            waiting.connectionUri

                                        else
                                            Nothing
                                    , connecting =
                                        if waiting.active then
                                            True

                                        else if walletModel.nwaUri /= Nothing then
                                            False

                                        else
                                            walletModel.connecting
                                    , error =
                                        if waiting.active then
                                            Nothing

                                        else
                                            walletModel.error
                                }
                      }
                    , Effect.none
                    )

                _ ->
                    ( model, Effect.none )

        "nwcConnectResult" ->
            case ( model.data, Decode.decodeValue nwcConnectResultDecoder message.value ) of
                ( WalletData walletModel, Ok result ) ->
                    if result.ok then
                        ( { model
                            | data =
                                WalletData
                                    { walletModel
                                        | connecting = False
                                        , uriDraft = ""
                                        , error = Nothing
                                        , nwaUri = Nothing
                                    }
                          }
                        , Effect.none
                        )

                    else
                        ( { model
                            | data =
                                WalletData
                                    { walletModel
                                        | connecting = False
                                        , nwaUri = Nothing
                                        , error =
                                            Just
                                                (result.reason
                                                    |> Maybe.withDefault (Translations.nwcConnectErrorText [ shared.browserEnv.translations ])
                                                )
                                    }
                          }
                        , Effect.none
                        )

                _ ->
                    ( model, Effect.none )

        "cashuWalletCreated" ->
            case ( model.data, Decode.decodeValue cashuWalletCreatedDecoder message.value ) of
                ( EcashData ecashModel, Ok created ) ->
                    let
                        writeRelays =
                            Nostr.getWriteRelayUrlsForPubKey shared.nostr user.pubKey

                        mints =
                            if List.isEmpty ecashModel.mints then
                                [ created.mintUrl ]

                            else
                                ecashModel.mints

                        walletEvt =
                            CashuWallet.walletEvent user.pubKey created.privkey mints

                        recEvt =
                            CashuWallet.mintRecommendationEvent user.pubKey writeRelays mints (Just created.pubkey)
                    in
                    ( { model
                        | data =
                            EcashData
                                { ecashModel
                                    | enabling = False
                                    , mints = mints
                                }
                      }
                    , Effect.batch
                        [ walletEvt
                            |> SendCashuWallet
                            |> Shared.Msg.SendNostrEvent
                            |> Effect.sendSharedMsg
                        , recEvt
                            |> SendNutzapMintRecommendation
                            |> Shared.Msg.SendNostrEvent
                            |> Effect.sendSharedMsg
                        ]
                    )

                ( EcashData ecashModel, Err _ ) ->
                    ( { model | data = EcashData { ecashModel | enabling = False } }, Effect.none )

                _ ->
                    ( model, Effect.none )

        "published" ->
            case ( model.data, Nostr.External.decodeSendId message.value, Nostr.External.decodeEvent message.value ) of
                ( RelaysData relaysModel, Ok incomingSendId, _ ) ->
                    case relaysModel.state of
                        RelayListStateSaving sendRequestId ->
                            if sendRequestId == incomingSendId then
                                ( { model
                                    | data =
                                        RelaysData
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

                ( MutesData mutesModel, Ok incomingSendId, _ ) ->
                    case mutesModel.state of
                        MuteListStateSaving sendRequestId ->
                            if sendRequestId == incomingSendId then
                                ( { model | data = MutesData { state = MuteListStateEditing } }
                                , Effect.none
                                )

                            else
                                ( model, Effect.none )

                        _ ->
                            ( model, Effect.none )

                ( ProfileData profileModel, Ok incomingSendId, Ok event ) ->
                    case ( profileModel.state, profileFromEvent event ) of
                        ( EditStateSaving sendRequestId, Just profile ) ->
                            if sendRequestId == incomingSendId then
                                ( { model
                                    | data =
                                        ProfileData
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
                                    | data =
                                        MediaServersData
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
                                    | data =
                                        MediaServersData
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


type alias CashuWalletCreated =
    { privkey : String
    , pubkey : String
    , mintUrl : String
    }


cashuWalletCreatedDecoder : Decode.Decoder CashuWalletCreated
cashuWalletCreatedDecoder =
    Decode.map3 CashuWalletCreated
        (Decode.field "privkey" Decode.string)
        (Decode.field "pubkey" Decode.string)
        (Decode.field "mintUrl" Decode.string)


type alias NwcStatus =
    { connected : Bool
    , alias : Maybe String
    , network : Maybe String
    , lud16 : Maybe String
    , balanceSats : Maybe Int
    , walletPubkeyShort : Maybe String
    , weblnAvailable : Bool
    , canAutoPay : Bool
    }


nwcStatusDecoder : Decode.Decoder NwcStatus
nwcStatusDecoder =
    Decode.succeed NwcStatus
        |> DecodePipeline.required "connected" Decode.bool
        |> DecodePipeline.custom
            (Decode.oneOf
                [ Decode.field "alias" (Decode.nullable Decode.string)
                , Decode.succeed Nothing
                ]
                |> Decode.map (Maybe.andThen emptyStringToNothing)
            )
        |> DecodePipeline.custom
            (Decode.oneOf
                [ Decode.field "network" (Decode.nullable Decode.string)
                , Decode.succeed Nothing
                ]
                |> Decode.map (Maybe.andThen emptyStringToNothing)
            )
        |> DecodePipeline.custom
            (Decode.oneOf
                [ Decode.field "lud16" (Decode.nullable Decode.string)
                , Decode.succeed Nothing
                ]
                |> Decode.map (Maybe.andThen emptyStringToNothing)
            )
        |> DecodePipeline.custom
            (Decode.oneOf
                [ Decode.field "balanceSats" (Decode.nullable Decode.int)
                , Decode.succeed Nothing
                ]
            )
        |> DecodePipeline.custom
            (Decode.oneOf
                [ Decode.field "walletPubkeyShort" (Decode.nullable Decode.string)
                , Decode.succeed Nothing
                ]
                |> Decode.map (Maybe.andThen emptyStringToNothing)
            )
        |> DecodePipeline.custom
            (Decode.oneOf
                [ Decode.field "weblnAvailable" Decode.bool
                , Decode.succeed False
                ]
            )
        |> DecodePipeline.custom
            (Decode.oneOf
                [ Decode.field "canAutoPay" Decode.bool
                , Decode.succeed False
                ]
            )


type alias NwaWaiting =
    { active : Bool
    , connectionUri : Maybe String
    }


nwaWaitingDecoder : Decode.Decoder NwaWaiting
nwaWaitingDecoder =
    Decode.map2 NwaWaiting
        (Decode.field "active" Decode.bool)
        (Decode.oneOf
            [ Decode.field "connectionUri" (Decode.nullable Decode.string)
            , Decode.succeed Nothing
            ]
            |> Decode.map (Maybe.andThen emptyStringToNothing)
        )


type alias NwcConnectResult =
    { ok : Bool
    , reason : Maybe String
    }


nwcConnectResultDecoder : Decode.Decoder NwcConnectResult
nwcConnectResultDecoder =
    Decode.map2 NwcConnectResult
        (Decode.field "ok" Decode.bool)
        (Decode.maybe (Decode.field "reason" Decode.string))


emptyStringToNothing : String -> Maybe String
emptyStringToNothing value =
    if String.isEmpty value then
        Nothing

    else
        Just value


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


sendNip96MediaServerListCmd : BrowserEnv -> Nostr.Model -> PubKey -> List ServerUrl -> Effect msg
sendNip96MediaServerListCmd browserEnv nostr pubKey mediaServers =
    eventWithNip96ServerList browserEnv pubKey mediaServers
        |> SendFileStorageServerList (Nostr.getWriteRelayUrlsForPubKey nostr pubKey)
        |> Shared.Msg.SendNostrEvent
        |> Effect.sendSharedMsg


sendBlossomMediaServerListCmd : BrowserEnv -> Nostr.Model -> PubKey -> List ServerUrl -> Effect msg
sendBlossomMediaServerListCmd browserEnv nostr pubKey mediaServers =
    eventWithBlossomServerList browserEnv pubKey mediaServers
        |> SendFileStorageServerList (Nostr.getWriteRelayUrlsForPubKey nostr pubKey)
        |> Shared.Msg.SendNostrEvent
        |> Effect.sendSharedMsg


relayListWithRole : List RelayUrl -> RelayRole -> List RelayMetadata
relayListWithRole relayUrls role =
    relayUrls
        |> List.map
            (\relayUrl ->
                { url = relayUrl, role = role }
            )


sendRelayListCmd : PubKey -> List RelayMetadata -> Effect msg
sendRelayListCmd pubKey relays =
    let
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
    eventWithRelayList pubKey relays
        |> SendRelayList relayUrls
        |> Shared.Msg.SendNostrEvent
        |> Effect.sendSharedMsg


sendPrivateRelayListCmd : PubKey -> List RelayUrl -> List RelayUrl -> Effect msg
sendPrivateRelayListCmd pubKey writeRelays privateRelays =
    -- Announce kind 10013 on write relays; draft traffic uses the private URLs themselves.
    eventWithPrivateRelayList pubKey privateRelays
        |> SendPrivateRelayList writeRelays
        |> Shared.Msg.SendNostrEvent
        |> Effect.sendSharedMsg


sendSearchRelayListCmd : PubKey -> List RelayUrl -> List RelayUrl -> Effect msg
sendSearchRelayListCmd pubKey writeRelays searchRelays =
    eventWithSearchRelayList pubKey searchRelays
        |> SendSearchRelayList writeRelays
        |> Shared.Msg.SendNostrEvent
        |> Effect.sendSharedMsg


sendBlockedRelayListCmd : PubKey -> List RelayUrl -> List RelayUrl -> Effect msg
sendBlockedRelayListCmd pubKey writeRelays blockedRelays =
    eventWithBlockedRelayList pubKey blockedRelays
        |> SendBlockedRelayList writeRelays
        |> Shared.Msg.SendNostrEvent
        |> Effect.sendSharedMsg


publishedBlockedRelays : Nostr.Model -> PubKey -> List RelayUrl -> List RelayUrl
publishedBlockedRelays nostr pubKey extra =
    RelayList.withUniqueEntries (Pareto.blockedRelays ++ Nostr.getUserBlockedRelayUrls nostr pubKey ++ extra)


categoryFromQuery : Maybe String -> Category
categoryFromQuery maybeCategory =
    maybeCategory
        |> Maybe.andThen categoryFromString
        |> Maybe.withDefault Relays


categoryDataMatches : Model -> Category -> Bool
categoryDataMatches model category =
    case ( category, model.data ) of
        ( Relays, RelaysData _ ) ->
            True

        ( MediaServers, MediaServersData _ ) ->
            True

        ( Profile, ProfileData _ ) ->
            True

        ( Mutes, MutesData _ ) ->
            True

        ( Wallet, WalletData _ ) ->
            True

        ( Ecash, EcashData _ ) ->
            True

        _ ->
            False


{-| Shared entry for tab clicks and `?category=` URL changes.
Skip when selection and loaded data already match (avoids a double load after replaceRoute).
-}
switchToCategory : Auth.User -> Shared.Model -> Model -> Category -> ( Model, Effect Msg )
switchToCategory user shared model category =
    if Categories.selected model.categories == category && categoryDataMatches model category then
        ( model, Effect.none )

    else
        updateModelWithCategory user shared model category


updateModelWithCategory : Auth.User -> Shared.Model -> Model -> Category -> ( Model, Effect Msg )
updateModelWithCategory user shared model category =
    let
        modelWithSelection =
            { model | categories = Categories.select model.categories category }

        ( modelReady, leaveEffect ) =
            case modelWithSelection.data of
                EcashData _ ->
                    closeEcashReceiveDialog user shared modelWithSelection

                WalletData walletModel ->
                    if walletModel.nwaUri /= Nothing || walletModel.connecting then
                        ( { modelWithSelection | data = WalletData { walletModel | nwaUri = Nothing, connecting = False } }
                        , Effect.sendCmd Ports.cancelNwaConnect
                        )

                    else
                        ( modelWithSelection, Effect.none )

                _ ->
                    ( modelWithSelection, Effect.none )

        ( newModel, effect ) =
            case category of
                Relays ->
                    ( { modelReady | data = RelaysData emptyRelaysModel }
                    , RequestRelayLists { emptyEventFilter | kinds = Just [ KindRelayListMetadata, KindBlockedRelaysList, KindSearchRelaysList, KindPrivateRelayList, KindRelayListForDMs ], authors = Just [ user.pubKey ] }
                        |> Nostr.createRequest shared.nostr "Relay lists of user" []
                        |> Shared.Msg.RequestNostrEvents
                        |> Effect.sendSharedMsg
                    )

                MediaServers ->
                    ( { modelReady | data = MediaServersData emptyMediaServersModel }
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
                    ( { modelReady | data = ProfileData profileModel }
                    , Effect.batch
                        [ profileEffect
                        , Shared.Msg.LoadUserDataByPubKey user.pubKey
                            |> Effect.sendSharedMsg
                        ]
                    )

                Mutes ->
                    ( { modelReady | data = MutesData emptyMutesModel }
                    , Effect.none
                    )

                Ecash ->
                    ( { modelReady | data = EcashData (emptyEcashModel shared) }
                    , Effect.none
                    )

                Wallet ->
                    ( { modelReady | data = WalletData emptyWalletModel }
                    , Effect.sendCmd Ports.getNwcStatus
                    )
    in
    ( newModel
    , Effect.batch
        [ Effect.replaceRoute { path = model.path, query = Dict.singleton categoryParamName (stringFromCategory category), hash = Nothing }
        , leaveEffect
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

        ( Mutes, MutesData mutesModel ) ->
            viewMutes shared user mutesModel

        ( Ecash, EcashData ecashModel ) ->
            viewEcash shared user ecashModel

        ( Wallet, WalletData walletModel ) ->
            viewWallet shared walletModel

        _ ->
            emptyHtml


viewWalletDetails : I18Next.Translations -> WalletModel -> Html Msg
viewWalletDetails translations walletModel =
    if not walletModel.connected then
        emptyHtml

    else
        let
            row : String -> String -> Html Msg
            row label value =
                p
                    [ css [ Tw.text_sm ] ]
                    [ text (label ++ ": " ++ value) ]

            aliasRow =
                case ( walletModel.alias, walletModel.lud16 ) of
                    ( Just alias, Just lud16 ) ->
                        if alias == lud16 then
                            emptyHtml

                        else
                            row (Translations.nwcAliasLabel [ translations ]) alias

                    ( Just alias, Nothing ) ->
                        row (Translations.nwcAliasLabel [ translations ]) alias

                    _ ->
                        emptyHtml

            lud16Row =
                case walletModel.lud16 of
                    Just lud16 ->
                        row (Translations.nwcLud16Label [ translations ]) lud16

                    Nothing ->
                        emptyHtml

            networkRow =
                case walletModel.network of
                    Just network ->
                        row (Translations.nwcNetworkLabel [ translations ]) network

                    Nothing ->
                        emptyHtml

            balanceRow =
                case walletModel.balanceSats of
                    Just balance ->
                        row
                            (Translations.nwcBalanceLabel [ translations ])
                            (String.fromInt balance ++ " sats")

                    Nothing ->
                        emptyHtml

            pubkeyRow =
                case walletModel.walletPubkeyShort of
                    Just pubkey ->
                        row (Translations.nwcWalletIdLabel [ translations ]) pubkey

                    Nothing ->
                        emptyHtml
        in
        div
            [ css [ Tw.flex, Tw.flex_col, Tw.gap_1 ] ]
            [ aliasRow
            , lud16Row
            , networkRow
            , balanceRow
            , pubkeyRow
            ]


viewWallet : Shared.Model -> WalletModel -> Html Msg
viewWallet shared walletModel =
    let
        translations =
            shared.browserEnv.translations

        statusText =
            if walletModel.connected then
                Translations.nwcConnectedLabel [ translations ]

            else if walletModel.canAutoPay then
                Translations.nwcWeblnReadyLabel [ translations ]

            else
                Translations.nwcDisconnectedLabel [ translations ]

        busy =
            walletModel.connecting

        methodButtons =
            [ Button.new
                { label =
                    if busy && walletModel.nwaUri == Nothing then
                        Translations.nwcConnectingButtonTitle [ translations ]

                    else
                        Translations.nwcConnectAlbyButtonTitle [ translations ]
                , onClick =
                    if busy then
                        Nothing

                    else
                        Just ConnectNwcAlby
                , theme = shared.theme
                }
                |> Button.withTypePrimary
                |> Button.withDisabled busy
                |> Button.view
            , Button.new
                { label = Translations.nwcConnectQrButtonTitle [ translations ]
                , onClick =
                    if busy then
                        Nothing

                    else
                        Just StartNwaConnect
                , theme = shared.theme
                }
                |> Button.withTypeSecondary
                |> Button.withDisabled busy
                |> Button.view
            ]
                ++ (if walletModel.weblnAvailable then
                        [ Button.new
                            { label = Translations.nwcConnectWeblnButtonTitle [ translations ]
                            , onClick =
                                if busy then
                                    Nothing

                                else
                                    Just EnableWebln
                            , theme = shared.theme
                            }
                            |> Button.withTypeSecondary
                            |> Button.withDisabled busy
                            |> Button.view
                        ]

                    else
                        []
                   )

        nwaSection =
            case walletModel.nwaUri of
                Just uri ->
                    let
                        qrCode =
                            uri
                                |> QRCode.fromString
                                |> Result.map
                                    (\qr ->
                                        qr
                                            |> QRCode.toSvg
                                                [ SvgAttr.width "220px"
                                                , SvgAttr.height "220px"
                                                ]
                                            |> Html.fromUnstyled
                                    )
                                |> Result.withDefault (text "")
                    in
                    div
                        [ css [ Tw.flex, Tw.flex_col, Tw.items_center, Tw.gap_3 ] ]
                        [ p [] [ text <| Translations.nwcQrWaitingText [ translations ] ]
                        , div
                            [ css [ Tw.bg_color Theme.white, Tw.p_2, Tw.rounded_md ] ]
                            [ qrCode ]
                        , p
                            [ css [ Tw.text_xs, Tw.break_all, Tw.max_w_xs, Tw.text_center ] ]
                            [ text (String.left 48 uri ++ "…") ]
                        , Button.new
                            { label = Translations.nwcCancelQrButtonTitle [ translations ]
                            , onClick = Just CancelNwaConnect
                            , theme = shared.theme
                            }
                            |> Button.withTypeSecondary
                            |> Button.view
                        ]

                Nothing ->
                    emptyHtml
    in
    div
        [ css
            [ Tw.flex
            , Tw.flex_col
            , Tw.gap_6
            ]
        ]
        [ h3
            [ css
                [ Tw.text_xl
                , Tw.font_semibold
                ]
            ]
            [ text <| Translations.nwcSectionTitle [ translations ] ]
        , p [] [ text <| Translations.nwcSectionDescription [ translations ] ]
        , p []
            [ text
                (Translations.nwcStatusLabel [ translations ]
                    ++ ": "
                    ++ statusText
                )
            ]
        , viewWalletDetails translations walletModel
        , if walletModel.connected && walletModel.nwaUri == Nothing then
            Button.new
                { label = Translations.nwcDisconnectButtonTitle [ translations ]
                , onClick = Just DisconnectNwc
                , theme = shared.theme
                }
                |> Button.withTypeSecondary
                |> Button.view

          else
            div
                [ css [ Tw.flex, Tw.flex_col, Tw.gap_4, Tw.max_w_xl ] ]
                [ if walletModel.canAutoPay && not walletModel.connected then
                    p
                        [ css [ Tw.text_sm ] ]
                        [ text <| Translations.nwcWeblnReadyLabel [ translations ] ]

                  else
                    text ""
                , div
                    [ css [ Tw.flex, Tw.flex_col, Tw.gap_2 ] ]
                    methodButtons
                , nwaSection
                , p
                    [ css [ Tw.text_sm, Tw.font_medium, Tw.mt_2 ] ]
                    [ text <| Translations.nwcManualFallbackTitle [ translations ] ]
                , EntryField.new
                    { value = walletModel.uriDraft
                    , onInput = UpdateNwcUriDraft
                    , theme = shared.theme
                    }
                    |> EntryField.withPlaceholder (Translations.nwcUriPlaceholder [ translations ])
                    |> EntryField.withType EntryField.FieldTypeText
                    |> EntryField.view
                , case walletModel.error of
                    Just err ->
                        p
                            [ css [ Tw.text_color Theme.red_600, Tw.text_sm ] ]
                            [ text err ]

                    Nothing ->
                        text ""
                , Button.new
                    { label =
                        if busy && walletModel.nwaUri == Nothing then
                            Translations.nwcConnectingButtonTitle [ translations ]

                        else
                            Translations.nwcConnectButtonTitle [ translations ]
                    , onClick =
                        if busy then
                            Nothing

                        else
                            Just ConnectNwc
                    , theme = shared.theme
                    }
                    |> Button.withTypeSecondary
                    |> Button.withDisabled busy
                    |> Button.view
                ]
        ]


viewEcash : Shared.Model -> Auth.User -> EcashModel -> Html Msg
viewEcash shared user ecashModel =
    let
        styles =
            stylesForTheme shared.theme

        maybeWallet =
            Nostr.getCashuWallet shared.nostr

        maybeRec =
            Nostr.getNutzapMintRecommendation shared.nostr

        balance =
            Nostr.getCashuBalance shared.nostr

        enabled =
            case ( maybeWallet, maybeRec ) of
                ( Just _, Just rec ) ->
                    rec.p2pkPubkey /= Nothing && not (List.isEmpty rec.mints)

                ( Just _, Nothing ) ->
                    True

                _ ->
                    False

        displayMints =
            if enabled then
                maybeWallet
                    |> Maybe.map .mints
                    |> Maybe.withDefault (Maybe.map .mints maybeRec |> Maybe.withDefault [])

            else
                ecashModel.mints

        readOnly =
            signingPubKeyAvailable shared.loginStatus
                |> not

        mintDraftValid =
            normalizeMintUrl ecashModel.mintDraft /= Nothing
    in
    div
        [ css
            [ Tw.flex
            , Tw.flex_col
            , Tw.gap_6
            ]
        ]
        [ h3
            (styles.colorStyleGrayscaleTitle ++ styles.textStyleH3)
            [ text <| Translations.ecashSectionTitle [ shared.browserEnv.translations ] ]
        , p [] [ text <| Translations.ecashSectionDescription [ shared.browserEnv.translations ] ]
        , p
            (styles.colorStyleGrayscaleText ++ styles.textStyleBody)
            [ text
                (Translations.ecashStatusLabel [ shared.browserEnv.translations ]
                    ++ ": "
                    ++ (if enabled then
                            Translations.ecashEnabledLabel [ shared.browserEnv.translations ]

                        else
                            Translations.ecashDisabledLabel [ shared.browserEnv.translations ]
                       )
                )
            ]
        , p
            (styles.colorStyleGrayscaleText ++ styles.textStyleBody)
            [ text (Translations.ecashBalanceLabel [ shared.browserEnv.translations ] ++ ": " ++ String.fromInt balance ++ " sats") ]
        , if enabled && not readOnly then
            div
                [ css
                    [ Tw.flex
                    , Tw.flex_row
                    , Tw.flex_wrap
                    , Tw.gap_3
                    ]
                ]
                [ CashuReceiveDialog.new
                    { model = ecashModel.receiveDialog
                    , toMsg = CashuReceiveDialogSent
                    , browserEnv = shared.browserEnv
                    , theme = shared.theme
                    , mints = displayMints
                    , userPubKey = user.pubKey
                    }
                    |> CashuReceiveDialog.view
                , CashuWithdrawDialog.new
                    { model = ecashModel.withdrawDialog
                    , toMsg = CashuWithdrawDialogSent
                    , browserEnv = shared.browserEnv
                    , theme = shared.theme
                    , mints = displayMints
                    , userPubKey = user.pubKey
                    , nostr = shared.nostr
                    }
                    |> CashuWithdrawDialog.view
                ]

          else
            emptyHtml
        , div
            [ css
                [ Tw.flex
                , Tw.flex_col
                , Tw.gap_2
                ]
            ]
            (p
                (styles.colorStyleGrayscaleText ++ styles.textStyleBody)
                [ text (Translations.ecashMintsLabel [ shared.browserEnv.translations ] ++ ":") ]
                :: (if List.isEmpty displayMints then
                        [ p
                            (styles.colorStyleGrayscaleMuted ++ styles.textStyleBody)
                            [ text "—" ]
                        ]

                    else
                        List.map
                            (\mint ->
                                div
                                    [ css
                                        [ Tw.flex
                                        , Tw.flex_row
                                        , Tw.items_center
                                        , Tw.gap_2
                                        , Tw.ml_2
                                        ]
                                    ]
                                    [ p
                                        (styles.colorStyleGrayscaleText ++ styles.textStyleBody)
                                        [ text mint ]
                                    , if readOnly || enabled then
                                        emptyHtml

                                      else
                                        div
                                            [ css
                                                [ Tw.cursor_pointer
                                                , Tw.text_color styles.colorB3
                                                , darkMode
                                                    [ Tw.text_color styles.colorB3DarkMode
                                                    ]
                                                ]
                                            , Events.onClick (RemoveEcashMint mint)
                                            , Attr.attribute "data-test" ("remove-ecash-mint-" ++ mint)
                                            , Attr.title (Translations.ecashRemoveMintButtonTitle [ shared.browserEnv.translations ])
                                            ]
                                            [ Icon.FeatherIcon FeatherIcons.delete
                                                |> Icon.view
                                            ]
                                    ]
                            )
                            displayMints
                   )
            )
        , if readOnly || enabled then
            emptyHtml

          else
            addEcashMintBox
                shared.theme
                shared.browserEnv.translations
                ecashModel.mintDraft
                mintDraftValid
                { identifier = "ecash-mint-suggestions"
                , suggestions = missingMints displayMints [ CashuWallet.defaultMintUrl ]
                }
        , if readOnly then
            emptyHtml

          else if enabled then
            Button.new
                { label = Translations.ecashDisableButtonTitle [ shared.browserEnv.translations ]
                , onClick = Just DisableEcashWallet
                , theme = shared.theme
                }
                |> Button.view

          else
            Button.new
                { label =
                    if ecashModel.enabling then
                        Translations.ecashEnablingButtonTitle [ shared.browserEnv.translations ]

                    else
                        Translations.ecashEnableButtonTitle [ shared.browserEnv.translations ]
                , onClick =
                    if ecashModel.enabling || List.isEmpty ecashModel.mints then
                        Nothing

                    else
                        Just EnableEcashWallet
                , theme = shared.theme
                }
                |> Button.withTypePrimary
                |> Button.withDisabled (List.isEmpty ecashModel.mints)
                |> Button.view
        ]



addEcashMintBox : Theme -> I18Next.Translations -> Maybe String -> Bool -> Suggestions -> Html Msg
addEcashMintBox theme translations maybeDraft draftValid suggestions =
    let
        styles =
            stylesForTheme theme

        showProtocolPrefix =
            maybeDraft
                |> Maybe.map (\value -> not <| String.startsWith "https://" value || String.startsWith "http://" value)
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
                            , Tw.h_10
                            , Tw.items_center
                            , Tw.justify_start
                            , Tw.left_3
                            , Tw.top_0
                            , Tw.pointer_events_none
                            , Tw.whitespace_nowrap
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
                    ++ [ Attr.placeholder <| Translations.ecashMintPlaceholder [ translations ]
                       , Attr.value (Maybe.withDefault "" maybeDraft)
                       , Attr.type_ "url"
                       , Attr.spellcheck False
                       , Attr.list suggestions.identifier
                       , Events.onInput UpdateEcashMintDraft
                       , css
                            ([ Tw.appearance_none
                             , Tw.bg_scroll
                             , Tw.bg_clip_border
                             , Tw.rounded_md
                             , Tw.border_2
                             , Tw.box_border
                             , Tw.cursor_text
                             , Tw.block
                             , Tw.pe_16
                             , Tw.pr_16
                             , Tw.h_10
                             , Tw.w_full
                             ]
                                ++ (if showProtocolPrefix then
                                        [ Tw.ps_20
                                        , Tw.pl_20
                                        ]

                                    else
                                        [ Tw.ps_3
                                        , Tw.pl_3
                                        ]
                                   )
                            )
                       ]
                )
                []
            , mintSuggestionDataList suggestions
            ]
        , Button.new
            { label = Translations.ecashAddMintButtonTitle [ translations ]
            , onClick =
                if draftValid then
                    Just AddEcashMint

                else
                    Nothing
            , theme = theme
            }
            |> Button.withDisabled (not draftValid)
            |> Button.view
        ]


mintSuggestionDataList : Suggestions -> Html Msg
mintSuggestionDataList suggestions =
    datalist
        [ Attr.id suggestions.identifier ]
        (suggestions.suggestions
            |> List.map
                (\mintUrl ->
                    option [ Attr.value mintUrl ] []
                )
        )


{-| Return suggested mint hosts (without https://) that are not already added.
-}
missingMints : List String -> List String -> List String
missingMints addedMints recommendedMints =
    let
        normalizedAdded =
            addedMints
                |> List.filterMap (\added -> normalizeMintUrl (Just added))
    in
    recommendedMints
        |> List.filterMap
            (\mintUrl ->
                case normalizeMintUrl (Just mintUrl) of
                    Just normalized ->
                        if List.member normalized normalizedAdded then
                            Nothing

                        else
                            Just (stripMintProtocol mintUrl)

                    Nothing ->
                        Nothing
            )


stripMintProtocol : String -> String
stripMintProtocol value =
    if String.startsWith "https://" value then
        String.dropLeft 8 value

    else if String.startsWith "http://" value then
        String.dropLeft 7 value

    else
        value


normalizeMintUrl : Maybe String -> Maybe String
normalizeMintUrl maybeDraft =
    maybeDraft
        |> Maybe.map String.trim
        |> Maybe.andThen
            (\draft ->
                if draft == "" then
                    Nothing

                else
                    let
                        withProtocol =
                            if String.startsWith "https://" draft || String.startsWith "http://" draft then
                                draft

                            else
                                "https://" ++ draft
                    in
                    Just (stripTrailingSlash withProtocol)
            )


stripTrailingSlash : String -> String
stripTrailingSlash value =
    if String.endsWith "/" value && String.length value > 8 then
        String.dropRight 1 value

    else
        value


type alias Suggestions =
    { identifier : String
    , suggestions : List String
    }


viewMutes : Shared.Model -> Auth.User -> MutesModel -> Html Msg
viewMutes shared user mutesModel =
    let
        styles =
            stylesForTheme shared.theme

        muted =
            Nostr.getMuteList shared.nostr user.pubKey
                |> Maybe.withDefault []

        readOnly =
            signingPubKeyAvailable shared.loginStatus
                |> not

        saving =
            case mutesModel.state of
                MuteListStateSaving _ ->
                    True

                _ ->
                    False
    in
    div
        [ css
            [ Tw.flex
            , Tw.flex_col
            , Tw.gap_4
            ]
        ]
        [ h3
            (styles.colorStyleGrayscaleTitle ++ styles.textStyleH3)
            [ text <| Translations.mutesSectionTitle [ shared.browserEnv.translations ] ]
        , p [] [ text <| Translations.mutesDescription [ shared.browserEnv.translations ] ]
        , if List.isEmpty muted then
            p [] [ text <| Translations.noMutesText [ shared.browserEnv.translations ] ]

          else
            div
                [ css
                    [ Tw.flex
                    , Tw.flex_col
                    , Tw.gap_2
                    ]
                ]
                (List.filterMap (viewMutedEntry shared user.pubKey readOnly saving) muted)
        ]


viewMutedEntry : Shared.Model -> PubKey -> Bool -> Bool -> Following -> Maybe (Html Msg)
viewMutedEntry shared userPubKey readOnly saving following =
    case following of
        FollowingPubKey { pubKey } ->
            let
                label =
                    Nostr.getProfile shared.nostr pubKey
                        |> Maybe.map (profileDisplayName pubKey)
                        |> Maybe.withDefault pubKey
            in
            Just
                (div
                    [ css
                        [ Tw.flex
                        , Tw.flex_row
                        , Tw.items_center
                        , Tw.gap_2
                        , Tw.p_2
                        , Tw.border_b_2
                        ]
                    ]
                    [ div [ css [ Tw.grow ] ] [ text label ]
                    , if not readOnly then
                        Button.new
                            { label = Translations.unmuteButtonTitle [ shared.browserEnv.translations ]
                            , onClick = Just (UnmutePubKey userPubKey pubKey)
                            , theme = shared.theme
                            }
                            |> Button.withIntermediateState saving
                            |> Button.view

                      else
                        emptyHtml
                    ]
                )

        FollowingHashtag hashtag ->
            Just
                (div
                    [ css [ Tw.p_2, Tw.border_b_2 ] ]
                    [ text ("#" ++ hashtag) ]
                )


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
            , Tw.gap_8
            ]
        ]
        [ viewConfigIssues shared.browserEnv (Translations.relayIssuesTitle [ shared.browserEnv.translations ]) configCheckIssues
        , outboxRelaySection shared user relaysModel
        , inboxRelaySection shared user relaysModel
        , searchRelaySection shared user relaysModel
        , privateRelaySection shared user relaysModel
        , blockedRelaySection shared user relaysModel
        , localRelaySection shared user relaysModel
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
            signingPubKeyAvailable shared.loginStatus
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
            , viewRelayList shared.theme shared.browserEnv.translations readOnly (AddDefaultOutboxRelays suggestedOutboxRelays) (RemoveRelay user.pubKey WriteRelay << Relay.fromString) (Translations.addDefaultRelaysButtonTitle [ shared.browserEnv.translations ]) outboxRelays
        , if not readOnly then
                addRelayBox shared.theme shared.browserEnv.translations relaysModel.outboxRelay outboxRelaySuggestions (updateRelayModelOutbox relaysModel) (AddOutboxRelay user.pubKey << Relay.fromString) saving "outbox-relay-add-button"

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
            signingPubKeyAvailable shared.loginStatus
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
            (styles.colorStyleGrayscaleTitle ++ styles.textStyleH3)
            [ text <| Translations.inboxSectionTitle [ shared.browserEnv.translations ] ]
        , p [] [ text <| Translations.inboxRelaysDescription [ shared.browserEnv.translations ] ]
        , viewRelayList shared.theme shared.browserEnv.translations readOnly (AddDefaultInboxRelays suggestedInboxRelays) (RemoveRelay user.pubKey ReadRelay << Relay.fromString) (Translations.addDefaultRelaysButtonTitle [ shared.browserEnv.translations ]) inboxRelays
        , if not readOnly then
            addRelayBox shared.theme shared.browserEnv.translations relaysModel.inboxRelay inboxRelaySuggestions (updateRelayModelInbox relaysModel) (AddInboxRelay user.pubKey << Relay.fromString) saving "inbox-relay-add-button"

          else
            emptyHtml
        ]


searchRelaySection : Shared.Model -> Auth.User -> RelaysModel -> Html Msg
searchRelaySection shared user relaysModel =
    let
        styles =
            stylesForTheme shared.theme

        searchRelays =
            Nostr.getSearchRelaysForPubKey shared.nostr user.pubKey
                |> List.filter (\relay -> not (Nostr.isBlockedRelay shared.nostr relay.url))

        searchRelaySuggestions =
            { identifier = "search-relay-suggestions"
            , suggestions =
                missingRelays searchRelays Pareto.defaultSearchRelays
            }

        readOnly =
            signingPubKeyAvailable shared.loginStatus
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
            (styles.colorStyleGrayscaleTitle ++ styles.textStyleH3)
            [ text <| Translations.searchSectionTitle [ shared.browserEnv.translations ] ]
        , p [] [ text <| Translations.searchRelaysDescription [ shared.browserEnv.translations ] ]
        , viewRelayList shared.theme shared.browserEnv.translations readOnly (AddDefaultSearchRelays user.pubKey Pareto.defaultSearchRelays) (RemoveSearchRelay user.pubKey << Relay.fromString) (Translations.addDefaultRelaysButtonTitle [ shared.browserEnv.translations ]) searchRelays
        , if not readOnly then
            addRelayBox shared.theme shared.browserEnv.translations relaysModel.searchRelay searchRelaySuggestions (updateRelayModelSearch relaysModel) (AddSearchRelay user.pubKey << Relay.fromString) saving "search-relay-add-button"

          else
            emptyHtml
        ]


blockedRelaySection : Shared.Model -> Auth.User -> RelaysModel -> Html Msg
blockedRelaySection shared user relaysModel =
    let
        styles =
            stylesForTheme shared.theme

        blockedRelays =
            Nostr.getBlockedRelayUrls shared.nostr
                |> resolveRelayRecords shared.nostr

        readOnly =
            signingPubKeyAvailable shared.loginStatus
                |> not

        saving =
            case relaysModel.state of
                RelayListStateSaving _ ->
                    True

                _ ->
                    False

        emptySuggestions =
            { identifier = "blocked-relay-suggestions"
            , suggestions = []
            }
    in
    div []
        [ h3
            (styles.colorStyleGrayscaleTitle ++ styles.textStyleH3)
            [ text <| Translations.blockedSectionTitle [ shared.browserEnv.translations ] ]
        , p
            [ css [ Tw.mb_4 ] ]
            [ text <| Translations.blockedRelaysDescription [ shared.browserEnv.translations ] ]
        , viewBlockedRelayList shared.theme shared.browserEnv.translations readOnly (RemoveBlockedRelay user.pubKey) blockedRelays
        , if not readOnly then
            addRelayBox shared.theme shared.browserEnv.translations relaysModel.blockedRelay emptySuggestions (updateRelayModelBlocked relaysModel) (AddBlockedRelay user.pubKey << Relay.fromString) saving "blocked-relay-add-button"

          else
            emptyHtml
        ]


privateRelaySection : Shared.Model -> Auth.User -> RelaysModel -> Html Msg
privateRelaySection shared user relaysModel =
    let
        styles =
            stylesForTheme shared.theme

        privateRelays =
            Nostr.getPrivateRelayUrls shared.nostr user.pubKey
                |> resolveRelayRecords shared.nostr

        readOnly =
            signingPubKeyAvailable shared.loginStatus
                |> not

        saving =
            case relaysModel.state of
                RelayListStateSaving _ ->
                    True

                _ ->
                    False

        emptySuggestions =
            { identifier = "private-relay-suggestions"
            , suggestions = []
            }
    in
    div []
        [ h3
            (styles.colorStyleGrayscaleTitle ++ styles.textStyleH3)
            [ text <| Translations.privateSectionTitle [ shared.browserEnv.translations ] ]
        , p
            [ css [ Tw.mb_4 ] ]
            [ text <| Translations.privateRelaysDescription [ shared.browserEnv.translations ] ]
        , viewSimpleRelayList shared.theme shared.browserEnv.translations readOnly (RemovePrivateRelay user.pubKey) privateRelays
        , if not readOnly then
            addRelayBox shared.theme shared.browserEnv.translations relaysModel.privateRelay emptySuggestions (updateRelayModelPrivate relaysModel) (AddPrivateRelay user.pubKey << Relay.fromString) saving "private-relay-add-button"

          else
            emptyHtml
        ]


citrineRelayUrl : RelayUrl
citrineRelayUrl =
    Relay.fromString "ws://127.0.0.1:4869"


localRelaySection : Shared.Model -> Auth.User -> RelaysModel -> Html Msg
localRelaySection shared _ relaysModel =
    let
        styles =
            stylesForTheme shared.theme

        localRelays =
            Nostr.getLocalRelayUrls shared.nostr
                |> resolveRelayRecords shared.nostr

        localSuggestions =
            { identifier = "local-relay-suggestions"
            , suggestions =
                [ citrineRelayUrl ]
                    |> List.filter
                        (\url ->
                            localRelays
                                |> List.all (\relay -> Relay.toKey relay.url /= Relay.toKey url)
                        )
                    |> List.map Relay.toWire
            }

        readOnly =
            False

        saving =
            False
    in
    div []
        [ h3
            (styles.colorStyleGrayscaleTitle ++ styles.textStyleH3)
            [ text <| Translations.localSectionTitle [ shared.browserEnv.translations ] ]
        , p [] [ text <| Translations.localRelaysDescription [ shared.browserEnv.translations ] ]
        , viewRelayList shared.theme shared.browserEnv.translations readOnly (AddSuggestedLocalRelays [ citrineRelayUrl ]) (RemoveLocalRelay << Relay.fromString) (Translations.addDefaultRelayButtonTitle [ shared.browserEnv.translations ]) localRelays
        , addRelayBox shared.theme shared.browserEnv.translations relaysModel.localRelay localSuggestions (updateRelayModelLocal relaysModel) (AddLocalRelay << Relay.fromString) saving "local-relay-add-button"
        ]


resolveRelayRecords : Nostr.Model -> List RelayUrl -> List Relay
resolveRelayRecords nostr urls =
    urls
        |> List.map
            (\url ->
                Nostr.getRelayData nostr url
                    |> Maybe.withDefault
                        { url = url
                        , state = RelayStateUnknown
                        , nip11 = Nothing
                        }
            )


viewSimpleRelayList : Theme -> I18Next.Translations -> Bool -> (RelayUrl -> Msg) -> List Relay -> Html Msg
viewSimpleRelayList theme translations readOnly removeMsg relays =
    viewBlockedRelayList theme translations readOnly removeMsg relays


viewBlockedRelayList : Theme -> I18Next.Translations -> Bool -> (RelayUrl -> Msg) -> List Relay -> Html Msg
viewBlockedRelayList theme translations readOnly removeMsg relays =
    if List.isEmpty relays then
        if readOnly then
            text <| Translations.relayReadOnlyLoginInfo [ translations ]

        else
            emptyHtml

    else
        div
            [ css
                [ Tw.flex
                , Tw.flex_col
                , Tw.my_2
                , Tw.gap_2
                ]
            ]
            (List.map
                (\relay ->
                    let
                        clientBlocked =
                            List.any (\blocked -> Relay.toKey blocked == Relay.toKey relay.url) Pareto.blockedRelays
                    in
                    viewRelay (readOnly || clientBlocked) (\key -> removeMsg (Relay.fromString key)) relay
                )
                relays
            )



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


missingRelays : List Relay -> List RelayUrl -> List String
missingRelays addedRelays recommendedRelays =
    recommendedRelays
        |> List.filter
            (\relayUrl ->
                addedRelays
                    |> List.filter
                        (\addedRelay ->
                            Relay.toKey addedRelay.url == Relay.toKey relayUrl
                        )
                    |> List.isEmpty
            )
        |> List.map Relay.host


updateRelayModelOutbox : RelaysModel -> Maybe String -> RelaysModel
updateRelayModelOutbox relaysModel value =
    { relaysModel | outboxRelay = value }


updateRelayModelInbox : RelaysModel -> Maybe String -> RelaysModel
updateRelayModelInbox relaysModel value =
    { relaysModel | inboxRelay = value }


updateRelayModelPrivate : RelaysModel -> Maybe String -> RelaysModel
updateRelayModelPrivate relaysModel value =
    { relaysModel | privateRelay = value }


updateRelayModelLocal : RelaysModel -> Maybe String -> RelaysModel
updateRelayModelLocal relaysModel value =
    { relaysModel | localRelay = value }


updateRelayModelSearch : RelaysModel -> Maybe String -> RelaysModel
updateRelayModelSearch relaysModel value =
    { relaysModel | searchRelay = value }


updateRelayModelBlocked : RelaysModel -> Maybe String -> RelaysModel
updateRelayModelBlocked relaysModel value =
    { relaysModel | blockedRelay = value }


addRelayBox : Theme -> I18Next.Translations -> Maybe String -> Suggestions -> (Maybe String -> RelaysModel) -> (String -> Msg) -> Bool -> String -> Html Msg
addRelayBox theme translations maybeValue suggestions updateRelayFn addRelayMsg saving testAttribute =
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
            |> Button.withTestAttribute testAttribute
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


viewRelayList : Theme -> I18Next.Translations -> Bool -> Msg -> (String -> Msg) -> String -> List Relay -> Html Msg
viewRelayList theme translations readOnly addDefaultRelaysMsg removeMsg addDefaultButtonLabel relays =
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
                    { label = addDefaultButtonLabel
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
            [ text (Relay.hostPort relay.url)
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
            , Tw.text_color styles.colorB3
            , darkMode
                [ Tw.text_color styles.colorB3DarkMode
                ]
            ]
        , Events.onClick (removeMsg (Relay.toKey relay.url))
        , Attr.attribute "data-test" ("remove-relay-button-" ++ Relay.toKey relay.url)
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
        , blossomServersSection shared user mediaServersModel
        , nip96ServersSection shared user mediaServersModel
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
            signingPubKeyAvailable shared.loginStatus
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
        , viewMediaServersList shared.theme shared.browserEnv.translations readOnly Nothing (RemoveNip96MediaServer user.pubKey) nip96Servers
        , if not readOnly then
            addMediaServerBox shared.theme shared.browserEnv.translations mediaServersModel.nip96Server nip96ServerSuggestions (updateNip96Server mediaServersModel) (AddNip96MediaServer user.pubKey) saving "nip96-server-add-button"

          else
            text <| Translations.mediaServerReadOnlyLoginInfo [ shared.browserEnv.translations ]
        ]


suggestedNip96Servers : Shared.Model -> PubKey -> List String
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
            , suggestions =
                missingMediaServers blossomServers suggestedServers
            }

        readOnly =
            signingPubKeyAvailable shared.loginStatus
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
        , viewMediaServersList shared.theme shared.browserEnv.translations readOnly (Just <| AddDefaultBlossomMediaServers user.pubKey suggestedServers) (RemoveBlossomMediaServer user.pubKey) blossomServers
        , if not readOnly then
            addMediaServerBox shared.theme shared.browserEnv.translations mediaServersModel.blossomServer blossomServerSuggestions (updateBlossomServer mediaServersModel) (AddBlossomMediaServer user.pubKey) saving "blossom-server-add-button"

          else
            text <| Translations.mediaServerReadOnlyLoginInfo [ shared.browserEnv.translations ]
        ]


suggestedBlossomServers : Shared.Model -> PubKey -> List String
suggestedBlossomServers shared pubKey =
    Nostr.getDefaultBlossomServers shared.nostr pubKey


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
            , Tw.text_color styles.colorB3
            , darkMode
                [ Tw.text_color styles.colorB3DarkMode
                ]
            ]
        , Events.onClick (removeMsg mediaServer)
        ]
        [ Icon.FeatherIcon FeatherIcons.delete
            |> Icon.view
        ]


addMediaServerBox : Theme -> I18Next.Translations -> Maybe String -> Suggestions -> (Maybe String -> MediaServersModel) -> (String -> Msg) -> Bool -> String -> Html Msg
addMediaServerBox theme translations maybeValue suggestions updateMediaServerFn addMediaServerMsg saving testAttribute =
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
            |> Button.withTestAttribute testAttribute
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
            signingPubKeyAvailable shared.loginStatus
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
                        , mute = Nothing
                        , subscribe = Nothing
                        , theme = shared.theme
                        , validation =
                            Nostr.getProfileValidationStatus shared.nostr user.pubKey
                                |> Maybe.withDefault ValidationUnknown
                        , zap = Nothing
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
                |> Maybe.map
                    (\lud06String ->
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

        lud16Suggestion =
            portalUserData
                |> Maybe.andThen .lud16
                |> Maybe.map Lud16.lud16ToString

        lud16FieldDescription =
            case lud16Suggestion of
                Just lud16 ->
                    Translations.profileLud16FieldRedirectDescription [ shared.browserEnv.translations ] { redirectUrl = lud16 }

                Nothing ->
                    Translations.profileLud16FieldDescription [ shared.browserEnv.translations ]
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
            |> Button.withDisabled profileNotChanged
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
            |> EntryField.withDescription lud16FieldDescription
            |> EntryField.withSuggestions "lud16" (lud16Suggestion |> Maybe.map List.singleton |> Maybe.withDefault [])
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
        ( imageUrl, testAttribute ) =
            case imageUploadType of
                ImagePicture ->
                    ( profileModel.picture, "profile-picture-button" )

                ImageBanner ->
                    ( profileModel.banner, "profile-banner-button" )
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
            |> Button.withTestAttribute testAttribute
            |> Button.view
        ]


viewImage : (Bool -> Msg) -> String -> Html Msg
viewImage onImageLoadedMsg imageUrl =
    case Url.fromString imageUrl of
        Just _ ->
            Html.img
                [ Attr.src imageUrl
                , Attr.alt "Selected image"
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
