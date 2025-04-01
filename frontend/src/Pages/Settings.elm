module Pages.Settings exposing (Model, Msg, page)

import Auth
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
import Layouts
import Nostr
import Nostr.Event exposing (Kind(..), emptyEventFilter)
import Nostr.Lud16 as Lud16
import Nostr.Nip05 as Nip05
import Nostr.Nip96 as Nip96 exposing (eventWithNip96ServerList)
import Nostr.Profile exposing (Profile, ProfileValidation(..), emptyProfile, eventFromProfile, profilesEqual)
import Nostr.Relay as Relay exposing (Relay, RelayState(..), hostWithoutProtocol)
import Nostr.RelayListMetadata exposing (RelayMetadata, eventWithRelayList, extendRelayList, removeFromRelayList)
import Nostr.Request exposing (RequestData(..))
import Nostr.Send exposing (SendRequest(..))
import Nostr.Types exposing (PubKey, RelayRole(..), RelayUrl, ServerUrl)
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
import Ui.Shared exposing (emptyHtml)
import Ui.Styles exposing (Theme, stylesForTheme)
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
        |> Page.withLayout (toLayout shared.theme)


toLayout : Theme -> Model -> Layouts.Layout Msg
toLayout theme _ =
    Layouts.Sidebar
        { styles = stylesForTheme theme }



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
    }


type alias MediaServersModel =
    { nip96Server : Maybe String
    , blossomServer : Maybe String
    }


type alias ProfileModel =
    { nip05 : String
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
    }


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
      }
    , mediaSelectorEffect
    )


profileFromProfileModel : PubKey -> ProfileModel -> Profile
profileFromProfileModel pubKey profileModel =
    { nip05 = Nip05.parseNip05 profileModel.nip05
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


emptyRelaysModel =
    { outboxRelay = Nothing
    , inboxRelay = Nothing
    , searchRelay = Nothing
    }


emptyMediaServersModel : MediaServersModel
emptyMediaServersModel =
    { nip96Server = Nothing
    , blossomServer = Nothing
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
      }
    , mediaSelectorEffect
    )


type Category
    = Relays
    | MediaServers
    | Profile


availableCategories : I18Next.Translations -> List (Categories.CategoryData Category)
availableCategories translations =
    [ { category = Relays
      , title = Translations.relaysCategory [ translations ]
      }
    , { category = MediaServers
      , title = Translations.mediaServersCategory [ translations ]
      }
    , { category = Profile
      , title = Translations.profileCategory [ translations ]
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
    | UpdateProfileModel ProfileModel
    | OpenImageSelection ImageUploadType
    | MediaSelectorSent (MediaSelector.Msg Msg)
    | ImageSelected MediaSelector.UploadedFile
    | SaveProfile Profile
    | CreateProfile


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
            ( { model | data = RelaysData emptyRelaysModel }
            , Nostr.getRelayListForPubKey shared.nostr pubKey
                |> extendRelayList (relayListWithRole [ relayUrl ] WriteRelay)
                |> sendRelayListCmd pubKey
            )

        AddInboxRelay pubKey relayUrl ->
            ( { model | data = RelaysData emptyRelaysModel }
            , Nostr.getRelayListForPubKey shared.nostr pubKey
                |> extendRelayList (relayListWithRole [ relayUrl ] ReadRelay)
                |> sendRelayListCmd pubKey
            )

        AddSearchRelay _ _ ->
            ( model, Effect.none )

        AddDefaultOutboxRelays relayUrls ->
            ( model
            , Nostr.getRelayListForPubKey shared.nostr user.pubKey
                |> extendRelayList (relayListWithRole relayUrls WriteRelay)
                |> sendRelayListCmd user.pubKey
            )

        AddDefaultInboxRelays relayUrls ->
            ( model
            , Nostr.getRelayListForPubKey shared.nostr user.pubKey
                |> extendRelayList (relayListWithRole relayUrls ReadRelay)
                |> sendRelayListCmd user.pubKey
            )

        RemoveRelay pubKey relayRole relayUrl ->
            ( model
            , Nostr.getRelayListForPubKey shared.nostr pubKey
                |> removeFromRelayList { url = relayUrl, role = relayRole }
                |> sendRelayListCmd pubKey
            )

        UpdateMediaServerModel mediaServersModel ->
            ( { model | data = MediaServersData mediaServersModel }, Effect.none )

        AddNip96MediaServer pubKey mediaServer ->
            ( { model | data = MediaServersData emptyMediaServersModel }
            , Nostr.getNip96Servers shared.nostr pubKey
                |> extendMediaServerList mediaServer
                |> sendMediaServerListCmd shared.browserEnv pubKey
            )

        RemoveNip96MediaServer pubKey mediaServer ->
            ( model
            , Nostr.getNip96Servers shared.nostr pubKey
                |> removeMediaServerFromList mediaServer
                |> sendMediaServerListCmd shared.browserEnv pubKey
            )

        AddDefaultNip96MediaServers pubKey mediaServers ->
            ( model
            , mediaServers
                |> sendMediaServerListCmd shared.browserEnv pubKey
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
                        { user = user
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
            ( model
            , eventFromProfile profile.pubKey profile
                |> SendProfile (Nostr.getWriteRelayUrlsForPubKey shared.nostr profile.pubKey)
                |> Shared.Msg.SendNostrEvent
                |> Effect.sendSharedMsg
            )

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


sendMediaServerListCmd : BrowserEnv -> PubKey -> List ServerUrl -> Effect msg
sendMediaServerListCmd browserEnv pubKey mediaServers =
    eventWithNip96ServerList browserEnv pubKey mediaServers
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
    case model.data of
        ProfileData profileModel ->
            Sub.map MediaSelectorSent (MediaSelector.subscribe profileModel.mediaSelector)

        _ ->
            Sub.none



-- VIEW


view : Auth.User -> Shared.Model -> Model -> View Msg
view user shared model =
    { title = Translations.pageTitle [ shared.browserEnv.translations ]
    , body =
        [ Categories.new
            { model = model.categories
            , toMsg = CategoriesSent
            , onSelect = CategorySelected
            , equals = (==)
            , image = \_ -> Nothing
            , categories = availableCategories shared.browserEnv.translations
            , browserEnv = shared.browserEnv
            , styles = stylesForTheme shared.theme
            }
            |> Categories.view
        , viewCategory shared model user
        ]
    }


viewCategory : Shared.Model -> Model -> Auth.User -> Html Msg
viewCategory shared model user =
    case ( Categories.selected model.categories, model.data ) of
        ( Relays, RelaysData relaysModel ) ->
            viewRelays shared user relaysModel

        ( MediaServers, MediaServersData mediaServersModel ) ->
            viewMediaServers shared user mediaServersModel

        ( Profile, ProfileData profileModel ) ->
            viewProfile shared user profileModel

        _ ->
            emptyHtml


type alias Suggestions =
    { identifier : String
    , suggestions : List String
    }


viewRelays : Shared.Model -> Auth.User -> RelaysModel -> Html Msg
viewRelays shared user relaysModel =
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
            , Tw.m_20
            ]
        ]
        [ outboxRelaySection shared user relaysModel
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
                addRelayBox shared.theme shared.browserEnv.translations relaysModel.outboxRelay outboxRelaySuggestions (updateRelayModelOutbox relaysModel) (AddOutboxRelay user.pubKey)

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
            addRelayBox shared.theme shared.browserEnv.translations relaysModel.inboxRelay inboxRelaySuggestions (updateRelayModelInbox relaysModel) (AddInboxRelay user.pubKey)

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


addRelayBox : Theme -> I18Next.Translations -> Maybe String -> Suggestions -> (Maybe String -> RelaysModel) -> (String -> Msg) -> Html Msg
addRelayBox theme translations maybeValue suggestions updateRelayFn addRelayMsg =
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
            , Tw.w_96
            ]
        ]
        [ viewRelayConnectionIndicator relay
        , viewRelayImage (Relay.iconUrl relay)
        , div
            [ css
                [ Tw.grow
                , Tw.w_96
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
    div
        [ css
            [ Tw.text_color Theme.slate_500
            , Tw.cursor_pointer
            ]
        , Events.onClick (removeMsg relay.urlWithoutProtocol)
        ]
        [ Icon.FeatherIcon FeatherIcons.delete
            |> Icon.view
        ]


viewMediaServers : Shared.Model -> Auth.User -> MediaServersModel -> Html Msg
viewMediaServers shared user mediaServersModel =
    div
        [ css
            [ Tw.flex
            , Tw.flex_col
            , Tw.gap_2
            , Tw.m_20
            ]
        ]
        [ nip96ServersSection shared user mediaServersModel

        -- , blossomServersection shared user relaysModel
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
    in
    div []
        [ h3
            (styles.colorStyleGrayscaleTitle ++ styles.textStyleH3)
            [ text <| Translations.nip96ServersSectionTitle [ shared.browserEnv.translations ] ]
        , p [] [ text <| Translations.nip96ServersDescription [ shared.browserEnv.translations ] ]
        , viewMediaServersList shared.theme shared.browserEnv.translations readOnly (AddDefaultNip96MediaServers user.pubKey suggestedServers) (RemoveNip96MediaServer user.pubKey) nip96Servers
        , if not readOnly then
            addMediaServerBox shared.theme shared.browserEnv.translations mediaServersModel.nip96Server nip96ServerSuggestions (updateNip96Server mediaServersModel) (AddNip96MediaServer user.pubKey)

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


viewMediaServersList : Theme -> I18Next.Translations -> Bool -> Msg -> (String -> Msg) -> List String -> Html Msg
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
                    , onClick = Just addDefaultMediaServersMsg
                    , theme = theme
                    }
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
    div
        [ css
            [ Tw.text_color Theme.slate_500
            , Tw.cursor_pointer
            ]
        , Events.onClick (removeMsg mediaServer)
        ]
        [ Icon.FeatherIcon FeatherIcons.delete
            |> Icon.view
        ]


addMediaServerBox : Theme -> I18Next.Translations -> Maybe String -> Suggestions -> (Maybe String -> MediaServersModel) -> (String -> Msg) -> Html Msg
addMediaServerBox theme translations maybeValue suggestions updateMediaServerFn addMediaServerMsg =
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
            |> Button.view
        ]


mediaServerUrlValid : Maybe ServerUrl -> Bool
mediaServerUrlValid maybeMediaServerUrl =
    case maybeMediaServerUrl of
        Just url ->
            Url.fromString url /= Nothing

        Nothing ->
            False


viewProfile : Shared.Model -> Auth.User -> ProfileModel -> Html Msg
viewProfile shared user profileModel =
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
                        , following = UnknownFollowing
                        , subscribe = Nothing
                        , theme = shared.theme
                        , validation =
                            Nostr.getProfileValidationStatus shared.nostr user.pubKey
                                |> Maybe.withDefault ValidationUnknown
                        }
                        shared

                ( Just _, False ) ->
                    viewProfileEditor shared user profileModel

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
            , Tw.m_20
            ]
        ]
        [ viewUserProfile
        ]


viewProfileEditor : Shared.Model -> Auth.User -> ProfileModel -> Html Msg
viewProfileEditor shared user profileModel =
    let
        portalUserData =
            Nostr.getPortalUserInfo shared.nostr user.pubKey

        profileNotChanged =
            case profileModel.savedProfile of
                Just savedProfile ->
                    profilesEqual savedProfile (profileFromProfileModel user.pubKey profileModel)

                Nothing ->
                    False
    in
    div
        [ css
            [ Tw.flex
            , Tw.flex_col
            , Tw.gap_2
            ]
        ]
        [ Button.new
            { label = Translations.profileSaveButtonTitle [ shared.browserEnv.translations ]
            , onClick = Just <| SaveProfile (profileFromProfileModel user.pubKey profileModel)
            , theme = shared.theme
            }
            |> Button.withDisabled profileNotChanged
            |> Button.view
        , div
            [ css
                [ Tw.flex
                , Tw.flex_row
                , Tw.gap_2
                ]
            ]
            [ EntryField.new
                { value = profileModel.name
                , onInput = \name -> UpdateProfileModel { profileModel | name = name }
                , theme = shared.theme
                }
                |> EntryField.withLabel "name"
                |> EntryField.withPlaceholder "name"
                |> EntryField.withSuggestions "name" (portalUserData |> Maybe.andThen .username |> Maybe.map List.singleton |> Maybe.withDefault [])
                |> EntryField.view
            , EntryField.new
                { value = profileModel.nip05
                , onInput = \nip05 -> UpdateProfileModel { profileModel | nip05 = nip05 }
                , theme = shared.theme
                }
                |> EntryField.withLabel "nip05"
                |> EntryField.withPlaceholder "nip05"
                |> EntryField.withSuggestions "nip05" (portalUserData |> Maybe.andThen .nip05 |> Maybe.map Nip05.nip05ToString |> Maybe.map List.singleton |> Maybe.withDefault [])
                |> EntryField.withType EntryField.FieldTypeEmail
                |> EntryField.view
            ]
        , EntryField.new
            { value = profileModel.about
            , onInput = \about -> UpdateProfileModel { profileModel | about = about }
            , theme = shared.theme
            }
            |> EntryField.withLabel "about"
            |> EntryField.withPlaceholder "about"
            |> EntryField.withRows 2
            |> EntryField.view
        , viewImageSelection shared ImagePicture profileModel
        , EntryField.new
            { value = profileModel.picture
            , onInput = \picture -> UpdateProfileModel { profileModel | picture = picture }
            , theme = shared.theme
            }
            |> EntryField.withLabel "picture"
            |> EntryField.withPlaceholder "picture"
            |> EntryField.withType EntryField.FieldTypeUrl
            |> EntryField.view
        , viewImageSelection shared ImageBanner profileModel
        , EntryField.new
            { value = profileModel.banner
            , onInput = \banner -> UpdateProfileModel { profileModel | banner = banner }
            , theme = shared.theme
            }
            |> EntryField.withLabel "banner"
            |> EntryField.withPlaceholder "banner"
            |> EntryField.withType EntryField.FieldTypeUrl
            |> EntryField.view
        , EntryField.new
            { value = profileModel.lud16
            , onInput = \lud16 -> UpdateProfileModel { profileModel | lud16 = lud16 }
            , theme = shared.theme
            }
            |> EntryField.withLabel "lud16"
            |> EntryField.withPlaceholder "lud16"
            |> EntryField.withSuggestions "lud16" (portalUserData |> Maybe.andThen .lud16 |> Maybe.map Lud16.lud16ToString |> Maybe.map List.singleton |> Maybe.withDefault [])
            |> EntryField.withType EntryField.FieldTypeEmail
            |> EntryField.view
        , EntryField.new
            { value = profileModel.displayName
            , onInput = \displayName -> UpdateProfileModel { profileModel | displayName = displayName }
            , theme = shared.theme
            }
            |> EntryField.withLabel "display_name"
            |> EntryField.withPlaceholder "display_name"
            |> EntryField.view
        , EntryField.new
            { value = profileModel.website
            , onInput = \website -> UpdateProfileModel { profileModel | website = website }
            , theme = shared.theme
            }
            |> EntryField.withLabel "website"
            |> EntryField.withPlaceholder "website"
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


viewImageSelection : Shared.Model -> ImageUploadType -> ProfileModel -> Html Msg
viewImageSelection shared imageUploadType profileModel =
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
        [ viewImage imageUrl
        , Button.new
            { label = Translations.imageSelectionButtonTitle [ shared.browserEnv.translations ]
            , onClick = Just <| OpenImageSelection imageUploadType
            , theme = shared.theme
            }
            |> Button.withTypeSecondary
            |> Button.view
        ]


viewImage : String -> Html Msg
viewImage imageUrl =
    case Url.fromString imageUrl of
        Just _ ->
            Html.img
                [ Attr.src imageUrl
                , css
                    [ Tw.w_20
                    , Tw.min_h_full
                    , Tw.mt_3
                    ]
                ]
                []

        Nothing ->
            emptyHtml
