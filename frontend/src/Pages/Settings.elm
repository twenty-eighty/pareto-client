module Pages.Settings exposing (Model, Msg, page)

import Auth
import Components.Button as Button
import Components.Categories as Categories
import Components.Icon as Icon
import Css
import Effect exposing (Effect)
import FeatherIcons
import Html.Styled as Html exposing (Html, a, datalist, div, h3, input, li, option, p, text, ul)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events as Events exposing (..)
import I18Next
import Layouts
import Nostr
import Nostr.Event exposing (Kind(..), emptyEventFilter)
import Nostr.Profile exposing (ProfileValidation(..))
import Nostr.Relay as Relay exposing (Relay, RelayState(..), hostWithoutProtocol)
import Nostr.RelayListMetadata exposing (RelayMetadata, eventWithRelayList, extendRelayList, removeFromRelayList)
import Nostr.Request exposing (RequestData(..))
import Nostr.Send exposing (SendRequest(..))
import Nostr.Types exposing (PubKey, RelayRole(..), RelayUrl)
import Page exposing (Page)
import Pareto
import Route exposing (Route)
import Shared
import Shared.Msg
import Tailwind.Breakpoints as Bp
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw
import Translations.Settings as Translations
import Ui.Profile exposing (FollowType(..))
import Ui.Relay exposing (viewRelayImage)
import Ui.Styles exposing (Styles, Theme, stylesForTheme)
import View exposing (View)


page : Auth.User -> Shared.Model -> Route () -> Page Model Msg
page user shared _ =
    Page.new
        { init = init user shared
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
    }


type alias RelaysModel =
    { outboxRelay : Maybe String
    , inboxRelay : Maybe String
    , searchRelay : Maybe String
    }


type alias MediaServersModel =
    { nip96Server : Maybe String
    , blossomServer : Maybe String
    }


emptyRelaysModel =
    { outboxRelay = Nothing
    , inboxRelay = Nothing
    , searchRelay = Nothing
    }



--   emptyMediaServersModel : MediaServersModel
--   emptyMediaServersModel =
--       { nip96Server = Nothing
--       , blossomServer = Nothing
--       }


type Category
    = Relays RelaysModel
    | MediaServers MediaServersModel
    | Profile


availableCategories : I18Next.Translations -> List (Categories.CategoryData Category)
availableCategories translations =
    [ { category = Relays emptyRelaysModel
      , title = Translations.relaysCategory [ translations ]
      }

    --   , { category = MediaServers emptyMediaServersModel
    --     , title = Translations.mediaServersCategory [ translations ]
    --     }
    , { category = Profile
      , title = Translations.profileCategory [ translations ]
      }
    ]


init : Auth.User -> Shared.Model -> () -> ( Model, Effect Msg )
init user shared () =
    let
        initialCategory =
            Relays emptyRelaysModel
    in
    updateModelWithCategory
        user
        shared
        { categories = Categories.init { selected = initialCategory }
        }
        initialCategory



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
            ( { model | categories = Categories.select model.categories (Relays relaysModel) }, Effect.none )

        AddOutboxRelay pubKey relayUrl ->
            ( { model | categories = Categories.select model.categories (Relays emptyRelaysModel) }
            , Nostr.getRelayListForPubKey shared.nostr pubKey
                |> extendRelayList (relayListWithRole [ relayUrl ] WriteRelay)
                |> sendRelayListCmd pubKey
            )

        AddInboxRelay pubKey relayUrl ->
            ( { model | categories = Categories.select model.categories (Relays emptyRelaysModel) }
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
        ( request, requestDescription ) =
            case category of
                Relays _ ->
                    ( RequestRelayLists { emptyEventFilter | kinds = Just [ KindRelayListMetadata, KindBlockedRelaysList, KindSearchRelaysList, KindPrivateRelayList, KindRelayListForDMs ], authors = Just [ user.pubKey ] }
                    , "Relay lists of user"
                    )

                MediaServers _ ->
                    ( RequestMediaServerLists { emptyEventFilter | kinds = Just [ KindUserServerList, KindFileStorageServerList ], authors = Just [ user.pubKey ] }
                    , "Media server lists of user"
                    )

                Profile ->
                    ( RequestUserData { emptyEventFilter | kinds = Just [ KindUserMetadata ], authors = Just [ user.pubKey ] }
                    , "Profile of user"
                    )
    in
    ( model
    , request
        |> Nostr.createRequest shared.nostr requestDescription []
        |> Shared.Msg.RequestNostrEvents
        |> Effect.sendSharedMsg
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
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
            , equals = categoryEquals
            , categories = availableCategories shared.browserEnv.translations
            , browserEnv = shared.browserEnv
            , styles = stylesForTheme shared.theme
            }
            |> Categories.view
        , viewCategory shared model user
        ]
    }


categoryEquals : Category -> Category -> Bool
categoryEquals category1 category2 =
    case ( category1, category2 ) of
        ( Relays _, Relays _ ) ->
            True

        ( Relays _, _ ) ->
            False

        ( _, Relays _ ) ->
            False

        ( MediaServers _, MediaServers _ ) ->
            True

        ( MediaServers _, _ ) ->
            False

        ( _, MediaServers _ ) ->
            False

        ( Profile, Profile ) ->
            True


viewCategory : Shared.Model -> Model -> Auth.User -> Html Msg
viewCategory shared model user =
    case Categories.selected model.categories of
        Relays relaysModel ->
            viewRelays shared model user relaysModel

        MediaServers mediaServersModel ->
            viewMediaServers shared model user mediaServersModel

        Profile ->
            viewProfile shared model user


type alias RelaySuggestions =
    { identifier : String
    , suggestions : List String
    }


viewRelays : Shared.Model -> Model -> Auth.User -> RelaysModel -> Html Msg
viewRelays shared _ user relaysModel =
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

        inboxRelays =
            Nostr.getNip65ReadRelaysForPubKey shared.nostr user.pubKey

        suggestedInboxRelays =
            suggestedRelays shared user.pubKey ReadRelay

        inboxRelaySuggestions =
            { identifier = "inbox-relay-suggestions"
            , suggestions =
                missingRelays inboxRelays suggestedInboxRelays
            }

        {-
           searchRelays =
               Nostr.getSearchRelaysForPubKey shared.nostr user.pubKey

           searchRelaySuggestions =
               { identifier = "search-relay-suggestions"
               , suggestions =
                   missingRelays inboxRelays Pareto.defaultSearchRelays
               }
        -}
    in
    div
        [ css
            [ Tw.flex
            , Tw.flex_col
            , Tw.gap_2
            , Tw.m_20
            ]
        ]
        [ h3
            (styles.colorStyleGrayscaleTitle ++ styles.textStyleH3)
            [ text <| Translations.outboxSectionTitle [ shared.browserEnv.translations ] ]
        , p [] [ text <| Translations.outboxRelaysDescription [ shared.browserEnv.translations ] ]
        , viewRelayList shared.theme shared.browserEnv.translations (AddDefaultOutboxRelays suggestedOutboxRelays) (RemoveRelay user.pubKey WriteRelay) outboxRelays
        , addRelayBox shared.theme shared.browserEnv.translations relaysModel.outboxRelay outboxRelaySuggestions (updateRelayModelOutbox relaysModel) (AddOutboxRelay user.pubKey)
        , h3
            (styles.colorStyleGrayscaleTitle
                ++ styles.textStyleH3
                ++ [ css [ Tw.mt_3 ] ]
            )
            [ text <| Translations.inboxSectionTitle [ shared.browserEnv.translations ] ]
        , p [] [ text <| Translations.inboxRelaysDescription [ shared.browserEnv.translations ] ]
        , viewRelayList shared.theme shared.browserEnv.translations (AddDefaultInboxRelays suggestedInboxRelays) (RemoveRelay user.pubKey ReadRelay) inboxRelays
        , addRelayBox shared.theme shared.browserEnv.translations relaysModel.inboxRelay inboxRelaySuggestions (updateRelayModelInbox relaysModel) (AddInboxRelay user.pubKey)

        -- , viewRelayList searchRelays
        -- , addRelayBox shared.theme shared.browserEnv.translations relaysModel.searchRelay (updateRelayModelSearch relaysModel) (AddSearchRelay user.pubKey)
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


addRelayBox : Theme -> I18Next.Translations -> Maybe String -> RelaySuggestions -> (Maybe String -> RelaysModel) -> (String -> Msg) -> Html Msg
addRelayBox theme translations maybeValue relaySuggestions updateFn addRelayMsg =
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
                       , Attr.list relaySuggestions.identifier
                       , Events.onInput
                            (\relayText ->
                                if relayText /= "" then
                                    UpdateRelayModel <| updateFn (Just <| relayText)

                                else
                                    UpdateRelayModel <| updateFn Nothing
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
            , relaySuggestionDataList relaySuggestions
            ]
        , Button.new
            { label = Translations.addRelayButtonTitle [ translations ]
            , onClick = Maybe.map addRelayMsg maybeValue
            , theme = theme
            }
            |> Button.withDisabled (not <| relayUrlValid maybeValue)
            |> Button.view
        ]


relaySuggestionDataList : RelaySuggestions -> Html Msg
relaySuggestionDataList relaySuggestions =
    datalist
        [ Attr.id relaySuggestions.identifier
        ]
        (relaySuggestions.suggestions
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


viewRelayList : Theme -> I18Next.Translations -> Msg -> (String -> Msg) -> List Relay -> Html Msg
viewRelayList theme translations addDefaultRelaysMsg removeMsg relays =
    if List.length relays > 0 then
        div
            [ css
                [ Tw.flex
                , Tw.flex_col
                , Tw.my_2
                , Tw.gap_2
                ]
            ]
            (List.map (viewRelay removeMsg) relays)

    else
        div
            [ css
                [ Tw.flex
                , Tw.flex_col
                , Tw.gap_2
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


viewRelay : (String -> Msg) -> Relay -> Html Msg
viewRelay removeMsg relay =
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
        , removeRelayButton relay removeMsg
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


viewMediaServers : Shared.Model -> Model -> Auth.User -> MediaServersModel -> Html Msg
viewMediaServers shared _ user _ =
    let
        blossomServers =
            Nostr.getBlossomServers shared.nostr user.pubKey

        nip96Servers =
            Nostr.getNip96Servers shared.nostr user.pubKey
    in
    div
        [ css
            [ Tw.flex
            , Tw.flex_col
            ]
        ]
        [ div
            [ css
                [ Tw.flex
                , Tw.flex_col
                ]
            ]
            (text "NIP-96 Servers"
                :: (nip96Servers
                        |> List.map viewNip96Server
                   )
            )
        , div
            [ css
                [ Tw.flex
                , Tw.flex_col
                ]
            ]
            (text "Blossom Servers"
                :: (blossomServers
                        |> List.map viewBlossomServer
                   )
            )
        ]


viewNip96Server : String -> Html Msg
viewNip96Server urlWithoutProtocol =
    div
        []
        [ text urlWithoutProtocol
        ]


viewBlossomServer : String -> Html Msg
viewBlossomServer urlWithoutProtocol =
    div
        []
        [ text urlWithoutProtocol
        ]


viewProfile : Shared.Model -> Model -> Auth.User -> Html Msg
viewProfile shared _ user =
    let
        styles =
            stylesForTheme shared.theme

        viewUserProfile =
            Nostr.getProfile shared.nostr user.pubKey
                |> Maybe.map
                    (\profile ->
                        Ui.Profile.viewProfile
                            profile
                            { browserEnv = shared.browserEnv
                            , following = UnknownFollowing
                            , isAuthor = Nostr.isAuthor shared.nostr user.pubKey
                            , subscribe = Nothing
                            , theme = shared.theme
                            , validation =
                                Nostr.getProfileValidationStatus shared.nostr user.pubKey
                                    |> Maybe.withDefault ValidationUnknown
                            }
                            shared
                    )
                |> Maybe.withDefault (div [] [])
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
        , div
            (styles.colorStyleGrayscaleTitle ++ styles.textStyleH3)
            [ text <| Translations.profileEditorTitle [ shared.browserEnv.translations ] ]
        , p [] [ text <| Translations.profileEditorNotImplementedText [ shared.browserEnv.translations ] ]
        , ul []
            (List.map (viewRecommendedProfileEditor styles) recommendedProfileEditors)
        ]


type alias RecommendedProfileEditor =
    { title : String
    , url : String
    }


recommendedProfileEditors : List RecommendedProfileEditor
recommendedProfileEditors =
    [ { url = "https://metadata.nostr.com/", title = "Nostr Profile Manager" }
    ]


viewRecommendedProfileEditor : Styles Msg -> RecommendedProfileEditor -> Html Msg
viewRecommendedProfileEditor styles recommendedProfileEditor =
    li []
        [ a
            (styles.colorStyleLinks
                ++ styles.textStyleLinks
                ++ [ Attr.href recommendedProfileEditor.url
                   , Attr.target "_blank"
                   , Attr.rel "noopener noreferrer"
                   ]
            )
            [ text recommendedProfileEditor.title ]
        ]
