module Layouts.Sidebar exposing (Model, Msg, Props, clientRoleForRoutePath, layout, map)

import BrowserEnv exposing (BrowserEnv)
import Components.Button
import Components.Icon as Icon exposing (Icon(..))
import Components.OnboardingDialog as OnboardingDialog
import Css
import Dict
import Effect exposing (Effect)
import FeatherIcons
import Html.Styled as Html exposing (Html, a, aside, button, div, img, input, label, main_, span, text)
import Html.Styled.Attributes as Attr exposing (class, css, src)
import Html.Styled.Events as Events exposing (..)
import I18Next
import Layout exposing (Layout)
import ModalDialog exposing (ModalDialog)
import Nostr
import Nostr.BookmarkList exposing (bookmarksCount)
import Nostr.Profile exposing (Profile)
import Nostr.Types exposing (Following(..))
import Ports
import Route exposing (Route)
import Route.Path
import Shared
import Shared.Model exposing (ClientRole(..), LoginStatus(..))
import Shared.Msg
import Tailwind.Breakpoints as Bp
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw
import Translations.Sidebar as Translations
import Ui.Profile
import Ui.Styles exposing (Styles)
import View exposing (View)


type alias Props contentMsg =
    { styles : Styles contentMsg
    }


map : (msg1 -> msg2) -> Props msg1 -> Props msg2
map toMsg props =
    { styles = Ui.Styles.map toMsg props.styles
    }


clientRoleForRoutePath : BrowserEnv.Environment -> Route.Path.Path -> ClientRole
clientRoleForRoutePath environment path =
    sidebarItems
        { isAuthor = False
        , isBetaTester = False
        , isLoggedIn = False
        , environment = environment
        , clientRole = ClientReader
        , sendsNewsletters = False
        , translations = I18Next.initialTranslations
        , maybeBookmarksCount = Nothing
        , currentPath = path
        }
        |> List.any (\item -> item.path == path)
        |> (\isInReaderList ->
                if isInReaderList then
                    ClientReader

                else
                    ClientCreator
           )


type alias SidebarItemData =
    { path : Route.Path.Path
    , title : String
    , icon : Icon
    , requiresLogin : Bool
    , requiresAuthor : Bool
    , disabled : Bool
    }


type alias SidebarItemParams =
    { isAuthor : Bool
    , isBetaTester : Bool
    , isLoggedIn : Bool
    , environment : BrowserEnv.Environment
    , clientRole : ClientRole
    , sendsNewsletters : Bool
    , translations : I18Next.Translations
    , maybeBookmarksCount : Maybe Int
    , currentPath : Route.Path.Path
    }


routePathIsInList : SidebarItemParams -> Bool
routePathIsInList sidebarItemParams =
    sidebarItems sidebarItemParams
        |> List.any (\item -> item.path == sidebarItemParams.currentPath)


sidebarItems : SidebarItemParams -> List SidebarItemData
sidebarItems { isAuthor, isBetaTester, isLoggedIn, clientRole, sendsNewsletters, translations, maybeBookmarksCount } =
    rawSidebarItems clientRole translations
        |> List.filter (sidebarItemVisible isLoggedIn isAuthor isBetaTester)
        |> List.filterMap
            (\sidebarItem ->
                -- item-specific adaptions
                case sidebarItem.path of
                    Route.Path.Bookmarks ->
                        maybeBookmarksCount
                            |> Maybe.map
                                (\bookmarksCount ->
                                    -- add bookmarks count to title
                                    { sidebarItem | title = sidebarItem.title ++ "\u{00A0}" ++ countBadge bookmarksCount }
                                )

                    Route.Path.Newsletters ->
                        -- currently in development
                        Just { sidebarItem | disabled = not sendsNewsletters }

                    Route.Path.Subscribers ->
                        -- currently in development
                        Just { sidebarItem | disabled = not sendsNewsletters }

                    _ ->
                        Just sidebarItem
            )


rawSidebarItems : ClientRole -> I18Next.Translations -> List SidebarItemData
rawSidebarItems clientRole translations =
    case clientRole of
        ClientReader ->
            [ { path = Route.Path.Read, title = Translations.readMenuItemText [ translations ], icon = FeatherIcon FeatherIcons.bookOpen, requiresLogin = False, requiresAuthor = False, disabled = False }
            , { path = Route.Path.Search, title = Translations.searchMenuItemText [ translations ], icon = FeatherIcon FeatherIcons.search, requiresLogin = False, requiresAuthor = False, disabled = False }

            --, { path = Route.Path.Communities, title = Translations.communitiesMenuItemText [ translations ], icon = FeatherIcon FeatherIcons.globe, requiresLogin = False, requiresAuthor = False, disabled = False }
            , { path = Route.Path.Bookmarks, title = Translations.bookmarksMenuItemText [ translations ], icon = FeatherIcon FeatherIcons.bookmark, requiresLogin = True, requiresAuthor = False, disabled = False }

            --, { path = Route.Path.Messages, title = Translations.messagesMenuItemText [ translations ], icon = FeatherIcon FeatherIcons.mail, requiresLogin = True, requiresAuthor = False, disabled = True }
            --, { path = Route.Path.Notifications, title = Translations.notificationsMenuItemText [ translations ], icon = FeatherIcon FeatherIcons.bell, requiresLogin = True, requiresAuthor = False, disabled = True }
            , { path = Route.Path.Settings, title = Translations.settingsMenuItemText [ translations ], icon = FeatherIcon FeatherIcons.settings, requiresLogin = True, requiresAuthor = False, disabled = False }
            , { path = Route.Path.About, title = Translations.aboutMenuItemText [ translations ], icon = FeatherIcon FeatherIcons.helpCircle, requiresLogin = False, requiresAuthor = False, disabled = False }
            ]

        ClientCreator ->
            [ { path = Route.Path.Posts, title = Translations.postsMenuItemText [ translations ], icon = FeatherIcon FeatherIcons.fileText, requiresLogin = True, requiresAuthor = False, disabled = False }
            , { path = Route.Path.Write, title = Translations.writeMenuItemText [ translations ], icon = FeatherIcon FeatherIcons.feather, requiresLogin = True, requiresAuthor = False, disabled = False }
            , { path = Route.Path.Subscribers, title = Translations.subscribersMenuItemText [ translations ], icon = FeatherIcon FeatherIcons.users, requiresLogin = True, requiresAuthor = True, disabled = False }
            , { path = Route.Path.Newsletters, title = Translations.newslettersMenuItemText [ translations ], icon = FeatherIcon FeatherIcons.mail, requiresLogin = True, requiresAuthor = True, disabled = False }
            , { path = Route.Path.Search, title = Translations.searchMenuItemText [ translations ], icon = FeatherIcon FeatherIcons.search, requiresLogin = False, requiresAuthor = False, disabled = False }
            , { path = Route.Path.Media, title = Translations.mediaMenuItemText [ translations ], icon = FeatherIcon FeatherIcons.image, requiresLogin = False, requiresAuthor = False, disabled = False }

            --, { path = Route.Path.Messages, title = Translations.messagesMenuItemText [ translations ], icon = FeatherIcon FeatherIcons.mail, requiresLogin = True, requiresAuthor = False, disabled = True }
            --, { path = Route.Path.Notifications, title = Translations.notificationsMenuItemText [ translations ], icon = FeatherIcon FeatherIcons.bell, requiresLogin = True, requiresAuthor = False, disabled = True }
            , { path = Route.Path.Settings, title = Translations.settingsMenuItemText [ translations ], icon = FeatherIcon FeatherIcons.settings, requiresLogin = True, requiresAuthor = False, disabled = False }
            ]


layout : Props contentMsg -> Shared.Model -> Route () -> Layout () Model Msg contentMsg
layout props shared route =
    Layout.new
        { init = init
        , update = update shared
        , view = view props.styles shared route.path
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { modalDialog : ModalDialog
    }


type ModalDialog
    = NoModalDialog
    | GetStartedDialog (OnboardingDialog.Model Msg)
    | ProfileMenu


init : () -> ( Model, Effect Msg )
init _ =
    -- ( { modalDialog = GetStartedDialog <| OnboardingDialog.init { onClose = CloseModal } }
    ( { modalDialog = NoModalDialog }
    , Effect.none
    )



-- UPDATE


type Msg
    = OpenGetStarted
    | OpenProfileMenu
    | CloseModal
    | LoginDialogSent OnboardingDialog.Msg
    | SwitchClientRole Bool Bool


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update _ msg model =
    case msg of
        OpenGetStarted ->
            ( model
            , Effect.sendCmd Ports.loginSignUp
            )

        OpenProfileMenu ->
            ( { model | modalDialog = ProfileMenu }
            , Effect.none
            )

        CloseModal ->
            ( { model | modalDialog = NoModalDialog }
            , Effect.none
            )

        LoginDialogSent innerMsg ->
            case model.modalDialog of
                GetStartedDialog dialog ->
                    OnboardingDialog.update
                        { msg = innerMsg
                        , model = dialog
                        , toModel = \loginDialog -> { model | modalDialog = GetStartedDialog loginDialog }
                        , toMsg = LoginDialogSent
                        }

                _ ->
                    ( model
                    , Effect.none
                    )

        SwitchClientRole changePath _ ->
            ( model, Effect.sendSharedMsg <| Shared.Msg.SwitchClientRole changePath )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Styles contentMsg -> Shared.Model.Model -> Route.Path.Path -> { toContentMsg : Msg -> contentMsg, content : View contentMsg, model : Model } -> View contentMsg
view styles shared path { toContentMsg, model, content } =
    { title = content.title ++ " | Pareto"
    , body =
        [ div
            [ css
                [ Tw.h_full
                ]
            ]
            [ viewSidebar styles shared path toContentMsg content.body
            , viewModalDialog model |> Html.map toContentMsg
            ]
        ]
    }


viewModalDialog : Model -> Html Msg
viewModalDialog model =
    case model.modalDialog of
        GetStartedDialog dialog ->
            OnboardingDialog.new
                { model = dialog
                , toMsg = LoginDialogSent
                }
                |> OnboardingDialog.view

        _ ->
            div [] []


viewSidebar : Styles contentMsg -> Shared.Model.Model -> Route.Path.Path -> (Msg -> contentMsg) -> List (Html contentMsg) -> Html contentMsg
viewSidebar styles shared currentPath toContentMsg content =
    let
        maybeBookmarksCount =
            Shared.loggedInPubKey shared.loginStatus
                |> Maybe.andThen (Nostr.getBookmarks shared.nostr)
                |> Maybe.map bookmarksCount
                |> Maybe.andThen
                    (\count ->
                        if count > 0 then
                            Just count

                        else
                            Nothing
                    )

        maybeUserPubKey =
            Shared.loggedInPubKey shared.loginStatus

        sidebarItemParams =
            { isAuthor =
                maybeUserPubKey
                    |> Maybe.map (Nostr.isAuthor shared.nostr)
                    |> Maybe.withDefault False
            , isBetaTester =
                maybeUserPubKey
                    |> Maybe.map (Nostr.isBetaTester shared.nostr)
                    |> Maybe.withDefault False
            , isLoggedIn =
                Shared.loggedIn shared
            , environment = shared.browserEnv.environment
            , clientRole = shared.role
            , sendsNewsletters =
                maybeUserPubKey
                    |> Maybe.map
                        (\pubKey ->
                            Nostr.sendsNewsletterPubKey shared.nostr pubKey == Just True
                        )
                    |> Maybe.withDefault False
            , translations = shared.browserEnv.translations
            , maybeBookmarksCount = maybeBookmarksCount
            , currentPath = currentPath
            }
    in
    Html.div
        (styles.colorStyleGrayscaleTitle
            ++ styles.colorStyleBackground
            ++ [ css
                    [ Tw.h_screen
                    ]
               ]
        )
        [ div
            [ css
                [ Tw.flex
                ]
            ]
            [ aside
                (styles.colorStyleSitebarBackground
                    ++ [ css
                            [ Tw.p_2
                            , Tw.h_14
                            , Tw.fixed
                            , Tw.bottom_0
                            , Tw.w_screen
                            , Tw.z_10
                            , Tw.flex
                            , Tw.flex_row
                            , Tw.space_x_4
                            , Bp.xl
                                [ Tw.w_52
                                ]
                            , Bp.sm
                                [ Tw.inline
                                , Tw.w_20
                                , Tw.relative
                                , Tw.justify_items_center
                                , Tw.h_screen
                                , Tw.border_r
                                , Tw.border_color Theme.gray_200
                                , Tw.z_0
                                ]
                            ]
                       ]
                )
                [ viewBannerSmall shared.browserEnv
                , viewSidebarItems styles sidebarItemParams
                ]
            , div
                [ css
                    [ Tw.flex_1
                    ]
                ]
                [ div
                    [ css
                        [ Tw.flex
                        , Tw.justify_between
                        , Tw.items_center
                        , Tw.bg_cover
                        , Tw.bg_center
                        , Tw.h_20
                        , Tw.mb_6
                        ]
                    , Attr.style "background-image" "url('/images/Pareto-Banner-back.png')"
                    ]
                    [ -- viewBanner
                      if roleSwitchButtonEnabled shared.nostr shared.loginStatus then
                        clientRoleSwitch sidebarItemParams

                      else
                        div [] []
                    , img
                        [ src "/images/Pareto-Banner-Text.svg"
                        , css
                            [ Tw.overflow_x_auto
                            , Tw.h_7
                            ]
                        ]
                        []
                    , div
                        [ css
                            [ Tw.flex_shrink_0
                            , Tw.pr_4
                            ]
                        ]
                        [ loginButton shared (profileForUser shared shared.loginStatus)
                        ]
                    ]
                    |> Html.map toContentMsg
                , viewMainContent content
                ]
            ]
        ]


viewMainContent : List (Html contentMsg) -> Html contentMsg
viewMainContent content =
    main_
        [ class "page"
        , css
            []
        ]
        content


viewBannerSmall : BrowserEnv -> Html contentMsg
viewBannerSmall browserEnv =
    let
        bannerImageWide =
            if browserEnv.darkMode then
                "/images/icon/Pareto-Log7.png"

            else
                "/images/icon/Pareto-Log5.png"

        bannerImageNarrow =
            "/images/icon/Pareto-Log1.png"
    in
    a
        [ css
            [ Tw.flex
            , Tw.space_x_2
            , Bp.sm
                [ Tw.mt_3
                , Tw.mb_11
                , Tw.items_center
                ]
            ]
        , Attr.href <| Route.Path.toString Route.Path.Read
        ]
        [ div
            [ Attr.src bannerImageWide
            , Attr.alt "Banner"
            , css
                [ Tw.w_0
                , Bp.xl
                    [ Tw.w_36
                    , Tw.h_10
                    , Tw.ml_2
                    ]
                ]
            ]
            [ img
                [ Attr.src bannerImageWide
                , Attr.alt "Banner"
                ]
                []
            ]
        , div
            [ css
                [ Tw.h_10
                , Tw.w_8
                , Bp.xl
                    [ Tw.w_0
                    ]
                , Bp.sm
                    [ Tw.w_10
                    , Tw.mr_0
                    ]
                ]
            ]
            [ img
                [ Attr.src bannerImageNarrow
                , Attr.alt "Banner"
                ]
                []
            ]
        ]


roleSwitchButtonEnabled : Nostr.Model -> LoginStatus -> Bool
roleSwitchButtonEnabled nostr loginStatus =
    case loginStatus of
        LoggedIn userPubKey ->
            -- check here also for author because authors list is available immediately when the client starts
            Nostr.isAuthor nostr userPubKey || Nostr.isEditor nostr userPubKey

        _ ->
            False


clientRoleSwitch : SidebarItemParams -> Html Msg
clientRoleSwitch sidebarItemParams =
    let
        currentPathPresentForOtherRole =
            case sidebarItemParams.clientRole of
                ClientReader ->
                    routePathIsInList
                        { sidebarItemParams | clientRole = ClientCreator }

                ClientCreator ->
                    routePathIsInList
                        { sidebarItemParams | clientRole = ClientReader }
    in
    {- Switch Container -}
    div
        [ css
            [ Tw.flex
            , Tw.items_center
            , Tw.space_x_4
            , Bp.lg [ Tw.mx_10 ]
            , Tw.ml_4
            ]
        ]
        [ {- Label for the Switch -}
          span
            [ css
                [ Tw.text_color Theme.gray_700
                , Tw.font_medium
                ]
            ]
            [ if sidebarItemParams.clientRole == ClientReader then
                text <| Translations.readerClientRoleText [ sidebarItemParams.translations ]

              else
                text <| Translations.creatorClientRoleText [ sidebarItemParams.translations ]
            ]
        , {- Switch -}
          label
            [ Attr.for "toggle-switch"
            , css
                [ Tw.relative
                , Tw.cursor_pointer
                ]
            ]
            [ {- Hidden checkbox -}
              input
                [ Attr.type_ "checkbox"
                , Attr.id "toggle-switch"
                , Events.onCheck <| SwitchClientRole (not currentPathPresentForOtherRole)
                , css
                    [ Tw.sr_only
                    ]
                ]
                []
            , {- Switch Background -}
              if sidebarItemParams.clientRole == ClientReader then
                div
                    [ Attr.id "switch-background"
                    , css
                        [ Tw.w_11
                        , Tw.h_6
                        , Tw.rounded_full
                        , Tw.transition_all
                        , Tw.duration_300
                        , Tw.bg_color Theme.gray_300
                        ]
                    ]
                    []

              else
                div
                    [ Attr.id "switch-background"
                    , css
                        [ Tw.w_11
                        , Tw.h_6
                        , Tw.rounded_full
                        , Tw.transition_all
                        , Tw.duration_300
                        , Tw.bg_color Theme.blue_600
                        ]
                    ]
                    []
            , {- Switch Knob -}
              if sidebarItemParams.clientRole == ClientReader then
                div
                    [ Attr.id "switch-knob"
                    , css
                        [ Tw.absolute
                        , Tw.top_0_dot_5
                        , Tw.left_0_dot_5
                        , Tw.w_5
                        , Tw.h_5
                        , Tw.rounded_full
                        , Tw.bg_color Theme.white
                        , Tw.border
                        , Tw.border_color Theme.gray_300
                        , Tw.transition_transform
                        , Tw.duration_300
                        ]
                    ]
                    []

              else
                div
                    [ Attr.id "switch-knob"
                    , css
                        [ Tw.absolute
                        , Tw.top_0_dot_5
                        , Tw.left_0_dot_5
                        , Tw.w_5
                        , Tw.h_5
                        , Tw.rounded_full
                        , Tw.bg_color Theme.white
                        , Tw.border
                        , Tw.border_color Theme.white
                        , Tw.transform
                        , Tw.translate_x_full
                        , Tw.transition_transform
                        , Tw.duration_300
                        ]
                    ]
                    []
            ]
        ]


profileForUser : Shared.Model -> LoginStatus -> Maybe Profile
profileForUser shared loggedIn =
    case loggedIn of
        LoggedIn pubKey ->
            Nostr.getProfile shared.nostr pubKey

        _ ->
            Nothing


viewSidebarItems : Styles contentMsg -> SidebarItemParams -> Html contentMsg
viewSidebarItems styles sidebarItemParams =
    let
        visibleSidebarItems =
            sidebarItems sidebarItemParams
    in
    div
        [ css
            [ Tw.grid
            , Tw.grid_rows_1
            , Tw.grid_cols_8
            , Tw.h_10
            , Tw.w_full
            , Bp.xl
                [ Tw.w_44
                ]
            , Bp.sm
                [ Tw.grid_rows_9
                , Tw.grid_cols_1
                , Tw.w_14
                , Css.property "height" "560px"
                ]
            ]
        ]
        (List.map (viewSidebarItem styles sidebarItemParams.currentPath) visibleSidebarItems)


countBadge : Int -> String
countBadge count =
    case count of
        1 ->
            "①"

        2 ->
            "②"

        3 ->
            "③"

        4 ->
            "④"

        5 ->
            "⑤"

        6 ->
            "⑥"

        7 ->
            "⑦"

        8 ->
            "⑧"

        9 ->
            "⑨"

        10 ->
            "⑩"

        11 ->
            "⑪"

        12 ->
            "⑫"

        13 ->
            "⑬"

        14 ->
            "⑭"

        15 ->
            "⑮"

        16 ->
            "⑯"

        17 ->
            "⑰"

        18 ->
            "⑱"

        19 ->
            "⑲"

        20 ->
            "⑳"

        otherNumber ->
            "(" ++ String.fromInt otherNumber ++ ")"


sidebarItemVisible : Bool -> Bool -> Bool -> SidebarItemData -> Bool
sidebarItemVisible isLoggedIn isAuthor isBetaTester sidebarItem =
    if isBetaTester then
        True

    else if sidebarItem.requiresAuthor then
        isAuthor

    else if sidebarItem.requiresLogin then
        isLoggedIn

    else
        True


viewSidebarItem : Styles contentMsg -> Route.Path.Path -> SidebarItemData -> Html contentMsg
viewSidebarItem styles currentPath itemData =
    let
        ( foreground, background, linkAttr ) =
            if itemData.disabled then
                ( styles.colorStyleSitebarItemDisabled
                , []
                , []
                )

            else if currentPath == itemData.path then
                ( styles.colorStyleSitebarItemActive
                , styles.colorStyleSitebarItemActiveBackground
                , []
                )

            else
                ( styles.colorStyleSitebarItemEnabled
                , []
                , [ Attr.href <| Route.toString { path = itemData.path, hash = Nothing, query = Dict.empty } ]
                )
    in
    a
        (css
            [ Tw.py_2
            , Tw.w_10
            , Bp.xl
                [ Tw.w_40
                , Tw.flex
                , Tw.flex_row
                ]
            , Bp.sm
                [ Tw.rounded_full
                , Tw.h_10
                ]
            ]
            :: linkAttr
            ++ background
            ++ foreground
        )
        [ div
            [ css
                [ Tw.flex
                , Tw.justify_center
                , Bp.sm
                    [ Tw.min_w_10
                    , Tw.h_10
                    ]
                ]
            ]
            [ Icon.view itemData.icon ]
        , span
            (css
                [ Tw.hidden
                , Bp.xl
                    [ Tw.inline
                    ]
                ]
                :: foreground
            )
            [ text itemData.title ]
        ]


loginButton : Shared.Model -> Maybe Profile -> Html Msg
loginButton shared maybeProfile =
    case shared.loginStatus of
        Shared.Model.LoggedIn _ ->
            loggedInButton maybeProfile

        _ ->
            getStartedButton shared.theme shared.browserEnv


loggedInButton : Maybe Profile -> Html Msg
loggedInButton maybeProfile =
    button
        [ css
            [ Tw.bg_color Theme.gray_100
            , Tw.text_color Theme.white
            , Tw.py_2
            , Tw.px_2
            , Tw.rounded_full
            , Tw.border_hidden
            ]
        , Events.onClick OpenProfileMenu
        ]
        [ img
            [ Attr.src <| Ui.Profile.profilePicture 56 maybeProfile
            , css
                [ Tw.w_14
                , Tw.h_14
                , Tw.border_hidden
                , Tw.rounded_full
                ]
            ]
            []
        ]


getStartedButton : Ui.Styles.Theme -> BrowserEnv -> Html Msg
getStartedButton theme browserEnv =
    Components.Button.new
        { label = Translations.getStartedButtonText [ browserEnv.translations ]
        , onClick = Just OpenGetStarted
        , theme = theme
        }
        |> Components.Button.view
