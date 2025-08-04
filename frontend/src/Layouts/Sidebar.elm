module Layouts.Sidebar exposing (Model, Msg, Props, clientRoleForRoutePath, layout, map, new, withRightPart, withTopPart)

--import Browser.Events as Events

import BrowserEnv exposing (BrowserEnv, Environment)
import Components.AlertTimerMessage as AlertTimerMessage
import Components.Button
import Components.Icon as Icon exposing (Icon(..))
import Components.Switch as Switch
import Css
import Dict
import Effect exposing (Effect)
import FeatherIcons
import Graphics
import Html.Styled as Html exposing (Html, a, aside, div, img, main_, span, text)
import Html.Styled.Attributes as Attr exposing (class, css)
import Html.Styled.Events as Events exposing (..)
import I18Next
import Layout exposing (Layout)
import Nostr
import Nostr.BookmarkList exposing (bookmarksCount)
import Nostr.ConfigCheck as ConfigCheck
import Nostr.Profile exposing (Profile)
import Nostr.Types exposing (Following(..), LoginStatus(..), loggedInPubKey)
import Ports
import Route exposing (Route)
import Route.Path
import Shared
import Shared.Model exposing (ClientRole(..))
import Shared.Msg
import Tailwind.Breakpoints as Bp
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw
import Translations.Sidebar as Translations
import Ui.Profile
import Ui.Shared exposing (countBadge, emptyHtml)
import Ui.Styles exposing (Theme(..), darkMode, print)
import View exposing (View)


type alias Props contentMsg =
    { theme : Theme
    , fixedRightPart : Maybe (Html contentMsg)
    , fixedTopPart : Maybe ( Html contentMsg, String )
    }


map : (msg1 -> msg2) -> Props msg1 -> Props msg2
map toMsg props =
    { theme = props.theme
    , fixedRightPart = Maybe.map (Html.map toMsg) props.fixedRightPart
    , fixedTopPart = Maybe.map (\( html, height ) -> ( Html.map toMsg html, height )) props.fixedTopPart
    }


new : { theme : Theme } -> Props contentMsg
new { theme } =
    { theme = theme
    , fixedRightPart = Nothing
    , fixedTopPart = Nothing
    }


withRightPart : Html contentMsg -> Props contentMsg -> Props contentMsg
withRightPart rightPart props =
    { props | fixedRightPart = Just rightPart }


withTopPart : Html contentMsg -> String -> Props contentMsg -> Props contentMsg
withTopPart topPart height props =
    { props | fixedTopPart = Just ( topPart, height ) }



-- this function checks if a route will be available in reader mode after login


clientRoleForRoutePath : BrowserEnv.Environment -> Route.Path.Path -> ClientRole
clientRoleForRoutePath environment path =
    sidebarItems
        { configIssues = 0
        , isAuthor = False
        , isBetaTester = False
        , isLoggedIn = True
        , environment = environment
        , clientRole = ClientReader
        , sendsNewsletters = False
        , translations = I18Next.initialTranslations
        , maybeBookmarksCount = Nothing
        , currentPath = path
        , testMode = BrowserEnv.TestModeOff
        , theme = Ui.Styles.dummyTheme
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
    , ariaLabel : String
    , icon : Icon
    , requiresLogin : Bool
    , requiresAuthor : Bool
    , requiresBetaTester : Bool
    , disabled : Bool
    }


type alias SidebarItemParams =
    { configIssues : Int
    , isAuthor : Bool
    , isBetaTester : Bool
    , isLoggedIn : Bool
    , environment : BrowserEnv.Environment
    , clientRole : ClientRole
    , sendsNewsletters : Bool
    , translations : I18Next.Translations
    , maybeBookmarksCount : Maybe Int
    , currentPath : Route.Path.Path
    , testMode : BrowserEnv.TestMode
    , theme : Theme
    }


routePathIsInList : SidebarItemParams -> Bool
routePathIsInList sidebarItemParams =
    sidebarItems sidebarItemParams
        |> List.any (\item -> item.path == sidebarItemParams.currentPath)


sidebarItems : SidebarItemParams -> List SidebarItemData
sidebarItems { configIssues, isAuthor, isBetaTester, isLoggedIn, clientRole, sendsNewsletters, translations, maybeBookmarksCount } =
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

                    Route.Path.Settings ->
                        if configIssues > 0 then
                            Just { sidebarItem | title = sidebarItem.title ++ "\u{00A0}" ++ countBadge configIssues }

                        else
                            Just sidebarItem

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
            [ { path = Route.Path.Read
              , title = Translations.readMenuItemText [ translations ]
              , ariaLabel = Translations.readMenuItemText [ translations ]
              , icon = FeatherIcon FeatherIcons.bookOpen
              , requiresLogin = False
              , requiresAuthor = False
              , requiresBetaTester = False
              , disabled = False
              }
            , { path = Route.Path.Pictures
              , title = Translations.picturesMenuItemText [ translations ]
              , ariaLabel = Translations.picturesMenuItemText [ translations ]
              , icon = FeatherIcon FeatherIcons.image
              , requiresLogin = False
              , requiresAuthor = False
              , requiresBetaTester = False
              , disabled = False
              }
            , { path = Route.Path.Search
              , title = Translations.searchMenuItemText [ translations ]
              , ariaLabel = Translations.searchMenuItemText [ translations ]
              , icon = FeatherIcon FeatherIcons.search
              , requiresLogin = False
              , requiresAuthor = False
              , requiresBetaTester = False
              , disabled = False
              }

            --, { path = Route.Path.Communities, title = Translations.communitiesMenuItemText [ translations ], icon = FeatherIcon FeatherIcons.globe, requiresLogin = False, requiresAuthor = False, disabled = False }
            , { path = Route.Path.Bookmarks
              , title = Translations.bookmarksMenuItemText [ translations ]
              , ariaLabel = Translations.bookmarksMenuItemText [ translations ]
              , icon = FeatherIcon FeatherIcons.bookmark
              , requiresLogin = True
              , requiresAuthor = False
              , requiresBetaTester = False
              , disabled = False
              }
            , { path = Route.Path.Authors
              , title = Translations.authorsMenuItemText [ translations ]
              , ariaLabel = Translations.authorsMenuItemText [ translations ]
              , icon = FeatherIcon FeatherIcons.users
              , requiresLogin = False
              , requiresAuthor = False
              , requiresBetaTester = False
              , disabled = False
              }

            --, { path = Route.Path.Messages, title = Translations.messagesMenuItemText [ translations ], icon = FeatherIcon FeatherIcons.mail, requiresLogin = True, requiresAuthor = False, disabled = True }
            --, { path = Route.Path.Notifications, title = Translations.notificationsMenuItemText [ translations ], icon = FeatherIcon FeatherIcons.bell, requiresLogin = True, requiresAuthor = False, disabled = True }
            , { path = Route.Path.Settings
              , title = Translations.settingsMenuItemText [ translations ]
              , ariaLabel = Translations.settingsMenuItemText [ translations ]
              , icon = FeatherIcon FeatherIcons.settings
              , requiresLogin = True
              , requiresAuthor = False
              , requiresBetaTester = False
              , disabled = False
              }
            , { path = Route.Path.About
              , title = Translations.aboutMenuItemText [ translations ]
              , ariaLabel = Translations.aboutMenuItemText [ translations ]
              , icon = FeatherIcon FeatherIcons.helpCircle
              , requiresLogin = False
              , requiresAuthor = False
              , requiresBetaTester = False
              , disabled = False
              }
            ]

        ClientCreator ->
            [ { path = Route.Path.Posts
              , title = Translations.postsMenuItemText [ translations ]
              , ariaLabel = Translations.postsMenuItemText [ translations ]
              , icon = FeatherIcon FeatherIcons.fileText
              , requiresLogin = True
              , requiresAuthor = False
              , requiresBetaTester = False
              , disabled = False
              }
            , { path = Route.Path.Write
              , title = Translations.writeMenuItemText [ translations ]
              , ariaLabel = Translations.writeMenuItemText [ translations ]
              , icon = FeatherIcon FeatherIcons.feather
              , requiresLogin = True
              , requiresAuthor = False
              , requiresBetaTester = False
              , disabled = False
              }
            , { path = Route.Path.Subscribers
              , title = Translations.subscribersMenuItemText [ translations ]
              , ariaLabel = Translations.subscribersMenuItemText [ translations ]
              , icon = FeatherIcon FeatherIcons.users
              , requiresLogin = True
              , requiresAuthor = True
              , requiresBetaTester = False
              , disabled = False
              }
            , { path = Route.Path.Newsletters
              , title = Translations.newslettersMenuItemText [ translations ]
              , ariaLabel = Translations.newslettersMenuItemText [ translations ]
              , icon = FeatherIcon FeatherIcons.mail
              , requiresLogin = True
              , requiresAuthor = True
              , requiresBetaTester = False
              , disabled = False
              }
            , { path = Route.Path.Search
              , title = Translations.searchMenuItemText [ translations ]
              , ariaLabel = Translations.searchMenuItemText [ translations ]
              , icon = FeatherIcon FeatherIcons.search
              , requiresLogin = False
              , requiresAuthor = False
              , requiresBetaTester = False
              , disabled = False
              }
            , { path = Route.Path.Media
              , title = Translations.mediaMenuItemText [ translations ]
              , ariaLabel = Translations.mediaMenuItemText [ translations ]
              , icon = FeatherIcon FeatherIcons.image
              , requiresLogin = False
              , requiresAuthor = False
              , requiresBetaTester = False
              , disabled = False
              }

            --, { path = Route.Path.Messages, title = Translations.messagesMenuItemText [ translations ], ariaLabel = Translations.messagesMenuItemText [ translations ], icon = FeatherIcon FeatherIcons.mail, requiresLogin = True, requiresAuthor = False, disabled = True }
            --, { path = Route.Path.Notifications, title = Translations.notificationsMenuItemText [ translations ], ariaLabel = Translations.notificationsMenuItemText [ translations ], icon = FeatherIcon FeatherIcons.bell, requiresLogin = True, requiresAuthor = False, disabled = True }
            , { path = Route.Path.Settings
              , title = Translations.settingsMenuItemText [ translations ]
              , ariaLabel = Translations.settingsMenuItemText [ translations ]
              , icon = FeatherIcon FeatherIcons.settings
              , requiresLogin = True
              , requiresAuthor = False
              , requiresBetaTester = False
              , disabled = False
              }
            ]


layout : Props contentMsg -> Shared.Model -> Route () -> Layout () Model Msg contentMsg
layout props shared route =
    Layout.new
        { init = init
        , update = update shared
        , view = view props shared route.path
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { articleInfoToggle : Bool }


init : () -> ( Model, Effect Msg )
init _ =
    ( { articleInfoToggle = False }
    , Effect.none
    )



-- UPDATE


type Msg
    = OpenGetStarted
    | SetClientRole Bool ClientRole
    | SetTestMode BrowserEnv.TestMode
    | ToggleArticleInfo Bool


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update _ msg model =
    case msg of
        OpenGetStarted ->
            ( model, Effect.sendCmd Ports.loginSignUp )

        SetClientRole changePath clientRole ->
            ( model, Effect.sendSharedMsg <| Shared.Msg.SetClientRole changePath clientRole )

        SetTestMode testMode ->
            ( model, Effect.sendSharedMsg <| Shared.Msg.SetTestMode testMode )

        ToggleArticleInfo flag ->
            ( { model | articleInfoToggle = flag }, Effect.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Props contentMsg -> Shared.Model.Model -> Route.Path.Path -> { toContentMsg : Msg -> contentMsg, content : View contentMsg, model : Model } -> View contentMsg
view props shared path { toContentMsg, content, model } =
    let
        styles =
            Ui.Styles.stylesForTheme props.theme
    in
    { title = content.title ++ " | Pareto"
    , body =
        [ div
            (css
                [ Tw.h_full
                , Tw.overflow_clip
                , print
                    [ Tw.h_auto
                    , Tw.overflow_visible
                    ]
                ]
                :: styles.colorStyleBackground
            )
            [ viewSidebar props shared model path toContentMsg content.body
            , viewLinktoInternalPage shared.nostr
            , AlertTimerMessage.new
                { model = shared.alertTimerMessage
                , theme = shared.theme
                }
                |> AlertTimerMessage.view
            ]
        ]
    }


viewSidebar : Props contentMsg -> Shared.Model.Model -> Model -> Route.Path.Path -> (Msg -> contentMsg) -> List (Html contentMsg) -> Html contentMsg
viewSidebar props shared model currentPath toContentMsg content =
    let
        styles =
            Ui.Styles.stylesForTheme props.theme

        maybeBookmarksCount =
            loggedInPubKey shared.loginStatus
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
            loggedInPubKey shared.loginStatus

        sidebarItemParams =
            { configIssues = ConfigCheck.getIssues shared.configCheck |> List.length
            , isAuthor =
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
            , testMode = shared.browserEnv.testMode
            , theme = shared.theme
            }

        isArticlePage =
            case currentPath of
                Route.Path.A_Addr_ _ ->
                    True

                Route.Path.U_User__Identifier_ _ ->
                    True

                _ ->
                    False

        articleInfoToggle =
            div
                [ css
                    [ Tw.fixed
                    , Tw.top_96
                    , Tw.right_4
                    , Tw.z_10
                    , darkMode [ Tw.text_color styles.color5DarkMode ]
                    , Tw.text_color styles.color5
                    , Bp.lg [ Tw.hidden ]
                    , if isArticlePage then
                        Tw.block

                      else
                        Tw.hidden
                    ]
                , Events.onClick (toContentMsg (ToggleArticleInfo (not model.articleInfoToggle)))
                ]
                [ if model.articleInfoToggle == True then
                    Icon.FeatherIcon FeatherIcons.bookOpen |> Icon.view

                  else
                    Icon.FeatherIcon FeatherIcons.info |> Icon.view
                ]
    in
    Html.div
        (styles.colorStyleGrayscaleTitle
            ++ [ css
                    [ Tw.h_screen
                    , Tw.overflow_hidden
                    , print
                        [ Tw.h_auto
                        , Tw.overflow_visible
                        ]
                    ]
               ]
        )
        [ div
            [ css
                [ Tw.flex
                ]
            ]
            [ aside
                (styles.colorStyleBorders
                    ++ styles.colorStyleBackground
                    ++ [ css
                            [ Tw.p_2
                            , Tw.h_14
                            , Tw.fixed
                            , Tw.bottom_0
                            , Tw.left_0
                            , Tw.w_screen
                            , Tw.flex
                            , Tw.flex_row
                            , Tw.space_x_4
                            , Bp.xl
                                [ Tw.w_52 ]
                            , Bp.sm
                                [ Tw.flex_col
                                , Tw.w_20
                                , Tw.relative
                                , Tw.items_center
                                , Tw.h_screen
                                , Tw.border_r
                                ]
                            , darkMode [ Tw.bg_color Theme.black ]
                            , Tw.z_10
                            , print
                                [ Tw.hidden
                                ]
                            ]
                       ]
                )
                [ viewBannerSmall shared.browserEnv
                , viewSidebarItems props.theme sidebarItemParams
                ]
            , div
                [ css
                    [ Tw.flex
                    , Tw.flex_col
                    , Tw.w_full
                    , print [ Tw.block ]
                    ]
                ]
                [ Html.header
                    [ css
                        [ Tw.flex
                        , Tw.flex_row
                        , Tw.justify_between
                        , Tw.items_center
                        , Tw.bg_cover
                        , Tw.bg_center
                        , Tw.h_20
                        , print
                            [ Tw.hidden
                            ]
                        ]
                    , Attr.style "background-image" "url('/images/Pareto-Banner-back.webp')"
                    ]
                    [ -- viewBanner
                      if roleSwitchButtonEnabled shared.nostr shared.loginStatus then
                        clientRoleSwitch sidebarItemParams

                      else
                        emptyHtml
                    , if sidebarItemParams.isBetaTester then
                        testModeSwitch sidebarItemParams

                      else
                        emptyHtml
                    , div
                        [ css
                            [ Tw.w_full
                            , Tw.h_auto
                            ]
                        ]
                        [ Graphics.paretoBannerText ]
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
                , props.fixedTopPart
                    |> Maybe.map (\( html, _ ) -> html)
                    |> Maybe.withDefault emptyHtml
                , div
                    [ css
                        [ Tw.flex
                        , Tw.flex_1
                        , print [ Tw.block ]
                        ]
                    ]
                    [ div
                        [ css
                            [ Bp.lg [ Tw.contents ]
                            , if model.articleInfoToggle && isArticlePage then
                                Tw.hidden

                              else
                                Tw.contents
                            ]
                        ]
                        [ viewMainContent content (props.fixedTopPart |> Maybe.map (\( _, height ) -> height)) ]
                    , props.fixedRightPart
                        |> Maybe.map
                            (\html ->
                                div
                                    [ css
                                        [ print [ Tw.hidden ]
                                        , Bp.lg [ Tw.block, Tw.grow_0 ]
                                        , if not model.articleInfoToggle then
                                            Tw.hidden

                                          else
                                            Tw.block
                                        , Tw.grow
                                        ]
                                    ]
                                    [ html ]
                            )
                        |> Maybe.withDefault emptyHtml
                    , articleInfoToggle
                    ]
                ]
            ]
        ]


viewMainContent : List (Html contentMsg) -> Maybe String -> Html contentMsg
viewMainContent content maybeFixedTopPartHeight =
    let
        topPartHeight =
            maybeFixedTopPartHeight |> Maybe.withDefault "0px"
    in
    div
        [ css
            [ Tw.flex_1
            , Tw.relative
            ]
        ]
        [ main_
            [ class "page"
            , Attr.id Shared.contentId
            , css
                [ Tw.flex_1
                , Tw.overflow_y_auto
                , Tw.relative
                , Css.property "height" ("calc(100vh - 5rem - 56px - " ++ topPartHeight ++ ")") -- 5rem = 80px for header
                , Bp.sm
                    [ Css.property "height" ("calc(100vh - 5rem - " ++ topPartHeight ++ ")") -- 5rem = 80px for header
                    ]
                , print
                    [ Tw.overflow_y_visible
                    , Css.property "height" "auto"
                    ]
                ]
            ]
            [ div
                [ css [ Tw.mb_4 ] ]
                content
            ]
        ]


viewBannerSmall : BrowserEnv -> Html contentMsg
viewBannerSmall browserEnv =
    a
        [ css
            [ Tw.flex
            , Tw.space_x_2
            , Bp.sm
                [ Tw.mt_3
                , Tw.mb_11
                , Tw.flex
                , Tw.justify_center
                ]
            ]
        , Attr.attribute "aria-label" (Translations.linkToHomeAriaLabel [ browserEnv.translations ])
        , Attr.href <| Route.Path.toString Route.Path.Read
        ]
        [ div
            [ Attr.alt "Logo"
            , css
                [ Tw.hidden
                , Tw.overflow_hidden
                , Bp.xl
                    [ Tw.block
                    , Tw.ml_2
                    ]
                ]
            ]
            [ Graphics.paretoLogo5 150 browserEnv.darkMode
            ]
        , div
            [ Attr.alt "Logo"
            , css
                [ Tw.block
                , Tw.w_8
                , Bp.xl
                    [ Tw.hidden
                    ]
                , Bp.sm
                    [ Tw.h_auto
                    , Tw.w_auto
                    ]
                ]
            ]
            [ Graphics.paretoLogo1 30 browserEnv.darkMode
            ]
        ]


roleSwitchButtonEnabled : Nostr.Model -> LoginStatus -> Bool
roleSwitchButtonEnabled nostr loginStatus =
    case loginStatus of
        LoggedIn userPubKey _ ->
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
    Switch.new
        { id = "clientrole"
        , onClick = SetClientRole (not currentPathPresentForOtherRole)
        , labelOff = Translations.readerClientRoleText [ sidebarItemParams.translations ]
        , labelOn = Translations.creatorClientRoleText [ sidebarItemParams.translations ]
        , state = sidebarItemParams.clientRole
        , stateOff = ClientReader
        , stateOn = ClientCreator
        , theme = sidebarItemParams.theme
        }
        |> Switch.view


testModeSwitch : SidebarItemParams -> Html Msg
testModeSwitch sidebarItemParams =
    Switch.new
        { id = "testmode"
        , onClick = SetTestMode
        , labelOff = Translations.testModeOffText [ sidebarItemParams.translations ]
        , labelOn = Translations.testModeOnText [ sidebarItemParams.translations ]
        , state = sidebarItemParams.testMode
        , stateOff = BrowserEnv.TestModeOff
        , stateOn = BrowserEnv.TestModeEnabled
        , theme = sidebarItemParams.theme
        }
        |> Switch.view


profileForUser : Shared.Model -> LoginStatus -> Maybe Profile
profileForUser shared loggedIn =
    case loggedIn of
        LoggedIn pubKey _ ->
            Nostr.getProfile shared.nostr pubKey

        _ ->
            Nothing


viewSidebarItems : Theme -> SidebarItemParams -> Html contentMsg
viewSidebarItems theme sidebarItemParams =
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

                -- compensate calculated margin left 16
                , Tw.pr_4
                , Tw.justify_items_center
                , Css.property "height" "560px"
                ]
            ]
        ]
        (List.map (viewSidebarItem theme sidebarItemParams.currentPath) visibleSidebarItems)


sidebarItemVisible : Bool -> Bool -> Bool -> SidebarItemData -> Bool
sidebarItemVisible isLoggedIn isAuthor isBetaTester sidebarItem =
    if isBetaTester then
        True

    else if sidebarItem.requiresBetaTester then
        isBetaTester

    else if sidebarItem.requiresAuthor then
        isAuthor

    else if sidebarItem.requiresLogin then
        isLoggedIn

    else
        True


viewSidebarItem : Theme -> Route.Path.Path -> SidebarItemData -> Html contentMsg
viewSidebarItem theme currentPath itemData =
    let
        styles =
            Ui.Styles.stylesForTheme theme

        colorStyleSitebarItemActive =
            [ css
                [ Tw.text_color styles.color1
                , Tw.bg_color styles.color3
                , darkMode [ Tw.bg_color styles.color4 ]
                ]
            ]

        colorStyleSitebarItemEnabled =
            [ css
                [ Tw.text_color styles.color3
                , darkMode
                    [ Tw.text_color styles.color2
                    ]
                ]
            ]

        colorStyleSitebarItemDisabled =
            [ css
                [ Tw.text_color styles.color2
                , Bp.sm
                    [ Tw.text_color styles.color2
                    , darkMode
                        [ Tw.text_color styles.color2DarkMode
                        ]
                    ]
                ]
            ]

        ( element, foreground, linkAttr ) =
            if itemData.disabled then
                ( div
                , colorStyleSitebarItemDisabled
                , []
                )

            else if currentPath == itemData.path then
                ( div
                , colorStyleSitebarItemActive
                , []
                )

            else
                ( a
                , colorStyleSitebarItemEnabled
                , [ Attr.href <| Route.toString { path = itemData.path, hash = Nothing, query = Dict.empty }
                  , Attr.attribute "aria-label" itemData.ariaLabel
                  ]
                )
    in
    element
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
        LoggedIn _ _ ->
            loggedInButton shared.browserEnv.environment shared.theme maybeProfile

        _ ->
            getStartedButton shared.theme shared.browserEnv


loggedInButton : Environment -> Theme -> Maybe Profile -> Html Msg
loggedInButton environment theme maybeProfile =
    let
        styles =
            Ui.Styles.stylesForTheme theme
    in
    div
        (css
            [ Tw.bg_color styles.color1
            , Tw.py_2
            , Tw.px_2
            , Tw.rounded_full
            , Tw.border_hidden
            ]
            :: styles.colorStyleIcons
        )
        [ img
            [ Attr.src <| Ui.Profile.profilePicture environment 56 maybeProfile
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



-- in case there are internal errors recorded,
-- a small icon is displayed top/right of the screen that
-- leads to the /internals page showing the error messages


viewLinktoInternalPage : Nostr.Model -> Html msg
viewLinktoInternalPage nostr =
    case Nostr.getErrorMessages nostr of
        [] ->
            emptyHtml

        _ ->
            a
                [ css
                    [ Tw.absolute
                    , Tw.top_2
                    , Tw.right_2
                    ]
                , Attr.href <| Route.Path.toString Route.Path.Internals
                ]
                [ Icon.MaterialIcon Icon.MaterialInfo 10 Icon.Inherit
                    |> Icon.view
                ]
