module Layouts.Sidebar exposing (Model, Msg, Props, layout, map, clientRoleForRoutePath)

import BrowserEnv exposing (BrowserEnv)
import Components.Button
import Components.ContextMenu
import Components.Icon as Icon exposing (Icon(..))
import Components.OnboardingDialog as OnboardingDialog
import Css
import Dict
import Effect exposing (Effect)
import FeatherIcons
import Graphics
import Html.Styled as Html exposing (Html, a, article, aside, button, div, h2, h3, h4, img, input, label, main_, p, span, text)
import Html.Styled.Attributes as Attr exposing (class, css)
import Html.Styled.Events as Events exposing (..)
import I18Next
import Layout exposing (Layout)
import ModalDialog exposing (ModalDialog)
import Nostr
import Nostr.FollowList exposing (Following(..))
import Nostr.Types exposing (PubKey)
import Nostr.Profile exposing (Profile)
import Pareto
import Ports
import Route exposing (Route)
import Route.Path
import Shared
import Shared.Model exposing (ClientRole(..), LoginStatus(..))
import Shared.Msg
import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw
import Tailwind.Theme as Theme
import Translations.Sidebar as Translations
import Ui.Styles exposing (Styles)
import View exposing (View)
import Ui.Styles exposing (darkMode)


type alias Props contentMsg =
    { styles : Styles contentMsg
    }

map : (msg1 -> msg2) -> Props msg1 -> Props msg2
map toMsg props =
    { styles = Ui.Styles.map toMsg props.styles
    }

clientRoleForRoutePath : Route.Path.Path -> ClientRole
clientRoleForRoutePath path =
    sidebarItems ClientReader I18Next.initialTranslations
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
    , disabled : Bool
    }

sidebarItems : ClientRole -> I18Next.Translations -> List (SidebarItemData)
sidebarItems clientRole translations =
    case clientRole of
        ClientReader ->
            [ { path = Route.Path.Read, title = Translations.readMenuItemText [ translations ], icon = FeatherIcon FeatherIcons.bookOpen, requiresLogin = False, disabled = False }
            , { path = Route.Path.Search, title = Translations.searchMenuItemText [ translations ], icon = FeatherIcon FeatherIcons.search, requiresLogin = False, disabled = True }
          --, { path = Route.Path.Communities, title = Translations.communitiesMenuItemText [ translations ], icon = FeatherIcon FeatherIcons.globe, requiresLogin = False, disabled = False }
            , { path = Route.Path.Bookmarks, title = Translations.bookmarksMenuItemText [ translations ], icon = FeatherIcon FeatherIcons.bookmark, requiresLogin = True, disabled = False }
            , { path = Route.Path.Messages, title = Translations.messagesMenuItemText [ translations ], icon = FeatherIcon FeatherIcons.mail, requiresLogin = True, disabled = True }
            , { path = Route.Path.Notifications, title = Translations.notificationsMenuItemText [ translations ], icon = FeatherIcon FeatherIcons.bell, requiresLogin = True, disabled = True }
            , { path = Route.Path.Settings, title = Translations.settingsMenuItemText [ translations ], icon = FeatherIcon FeatherIcons.settings, requiresLogin = True, disabled = True }
            , { path = Route.Path.About, title = Translations.aboutMenuItemText [ translations ], icon = FeatherIcon FeatherIcons.helpCircle, requiresLogin = False, disabled = False }
            ]

        ClientCreator ->
            [ { path = Route.Path.Posts, title = Translations.postsMenuItemText [ translations ], icon = FeatherIcon FeatherIcons.fileText, requiresLogin = True, disabled = False }
            , { path = Route.Path.Write, title = Translations.writeMenuItemText [ translations ], icon = FeatherIcon FeatherIcons.feather, requiresLogin = True, disabled = False }
            , { path = Route.Path.Search, title = Translations.searchMenuItemText [ translations ], icon = FeatherIcon FeatherIcons.search, requiresLogin = False, disabled = True }
            , { path = Route.Path.Media, title = Translations.mediaMenuItemText [ translations ], icon = FeatherIcon FeatherIcons.image, requiresLogin = False, disabled = False }
            , { path = Route.Path.Messages, title = Translations.messagesMenuItemText [ translations ], icon = FeatherIcon FeatherIcons.mail, requiresLogin = True, disabled = True }
            , { path = Route.Path.Notifications, title = Translations.notificationsMenuItemText [ translations ], icon = FeatherIcon FeatherIcons.bell, requiresLogin = True, disabled = True }
            , { path = Route.Path.Settings, title = Translations.settingsMenuItemText [ translations ], icon = FeatherIcon FeatherIcons.settings, requiresLogin = True, disabled = True }
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
    | LoginDialogSent (OnboardingDialog.Msg)
    | SwitchClientRole Bool


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
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
                        , toModel = \loginDialog -> { model | modalDialog = GetStartedDialog loginDialog}
                        , toMsg = LoginDialogSent
                        }
                _ ->
                    ( model
                    , Effect.none
                    )

        SwitchClientRole state ->
            ( model, Effect.sendSharedMsg Shared.Msg.SwitchClientRole )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Styles contentMsg -> Shared.Model.Model -> Route.Path.Path -> { toContentMsg : Msg -> contentMsg, content : View contentMsg, model : Model } -> View contentMsg
view styles shared path { toContentMsg, model, content } =
    { title = content.title ++ " | Pareto"
    , body = 
        [ div
            (styles.colorStyleBackground ++
            [ css
                [ Tw.h_full
                ]
            ])
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
                div [][]


viewSidebar : Styles contentMsg -> Shared.Model.Model -> Route.Path.Path -> (Msg -> contentMsg) -> List (Html contentMsg) -> Html contentMsg
viewSidebar styles shared currentPath toContentMsg content =
    Html.div
        (styles.colorStyleGrayscaleTitle)
        [ div
            [ css
                [ Tw.flex
                ]
            ]
            [ aside
                [ css
                    [ Tw.p_6
                    , Tw.border_r
                    , Tw.border_color Theme.gray_200
                    , Tw.w_0
                    , Tw.hidden
                    , Bp.xl
                        [ Tw.w_52
                        ]
                    , Bp.md
                        [ Tw.inline
                        , Tw.w_20
                        ]
                    ]
                ]
                [ viewSidebarItems styles shared.browserEnv shared.role (Shared.loggedIn shared) currentPath ]
        , div
            [ css
                [ Tw.flex_1
                , Tw.p_8
                , Tw.h_full
                ]
            ]
            [ div
                [ css
                    [ Tw.flex
                    , Tw.justify_between
                    , Tw.items_center
                    , Tw.mb_6
                    ]
                ]
                [ -- viewBanner
                 if roleSwitchButtonEnabled shared.nostr shared.loginStatus then
                    clientRoleSwitch shared.browserEnv.translations shared.role
                  else
                    div [][]
                , loginButton shared.browserEnv shared.loginStatus (profileForUser shared shared.loginStatus)
                ]
                |> Html.map toContentMsg
            , main_
                [ class "page"
                , css
                    [ Tw.w_full
                    ]
                ]
                content
            ]
        ]
    ]

viewBanner : Html contentMsg
viewBanner =
    div
        [ css
            [ Tw.flex
            , Tw.items_center
            , Tw.space_x_4
            ]
        ]
        [ img
            [ Attr.src "/images/pareto-banner.png"
            , Attr.alt "Banner"
            , css
                [ Tw.h_16
                ]
            ]
            []
        ]

viewBannerSmall : BrowserEnv -> Html contentMsg
viewBannerSmall browserEnv =
    let
        bannerImageWide =
            if browserEnv.darkMode then
                "/images/icon/Pareto-Log4.png"
            else
                "/images/icon/Pareto-Log5.png"

        bannerImageNarrow =
            "/images/icon/Pareto-Log1.png"
    in
    div
        [ css
            [ Tw.flex
            , Tw.items_center
            , Tw.space_x_2
            ]
        ]
        [ div
            [ Attr.src bannerImageWide
            , Attr.alt "Banner"
            , css
                [ Tw.h_10
                , Tw.w_0
                , Bp.xl
                    [ Tw.w_40
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
                [ Tw.h_8
                , Tw.w_8
                , Bp.xl
                    [ Tw.w_0
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
            Nostr.getFollowsList nostr Pareto.authorsKey
            |> Maybe.map (List.filter (\follows ->
                case follows of
                    FollowingPubKey { pubKey } ->
                        userPubKey == pubKey

                    _ ->
                        False
                    ))
            |> Maybe.map (not << List.isEmpty)
            |> Maybe.withDefault False

        _ ->
            False

clientRoleSwitch : I18Next.Translations -> ClientRole -> Html Msg
clientRoleSwitch translations clientRole =
    {- Switch Container -}
    div
        [ css
            [ Tw.flex
            , Tw.items_center
            , Tw.space_x_4
            ]
        ]
        [         {- Label for the Switch -}
        span
            [ css
                [ Tw.text_color Theme.gray_700
                , Tw.font_medium
                ]
            ]
            [ if clientRole == ClientReader then
                 text <| Translations.readerClientRoleText [ translations ]
              else  
                 text <| Translations.creatorClientRoleText [ translations ]
             ]
        ,         {- Switch -}
        label
            [ Attr.for "toggle-switch"
            , css
                [ Tw.relative
                , Tw.cursor_pointer
                ]
            ]
            [             {- Hidden checkbox -}
            input
                [ Attr.type_ "checkbox"
                , Attr.id "toggle-switch"
                , Events.onCheck SwitchClientRole
                , css
                    [ Tw.sr_only
                    ]
                ]
                []
            ,             {- Switch Background -}
            if clientRole == ClientReader then
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
            ,             {- Switch Knob -}
            if clientRole == ClientReader then
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



viewSidebarItems : Styles contentMsg -> BrowserEnv -> ClientRole -> Bool -> Route.Path.Path -> Html contentMsg
viewSidebarItems styles browserEnv clientRole loggedIn currentPath =
    let
        visibleSidebarItems =
            (sidebarItems clientRole browserEnv.translations)
            |> List.filter (sidebarItemVisible loggedIn)
    in
    
    div
        [ css
            [ Tw.flex
            , Tw.flex_col
            , Tw.items_start
            , Tw.space_y_4
            ]
        ]
        ([ viewBannerSmall browserEnv
        ] ++
        (List.map (viewSidebarItem styles currentPath) visibleSidebarItems)
        )


sidebarItemVisible : Bool -> SidebarItemData -> Bool
sidebarItemVisible loggedIn sidebarItem =
    loggedIn || not sidebarItem.requiresLogin

viewSidebarItem : Styles contentMsg -> Route.Path.Path -> SidebarItemData -> Html contentMsg
viewSidebarItem styles currentPath itemData =
    let
        (foreground, background, linkAttr) =
            if itemData.disabled then
                ( styles.colorStyleGrayscaleDisabled
                , []
                , []
                )
            else
                if currentPath == itemData.path then
                    ( styles.colorStyleSitebarItemActive
                    , styles.colorStyleSitebarItemActiveBackground
                    , [ ]
                    )
                else
                    ( styles.colorStyleLabel
                    , []
                    , [ Attr.href <| Route.toString { path = itemData.path, hash = Nothing, query = Dict.empty } ]
                    )
    in

    a
        (linkAttr ++
        [ css
            [ Tw.flex
            , Tw.items_center
            , Tw.space_x_2
            , Tw.py_2
            , Tw.rounded_full
            , Tw.w_0
            , Bp.xl
                [ Tw.w_40
                ]
            , Bp.md
                [ Tw.w_10
                ]
            ]
        ] ++ background ++ foreground)
        [ div
            [ css
                [ Tw.min_w_0
                , Tw.flex
                , Tw.items_center
                , Tw.justify_center
                , Bp.md
                    [ Tw.min_w_10
                    ] 
                ]
            ]
            [ Icon.view itemData.icon ]
        , span
            ([ css
                [ Tw.hidden 
                , Bp.xl
                    [ Tw.inline
                    ]
                ]
            ] ++ foreground)
            [ text itemData.title ]
        ]

loginButton : BrowserEnv -> LoginStatus -> Maybe Profile -> Html Msg
loginButton browserEnv loginStatus maybeProfile =
    case loginStatus of
        Shared.Model.LoggedIn pubKey ->
            loggedInButton maybeProfile

        _ ->
            getStartedButton browserEnv

loggedInButton : Maybe Profile -> Html Msg
loggedInButton maybeProfile =
            button
                [ css
                    [ Tw.bg_color Theme.gray_100
                    , Tw.text_color Theme.white
                    , Tw.py_2
                    , Tw.px_4
                    , Tw.rounded_full
                    , Tw.border_hidden
                    , Tw.space_x_2
                    , Tw.flex
                    , Tw.flex_row
                    , Css.hover
                        [ Tw.bg_color Theme.gray_300
                        ]
                    ]
                , Events.onClick OpenProfileMenu
                ]
                [ img 
                    [ Attr.src <| profileImage maybeProfile
                    , css
                        [ Tw.py_1
                        , Tw.px_1
                        , Tw.w_14
                        , Tw.h_14
                        , Tw.border_hidden
                        , Tw.rounded_full
                        ]
                    ]
                    []
                , div
                    [ css
                        [ Tw.py_1
                        , Tw.px_1
                        , Tw.w_8
                        , Tw.h_14
                        , Tw.border_hidden
                        , Tw.text_color Theme.gray_900
                        , Tw.grid
                        ]
                    ]
                    [div
                        [ css
                            [ Tw.w_8
                            , Tw.h_8
                            , Tw.place_self_center
                            ]
                        ]
                        [ Graphics.chakraIcon
                        ]
                    ]
                ]

profileImage : Maybe Profile -> String
profileImage maybeProfile =
    maybeProfile
    |> Maybe.andThen (\profile -> profile.picture)
    |> Maybe.withDefault "/images/avatars/placeholder_01.png" 

getStartedButton : BrowserEnv -> Html Msg
getStartedButton browserEnv =
    Components.Button.new
        { label = Translations.getStartedButtonText [ browserEnv.translations ]
        , onClick = OpenGetStarted
        }
        |> Components.Button.view
