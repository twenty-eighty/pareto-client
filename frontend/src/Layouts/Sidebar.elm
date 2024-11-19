module Layouts.Sidebar exposing (Model, Msg, Props, layout)

import BrowserEnv exposing (BrowserEnv)
import Components.OnboardingDialog as OnboardingDialog
import Css
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
import Nostr.Types exposing (PubKey)
import Nostr.Profile exposing (Profile)
import Pareto
import Route exposing (Route)
import Shared
import Shared.Model exposing (LoginStatus(..))
import Shared.Msg
import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw
import Tailwind.Theme as Theme
import Translations
import View exposing (View)
import Route.Path
import Dict
import Shared.Model exposing (ClientRole)
import Shared.Model exposing (ClientRole(..))


type alias Props =
    { 
    }

type alias SidebarItemData =
    { path : Route.Path.Path
    , title : String
    , icon : FeatherIcons.Icon
    , requiresLogin : Bool
    , disabled : Bool
    }

sidebarItems : ClientRole -> I18Next.Translations -> List (SidebarItemData)
sidebarItems clientRole translations =
    case clientRole of
        ClientConsumer ->
            [ { path = Route.Path.Read, title = Translations.readMenuItemText [ translations ], icon = FeatherIcons.bookOpen, requiresLogin = False, disabled = False }
            , { path = Route.Path.Search, title = Translations.searchMenuItemText [ translations ], icon = FeatherIcons.search, requiresLogin = False, disabled = True }
          --, { path = Route.Path.Communities, title = Translations.communitiesMenuItemText [ translations ], icon = FeatherIcons.globe, requiresLogin = False, disabled = False }
            , { path = Route.Path.Bookmarks, title = Translations.bookmarksMenuItemText [ translations ], icon = FeatherIcons.bookmark, requiresLogin = True, disabled = False }
            , { path = Route.Path.About, title = Translations.aboutMenuItemText [ translations ], icon = FeatherIcons.helpCircle, requiresLogin = False, disabled = False }
            ]

        ClientCreator ->
            [ { path = Route.Path.Posts, title = Translations.postsMenuItemText [ translations ], icon = FeatherIcons.fileText, requiresLogin = True, disabled = False }
            , { path = Route.Path.Write, title = Translations.writeMenuItemText [ translations ], icon = FeatherIcons.feather, requiresLogin = True, disabled = False }
            , { path = Route.Path.Search, title = Translations.searchMenuItemText [ translations ], icon = FeatherIcons.search, requiresLogin = False, disabled = True }
            ]


layout : Props -> Shared.Model -> Route () -> Layout () Model Msg contentMsg
layout props shared route =
    Layout.new
        { init = init
        , update = update shared
        , view = view shared route.path
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
            ( { model | modalDialog = GetStartedDialog <| OnboardingDialog.init { onClose = CloseModal } }
            , Effect.none
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


view : Shared.Model.Model -> Route.Path.Path -> { toContentMsg : Msg -> contentMsg, content : View contentMsg, model : Model } -> View contentMsg
view shared path { toContentMsg, model, content } =
    { title = content.title ++ " | Pareto"
    , body = 
        [ viewSidebar shared path toContentMsg content.body
        , viewModalDialog model |> Html.map toContentMsg
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


viewSidebar : Shared.Model.Model -> Route.Path.Path -> (Msg -> contentMsg) -> List (Html contentMsg) -> Html contentMsg
viewSidebar shared currentPath toContentMsg content =
        Html.div
                [ css
                    [ Tw.bg_color Theme.gray_50
                    , Tw.text_color Theme.gray_800
                    ]
                ]
                [ div
                    [ css
                        [ Tw.flex
                        , Tw.h_screen
                        ]
                    ]
                    [             {- Sidebar -}
                    aside
                        [ css
                            [ Tw.bg_color Theme.white
                            , Tw.p_6
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
                        [ viewSidebarItems shared.browserEnv.translations shared.role (Shared.loggedIn shared) currentPath ]
        , div
            [ css
                [ Tw.flex_1
                , Tw.p_8
                ]
            ]
            [                 {- Top Section -}
            div
                [ css
                    [ Tw.flex
                    , Tw.justify_between
                    , Tw.items_center
                    , Tw.mb_6
                    ]
                ]
                [ -- viewBanner
                 if roleSwitchButtonEnabled shared.nostr shared.loginStatus then
                    clientRoleSwitch shared.role
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

viewBannerSmall : Html contentMsg
viewBannerSmall =
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
                [ Tw.h_10
                , Tw.rounded_md
                ]
            ]
            []
        ]

roleSwitchButtonEnabled : Nostr.Model -> LoginStatus -> Bool
roleSwitchButtonEnabled nostr loginStatus =
    case loginStatus of
        LoggedIn pubKey ->
            Nostr.getFollowsList nostr Pareto.authorsKey
            |> Maybe.map (List.filter (\follows -> follows.pubKey == pubKey))
            |> Maybe.map (not << List.isEmpty)
            |> Maybe.withDefault False

        _ ->
            False

clientRoleSwitch : ClientRole -> Html Msg
clientRoleSwitch clientRole =
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
            [ if clientRole == ClientConsumer then
                 text "Consumer"
              else  
                 text "Creator"
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
            if clientRole == ClientConsumer then
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
            if clientRole == ClientConsumer then
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



viewSidebarItems : I18Next.Translations -> ClientRole -> Bool -> Route.Path.Path -> Html msg
viewSidebarItems translations clientRole loggedIn currentPath =
    let
        visibleSidebarItems =
            (sidebarItems clientRole translations)
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
        ([ viewBannerSmall
        ] ++
        (List.map (viewSidebarItem currentPath) visibleSidebarItems)
        )


sidebarItemVisible : Bool -> SidebarItemData -> Bool
sidebarItemVisible loggedIn sidebarItem =
    loggedIn || not sidebarItem.requiresLogin

viewSidebarItem : Route.Path.Path -> SidebarItemData -> Html msg
viewSidebarItem currentPath itemData =
    let
        (foreground, background) =
            if itemData.disabled then
                ( [ Tw.text_color Theme.gray_300
                  ]
                , [ 
                  ]
                )
            else
                if currentPath == itemData.path then
                    ( [ Tw.text_color Theme.purple_600
                    ]
                    , [ Tw.bg_color Theme.purple_100
                    ]
                    )
                else
                    ( [ Tw.text_color Theme.gray_600
                    ]
                    , [ Css.hover
                        [ Tw.bg_color Theme.purple_100
                        ]
                    ]
                    )
    in

    a
        [ Attr.href <| Route.toString { path = itemData.path, hash = Nothing, query = Dict.empty }
        , css
            ([ Tw.flex
            , Tw.items_center
            , Tw.space_x_2
            , Tw.w_0
            , Tw.py_2
            , Tw.rounded_full
            , Bp.xl
                [ Tw.w_40
                ]
            , Bp.md
                [ Tw.w_10
                ]
            ] ++ background ++ foreground)
        ]
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
            [ itemData.icon |> FeatherIcons.toHtml [] |> Html.fromUnstyled ]
        , span
            [ css
                [ Tw.hidden 
                , Bp.xl
                    ([ Tw.inline
                    ] ++ foreground)
                ]
            ]
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
    button
        [ css
            [ Tw.bg_color Theme.orange_500
            , Tw.text_color Theme.white
            , Tw.py_2
            , Tw.px_4
            , Tw.rounded_full
            , Css.hover
                [ Tw.bg_color Theme.orange_700
                ]
            ]
        , Events.onClick OpenGetStarted
        ]
        [ text <| Translations.getStartedButtonText [ browserEnv.translations ] ]