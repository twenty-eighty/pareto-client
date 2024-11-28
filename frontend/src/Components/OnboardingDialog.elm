module Components.OnboardingDialog exposing (Model, Msg, new, init, update, view, OnboardingDialog)

import Css
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, a, button, div, form, h2, input, label, li, text, ul)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events as Events exposing (..)
import Ports
import Svg.Styled as Svg exposing (svg, path)
import Svg.Styled.Attributes as SvgAttr
import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw
import Tailwind.Theme as Theme
import Shared.Msg exposing (Msg)
import Shared.Model exposing (Model)
import Ui.Styles exposing (fontFamilyUnbounded, fontFamilyInter)

type Msg
    = CloseDialog
    | LoginClicked
    | ExtensionLoginClicked
    | CreateAccountClicked


type Model msg =
    Model
        { state : DialogState
        , onClose : msg
        }

type DialogState
    = LoginOrCreateAccountSelection
    | LoginSelection

type OnboardingDialog msg
    = Settings
        { model : Model msg
        , toMsg : Msg -> msg
        }

new :
    { model : Model msg
    , toMsg : Msg -> msg
    }
    -> OnboardingDialog msg
new props =
    Settings
        { model = props.model
        , toMsg = props.toMsg
        }

init : { onClose : msg } -> Model msg
init props =
    Model
        { state = LoginOrCreateAccountSelection
        , onClose = props.onClose

        }

update :
    { msg : Msg
    , model : Model msg
    , toModel : Model msg -> model
    , toMsg : Msg -> msg
    }
    -> ( model, Effect msg )
update props  = 
    let
        (Model model) =
            props.model

        toParentModel : (Model msg, Effect msg) -> (model, Effect msg)
        toParentModel ( innerModel, effect ) =
            ( props.toModel innerModel
            , effect
            )
    in
    toParentModel <|
        case props.msg of
            CloseDialog ->
                ( Model  model
                , Effect.sendMsg model.onClose
                )

            LoginClicked ->
                ( Model { model | state = LoginSelection }
                , Effect.none
                )

            ExtensionLoginClicked ->
                ( Model { model | state = LoginSelection }
                , Effect.sendCmd Ports.loginWithExtension
                )

            CreateAccountClicked ->
                ( Model  model
                , Effect.none
                )

view : OnboardingDialog msg -> Html msg
view dialog =
    let
        (Settings settings) =
            dialog

        (Model model) =
            settings.model

        content =
            case model.state of
                LoginOrCreateAccountSelection ->
                    viewLoginOrCreateAccountSelection dialog

                LoginSelection ->
                    viewLoginSelection dialog
    in
        {- Dimmed Background -}
    div
        [ css
            [ Tw.fixed
            , Tw.inset_0
            , Tw.bg_color Theme.gray_900
            , Tw.bg_opacity_50
            , Tw.flex
            , Tw.justify_center
            , Tw.items_center
            ]
        ]
        [         {- Modal Container -}
        div
            [ css
                [ Tw.bg_color Theme.white
                , Tw.rounded_lg
                , Tw.shadow_lg
                , Tw.p_8
                , Tw.w_96
                ]
            ]
            [ content ]
        ]
    
viewLoginOrCreateAccountSelection : OnboardingDialog msg -> Html msg
viewLoginOrCreateAccountSelection (Settings settings) =
        div []
            [
            {- Modal Header -}
            div
                [ css
                    [ Tw.flex
                    , Tw.justify_between
                    , Tw.items_center
                    , Tw.mb_6
                    ]
                ]
                [ h2
                    [ css
                        [ Tw.text_xl
                        , Tw.text_color Theme.gray_600
                        , Tw.font_semibold
                        ]
                    , fontFamilyInter
                    ]
                    [ text "Get Started" ]
                , button
                    [ css
                        [ Tw.text_color Theme.gray_800
                        , Tw.w_8
                        , Tw.h_8
                        , Tw.grid
                        , Tw.place_content_center
                        , Css.hover
                            [ Tw.bg_color Theme.gray_100
                            , Tw.rounded_lg
                            ]
                        ]
                    , Events.onClick (CloseDialog |> settings.toMsg)
                    ]
                    [ div
                        [
                        css
                            [ Tw.w_3
                            , Tw.text_color Theme.gray_800
                            ]
                        ]
                        [ closeButtonSvg ]
                    ]
                ]
            , div
                [ css
                    [ Tw.flex
                    , Tw.justify_between
                    , Tw.items_center
                    , Tw.mb_2
                    ]
                ]
                [ h2
                    [ css
                        [ Tw.text_lg
                        , Tw.text_color Theme.gray_800
                        , Tw.font_bold
                        ]
                    , fontFamilyUnbounded
                    ]
                    [ text "I already have an account" ]
                ]
            , div
                [ css
                    [ Tw.flex
                    , Tw.justify_between
                    , Tw.items_center
                    , Tw.mb_6
                    ]
                ]
                [ h2
                    [ css
                        [ Tw.text_base
                        , Tw.text_color Theme.gray_800
                        ]
                    , fontFamilyInter
                    ]
                    [ text "If you already have a nostr account you can log in with your preferred method." ]
                ]
            , fullWidthButton settings "Log In" LoginClicked
            , div
                [ css
                    [ Tw.flex
                    , Tw.justify_between
                    , Tw.items_center
                    , Tw.mt_8
                    , Tw.mb_2
                    ]
                ]
                [ h2
                    [ css
                        [ Tw.text_lg
                        , Tw.text_color Theme.gray_800
                        , Tw.font_bold
                        ]
                    , fontFamilyUnbounded
                    ]
                    [ text "Create account" ]
                ]
            , div
                [ css
                    [ Tw.flex
                    , Tw.justify_between
                    , Tw.items_center
                    , Tw.mb_6
                    ]
                ]
                [ h2
                    [ css
                        [ Tw.text_base
                        , Tw.text_color Theme.gray_800
                        ]
                    , fontFamilyInter
                    ]
                    [ text "You don't have a nostr account? No problem! We can create one in a minute." ]
                ]
            , fullWidthButton settings "Create account" CreateAccountClicked
            ]

viewLoginSelection : OnboardingDialog msg -> Html msg
viewLoginSelection (Settings settings) =
        div []
            [
            {- Modal Header -}
            div
                [ css
                    [ Tw.flex
                    , Tw.justify_between
                    , Tw.items_center
                    , Tw.mb_6
                    ]
                ]
                [ h2
                    [ css
                        [ Tw.text_xl
                        , Tw.text_color Theme.gray_600
                        , Tw.font_semibold
                        ]
                    , fontFamilyInter
                    ]
                    [ text "Log In" ]
                , button
                    [ css
                        [ Tw.text_color Theme.gray_800
                        , Tw.w_8
                        , Tw.h_8
                        , Tw.grid
                        , Tw.place_content_center
                        , Css.hover
                            [ Tw.bg_color Theme.gray_100
                            , Tw.rounded_lg
                            ]
                        ]
                    , Events.onClick (CloseDialog |> settings.toMsg)
                    ]
                    [ div
                        [
                        css
                            [ Tw.w_3
                            , Tw.text_color Theme.gray_800
                            ]
                        ]
                        [ closeButtonSvg ]
                    ]
                ]
            , div
                [ css
                    [ Tw.flex
                    , Tw.justify_between
                    , Tw.items_center
                    , Tw.mb_2
                    ]
                ]
                [ h2
                    [ css
                        [ Tw.text_lg
                        , Tw.text_color Theme.gray_800
                        , Tw.font_bold
                        ]
                    , fontFamilyUnbounded
                    ]
                    [ text "Extension" ]
                ]
            , div
                [ css
                    [ Tw.flex
                    , Tw.justify_between
                    , Tw.items_center
                    , Tw.mb_2
                    ]
                ]
                [ h2
                    [ css
                        [ Tw.text_base
                        , Tw.text_color Theme.gray_800
                        ]
                    , fontFamilyInter
                    ]
                    [ text "Use a nostr extension for logging in." ]
                ]
            , ul
                [ Attr.style "list-style-type" "disc"
                , css
                    [ Tw.text_base
                    , Tw.text_color Theme.purple_900
                    , Tw.mb_2
                    ]
                ]
                [ li
                    [ css
                        [ Tw.items_center
                        , Tw.mb_2
                        ]
                    ]
                    [ a
                        [ css
                            [
                            ]
                        , fontFamilyInter
                        , Attr.href "https://getalby.com/"
                        ]
                        [ text "Alby" ]
                    ]
                ,  li
                    [ css
                        [ Tw.items_center
                        , Tw.mb_3
                        ]
                    ]
                    [ a
                        [ css
                            [ 
                            ]
                        , fontFamilyInter
                        , Attr.href "https://chrome.google.com/webstore/detail/nos2x/kpgefcfmnafjgpblomihpgmejjdanjjp"
                        ]
                        [ text "nos2x" ]
                    ]
                ]
            , halfWidthButton settings "Log In" ExtensionLoginClicked
            , div
                [ css
                    [ Tw.flex
                    , Tw.justify_between
                    , Tw.items_center
                    , Tw.mt_8
                    , Tw.mb_2
                    ]
                ]
                [ h2
                    [ css
                        [ Tw.text_lg
                        , Tw.text_color Theme.gray_800
                        , Tw.font_bold
                        ]
                    , fontFamilyUnbounded
                    ]
                    [ text "Create account" ]
                ]
            , div
                [ css
                    [ Tw.flex
                    , Tw.justify_between
                    , Tw.items_center
                    , Tw.mb_6
                    ]
                ]
                [ h2
                    [ css
                        [ Tw.text_base
                        , Tw.text_color Theme.gray_800
                        ]
                    , fontFamilyInter
                    ]
                    [ text "You don't have a nostr account? No problem! We can create one in a minute." ]
                ]
            , fullWidthButton settings "Create account" CreateAccountClicked
            ]

loginForm =
    [
                         {- Login Form -}
            form
                [ Attr.action "#"
                , Attr.method "POST"
                ]
                [                 {- Email Input -}
                div
                    [ css
                        [ Tw.mb_4
                        ]
                    ]
                    [ label
                        [ Attr.for "email"
                        , css
                            [ Tw.block
                            , Tw.text_sm
                            , Tw.font_medium
                            , Tw.text_color Theme.gray_700
                            , Tw.mb_1
                            ]
                        ]
                        [ text "Email" ]
                    , input
                        [ Attr.type_ "email"
                        , Attr.id "email"
                        , Attr.name "email"
                        , css
                            [ Tw.w_full
                            , Tw.px_4
                            , Tw.py_2
                            , Tw.border
                            , Tw.rounded_lg
                            , Css.focus
                                [ Tw.outline_none
                                , Tw.ring_2
                                , Tw.ring_color Theme.indigo_500
                                , Tw.border_color Theme.indigo_500
                                ]
                            ]
                        , Attr.placeholder "Enter your email"
                        ]
                        []
                    ]
                ,                 {- Password Input -}
                div
                    [ css
                        [ Tw.mb_6
                        ]
                    ]
                    [ label
                        [ Attr.for "password"
                        , css
                            [ Tw.block
                            , Tw.text_sm
                            , Tw.font_medium
                            , Tw.text_color Theme.gray_700
                            , Tw.mb_1
                            ]
                        ]
                        [ text "Password" ]
                    , input
                        [ Attr.type_ "password"
                        , Attr.id "password"
                        , Attr.name "password"
                        , css
                            [ Tw.w_full
                            , Tw.px_4
                            , Tw.py_2
                            , Tw.border
                            , Tw.rounded_lg
                            , Css.focus
                                [ Tw.outline_none
                                , Tw.ring_2
                                , Tw.ring_color Theme.indigo_500
                                , Tw.border_color Theme.indigo_500
                                ]
                            ]
                        , Attr.placeholder "Enter your password"
                        ]
                        []
                    ]
                ,                 {- Submit Button -}
                div
                    [ css
                        [ Tw.mb_4
                        ]
                    ]
                    [ button
                        [ Attr.type_ "submit"
                        , css
                            [ Tw.w_full
                            , Tw.bg_color Theme.indigo_600
                            , Tw.text_color Theme.white
                            , Tw.font_bold
                            , Tw.py_2
                            , Tw.px_4
                            , Tw.rounded_lg
                            , Css.focus
                                [ Tw.outline_none
                                , Tw.ring_2
                                , Tw.ring_color Theme.indigo_500
                                ]
                            , Css.hover
                                [ Tw.bg_color Theme.indigo_700
                                ]
                            ]
                        ]
                        [ text " Login " ]
                    ]
                ]
            ,             {- Additional Options -}
            div
                [ css
                    [ Tw.flex
                    , Tw.justify_between
                    , Tw.items_center
                    ]
                ]
                [ a
                    [ Attr.href "#"
                    , css
                        [ Tw.text_sm
                        , Tw.text_color Theme.indigo_600
                        , Css.hover
                            [ Tw.text_color Theme.indigo_800
                            ]
                        ]
                    ]
                    [ text "Forgot Password?" ]
                , a
                    [ Attr.href "#"
                    , css
                        [ Tw.text_sm
                        , Tw.text_color Theme.gray_600
                        , Css.hover
                            [ Tw.text_color Theme.gray_800
                            ]
                        ]
                    ]
                    [ text "Sign Up" ]
                ]
            ]

fullWidthButton settings title event =
             button
                [ css
                    [ Css.hover
                        [ Tw.bg_color Theme.gray_300
                        ]
                    , Tw.bg_color Theme.gray_200
                    , Tw.text_color Theme.gray_800
                    , Tw.py_2
                    , Tw.px_4
                    , Tw.w_full
                    , Tw.rounded_full
                    , Tw.font_semibold
                    ]
                , fontFamilyInter
                , Events.onClick (event |> settings.toMsg)
                ]
                [ text title ]

halfWidthButton settings title event =
             button
                [ css
                    [ Css.hover
                        [ Tw.bg_color Theme.orange_700
                        ]
                    , Tw.bg_color Theme.orange_500
                    , Tw.text_color Theme.white
                    , Tw.py_2
                    , Tw.px_20
                    , Tw.rounded_full
                    , Tw.font_semibold
                    ]
                , fontFamilyInter
                , Events.onClick (event |> settings.toMsg)
                ]
                [ text title ]

closeButtonSvg =
    svg
        [ SvgAttr.viewBox "0 0 24 24"
        , Attr.attribute "focusable" "false"
        , Attr.attribute "aria-hidden" "true"
        ]
        [ path
            [ SvgAttr.fill "currentColor"
            , SvgAttr.d "M.439,21.44a1.5,1.5,0,0,0,2.122,2.121L11.823,14.3a.25.25,0,0,1,.354,0l9.262,9.263a1.5,1.5,0,1,0,2.122-2.121L14.3,12.177a.25.25,0,0,1,0-.354l9.263-9.262A1.5,1.5,0,0,0,21.439.44L12.177,9.7a.25.25,0,0,1-.354,0L2.561.44A1.5,1.5,0,0,0,.439,2.561L9.7,11.823a.25.25,0,0,1,0,.354Z"
            ]
            []
        ]
    