module Components.ModalDialog exposing
    ( ModalDialog, new
    , view
    )

{-|


## Basic usage

@docs ModalDialog, new
@docs view


## Modifiers

-}

import Css
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events as Events
import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw
import Ui.Styles exposing (Theme(..), darkMode, stylesForTheme)



-- SETTINGS


type ModalDialog msg
    = Settings
        { title : String
        , buttons : List (Html msg)
        , content : List (Html msg)
        , onClose : msg
        , theme : Ui.Styles.Theme
        }


new : { title : String, buttons : List (Html msg), content : List (Html msg), onClose : msg, theme : Ui.Styles.Theme } -> ModalDialog msg
new props =
    Settings
        { title = props.title
        , buttons = props.buttons
        , content = props.content
        , onClose = props.onClose
        , theme = props.theme
        }



-- MODIFIERS
-- VIEW


view : ModalDialog msg -> Html msg
view (Settings settings) =
    let
        styles =
            stylesForTheme settings.theme
    in
    div
        (css
            [ Tw.fixed
            , Tw.inset_0
            , Tw.bg_opacity_50
            , Tw.flex
            , Tw.justify_center
            , Tw.items_center
            , Tw.z_50
            ]
            :: styles.colorStyleBackground
        )
        [ div
            (styles.colorStyleBackground
                ++ [ css
                        [ Tw.rounded_lg
                        , Tw.shadow_lg
                        , Tw.drop_shadow_md
                        , Tw.backdrop_blur_md
                        , Tw.shadow_color styles.colorB2
                        , Tw.max_h_screen
                        , darkMode
                            [ Tw.shadow_color styles.colorB2DarkMode
                            ]
                        , Tw.w_auto
                        , Bp.xxl
                            [ Tw.max_w_3xl
                            ]
                        , Bp.xl
                            [ Tw.max_w_2xl
                            ]
                        , Bp.lg
                            [ Tw.max_w_xl
                            ]
                        , Bp.md
                            [ Tw.max_w_lg
                            , Tw.p_8
                            ]
                        , Bp.sm
                            [ Tw.max_w_md
                            , Tw.p_4
                            ]
                        , Tw.max_w_sm
                        , Tw.p_2
                        ]
                   ]
            )
            [ div
                [ css
                    [ Tw.flex
                    , Tw.justify_between
                    , Tw.items_center
                    , Tw.border_b
                    , Tw.pb_4
                    , Tw.gap_4
                    ]
                ]
                [ h2
                    [ css
                        [ Tw.text_lg
                        , Tw.font_semibold
                        , Tw.text_color styles.colorB4
                        , darkMode [ Tw.text_color styles.colorB4DarkMode ]
                        ]
                    ]
                    [ text settings.title ]
                , button
                    ([ css
                        [ Css.hover
                            [ Tw.text_color styles.colorB2 ]
                        , darkMode
                            [ Css.hover
                                [ Tw.text_color styles.colorB2DarkMode ]
                            ]
                        ]
                     , Attr.id "close-modal"
                     , Events.onClick settings.onClose
                     ]
                        ++ styles.colorStyleGrayscaleText
                    )
                    [ text " ✕ " ]
                ]
            , div
                [ css
                    [ Tw.overflow_y_auto
                    , Tw.flex
                    , Tw.flex_col
                    , Tw.gap_2
                    , Tw.mt_4
                    ]
                ]
                settings.content
            , div
                [ css
                    [ Tw.flex
                    , Tw.justify_end
                    , Tw.gap_2
                    , Tw.mt_4
                    ]
                ]
                settings.buttons
            ]
        ]
