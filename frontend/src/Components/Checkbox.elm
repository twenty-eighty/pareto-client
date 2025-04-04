module Components.Checkbox exposing
    ( Checkbox, new
    , view
    , withDisabled
    , withImage
    )

{-|


## Basic usage

@docs Checkbox, new
@docs view


## Modifiers

@docs withStyleSuccess, withStyleWarning, withStyleDanger
@docs withSizeSmall
@docs withIconLeft, withIconRight
@docs withDisabled

-}

import Graphics
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Events
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw
import Ui.Shared exposing (emptyHtml)
import Ui.Styles exposing (Theme(..), darkMode)



-- SETTINGS


type Checkbox msg
    = Settings
        { label : String
        , onClick : Bool -> msg
        , checked : Bool
        , imageUrl : Maybe String
        , isOutlined : Bool
        , isDisabled : Bool
        , theme : Ui.Styles.Theme
        }


new : { label : String, onClick : Bool -> msg, checked : Bool, theme : Ui.Styles.Theme } -> Checkbox msg
new props =
    Settings
        { label = props.label
        , onClick = props.onClick
        , checked = props.checked
        , imageUrl = Nothing
        , isOutlined = False
        , isDisabled = False
        , theme = props.theme
        }



-- MODIFIERS


withImage : String -> Checkbox msg -> Checkbox msg
withImage imageUrl (Settings settings) =
    Settings { settings | imageUrl = Just imageUrl }


withDisabled : Bool -> Checkbox msg -> Checkbox msg
withDisabled isDisabled (Settings settings) =
    Settings { settings | isDisabled = isDisabled }



-- VIEW


view : Checkbox msg -> Html msg
view (Settings settings) =
    let
        styles =
            Ui.Styles.stylesForTheme ParetoTheme

        viewOptionalImage : Maybe String -> Html msg
        viewOptionalImage maybeImage =
            case maybeImage of
                Just imageUrl ->
                    span
                        [ Attr.css
                            [ Tw.flex
                            , Tw.items_center
                            , Tw.w_6
                            , Tw.h_6
                            , Tw.rounded_full
                            , Tw.bg_clip_border
                            ]
                        ]
                        [ object
                            [ Attr.attribute "data" imageUrl
                            , Attr.css
                                [ Tw.h_5
                                , Tw.w_5
                                , Tw.object_cover
                                , Tw.overflow_clip
                                ]
                            ]
                            [ div
                                (Attr.css
                                    [ Tw.text_color Theme.black
                                    , darkMode [ Tw.text_color Theme.white ]
                                    , Tw.items_center
                                    , Tw.bg_clip_border
                                    , Tw.rounded_full
                                    , Tw.h_5
                                    , Tw.w_5
                                    , Tw.flex
                                    , Tw.justify_center
                                    ]
                                    :: styles.colorStyleBackground
                                )
                                [ Graphics.defaultRelayImage 14
                                ]
                            ]
                        ]

                Nothing ->
                    text ""
    in
    -- ( styles.colorStyleButtonText ++ styles.colorStyleButtonBackground ++
    label
        [ Attr.css
            [ Tw.flex
            , Tw.relative
            , Tw.align_top
            , Tw.cursor_pointer
            , Tw.items_center
            ]
        ]
        [ input
            [ Attr.type_ "checkbox"
            , Events.onClick (settings.onClick (not settings.checked))
            , Attr.css
                [ Tw.absolute
                , Tw.w_px
                , Tw.h_px
                ]
            ]
            []
        , span
            (Attr.css
                [ Tw.border_2
                , Tw.border_r_2
                , Tw.cursor_pointer
                , Tw.w_4
                , Tw.h_4
                , Tw.flex
                , Tw.items_center
                , Tw.justify_center
                , Tw.text_color Theme.white
                , Tw.bg_color styles.color3
                , darkMode [ Tw.bg_color styles.color2DarkMode ]
                ]
                :: styles.colorStyleBorders
            )
            [ if settings.checked then
                div
                    [ Attr.css
                        [ Tw.flex
                        , Tw.items_center
                        , Tw.justify_center
                        , Tw.h_3
                        ]
                    ]
                    [ Graphics.checkboxCheckMark 10 ]

              else
                emptyHtml
            ]
        , span
            [ Attr.css
                [ Tw.w_4
                , Tw.ms_2
                , Tw.w_auto
                , Tw.leading_6
                ]
            ]
            [ div
                [ Attr.css
                    [ Tw.flex
                    , Tw.items_center
                    , Tw.w_auto
                    , Tw.gap_2
                    ]
                ]
                [ viewOptionalImage settings.imageUrl
                , p
                    [ Attr.css
                        [ Tw.w_auto
                        ]
                    ]
                    [ text settings.label
                    ]
                ]
            ]
        ]
