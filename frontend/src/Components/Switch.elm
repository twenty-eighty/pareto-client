module Components.Switch exposing
    ( Switch, new
    , view
    , withDisabled
    )

{-|


## Basic usage

@docs Switch, new
@docs view


## Modifiers

@docs withStyleSuccess, withStyleWarning, withStyleDanger
@docs withSizeSmall
@docs withIconLeft, withIconRight
@docs withDisabled

-}

import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events as Events
import Tailwind.Breakpoints as Bp
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw
import Ui.Styles exposing (Theme(..), darkMode)



-- SETTINGS


type Switch type_ msg
    = Settings
        { id : String
        , labelOff : String
        , labelOn : String
        , onClick : type_ -> msg
        , state : type_
        , isDisabled : Bool
        , stateOff : type_
        , stateOn : type_
        , theme : Ui.Styles.Theme
        }


new : { id : String, onClick : type_ -> msg, state : type_, labelOff : String, labelOn : String, stateOff : type_, stateOn : type_, theme : Ui.Styles.Theme } -> Switch type_ msg
new props =
    Settings
        { id = props.id
        , onClick = props.onClick
        , isDisabled = False
        , labelOff = props.labelOff
        , labelOn = props.labelOn
        , state = props.state
        , stateOff = props.stateOff
        , stateOn = props.stateOn
        , theme = props.theme
        }



-- MODIFIERS


withDisabled : Bool -> Switch type_ msg -> Switch type_ msg
withDisabled isDisabled (Settings settings) =
    Settings { settings | isDisabled = isDisabled }



-- VIEW


view : Switch type_ msg -> Html msg
view (Settings settings) =
    let
        otherState =
            if settings.state == settings.stateOn then
                settings.stateOff

            else
                settings.stateOn

        styles =
            Ui.Styles.stylesForTheme ParetoTheme
    in
    -- ( styles.colorStyleButtonText ++ styles.colorStyleButtonBackground ++
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
            (css
                [ Tw.font_medium ]
                :: styles.colorStyleLabel
            )
            [ if settings.state == settings.stateOn then
                text <| settings.labelOn

              else
                text <| settings.labelOff
            ]
        , {- Switch -}
          label
            [ Attr.for settings.id
            , css
                [ Tw.relative
                , Tw.cursor_pointer
                ]
            ]
            [ {- Hidden checkbox -}
              input
                [ Attr.type_ "checkbox"
                , Attr.attribute "data-test" ("switch-" ++ settings.labelOff ++ "-" ++ settings.labelOn)
                , Attr.id settings.id
                , Events.onClick (settings.onClick otherState)
                , css
                    [ Tw.sr_only
                    ]
                ]
                []
            , {- Switch Background -}
              if settings.state == settings.stateOff then
                div
                    [ Attr.id "switch-background"
                    , css
                        [ Tw.w_11
                        , Tw.h_6
                        , Tw.rounded_full
                        , Tw.transition_all
                        , Tw.duration_300
                        , Tw.bg_color styles.colorB1
                        , darkMode
                            [ Tw.bg_color styles.colorB1DarkMode
                            ]
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
              if settings.state == settings.stateOff then
                div
                    ([ Attr.id "switch-knob"
                     , css
                        [ Tw.absolute
                        , Tw.top_0_dot_5
                        , Tw.left_0_dot_5
                        , Tw.w_5
                        , Tw.h_5
                        , Tw.rounded_full
                        , Tw.bg_color Theme.white
                        , Tw.border
                        , Tw.transition_transform
                        , Tw.duration_300
                        ]
                     ]
                        ++ styles.colorStyleBorders
                    )
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
