module Components.Button exposing
    ( Button, new
    , view
    , withStyleSuccess, withStyleWarning, withStyleDanger
    , withSizeSmall
    , withIconLeft, withIconRight
    , withDisabled
    )

{-|

## Basic usage

@docs Button, new
@docs view

## Modifiers

@docs withStyleSuccess, withStyleWarning, withStyleDanger
@docs withSizeSmall
@docs withIconLeft, withIconRight
@docs withDisabled

-}
import Components.Icon exposing (Icon)
import Css
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events as Events
import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw
import Tailwind.Theme as Theme


-- SETTINGS


type Button msg
    = Settings
        { label : String
        , onClick : msg
        , style : Style
        , size : Size
        , iconLeft : Maybe Icon
        , iconRight : Maybe Icon
        , isOutlined : Bool
        , isDisabled : Bool
        }


new : { label : String, onClick : msg } -> Button msg
new props =
    Settings
        { label = props.label
        , onClick = props.onClick
        , style = Default
        , size = Normal
        , iconLeft = Nothing
        , iconRight = Nothing
        , isOutlined = False
        , isDisabled = False
        }



-- MODIFIERS


type Style
    = Default
    | Success
    | Warning
    | Danger


withStyleSuccess : Button msg -> Button msg
withStyleSuccess (Settings settings) =
    Settings { settings | style = Success }


withStyleWarning : Button msg -> Button msg
withStyleWarning (Settings settings) =
    Settings { settings | style = Warning }


withStyleDanger : Button msg -> Button msg
withStyleDanger (Settings settings) =
    Settings { settings | style = Danger }


type Size
    = Normal
    | Small


withSizeSmall : Button msg -> Button msg
withSizeSmall (Settings settings) =
    Settings { settings | size = Small }


withIconLeft : Icon -> Button msg -> Button msg
withIconLeft icon (Settings settings) =
    Settings { settings | iconLeft = Just icon }


withIconRight : Icon -> Button msg -> Button msg
withIconRight icon (Settings settings) =
    Settings { settings | iconRight = Just icon }


withDisabled : Bool -> Button msg -> Button msg
withDisabled isDisabled (Settings settings) =
    Settings { settings | isDisabled = isDisabled }



-- VIEW


view : Button msg -> Html msg
view (Settings settings) =
    let
        viewOptionalIcon : Maybe Icon -> Html msg
        viewOptionalIcon maybeIcon =
            case maybeIcon of
                Just icon ->
                    Components.Icon.view icon

                Nothing ->
                    text ""
    in
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
        , Events.onClick settings.onClick
        , class "button"
        , classList
            [ ( "is-success", settings.style == Success )
            , ( "is-warning", settings.style == Warning )
            , ( "is-danger", settings.style == Danger )
            , ( "is-small", settings.size == Small )
            ]
        , disabled settings.isDisabled
        ]
        [ viewOptionalIcon settings.iconLeft
        , text settings.label
        , viewOptionalIcon settings.iconRight
        ]
