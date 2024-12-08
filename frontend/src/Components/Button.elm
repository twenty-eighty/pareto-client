module Components.Button exposing
    ( Button, new
    , view
    , withTypePrimary, withTypeSecondary
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
import Ui.Styles
import Html

-- SETTINGS


type Button msg
    = Settings
        { label : String
        , onClick : msg
        , style : Style
        , size : Size
        , type_ : ButtonType
        , iconLeft : Maybe Icon
        , iconRight : Maybe Icon
        , isOutlined : Bool
        , isDisabled : Bool
        , theme : Ui.Styles.Theme
        }


new : { label : String, onClick : msg, theme : Ui.Styles.Theme } -> Button msg
new props =
    Settings
        { label = props.label
        , onClick = props.onClick
        , style = Default
        , size = Normal
        , type_ = RegularButton
        , iconLeft = Nothing
        , iconRight = Nothing
        , isOutlined = False
        , isDisabled = False
        , theme = props.theme
        }



-- MODIFIERS

type ButtonType
    = RegularButton
    | PrimaryButton
    | SecondaryButton


withTypePrimary : Button msg -> Button msg
withTypePrimary (Settings settings) =
    Settings { settings | type_ = PrimaryButton }


withTypeSecondary : Button msg -> Button msg
withTypeSecondary (Settings settings) =
    Settings { settings | type_ = SecondaryButton }



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
        buttonStyles =
            stylesForTheme (Settings settings)

        viewOptionalIcon : Maybe Icon -> Html msg
        viewOptionalIcon maybeIcon =
            case maybeIcon of
                Just icon ->
                    Components.Icon.view icon

                Nothing ->
                    text ""
    in
    div
        [ css
            [ Tw.flex
            , Tw.flex_row
            , Tw.gap_2
            ]
        ]
        [ button
            ( buttonStyles ++
            [ css
                [ Tw.py_2
                , Tw.px_4
                , Tw.flex
                , Tw.flex_row
                , Tw.gap_2
                , Tw.rounded_full
                , Css.hover
                    [ 
                    ]
                ]
            , Events.onClick settings.onClick
            , classList
                [ ( "is-success", settings.style == Success )
                , ( "is-warning", settings.style == Warning )
                , ( "is-danger", settings.style == Danger )
                , ( "is-small", settings.size == Small )
                ]
            , disabled settings.isDisabled
            ])
            [ viewOptionalIcon settings.iconLeft
            , text settings.label
            , viewOptionalIcon settings.iconRight
            ]
        ]

stylesForTheme : Button msg -> List (Attribute msg)
stylesForTheme (Settings settings) =
    let
        styles =
            Ui.Styles.stylesForTheme settings.theme

        (foregroundStyles, backgroundStyles) =
            case settings.type_ of
                RegularButton ->
                    if settings.isDisabled then
                        (styles.colorStyleDisabledButtonText, styles.colorStyleDisabledButtonBackground)
                    else
                        (styles.colorStyleRegularButtonText, styles.colorStyleRegularButtonBackground)

                PrimaryButton ->
                    if settings.isDisabled then
                        (styles.colorStyleDisabledButtonText, styles.colorStyleDisabledButtonBackground)
                    else
                        (styles.colorStylePrimaryButtonText, styles.colorStylePrimaryButtonBackground)

                SecondaryButton ->
                    if settings.isDisabled then
                        (styles.colorStyleDisabledButtonText, styles.colorStyleDisabledButtonBackground)
                    else
                        (styles.colorStyleSecondaryButtonText, styles.colorStyleSecondaryButtonBackground)

        attributes =
            foregroundStyles ++ backgroundStyles
    in
    attributes