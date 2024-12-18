module Components.SearchBar exposing
    ( SearchBar, new
    , view
    , withLink
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


type SearchBar msg
    = Settings
        { label : String
        , onClick : Maybe msg
        , link : Maybe String
        , style : Style
        , size : Size
        , type_ : ButtonType
        , iconLeft : Maybe Icon
        , iconRight : Maybe Icon
        , isOutlined : Bool
        , isDisabled : Bool
        , theme : Ui.Styles.Theme
        }


new : { label : String, onClick : Maybe msg, theme : Ui.Styles.Theme } -> SearchBar msg
new props =
    Settings
        { label = props.label
        , onClick = props.onClick
        , link = Nothing
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

withLink : Maybe String -> SearchBar msg -> SearchBar msg
withLink link (Settings settings) =
    Settings { settings | link = link }


type ButtonType
    = RegularButton
    | PrimaryButton
    | SecondaryButton


withTypePrimary : SearchBar msg -> SearchBar msg
withTypePrimary (Settings settings) =
    Settings { settings | type_ = PrimaryButton }


withTypeSecondary : SearchBar msg -> SearchBar msg
withTypeSecondary (Settings settings) =
    Settings { settings | type_ = SecondaryButton }



type Style
    = Default
    | Success
    | Warning
    | Danger


withStyleSuccess : SearchBar msg -> SearchBar msg
withStyleSuccess (Settings settings) =
    Settings { settings | style = Success }


withStyleWarning : SearchBar msg -> SearchBar msg
withStyleWarning (Settings settings) =
    Settings { settings | style = Warning }


withStyleDanger : SearchBar msg -> SearchBar msg
withStyleDanger (Settings settings) =
    Settings { settings | style = Danger }


type Size
    = Normal
    | Small


withSizeSmall : SearchBar msg -> SearchBar msg
withSizeSmall (Settings settings) =
    Settings { settings | size = Small }


withIconLeft : Icon -> SearchBar msg -> SearchBar msg
withIconLeft icon (Settings settings) =
    Settings { settings | iconLeft = Just icon }


withIconRight : Icon -> SearchBar msg -> SearchBar msg
withIconRight icon (Settings settings) =
    Settings { settings | iconRight = Just icon }


withDisabled : Bool -> SearchBar msg -> SearchBar msg
withDisabled isDisabled (Settings settings) =
    Settings { settings | isDisabled = isDisabled }



-- VIEW


view : SearchBar msg -> Html msg
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

        (element, onClickAttr) =
            case (settings.isDisabled, settings.onClick, settings.link) of
                (False, Just onClick, _) ->
                    (button, [ Events.onClick onClick ])

                (False, Nothing, Just link) ->
                    (a, [ href link ])

                (_, _, _) ->
                    (div, [ disabled True ])
    in
    div
        [ css
            [ Tw.flex
            , Tw.flex_row
            , Tw.gap_2
            ]
        ]
        [ element
            ( buttonStyles ++ onClickAttr ++
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
            , classList
                [ ( "is-success", settings.style == Success )
                , ( "is-warning", settings.style == Warning )
                , ( "is-danger", settings.style == Danger )
                , ( "is-small", settings.size == Small )
                ]
            ])
            [ viewOptionalIcon settings.iconLeft
            , text settings.label
            , viewOptionalIcon settings.iconRight
            ]
        ]

stylesForTheme : SearchBar msg -> List (Attribute msg)
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