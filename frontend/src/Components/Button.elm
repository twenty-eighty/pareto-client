module Components.Button exposing
    ( Button, new
    , view
    , withStyleSuccess, withStyleWarning, withStyleDanger
    , withSizeSmall
    , withContentLeft, withContentRight
    , withIconLeft, withIconRight
    , withDisabled
    , withHidden, withLink, withNewTabLink, withTypePrimary, withTypeSecondary
    , withIntermediateState
    )

{-|


## Basic usage

@docs Button, new
@docs view


## Modifiers

@docs withStyleSuccess, withStyleWarning, withStyleDanger
@docs withSizeSmall
@docs withContentLeft, withContentRight
@docs withIconLeft, withIconRight
@docs withDisabled

-}

import Components.Icon exposing (Icon)
import Css
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Events
import Tailwind.Utilities as Tw
import Ui.Shared exposing (emptyHtml)
import Ui.Styles exposing (darkMode)
import Svg.Loaders



-- SETTINGS


type Button msg
    = Settings
        { label : String
        , action : Action msg
        , style : Style
        , size : Size
        , type_ : ButtonType
        , contentLeft : Content msg
        , contentRight : Content msg
        , isOutlined : Bool
        , isDisabled : Bool
        , isHidden : Bool
        , isInIntermediateState : Bool
        , theme : Ui.Styles.Theme
        }


new : { label : String, onClick : Maybe msg, theme : Ui.Styles.Theme } -> Button msg
new props =
    Settings
        { label = props.label
        , action = props.onClick |> Maybe.map OnClick |> Maybe.withDefault NoOp
        , style = Default
        , size = Normal
        , type_ = RegularButton
        , contentLeft = NoContent
        , contentRight = NoContent
        , isOutlined = False
        , isDisabled = False
        , isHidden = False
        , isInIntermediateState = False
        , theme = props.theme
        }

type Content msg
    = ContentIcon Icon
    | ContentHtml (Html msg)
    | NoContent

type Action msg
    = OnClick msg
    | Link String
    | NewTabLink String
    | NoOp


-- MODIFIERS


withLink : Maybe String -> Button msg -> Button msg
withLink url (Settings settings) =
    Settings { settings | action =  url |> Maybe.map Link |> Maybe.withDefault NoOp }


withNewTabLink : String -> Button msg -> Button msg
withNewTabLink url (Settings settings) =
    Settings { settings | action = NewTabLink url }


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
    Settings { settings | contentLeft = ContentIcon icon }

withContentLeft : Html msg -> Button msg -> Button msg
withContentLeft content (Settings settings) =
    Settings { settings | contentLeft = ContentHtml content }

withIconRight : Icon -> Button msg -> Button msg
withIconRight icon (Settings settings) =
    Settings { settings | contentRight = ContentIcon icon }

withContentRight : Html msg -> Button msg -> Button msg
withContentRight content (Settings settings) =
    Settings { settings | contentRight = ContentHtml content }


withDisabled : Bool -> Button msg -> Button msg
withDisabled isDisabled (Settings settings) =
    Settings { settings | isDisabled = isDisabled }


withHidden : Bool -> Button msg -> Button msg
withHidden isHidden (Settings settings) =
    Settings { settings | isHidden = isHidden }


withIntermediateState : Bool -> Button msg -> Button msg
withIntermediateState isIntermediateState (Settings settings) =
    Settings { settings | isInIntermediateState = isIntermediateState }



-- VIEW


view : Button msg -> Html msg
view (Settings settings) =
    let
        buttonStyles =
            stylesForTheme (Settings settings)

        viewOptionalContent : Content msg -> Html msg
        viewOptionalContent content =
            case content of
                ContentIcon icon ->
                    Components.Icon.view icon

                ContentHtml html ->
                    html

                NoContent ->
                    text ""

        viewIntermediateStateIndicator : Html msg
        viewIntermediateStateIndicator =
            if settings.isInIntermediateState then
                div [ Attr.css [ Tw.flex, Tw.items_center, Tw.justify_center ] ]
                    [ Svg.Loaders.puff [ Svg.Loaders.size 16, Svg.Loaders.color "currentColor" ]
                        |> Html.fromUnstyled
                    ]

            else
                emptyHtml

        ( element, onClickAttr ) =
            case ( settings.isDisabled || settings.isInIntermediateState, settings.action ) of
                ( False, OnClick onClick ) ->
                    ( button, [ Events.onClick onClick ] )

                ( False, Link link ) ->
                    ( a, [ Attr.href link ] )

                ( False, NewTabLink url ) ->
                    ( a, [ Attr.target "_blank", Attr.href url ] )

                ( _, _) ->
                    ( div, [ Attr.disabled True ] )
    in
    if not settings.isHidden then
        div
            [ Attr.css
                [ Tw.flex
                , Tw.flex_row
                , Tw.gap_2
                ]
            ]
            [ element
                (buttonStyles
                    ++ onClickAttr
                    ++ [ Attr.css
                            [ Tw.py_2
                            , Tw.px_4
                            , Tw.flex
                            , Tw.flex_row
                            , Tw.gap_2
                            , Tw.rounded_full
                            , Css.hover
                                []
                            ]
                       , Attr.classList
                            [ ( "is-success", settings.style == Success )
                            , ( "is-warning", settings.style == Warning )
                            , ( "is-danger", settings.style == Danger )
                            , ( "is-small", settings.size == Small )
                            ]
                       ]
                )
                [ viewOptionalContent settings.contentLeft
                , text settings.label
                , viewOptionalContent settings.contentRight
                , viewIntermediateStateIndicator 
                ]
            ]

    else
        emptyHtml


stylesForTheme : Button msg -> List (Attribute msg)
stylesForTheme (Settings settings) =
    let
        styles =
            Ui.Styles.stylesForTheme settings.theme

        -- TODO: use style (Default | Success | Warning | Danger)
        ( foregroundStyles, backgroundStyles ) =
            case settings.type_ of
                RegularButton ->
                    if settings.isDisabled then
                        ( [ Tw.text_color styles.color3, darkMode [ Tw.text_color styles.color3DarkMode ] ]
                        , [ Tw.bg_color styles.color2, darkMode [ Tw.bg_color styles.color2DarkMode ] ]
                        )

                    else
                        ( [ Tw.text_color styles.color1, darkMode [ Tw.text_color styles.color1DarkMode ] ]
                        , [ Tw.bg_color styles.color4
                          , Tw.border_color styles.color1
                          , darkMode [ Tw.bg_color styles.color4DarkMode, Tw.border_color styles.color1DarkMode ]
                          ]
                        )

                PrimaryButton ->
                    if settings.isDisabled then
                        ( [ Tw.text_color styles.color3, darkMode [ Tw.text_color styles.color3DarkMode ] ]
                        , [ Tw.bg_color styles.color2, darkMode [ Tw.bg_color styles.color2DarkMode ] ]
                        )

                    else
                        ( [ Tw.text_color styles.color1, darkMode [ Tw.text_color styles.color1DarkMode ] ]
                        , [ Tw.bg_color styles.color4, darkMode [ Tw.bg_color styles.color4DarkMode ] ]
                        )

                SecondaryButton ->
                    if settings.isDisabled then
                        ( [ Tw.text_color styles.color3, darkMode [ Tw.text_color styles.color3DarkMode ] ]
                        , [ Tw.bg_color styles.color2, darkMode [ Tw.bg_color styles.color2DarkMode ] ]
                        )

                    else
                        ( [ Tw.text_color styles.color1, darkMode [ Tw.text_color styles.color1DarkMode ] ]
                        , [ Tw.bg_color styles.color3, darkMode [ Tw.bg_color styles.color3DarkMode ] ]
                        )
    in
    [ Attr.css (foregroundStyles ++ backgroundStyles) ]
