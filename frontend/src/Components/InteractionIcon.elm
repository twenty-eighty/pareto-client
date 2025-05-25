module Components.InteractionIcon exposing
    ( InteractionIcon, new
    , map
    , view
    , withAttributes
    )

{-|


## Basic usage

@docs InteractionIcon, new
@docs view

-}

import Components.Icon as Icon exposing (Icon)
import Css
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Events
import Svg.Styled as Svg
import Svg.Styled.Attributes as SvgAttr
import Tailwind.Utilities as Tw
import Ui.Shared exposing (emptyHtml)
import Ui.Styles



-- SETTINGS


type InteractionIcon msg
    = Settings
        { icon : Icon
        , actionInProgress : Bool
        , attributes : List (String, String)
        , onClick : Maybe msg
        , theme : Ui.Styles.Theme
        }


new : { icon : Icon, actionInProgress : Bool, onClick : Maybe msg, theme : Ui.Styles.Theme } -> InteractionIcon msg
new props =
    Settings
        { icon = props.icon
        , actionInProgress = props.actionInProgress
        , attributes = []
        , onClick = props.onClick
        , theme = props.theme
        }

map : (msg1 -> msg2) -> InteractionIcon msg1 -> InteractionIcon msg2
map toMsg (Settings settings) =
    Settings
        { icon = settings.icon
        , actionInProgress = settings.actionInProgress
        , attributes = settings.attributes
        , onClick = Maybe.map toMsg settings.onClick
        , theme = settings.theme
        }

withAttributes : List (String, String) -> InteractionIcon msg -> InteractionIcon msg
withAttributes attributes (Settings settings) =
    Settings { settings | attributes = attributes }


-- VIEW


view : InteractionIcon msg -> Html msg
view (Settings settings) =
    let
        viewProcessingIndicator : Html msg
        viewProcessingIndicator =
            if settings.actionInProgress then
                div [ Attr.css [ Tw.absolute, Tw.flex, Tw.items_center, Tw.justify_center ] ]
                    [ processingIndicator 34
                    ]

            else
                emptyHtml

        ( element, onClickAttr ) =
            case ( settings.actionInProgress, settings.onClick ) of
                ( True, _ ) ->
                    ( div, [] )

                ( False, Just onClick ) ->
                    ( button, [ Events.onClick onClick ] )

                ( False, Nothing ) ->
                    ( div, [] )

        attributes =
            settings.attributes
                |> List.map (\(key, value) -> (Attr.attribute key value))
        in
        div
            [ Attr.css
                [ Tw.flex
                , Tw.flex_row
                , Tw.py_2
                ]
            ]
            [ viewProcessingIndicator
            , element
                (attributes ++ onClickAttr
                    ++ [ Attr.css
                            [ Tw.py_2
                            , Tw.px_2
                            , Tw.flex
                            , Tw.flex_row
                            , Tw.gap_2
                            , Tw.rounded_full
                            , Css.hover
                                []
                            ]
                       ]
                )
                [ settings.icon
                    |> Icon.viewWithSize 20
                ]
            ]


processingIndicator : Int -> Html msg
processingIndicator size =
    Svg.svg
        [ SvgAttr.width (String.fromInt size)
        , SvgAttr.height (String.fromInt size)
        , SvgAttr.viewBox "0 0 48 48"
        ]
        [ Svg.circle
            [ SvgAttr.cx "24"
            , SvgAttr.cy "24"
            , SvgAttr.r "20"
            , SvgAttr.stroke "#888"
            , SvgAttr.strokeWidth "3"
            , SvgAttr.fill "none"
            , SvgAttr.strokeLinecap "round"
            , SvgAttr.strokeDasharray "100"
            , SvgAttr.strokeDashoffset "75"
            ]
            [ Svg.animateTransform
                [ SvgAttr.attributeName "transform"
                , SvgAttr.type_ "rotate"
                , SvgAttr.dur "0.8s"
                , SvgAttr.repeatCount "indefinite"
                , SvgAttr.from "0 24 24"
                , SvgAttr.to "360 24 24"
                ]
                []
            ]
        ]
    