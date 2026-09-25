module Components.InteractionIcon exposing
    ( InteractionIcon, new
    , map
    , view
    , withAttributes
    , withCompact
    , withTestAttribute
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
        , compact : Bool
        , onClick : Maybe msg
        , testAttribute : String
        , theme : Ui.Styles.Theme
        }


new : { icon : Icon, actionInProgress : Bool, onClick : Maybe msg, theme : Ui.Styles.Theme } -> InteractionIcon msg
new props =
    Settings
        { icon = props.icon
        , actionInProgress = props.actionInProgress
        , attributes = []
        , compact = False
        , onClick = props.onClick
        , testAttribute = "unnamed"
        , theme = props.theme
        }

map : (msg1 -> msg2) -> InteractionIcon msg1 -> InteractionIcon msg2
map toMsg (Settings settings) =
    Settings
        { icon = settings.icon
        , actionInProgress = settings.actionInProgress
        , attributes = settings.attributes
        , compact = settings.compact
        , onClick = Maybe.map toMsg settings.onClick
        , testAttribute = settings.testAttribute
        , theme = settings.theme
        }

withAttributes : List (String, String) -> InteractionIcon msg -> InteractionIcon msg
withAttributes attributes (Settings settings) =
    Settings { settings | attributes = attributes }


withCompact : InteractionIcon msg -> InteractionIcon msg
withCompact (Settings settings) =
    Settings { settings | compact = True }



withTestAttribute : String -> InteractionIcon msg -> InteractionIcon msg
withTestAttribute testAttribute (Settings settings) =
    Settings { settings | testAttribute = testAttribute }


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

        buttonStyles =
            if settings.compact then
                [ Tw.py_1
                , Tw.pl_1
                , Tw.pr_0
                , Tw.cursor_pointer
                , Tw.flex
                , Tw.flex_row
                , Tw.rounded_full
                ]

            else
                [ Tw.py_2
                , Tw.px_2
                , Tw.flex
                , Tw.flex_row
                , Tw.gap_2
                , Tw.rounded_full
                , Css.hover
                    []
                ]

        iconSize =
            if settings.compact then
                16

            else
                20
        in
        div
            [ Attr.css
                [ Tw.flex
                , Tw.flex_row
                ]
            ]
            [ viewProcessingIndicator
            , element
                ([ Attr.attribute "data-test" ("interaction-icon-" ++ settings.testAttribute) ]
                    ++ attributes
                    ++ onClickAttr
                    ++ [ Attr.css buttonStyles ]
                )
                [ settings.icon
                    |> Icon.viewWithSize iconSize
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
    