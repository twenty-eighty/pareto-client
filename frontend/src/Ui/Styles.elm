module Ui.Styles exposing (..)

import Css
import Css.Media
import Html.Styled as Html
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events exposing (..)
import Tailwind.Color as TwColor
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw


fontFamilyInter : Html.Attribute msg
fontFamilyInter =
    Attr.style "font-family" "Inter, sans-serif"


fontFamilyRobotoMono : Html.Attribute msg
fontFamilyRobotoMono =
    Attr.style "font-family" "Roboto Mono, monospaced"


fontFamilyUnbounded : Html.Attribute msg
fontFamilyUnbounded =
    Attr.style "font-family" "Unbounded"


fontFamilySourceSerifPro : Html.Attribute msg
fontFamilySourceSerifPro =
    Attr.style "font-family" "Source Serif Pro"


dummyTheme : Theme
dummyTheme =
    ParetoTheme


type Theme
    = ParetoTheme
    | CustomTheme CustomThemeParams


type alias CustomThemeParams =
    { colorB1 : Theme.Color
    , colorB1DarkMode : Theme.Color
    , colorB2 : Theme.Color
    , colorB2DarkMode : Theme.Color
    , colorB3 : Theme.Color
    , colorB3DarkMode : Theme.Color
    , colorB4 : Theme.Color
    , colorB4DarkMode : Theme.Color
    , colorB5 : Theme.Color
    , colorB5DarkMode : Theme.Color
    , colorG1 : Theme.Color
    , colorG1DarkMode : Theme.Color
    , colorG2 : Theme.Color
    , colorG2DarkMode : Theme.Color
    , colorG3 : Theme.Color
    , colorG3DarkMode : Theme.Color
    , colorG4 : Theme.Color
    , colorG4DarkMode : Theme.Color
    , colorG5 : Theme.Color
    , colorG5DarkMode : Theme.Color
    }


type alias StyleBundle msg =
    List (Html.Attribute msg)


stylesForTheme : Theme -> Styles msg
stylesForTheme theme =
    case theme of
        ParetoTheme ->
            paretoThemeStyles

        CustomTheme customThemeParams ->
            customThemeStyles customThemeParams


mapStyleBundle : (msg1 -> msg2) -> StyleBundle msg1 -> StyleBundle msg2
mapStyleBundle toMsg styles =
    styles
        |> List.map (\style -> Attr.map toMsg style)


map : (msg1 -> msg2) -> Styles msg1 -> Styles msg2
map toMsg props =
    { colorB1 = props.colorB1
    , colorB1DarkMode = props.colorB1DarkMode
    , colorB2 = props.colorB2
    , colorB2DarkMode = props.colorB1DarkMode
    , colorB3 = props.colorB3
    , colorB3DarkMode = props.colorB1DarkMode
    , colorB4 = props.colorB4
    , colorB4DarkMode = props.colorB1DarkMode
    , colorB5 = props.colorB5
    , colorB5DarkMode = props.colorB5DarkMode
    , colorG1 = props.colorG1
    , colorG1DarkMode = props.colorG1DarkMode
    , colorG2 = props.colorG2
    , colorG2DarkMode = props.colorG1DarkMode
    , colorG3 = props.colorG3
    , colorG3DarkMode = props.colorG1DarkMode
    , colorG4 = props.colorG4
    , colorG4DarkMode = props.colorG1DarkMode
    , colorG5 = props.colorG5
    , colorG5DarkMode = props.colorG5DarkMode
    , textStyleLinks = mapStyleBundle toMsg props.textStyleLinks
    , textStyleBody = mapStyleBundle toMsg props.textStyleBody
    , textStyleSemiboldLabel = mapStyleBundle toMsg props.textStyleSemiboldLabel
    , textStyleUppercaseLabel = mapStyleBundle toMsg props.textStyleUppercaseLabel
    , textStyleH1 = mapStyleBundle toMsg props.textStyleH1
    , textStyleH1Article = mapStyleBundle toMsg props.textStyleH1Article
    , textStyleH2 = mapStyleBundle toMsg props.textStyleH2
    , textStyleH3 = mapStyleBundle toMsg props.textStyleH3
    , textStyleH4 = mapStyleBundle toMsg props.textStyleH4
    , textStyleH4Article = mapStyleBundle toMsg props.textStyleH4Article
    , textStyle14 = mapStyleBundle toMsg props.textStyle14
    , colorStyleBackground = mapStyleBundle toMsg props.colorStyleBackground
    , colorStyleLabel = mapStyleBundle toMsg props.colorStyleLabel
    , colorStyleLinks = mapStyleBundle toMsg props.colorStyleLinks
    , colorStyleMedia = mapStyleBundle toMsg props.colorStyleMedia
    , colorStyleBorders = mapStyleBundle toMsg props.colorStyleBorders
    , colorStyleIcons = mapStyleBundle toMsg props.colorStyleIcons
    , colorStyleCover = mapStyleBundle toMsg props.colorStyleCover
    , colorStyleGrayscaleTitle = mapStyleBundle toMsg props.colorStyleGrayscaleTitle
    , colorStyleGrayscaleSummary = mapStyleBundle toMsg props.colorStyleGrayscaleSummary
    , colorStyleGrayscaleMuted = mapStyleBundle toMsg props.colorStyleGrayscaleMuted
    , colorStyleGrayscaleText = mapStyleBundle toMsg props.colorStyleGrayscaleText
    , colorStyleGrayscaleDisabled = mapStyleBundle toMsg props.colorStyleGrayscaleDisabled
    , colorStyleCode = mapStyleBundle toMsg props.colorStyleCode
    }


type alias Styles msg =
    { colorB1 : Theme.Color
    , colorB1DarkMode : Theme.Color
    , colorB2 : Theme.Color
    , colorB2DarkMode : Theme.Color
    , colorB3 : Theme.Color
    , colorB3DarkMode : Theme.Color
    , colorB4 : Theme.Color
    , colorB4DarkMode : Theme.Color
    , colorB5 : Theme.Color
    , colorB5DarkMode : Theme.Color
    , colorG1 : Theme.Color
    , colorG1DarkMode : Theme.Color
    , colorG2 : Theme.Color
    , colorG2DarkMode : Theme.Color
    , colorG3 : Theme.Color
    , colorG3DarkMode : Theme.Color
    , colorG4 : Theme.Color
    , colorG4DarkMode : Theme.Color
    , colorG5 : Theme.Color
    , colorG5DarkMode : Theme.Color
    , textStyleLinks : StyleBundle msg
    , textStyleBody : StyleBundle msg
    , textStyleSemiboldLabel : StyleBundle msg
    , textStyleUppercaseLabel : StyleBundle msg
    , textStyleH1 : StyleBundle msg
    , textStyleH1Article : StyleBundle msg
    , textStyleH2 : StyleBundle msg
    , textStyleH3 : StyleBundle msg
    , textStyleH4 : StyleBundle msg
    , textStyleH4Article : StyleBundle msg
    , textStyle14 : StyleBundle msg
    , colorStyleBackground : StyleBundle msg
    , colorStyleLabel : StyleBundle msg
    , colorStyleLinks : StyleBundle msg
    , colorStyleMedia : StyleBundle msg
    , colorStyleBorders : StyleBundle msg
    , colorStyleIcons : StyleBundle msg
    , colorStyleCover : StyleBundle msg
    , colorStyleGrayscaleTitle : StyleBundle msg
    , colorStyleGrayscaleSummary : StyleBundle msg
    , colorStyleGrayscaleMuted : StyleBundle msg
    , colorStyleGrayscaleText : StyleBundle msg
    , colorStyleGrayscaleDisabled : StyleBundle msg
    , colorStyleCode : StyleBundle msg
    }


paretoThemeStyles : Styles msg
paretoThemeStyles =
    let
        colorB1 =
            TwColor.arbitraryRgb 203 213 225

        colorB1Inverse =
            colorB5

        colorB2 =
            TwColor.arbitraryRgb 148 163 184

        colorB2Inverse =
            colorB4

        colorB3 =
            TwColor.arbitraryRgb 100 116 139

        colorB3Inverse =
            colorB3

        colorB4 =
            TwColor.arbitraryRgb 51 65 85

        colorB4Inverse =
            colorB2

        colorB5 =
            TwColor.arbitraryRgb 23 36 52

        colorB5Inverse =
            colorB1

        colorG1 =
            TwColor.arbitraryRgb 230 239 241

        colorG1Inverse =
            colorG5

        colorG2 =
            TwColor.arbitraryRgb 190 209 208

        colorG2Inverse =
            colorG4

        colorG3 =
            TwColor.arbitraryRgb 140 160 157

        colorG3Inverse =
            colorG3

        colorG4 =
            TwColor.arbitraryRgb 72 90 90

        colorG4Inverse =
            colorG2

        colorG5 =
            TwColor.arbitraryRgb 36 50 52

        colorG5Inverse =
            colorG1
    in
    customThemeStyles
        { colorB1 = colorB1
        , colorB1DarkMode = colorB1Inverse
        , colorB2 = colorB2
        , colorB2DarkMode = colorB2Inverse
        , colorB3 = colorB3
        , colorB3DarkMode = colorB3Inverse
        , colorB4 = colorB4
        , colorB4DarkMode = colorB4Inverse
        , colorB5 = colorB5
        , colorB5DarkMode = colorB5Inverse
        , colorG1 = colorG1
        , colorG1DarkMode = colorG1Inverse
        , colorG2 = colorG2
        , colorG2DarkMode = colorG2Inverse
        , colorG3 = colorG3
        , colorG3DarkMode = colorG3Inverse
        , colorG4 = colorG4
        , colorG4DarkMode = colorG4Inverse
        , colorG5 = colorG5
        , colorG5DarkMode = colorG5Inverse
        }


customThemeStyles : CustomThemeParams -> Styles msg
customThemeStyles { colorB1, colorB1DarkMode, colorB2, colorB2DarkMode, colorB3, colorB3DarkMode, colorB4, colorB4DarkMode, colorB5, colorB5DarkMode, colorG1, colorG1DarkMode, colorG2, colorG2DarkMode, colorG3, colorG3DarkMode, colorG4, colorG4DarkMode, colorG5, colorG5DarkMode } =
    { colorB1 = colorB1
    , colorB1DarkMode = colorB1DarkMode
    , colorB2 = colorB2
    , colorB2DarkMode = colorB2DarkMode
    , colorB3 = colorB3
    , colorB3DarkMode = colorB3DarkMode
    , colorB4 = colorB4
    , colorB4DarkMode = colorB4DarkMode
    , colorB5 = colorB5
    , colorB5DarkMode = colorB5DarkMode
    , colorG1 = colorG1
    , colorG1DarkMode = colorG1DarkMode
    , colorG2 = colorG2
    , colorG2DarkMode = colorG2DarkMode
    , colorG3 = colorG3
    , colorG3DarkMode = colorG3DarkMode
    , colorG4 = colorG4
    , colorG4DarkMode = colorG4DarkMode
    , colorG5 = colorG5
    , colorG5DarkMode = colorG5DarkMode
    , textStyleLinks =
        [ css
            [ Tw.text_base
            , Tw.font_medium
            , Tw.leading_6
            , Tw.tracking_normal
            , Tw.decoration_auto
            , Tw.underline
            ]
        , fontFamilyInter
        ]
    , textStyleBody =
        [ css
            [ Tw.text_base
            , Tw.font_medium
            , Tw.leading_6
            , Tw.tracking_normal

            -- , Tw.leading_relaxed
            ]
        , fontFamilyInter
        ]
    , textStyleSemiboldLabel =
        [ css
            [ Tw.text_base
            , Tw.font_semibold
            , Tw.leading_6
            , Tw.tracking_normal
            ]
        , fontFamilyInter
        ]
    , textStyleUppercaseLabel =
        [ css
            [ Tw.text_sm
            , Tw.font_semibold
            , Tw.tracking_wide
            ]
        , fontFamilyInter
        , Attr.style "line-height" "auto"
        ]
    , textStyleH1 =
        [ css
            [ Tw.text_3xl
            , Tw.font_semibold
            , Tw.tracking_tight
            ]
        , fontFamilyInter
        , Attr.style "line-height" "auto"
        ]
    , textStyleH1Article =
        [ css
            [ Tw.text_5xl
            , Tw.font_bold
            , Tw.leading_tight
            , Tw.hyphens_auto
            ]
        , fontFamilyInter
        , Attr.style "line-height" "auto"
        ]
    , textStyleH2 =
        [ css
            [ Tw.text_2xl
            , Tw.font_medium
            , Tw.tracking_tight
            ]
        , fontFamilyInter
        , Attr.style "line-height" "auto"
        ]
    , textStyleH3 =
        [ css
            [ Tw.text_xl
            , Tw.font_semibold
            , Tw.tracking_tight
            ]
        , fontFamilyInter
        , Attr.style "line-height" "auto"
        ]
    , textStyleH4 =
        [ css
            [ Tw.text_lg
            , Tw.font_semibold
            ]
        , fontFamilyInter
        ]
    , textStyleH4Article =
        [ css
            [ Tw.text_lg
            , Tw.font_normal
            , Tw.leading_7
            ]
        , fontFamilyInter
        ]
    , textStyle14 =
        [ css
            [ Tw.text_sm
            , Tw.font_normal
            , Tw.leading_5
            ]
        , fontFamilyInter
        ]
    , colorStyleBackground =
        [ css
            [ Tw.bg_color Theme.white
            , darkMode
                [ Tw.bg_color colorB1DarkMode
                ]
            ]
        ]
    , colorStyleLabel =
        [ css
            [ Tw.text_color colorB2
            , darkMode
                [ Tw.text_color colorB2DarkMode
                ]
            ]
        ]
    , colorStyleLinks =
        [ css
            [ Tw.text_color colorB4
            , darkMode
                [ Tw.text_color colorB4DarkMode ]
            ]
        ]
    , colorStyleMedia =
        [ css
            [ Tw.text_color colorB2
            , darkMode
                [ Tw.text_color colorB2DarkMode
                ]
            ]
        ]
    , colorStyleBorders =
        [ css
            [ Tw.border_color colorB4
            , darkMode
                [ Tw.border_color colorB4DarkMode
                ]
            ]
        ]
    , colorStyleIcons =
        [ css
            [ Tw.text_color colorB3
            , darkMode
                [ Tw.text_color colorB3DarkMode
                ]
            ]
        ]
    , colorStyleCover =
        [ css
            [ Tw.text_opacity_70
            , darkMode
                []
            ]
        ]
    , colorStyleGrayscaleTitle =
        [ css
            [ Tw.text_color colorB4
            , darkMode
                [ Tw.text_color colorB4DarkMode
                ]
            ]
        ]
    , colorStyleGrayscaleSummary =
        [ css
            [ Tw.text_color colorB4
            , darkMode
                [ Tw.text_color colorB4DarkMode
                ]
            ]
        ]
    , colorStyleGrayscaleMuted =
        [ css
            [ Tw.text_color colorB3
            , darkMode
                [ Tw.text_color colorB3DarkMode
                ]
            ]
        ]
    , colorStyleGrayscaleText =
        [ css
            [ Tw.text_color colorB4
            , darkMode
                [ Tw.text_color colorB4DarkMode
                ]
            ]
        ]
    , colorStyleGrayscaleDisabled =
        [ css
            [ Tw.text_color colorB2
            , darkMode
                [ Tw.text_color colorB2DarkMode
                ]
            ]
        ]
    , colorStyleCode =
        [ css
            [ Tw.text_color colorB4
            , darkMode
                [ Tw.text_color colorB4DarkMode
                ]
            ]
        ]
    }


darkMode : List Css.Style -> Css.Style
darkMode =
    Css.Media.withMediaQuery [ "(prefers-color-scheme: dark)" ]


print : List Css.Style -> Css.Style
print =
    Css.Media.withMediaQuery [ "print" ]
