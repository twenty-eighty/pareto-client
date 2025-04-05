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


type Theme
    = ParetoTheme


defaultTheme : Theme
defaultTheme =
    ParetoTheme


type alias StyleBundle msg =
    List (Html.Attribute msg)


stylesForTheme : Theme -> Styles msg
stylesForTheme theme =
    case theme of
        ParetoTheme ->
            paretoThemeStyles


mapStyleBundle : (msg1 -> msg2) -> StyleBundle msg1 -> StyleBundle msg2
mapStyleBundle toMsg styles =
    styles
        |> List.map (\style -> Attr.map toMsg style)


map : (msg1 -> msg2) -> Styles msg1 -> Styles msg2
map toMsg props =
    { color1 = props.color1
    , color1DarkMode = props.color1DarkMode
    , color2 = props.color2
    , color2DarkMode = props.color1DarkMode
    , color3 = props.color3
    , color3DarkMode = props.color1DarkMode
    , color4 = props.color4
    , color4DarkMode = props.color1DarkMode
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
    , colorStyleGrayscaleMedia = mapStyleBundle toMsg props.colorStyleGrayscaleMedia
    , colorStyleGrayscaleDisabled = mapStyleBundle toMsg props.colorStyleGrayscaleDisabled
    , colorStyleCode = mapStyleBundle toMsg props.colorStyleCode
    , colorStyleRegularButtonText = mapStyleBundle toMsg props.colorStyleRegularButtonText
    , colorStylePrimaryButtonText = mapStyleBundle toMsg props.colorStylePrimaryButtonText
    , colorStyleSecondaryButtonText = mapStyleBundle toMsg props.colorStyleSecondaryButtonText
    , colorStyleDisabledButtonText = mapStyleBundle toMsg props.colorStyleDisabledButtonText
    , colorStyleRegularButtonBackground = mapStyleBundle toMsg props.colorStyleRegularButtonBackground
    , colorStylePrimaryButtonBackground = mapStyleBundle toMsg props.colorStylePrimaryButtonBackground
    , colorStyleSecondaryButtonBackground = mapStyleBundle toMsg props.colorStyleSecondaryButtonBackground
    , colorStyleDisabledButtonBackground = mapStyleBundle toMsg props.colorStyleDisabledButtonBackground
    , colorStyleInverse = mapStyleBundle toMsg props.colorStyleInverse
    , effectStyleModalShadow = mapStyleBundle toMsg props.effectStyleModalShadow
    , effectStyleShadow1 = mapStyleBundle toMsg props.effectStyleShadow1
    , effectStyleSheetShadow = mapStyleBundle toMsg props.effectStyleSheetShadow
    , effectStyleNavShadow = mapStyleBundle toMsg props.effectStyleNavShadow
    , effectStyleButtonShadow = mapStyleBundle toMsg props.effectStyleButtonShadow
    , effectStyleButtonHover = mapStyleBundle toMsg props.effectStyleButtonHover
    }


type alias Styles msg =
    { color1 : Theme.Color
    , color1DarkMode : Theme.Color
    , color2 : Theme.Color
    , color2DarkMode : Theme.Color
    , color3 : Theme.Color
    , color3DarkMode : Theme.Color
    , color4 : Theme.Color
    , color4DarkMode : Theme.Color
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
    , colorStyleGrayscaleMedia : StyleBundle msg
    , colorStyleGrayscaleDisabled : StyleBundle msg
    , colorStyleCode : StyleBundle msg
    , colorStyleRegularButtonText : StyleBundle msg
    , colorStylePrimaryButtonText : StyleBundle msg
    , colorStyleSecondaryButtonText : StyleBundle msg
    , colorStyleDisabledButtonText : StyleBundle msg
    , colorStyleRegularButtonBackground : StyleBundle msg
    , colorStylePrimaryButtonBackground : StyleBundle msg
    , colorStyleSecondaryButtonBackground : StyleBundle msg
    , colorStyleDisabledButtonBackground : StyleBundle msg
    , colorStyleInverse : StyleBundle msg
    , effectStyleModalShadow : StyleBundle msg
    , effectStyleShadow1 : StyleBundle msg
    , effectStyleSheetShadow : StyleBundle msg
    , effectStyleNavShadow : StyleBundle msg
    , effectStyleButtonShadow : StyleBundle msg
    , effectStyleButtonHover : StyleBundle msg
    }


paretoThemeStyles : Styles msg
paretoThemeStyles =
    let
        color1 =
            TwColor.arbitraryRgb 203 213 225

        color1Inverse =
            color4

        color2 =
            TwColor.arbitraryRgb 148 163 184

        color2Inverse =
            color3

        color3 =
            TwColor.arbitraryRgb 100 116 139

        color3Inverse =
            color2

        color4 =
            TwColor.arbitraryRgb 51 65 85

        color4Inverse =
            color1
    in
    { color1 = color1
    , color1DarkMode = color1Inverse
    , color2 = color2
    , color2DarkMode = color2Inverse
    , color3 = color3
    , color3DarkMode = color3Inverse
    , color4 = color4
    , color4DarkMode = color4Inverse
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
            , Tw.tracking_tighter
            ]
        , fontFamilyInter
        , Attr.style "line-height" "auto"
        ]
    , textStyleH1Article =
        [ css
            [ Tw.text_5xl
            , Tw.font_bold
            , Tw.leading_tight
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
                [ Tw.bg_color Theme.black
                ]
            ]
        ]
    , colorStyleLabel =
        [ css
            [ Tw.text_color color2
            , darkMode
                [ Tw.text_color color2Inverse
                ]
            ]
        ]
    , colorStyleLinks =
        [ css
            [ Tw.text_color color4
            , darkMode
                [ Tw.text_color color4Inverse ]
            ]
        ]
    , colorStyleMedia =
        [ css
            [ Tw.text_color color2
            , darkMode
                [ Tw.text_color color2Inverse
                ]
            ]
        ]
    , colorStyleBorders =
        [ css
            [ Tw.border_color color4
            , darkMode
                [ Tw.border_color color4Inverse
                ]
            ]
        ]
    , colorStyleIcons =
        [ css
            [ Tw.text_color color1
            , darkMode
                [ Tw.text_color color1Inverse
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
            [ Tw.text_color color4
            , darkMode
                [ Tw.text_color color4Inverse
                ]
            ]
        ]
    , colorStyleGrayscaleSummary =
        [ css
            [ Tw.text_color color4
            , darkMode
                [ Tw.text_color color4Inverse
                ]
            ]
        ]
    , colorStyleGrayscaleMuted =
        [ css
            [ Tw.text_color color2
            , darkMode
                [ Tw.text_color color2Inverse
                ]
            ]
        ]
    , colorStyleGrayscaleText =
        [ css
            [ Tw.text_color color4
            , darkMode
                [ Tw.text_color color4Inverse
                ]
            ]
        ]
    , colorStyleGrayscaleMedia =
        [ css
            [ Tw.text_color color2
            , darkMode
                [ Tw.text_color color1
                ]
            ]
        ]
    , colorStyleGrayscaleDisabled =
        [ css
            [ Tw.text_color color2
            , darkMode
                [ Tw.text_color color2Inverse
                ]
            ]
        ]
    , colorStyleCode =
        [ css
            [ Tw.text_color color4
            , darkMode
                [ Tw.text_color color4Inverse
                ]
            ]
        ]
    , colorStyleRegularButtonText =
        [ css
            [ Tw.text_color color1
            , darkMode
                [ Tw.text_color color1Inverse
                ]
            ]
        ]
    , colorStylePrimaryButtonText =
        [ css
            [ Tw.text_color color3
            , darkMode
                [ Tw.text_color color3Inverse
                ]
            ]
        ]
    , colorStyleSecondaryButtonText =
        [ css
            [ Tw.text_color color1
            , darkMode
                [ Tw.text_color color1Inverse
                ]
            ]
        ]
    , colorStyleDisabledButtonText =
        [ css
            [ Tw.text_color color2
            , darkMode
                [ Tw.text_color color2Inverse
                ]
            ]
        ]
    , colorStyleRegularButtonBackground =
        [ css
            [ Tw.bg_color color4
            , Tw.border_color color1
            , Tw.border_2
            , darkMode
                [ Tw.bg_color color4Inverse
                , Tw.border_color color1Inverse
                ]
            ]
        ]
    , colorStylePrimaryButtonBackground =
        [ css
            [ Tw.bg_color color4
            , darkMode
                [ Tw.bg_color color4Inverse
                ]
            ]
        ]
    , colorStyleSecondaryButtonBackground =
        [ css
            [ Tw.bg_color color3
            , darkMode
                [ Tw.bg_color color3Inverse
                ]
            ]
        ]
    , colorStyleDisabledButtonBackground =
        [ css
            [ Tw.bg_color color1
            , darkMode
                [ Tw.bg_color color1Inverse
                ]
            ]
        ]
    , colorStyleInverse =
        [ css
            [ Tw.bg_color Theme.white
            , darkMode
                [ Tw.bg_color Theme.black
                ]
            ]
        ]
    , effectStyleModalShadow =
        [ css
            [ Tw.drop_shadow_md
            , Tw.backdrop_blur_md
            ]
        ]
    , effectStyleShadow1 =
        [ css
            [ Tw.drop_shadow_lg
            , Tw.backdrop_blur_md
            ]
        ]
    , effectStyleSheetShadow =
        [ css
            [ Tw.backdrop_blur
            ]
        , Attr.style "filter" "drop-shadow(0 -1px rgb(0 0 0 / 0.05))"
        ]
    , effectStyleNavShadow =
        [ css
            [ Tw.drop_shadow
            , Tw.backdrop_blur_sm
            ]
        ]
    , effectStyleButtonShadow =
        [ css
            [ Tw.drop_shadow
            , Tw.backdrop_blur_sm
            ]
        ]
    , effectStyleButtonHover =
        [ css
            [ Tw.drop_shadow
            , Tw.backdrop_blur_sm
            ]
        ]
    }


darkMode : List Css.Style -> Css.Style
darkMode =
    Css.Media.withMediaQuery [ "(prefers-color-scheme: dark)" ]
