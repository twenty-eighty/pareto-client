module Ui.Styles exposing (..)

import Css
import Css.Media
import Html.Styled as Html exposing (Html, a, article, aside, button, div, h1, h2, h3, h4, img, main_, p, span, text)
import Html.Styled.Attributes as Attr exposing (class, css, href)
import Html.Styled.Events as Events exposing (..)

import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw
import Tailwind.Theme as Theme
import Html.Attributes

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
    = ReferenceStyleTheme
    | ParetoTheme

type alias StyleBundle msg =
    List (Html.Attribute msg)

stylesForTheme : Theme -> Styles msg
stylesForTheme theme =
    case theme of
        ReferenceStyleTheme ->
            referenceDesignThemeStyles

        ParetoTheme ->
            paretoThemeStyles

mapStyleBundle : (msg1 -> msg2) -> StyleBundle msg1 -> StyleBundle msg2
mapStyleBundle toMsg styles =
    styles
    |> List.map (\style -> Attr.map toMsg style)

map : (msg1 -> msg2) -> Styles msg1 -> Styles msg2
map toMsg props =
    { textStyleReactions = mapStyleBundle toMsg props.textStyleReactions
    , textStyleLinks = mapStyleBundle toMsg props.textStyleLinks
    , textStyleBody = mapStyleBundle toMsg props.textStyleBody
    , textStyleDropdownsLabel = mapStyleBundle toMsg props.textStyleDropdownsLabel
    , textStyleSemiboldLabel = mapStyleBundle toMsg props.textStyleSemiboldLabel
    , textStyleUppercaseLabel = mapStyleBundle toMsg props.textStyleUppercaseLabel
    , textStyleH1 = mapStyleBundle toMsg props.textStyleH1
    , textStyleH1Article = mapStyleBundle toMsg props.textStyleH1Article
    , textStyleH2 = mapStyleBundle toMsg props.textStyleH2
    , textStyleH3 = mapStyleBundle toMsg props.textStyleH3
    , textStyleH4 = mapStyleBundle toMsg props.textStyleH4
    , textStyleH4Article = mapStyleBundle toMsg props.textStyleH4Article
    , textStyle14 = mapStyleBundle toMsg props.textStyle14
    , textStyleArticleHashtags = mapStyleBundle toMsg props.textStyleArticleHashtags 
    , textStyleArticleAuthor = mapStyleBundle toMsg props.textStyleArticleAuthor
    , textStyleArticleCode = mapStyleBundle toMsg props.textStyleArticleCode
    , textStyleArticleDate = mapStyleBundle toMsg props.textStyleArticleDate
    , textStyleHashtagLarge = mapStyleBundle toMsg props.textStyleHashtagLarge
    , colorStyleBackground = mapStyleBundle toMsg props.colorStyleBackground
    , colorStyleLabel = mapStyleBundle toMsg props.colorStyleLabel
    , colorStyleMedia = mapStyleBundle toMsg props.colorStyleMedia
    , colorStyleLabelBG = mapStyleBundle toMsg props.colorStyleLabelBG
    , colorStyleBorders = mapStyleBundle toMsg props.colorStyleBorders
    , colorStyleIcons = mapStyleBundle toMsg props.colorStyleIcons
    , colorStyleGreen = mapStyleBundle toMsg props.colorStyleGreen
    , colorStyleCover = mapStyleBundle toMsg props.colorStyleCover
    , colorStyleGrayscaleTitle = mapStyleBundle toMsg props.colorStyleGrayscaleTitle 
    , colorStyleGrayscaleMuted = mapStyleBundle toMsg props.colorStyleGrayscaleMuted 
    , colorStyleGrayscaleText = mapStyleBundle toMsg props.colorStyleGrayscaleText
    , colorStyleGrayscaleMedia = mapStyleBundle toMsg props.colorStyleGrayscaleMedia 
    , colorStyleGrayscaleDisabled = mapStyleBundle toMsg props.colorStyleGrayscaleDisabled
    , colorStyleArticleHashtags = mapStyleBundle toMsg props.colorStyleArticleHashtags
    , colorStyleRegularButtonText = mapStyleBundle toMsg props.colorStyleRegularButtonText
    , colorStylePrimaryButtonText = mapStyleBundle toMsg props.colorStylePrimaryButtonText
    , colorStyleSecondaryButtonText = mapStyleBundle toMsg props.colorStyleSecondaryButtonText
    , colorStyleDisabledButtonText = mapStyleBundle toMsg props.colorStyleDisabledButtonText
    , colorStyleRegularButtonBackground = mapStyleBundle toMsg props.colorStyleRegularButtonBackground
    , colorStylePrimaryButtonBackground = mapStyleBundle toMsg props.colorStylePrimaryButtonBackground
    , colorStyleSecondaryButtonBackground = mapStyleBundle toMsg props.colorStyleSecondaryButtonBackground
    , colorStyleDisabledButtonBackground = mapStyleBundle toMsg props.colorStyleDisabledButtonBackground
    , colorStyleInverse = mapStyleBundle toMsg props.colorStyleInverse
    , colorStyleSitebarBackground = mapStyleBundle toMsg props.colorStyleSitebarBackground
    , colorStyleSitebarItemActive = mapStyleBundle toMsg props.colorStyleSitebarItemActive
    , colorStyleSitebarItemActiveBackground = mapStyleBundle toMsg props.colorStyleSitebarItemActiveBackground
    , colorStyleSitebarItemActiveBorder = mapStyleBundle toMsg props.colorStyleSitebarItemActiveBorder
    , colorStyleSitebarItemInactiveBackground = mapStyleBundle toMsg props.colorStyleSitebarItemInactiveBackground
    , colorStyleSitebarItemEnabled = mapStyleBundle toMsg props.colorStyleSitebarItemEnabled
    , colorStyleSitebarItemDisabled = mapStyleBundle toMsg props.colorStyleSitebarItemDisabled
    , colorStyleCategoryActive = mapStyleBundle toMsg props.colorStyleCategoryActive
    , colorStyleCategoryActiveBackground = mapStyleBundle toMsg props.colorStyleCategoryActiveBackground
    , colorStyleCategoryActiveBorder = mapStyleBundle toMsg props.colorStyleCategoryActiveBorder
    , colorStyleCategoryInactiveBackground = mapStyleBundle toMsg props.colorStyleCategoryInactiveBackground
    , effectStyleModalShadow = mapStyleBundle toMsg props.effectStyleModalShadow
    , effectStyleShadow1 = mapStyleBundle toMsg props.effectStyleShadow1
    , effectStyleSheetShadow = mapStyleBundle toMsg props.effectStyleSheetShadow
    , effectStyleNavShadow = mapStyleBundle toMsg props.effectStyleNavShadow
    , effectStyleButtonShadow = mapStyleBundle toMsg props.effectStyleButtonShadow
    , effectStyleButtonHover = mapStyleBundle toMsg props.effectStyleButtonHover
    }


type alias Styles msg =
    { textStyleReactions : StyleBundle msg
    , textStyleLinks : StyleBundle msg
    , textStyleBody : StyleBundle msg
    , textStyleDropdownsLabel : StyleBundle msg
    , textStyleSemiboldLabel : StyleBundle msg
    , textStyleUppercaseLabel : StyleBundle msg
    , textStyleH1 : StyleBundle msg
    , textStyleH1Article : StyleBundle msg
    , textStyleH2 : StyleBundle msg
    , textStyleH3 : StyleBundle msg
    , textStyleH4 : StyleBundle msg
    , textStyleH4Article : StyleBundle msg
    , textStyle14 : StyleBundle msg
    , textStyleArticleHashtags : StyleBundle msg
    , textStyleArticleAuthor : StyleBundle msg
    , textStyleArticleCode : StyleBundle msg
    , textStyleArticleDate : StyleBundle msg
    , textStyleHashtagLarge : StyleBundle msg
    , colorStyleBackground : StyleBundle msg
    , colorStyleLabel : StyleBundle msg
    , colorStyleMedia : StyleBundle msg
    , colorStyleLabelBG : StyleBundle msg
    , colorStyleBorders : StyleBundle msg
    , colorStyleIcons : StyleBundle msg
    , colorStyleGreen : StyleBundle msg
    , colorStyleCover : StyleBundle msg
    , colorStyleGrayscaleTitle : StyleBundle msg
    , colorStyleGrayscaleMuted : StyleBundle msg
    , colorStyleGrayscaleText : StyleBundle msg
    , colorStyleGrayscaleMedia : StyleBundle msg
    , colorStyleGrayscaleDisabled : StyleBundle msg
    , colorStyleArticleHashtags : StyleBundle msg
    , colorStyleRegularButtonText : StyleBundle msg
    , colorStylePrimaryButtonText : StyleBundle msg
    , colorStyleSecondaryButtonText : StyleBundle msg
    , colorStyleDisabledButtonText : StyleBundle msg
    , colorStyleRegularButtonBackground : StyleBundle msg
    , colorStylePrimaryButtonBackground : StyleBundle msg
    , colorStyleSecondaryButtonBackground : StyleBundle msg
    , colorStyleDisabledButtonBackground : StyleBundle msg
    , colorStyleInverse : StyleBundle msg
    , colorStyleSitebarBackground : StyleBundle msg
    , colorStyleSitebarItemActive : StyleBundle msg
    , colorStyleSitebarItemActiveBackground : StyleBundle msg
    , colorStyleSitebarItemActiveBorder : StyleBundle msg
    , colorStyleSitebarItemInactiveBackground : StyleBundle msg
    , colorStyleSitebarItemEnabled : StyleBundle msg
    , colorStyleSitebarItemDisabled : StyleBundle msg
    , colorStyleCategoryActive : StyleBundle msg
    , colorStyleCategoryActiveBackground : StyleBundle msg
    , colorStyleCategoryInactiveBackground : StyleBundle msg
    , colorStyleCategoryActiveBorder : StyleBundle msg
    , effectStyleModalShadow : StyleBundle msg
    , effectStyleShadow1 : StyleBundle msg
    , effectStyleSheetShadow : StyleBundle msg
    , effectStyleNavShadow : StyleBundle msg
    , effectStyleButtonShadow : StyleBundle msg
    , effectStyleButtonHover : StyleBundle msg
    }

referenceDesignThemeStyles : Styles msg
referenceDesignThemeStyles =
    { textStyleReactions =
        [ css
            [ Tw.text_base
            , Tw.font_medium
            , Tw.tracking_normal
            ]
        , fontFamilyInter
        , Attr.style "line-height" "auto"
        ]
    , textStyleLinks =
        [ css
            [ Tw.text_sm
            , Tw.font_medium
            , Tw.leading_6
            , Tw.tracking_normal
            ]
        , fontFamilyInter
        ]
    , textStyleBody =
        [ css
            [ Tw.text_base
            , Tw.font_normal
            , Tw.leading_6
            , Tw.tracking_normal
            -- , Tw.leading_relaxed
            ]
        , fontFamilyInter
        ]
    , textStyleDropdownsLabel =
        [ css
            [ Tw.text_base
            , Tw.font_semibold
            , Tw.tracking_normal
            ]
        , fontFamilyInter
        , Attr.style "line-height" "auto"
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
            , Tw.font_semibold
            ]
        , fontFamilyInter
        , Attr.style "line-height" "auto"
        ]
    , textStyleH2 =
        [ css
            [ Tw.text_2xl
            , Tw.font_semibold
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
    , textStyleArticleHashtags =
        [ css
            [ Tw.text_sm
            , Tw.font_medium
            , Tw.leading_snug
            ]
        , fontFamilyRobotoMono
        ]
    , textStyleArticleAuthor =
        [ css
            [ Tw.text_sm
            , Tw.font_normal
            , Tw.leading_snug
            ]
        , fontFamilyRobotoMono
        ]
    , textStyleArticleCode =
        [ css
            [ Tw.text_xs
            , Tw.font_normal
            , Tw.capitalize
            , Tw.leading_tight
            ]
        , fontFamilyRobotoMono
        ]
    , textStyleArticleDate =
        [ css
            [ Tw.text_xs
            , Tw.font_normal
            , Tw.leading_tight
            ]
        , fontFamilyRobotoMono
        ]
    , textStyleHashtagLarge =
        [ css
            [ Tw.text_4xl
            , Tw.font_bold
            ]
        , fontFamilyRobotoMono
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
            [ Tw.text_color Theme.slate_600 -- should be #565C70
            , darkMode
                [ Tw.text_color Theme.slate_400
                ]
            ]
        ]
    , colorStyleMedia =
        [ css
            [ Tw.text_color Theme.slate_200 -- should be #D6D9E5
            , darkMode
                [ Tw.text_color Theme.slate_800
                ]
            ]
        ]
    , colorStyleLabelBG =
        [ css
            [ Tw.text_color Theme.slate_200 -- should be #D6D9E5
            , darkMode
                [ Tw.text_color Theme.slate_800
                ]
            ]
        ]
    , colorStyleBorders =
        [ css
            [ Tw.text_color Theme.slate_100 -- should be #DFE1EB
            , darkMode
                [ Tw.text_color Theme.slate_900
                ]
            ]
        ]
    , colorStyleIcons =
        [ css
            [ Tw.text_color Theme.slate_400 -- should be #999CAB
            , darkMode
                [ Tw.text_color Theme.slate_600
                ]
            ]
        ]
    , colorStyleGreen =
        [ css
            [ Tw.text_color Theme.emerald_400 -- should be Linear in Figma
            , darkMode
                [ Tw.text_color Theme.emerald_600
                ]
            ]
        ]
    , colorStyleCover =
        [ css
            [ Tw.text_opacity_70
            , darkMode
                [ 
                ]
            ]
        ]
    , colorStyleGrayscaleTitle =
        [ css
            [ Tw.text_color Theme.slate_800
            , darkMode
                [ Tw.text_color Theme.slate_200
                ]
            ]
        ]
    , colorStyleGrayscaleMuted =
        [ css
            [ Tw.text_color Theme.slate_400
            , darkMode
                [ Tw.text_color Theme.slate_600
                ]
            ]
        ]
    , colorStyleGrayscaleText =
        [ css
            [ Tw.text_color Theme.slate_500
            , darkMode
                [ Tw.text_color Theme.slate_500
                ]
            ]
        ]
    , colorStyleGrayscaleMedia =
        [ css
            [ Tw.text_color Theme.slate_300
            , darkMode
                [ Tw.text_color Theme.slate_700
                ]
            ]
        ]
    , colorStyleGrayscaleDisabled =
        [ css
            [ Tw.text_color Theme.slate_300
            , darkMode
                [ Tw.text_color Theme.slate_700
                ]
            ]
        ]
    , colorStyleArticleHashtags =
        [ css
            [ Tw.text_color Theme.blue_600
            , darkMode
                [ Tw.text_color Theme.blue_400
                ]
            ]
        ]
    , colorStyleRegularButtonText =
        [ css
            [ Tw.text_color Theme.slate_300
            , darkMode
                [ Tw.text_color Theme.slate_700
                ]
            ]
        ]
    , colorStylePrimaryButtonText =
        [ css
            [ Tw.text_color Theme.slate_300
            , darkMode
                [ Tw.text_color Theme.slate_700
                ]
            ]
        ]
    , colorStyleSecondaryButtonText =
        [ css
            [ Tw.text_color Theme.slate_300
            , darkMode
                [ Tw.text_color Theme.slate_700
                ]
            ]
        ]
    , colorStyleDisabledButtonText =
        [ css
            [ Tw.text_color Theme.slate_300
            , darkMode
                [ Tw.text_color Theme.slate_700
                ]
            ]
        ]
    , colorStyleRegularButtonBackground =
        [ css
            [ Tw.text_color Theme.slate_700
            , darkMode
                [ Tw.text_color Theme.slate_300
                ]
            ]
        ]
    , colorStylePrimaryButtonBackground =
        [ css
            [ Tw.text_color Theme.slate_700
            , darkMode
                [ Tw.text_color Theme.slate_300
                ]
            ]
        ]
    , colorStyleSecondaryButtonBackground =
        [ css
            [ Tw.text_color Theme.slate_700
            , darkMode
                [ Tw.text_color Theme.slate_300
                ]
            ]
        ]
    , colorStyleDisabledButtonBackground =
        [ css
            [ Tw.text_color Theme.slate_700
            , darkMode
                [ Tw.text_color Theme.slate_300
                ]
            ]
        ]
    , colorStyleInverse =
        [ css
            [ Tw.text_color Theme.white
            , darkMode
                [ Tw.text_color Theme.black
                ]
            ]
        ]
    , colorStyleSitebarBackground =
        [ css
            [ Tw.bg_color Theme.slate_800
            , Bp.sm
                [ Tw.bg_color Theme.slate_100
                ]
            , darkMode
                [ Tw.bg_color Theme.slate_800
                ]
            ]
        ]
    , colorStyleSitebarItemActive =
        [ css
            [ Tw.text_color Theme.purple_600
            , darkMode
                [ Tw.text_color Theme.purple_400
                ]
            ]
        ]
    , colorStyleSitebarItemActiveBackground =
        [ css
            [ Tw.bg_color Theme.purple_100
            , darkMode
                [ Tw.bg_color Theme.purple_900
                ]
            ]
        ]
    , colorStyleSitebarItemActiveBorder =
        [ css
            [ Tw.border_color Theme.slate_700
            , darkMode
                [ Tw.border_color Theme.slate_300
                ]
            ]
        ]
    , colorStyleSitebarItemInactiveBackground =
        [ css
            [ Tw.bg_color Theme.slate_100
            , darkMode
                [ Tw.bg_color Theme.slate_900
                ]
            ]
        ]
    , colorStyleSitebarItemEnabled =
        [ css
            [ Tw.bg_color Theme.slate_100
            , darkMode
                [ Tw.bg_color Theme.slate_900
                ]
            ]
        ]
    , colorStyleSitebarItemDisabled =
        [ css
            [ Tw.bg_color Theme.slate_100
            , darkMode
                [ Tw.bg_color Theme.slate_900
                ]
            ]
        ]
    , colorStyleCategoryActive =
        [ css
            [ Tw.text_color Theme.purple_600
            , darkMode
                [ Tw.text_color Theme.purple_400
                ]
            ]
        ]
    , colorStyleCategoryActiveBackground =
        [ css
            [ Tw.bg_color Theme.purple_100
            , darkMode
                [ Tw.bg_color Theme.purple_900
                ]
            ]
        ]
    , colorStyleCategoryActiveBorder =
        [ css
            [ Tw.border_color Theme.slate_700
            , darkMode
                [ Tw.border_color Theme.slate_300
                ]
            ]
        ]
    , colorStyleCategoryInactiveBackground =
        [ css
            [ Tw.bg_color Theme.slate_100
            , darkMode
                [ Tw.bg_color Theme.slate_900
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


paretoThemeStyles : Styles msg
paretoThemeStyles =
    let
        color1 =
            Theme.slate_700

        color1Inverse =
            Theme.slate_300

        color2 =
            Theme.slate_300

        color2Inverse =
            Theme.slate_700

        color3 =
            Theme.slate_100

        color3Inverse =
            Theme.slate_900
    in
    { textStyleReactions =
        [ css
            [ Tw.text_base
            , Tw.font_medium
            , Tw.tracking_normal
            ]
        , fontFamilyInter
        , Attr.style "line-height" "auto"
        ]
    , textStyleLinks =
        [ css
            [ Tw.text_base
            , Tw.font_medium
            , Tw.leading_6
            , Tw.tracking_normal
            ]
        , fontFamilyInter
        ]
    , textStyleBody =
        [ css
            [ Tw.text_base
            , Tw.font_normal
            , Tw.leading_6
            , Tw.tracking_normal
            -- , Tw.leading_relaxed
            ]
        , fontFamilyInter
        ]
    , textStyleDropdownsLabel =
        [ css
            [ Tw.text_base
            , Tw.font_semibold
            , Tw.tracking_normal
            ]
        , fontFamilyInter
        , Attr.style "line-height" "auto"
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
            , Tw.font_semibold
            , Tw.leading_tight
            ]
        , fontFamilyInter
        , Attr.style "line-height" "auto"
        ]
    , textStyleH2 =
        [ css
            [ Tw.text_2xl
            , Tw.font_semibold
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
    , textStyleArticleHashtags =
        [ css
            [ Tw.text_sm
            , Tw.font_medium
            , Tw.leading_snug
            ]
        , fontFamilyRobotoMono
        ]
    , textStyleArticleAuthor =
        [ css
            [ Tw.text_sm
            , Tw.font_normal
            , Tw.leading_snug
            ]
        , fontFamilyRobotoMono
        ]
    , textStyleArticleCode =
        [ css
            [ Tw.text_xs
            , Tw.font_normal
            , Tw.capitalize
            , Tw.leading_tight
            ]
        , fontFamilyRobotoMono
        ]
    , textStyleArticleDate =
        [ css
            [ Tw.text_xs
            , Tw.font_normal
            , Tw.leading_tight
            ]
        , fontFamilyRobotoMono
        ]
    , textStyleHashtagLarge =
        [ css
            [ Tw.text_4xl
            , Tw.font_bold
            ]
        , fontFamilyRobotoMono
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
            [ Tw.text_color Theme.slate_600 -- should be #565C70
            , darkMode
                [ Tw.text_color Theme.slate_400
                ]
            ]
        ]
    , colorStyleMedia =
        [ css
            [ Tw.text_color Theme.slate_200 -- should be #D6D9E5
            , darkMode
                [ Tw.text_color Theme.slate_800
                ]
            ]
        ]
    , colorStyleLabelBG =
        [ css
            [ Tw.text_color Theme.slate_200 -- should be #D6D9E5
            , darkMode
                [ Tw.text_color Theme.slate_800
                ]
            ]
        ]
    , colorStyleBorders =
        [ css
            [ Tw.text_color color3
            , darkMode
                [ Tw.text_color color3Inverse
                ]
            ]
        ]
    , colorStyleIcons =
        [ css
            [ Tw.text_color Theme.slate_400 -- should be #999CAB
            , darkMode
                [ Tw.text_color Theme.slate_600
                ]
            ]
        ]
    , colorStyleGreen =
        [ css
            [ Tw.text_color Theme.emerald_400 -- should be Linear in Figma
            , darkMode
                [ Tw.text_color Theme.emerald_600
                ]
            ]
        ]
    , colorStyleCover =
        [ css
            [ Tw.text_opacity_70
            , darkMode
                [ 
                ]
            ]
        ]
    , colorStyleGrayscaleTitle =
        [ css
            [ Tw.text_color Theme.slate_800
            , darkMode
                [ Tw.text_color Theme.slate_200
                ]
            ]
        ]
    , colorStyleGrayscaleMuted =
        [ css
            [ Tw.text_color Theme.slate_400
            , darkMode
                [ Tw.text_color Theme.slate_600
                ]
            ]
        ]
    , colorStyleGrayscaleText =
        [ css
            [ Tw.text_color Theme.slate_500
            , darkMode
                [ Tw.text_color Theme.slate_500
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
                [ Tw.text_color color1
                ]
            ]
        ]
    , colorStyleArticleHashtags =
        [ css
            [ Tw.text_color Theme.blue_600
            , darkMode
                [ Tw.text_color Theme.blue_400
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
            [ Tw.text_color color2
            , darkMode
                [ Tw.text_color color2Inverse
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
            [ Tw.bg_color color3
            , Tw.border_color color1
            , Tw.border_2
            , darkMode
                [ Tw.bg_color color3Inverse
                , Tw.border_color color1Inverse
                ]
            ]
        ]
    , colorStylePrimaryButtonBackground =
        [ css
            [ Tw.bg_color color1
            , darkMode
                [ Tw.bg_color color1Inverse
                ]
            ]
        ]
    , colorStyleSecondaryButtonBackground =
        [ css
            [ Tw.bg_color color2
            , darkMode
                [ Tw.bg_color color2Inverse
                ]
            ]
        ]
    , colorStyleDisabledButtonBackground =
        [ css
            [ Tw.bg_color color3
            , darkMode
                [ Tw.bg_color color3Inverse
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
    , colorStyleSitebarBackground =
        [ css
            [ Tw.bg_color Theme.slate_800
            , darkMode
                [ Tw.border_t_2
                ]
            , Bp.sm
                [ Tw.bg_color Theme.white
                , darkMode
                    [ Tw.bg_color Theme.black
                    ]
                ]
            ]
        ]
    , colorStyleSitebarItemActive =
        [ css
            [ Tw.text_color color1
            , darkMode
                [ Tw.text_color color2
                , Bp.sm
                    [ Tw.text_color color1
                    ]
                ]
            , Bp.sm
                [ Tw.text_color color1
                , darkMode
                    [ Tw.text_color color1
                    ]
                ]
            ]
        ]
    , colorStyleSitebarItemActiveBackground =
        [ css
            [ Tw.bg_color color2
            , Bp.sm
                [ Tw.bg_color color2
                ]
            , darkMode
                [ Tw.bg_color color1
                ]
            ]
        ]
    , colorStyleSitebarItemActiveBorder =
        [ css
            [ 
            ]
        ]
    , colorStyleSitebarItemInactiveBackground =
        [ css
            [ Tw.text_color color3
            , Bp.sm
                [ Tw.text_color color3Inverse
                ]
            ]
        ]
    , colorStyleSitebarItemEnabled =
        [ css
            [ Tw.text_color color2
            , Bp.sm
                [ Tw.text_color color1
                , darkMode
                    [ Tw.text_color color2
                    ]
                ]
            ]
        ]
    , colorStyleSitebarItemDisabled =
        [ css
            [ Tw.text_color color1
            , Bp.sm
                [ Tw.text_color color2
                , darkMode
                    [ Tw.text_color color1
                    ]
                ]
            ]
        ]
    , colorStyleCategoryActive =
        [ css
            [ Tw.text_color color1
            , darkMode
                [ Tw.text_color color2
                ]
            ]
        ]
    , colorStyleCategoryActiveBackground =
        [ css
            [ Tw.bg_color color2
            , darkMode
                [ Tw.bg_color color1
                ]
            ]
        ]
    , colorStyleCategoryActiveBorder =
        [ css
            [ Tw.border_2
            , Tw.border_color color1
            , darkMode
                [ Tw.border_color color2
                ]
            ]
        ]
    , colorStyleCategoryInactiveBackground =
        [ css
            [ Tw.bg_color color3
            , darkMode
                [ Tw.bg_color color3Inverse
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

