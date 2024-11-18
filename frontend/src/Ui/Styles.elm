module Ui.Styles exposing (..)

import Css
import Css.Media
import Html.Styled as Html exposing (Html, a, article, aside, button, div, h1, h2, h3, h4, img, main_, p, span, text)
import Html.Styled.Attributes as Attr exposing (class, css, href)
import Html.Styled.Events as Events exposing (..)

import Tailwind.Utilities as Tw
import Tailwind.Theme as Theme

fontFamilyInter : Html.Attribute msg
fontFamilyInter =
    Attr.style "font-family" "Inter, sans-serif"

fontFamilyUnbounded : Html.Attribute msg
fontFamilyUnbounded =
    Attr.style "font-family" "Unbounded"

fontFamilySourceSerifPro : Html.Attribute msg
fontFamilySourceSerifPro =
    Attr.style "font-family" "Source Serif Pro"


type alias Styles msg =
    { textStyleReactions : StyleBundle msg
    , textStyleLinks : StyleBundle msg
    , textStyleBody : StyleBundle msg
    , textStyleDropdownsLabel : StyleBundle msg
    , textStyleSemiboldLabel : StyleBundle msg
    , textStyleUppercaseLabel : StyleBundle msg
    , textStyleH1 : StyleBundle msg
    , textStyleH2 : StyleBundle msg
    , textStyleH3 : StyleBundle msg
    , textStyleH4 : StyleBundle msg
    , textStyle14 : StyleBundle msg
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
    , effectStyleModalShadow : StyleBundle msg
    , effectStyleShadow1 : StyleBundle msg
    , effectStyleSheetShadow : StyleBundle msg
    , effectStyleNavShadow : StyleBundle msg
    , effectStyleButtonShadow : StyleBundle msg
    , effectStyleButtonHover : StyleBundle msg
    }

type alias StyleBundle msg =
    List (Html.Attribute msg)

referenceDesignStyles : Styles msg
referenceDesignStyles =
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
                [ -- Tw.bg_color Theme.black
                ]
            ]
        ]
    , colorStyleLabel = 
        [ css
            [ Tw.text_color Theme.slate_600 -- should be #565C70
            , darkMode
                [ -- Tw.bg_color Theme.black
                ]
            ]
        ]
    , colorStyleMedia =
        [ css
            [ Tw.text_color Theme.slate_200 -- should be #D6D9E5
            , darkMode
                [ -- Tw.bg_color Theme.black
                ]
            ]
        ]
    , colorStyleLabelBG =
        [ css
            [ Tw.text_color Theme.slate_200 -- should be #D6D9E5
            , darkMode
                [ -- Tw.bg_color Theme.black
                ]
            ]
        ]
    , colorStyleBorders =
        [ css
            [ Tw.text_color Theme.slate_100 -- should be #DFE1EB
            , darkMode
                [ -- Tw.bg_color Theme.black
                ]
            ]
        ]
    , colorStyleIcons =
        [ css
            [ Tw.text_color Theme.slate_400 -- should be #999CAB
            , darkMode
                [ -- Tw.bg_color Theme.black
                ]
            ]
        ]
    , colorStyleGreen =
        [ css
            [ Tw.text_color Theme.emerald_400 -- should be Linear in Figma
            , darkMode
                [ -- Tw.bg_color Theme.black
                ]
            ]
        ]
    , colorStyleCover =
        [ css
            [ Tw.text_opacity_70
            , darkMode
                [ -- Tw.bg_color Theme.black
                ]
            ]
        ]
    , colorStyleGrayscaleTitle =
        [ css
            [ Tw.text_color Theme.slate_800
            , darkMode
                [ -- Tw.bg_color Theme.black
                ]
            ]
        ]
    , colorStyleGrayscaleMuted =
        [ css
            [ Tw.text_color Theme.slate_400
            , darkMode
                [ -- Tw.bg_color Theme.black
                ]
            ]
        ]
    , colorStyleGrayscaleText =
        [ css
            [ Tw.text_color Theme.slate_500
            , darkMode
                [ -- Tw.bg_color Theme.black
                ]
            ]
        ]
    , colorStyleGrayscaleMedia =
        [ css
            [ Tw.text_color Theme.slate_300
            , darkMode
                [ -- Tw.bg_color Theme.black
                ]
            ]
        ]
    , effectStyleModalShadow =
        [ css
            [ Tw.drop_shadow_md
            , Tw.backdrop_blur_md
            , darkMode
                [ -- Tw.bg_color Theme.black
                ]
            ]
        ]
    , effectStyleShadow1 =
        [ css
            [ Tw.drop_shadow_lg
            , Tw.backdrop_blur_md
            , darkMode
                [ -- Tw.bg_color Theme.black
                ]
            ]
        ]
    , effectStyleSheetShadow =
        [ css
            [ Tw.backdrop_blur
            , darkMode
                [ -- Tw.bg_color Theme.black
                ]
            ]
        , Attr.style "filter" "drop-shadow(0 -1px rgb(0 0 0 / 0.05))"
        ]
    , effectStyleNavShadow =
        [ css
            [ Tw.drop_shadow
            , Tw.backdrop_blur_sm
            , darkMode
                [ -- Tw.bg_color Theme.black
                ]
            ]
        ]
    , effectStyleButtonShadow =
        [ css
            [ Tw.drop_shadow
            , Tw.backdrop_blur_sm
            , darkMode
                [ -- Tw.bg_color Theme.black
                ]
            ]
        ]
    , effectStyleButtonHover =
        [ css
            [ Tw.drop_shadow
            , Tw.backdrop_blur_sm
            , darkMode
                [ -- Tw.bg_color Theme.black
                ]
            ]
        ]
    }

darkMode : List Css.Style -> Css.Style
darkMode =
    Css.Media.withMediaQuery [ "(prefers-color-scheme: dark)" ]

