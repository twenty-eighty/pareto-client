module Ui.Shared exposing (..)

import BrowserEnv exposing (BrowserEnv)
import Components.Icon as Icon exposing (Icon)
import Css
import Css.Media
import FeatherIcons
import Html.Styled as Html exposing (Html, a, article, aside, button, div, h2, h3, h4, img, main_, p, span, text)
import Html.Styled.Attributes as Attr exposing (class, css, href)
import Html.Styled.Events as Events exposing (..)
import Nostr.Reactions exposing (Interactions)
import Svg.Loaders
import Tailwind.Utilities as Tw
import Tailwind.Theme as Theme
import Ui.Styles exposing (Styles, Theme, darkMode, fontFamilyUnbounded, stylesForTheme)

pageLoadingIndicator : Html msg
pageLoadingIndicator =
    Svg.Loaders.rings []
    |> Html.fromUnstyled

thinBorderButton : msg -> String -> Html msg
thinBorderButton onClickMsg title =
    button
        [ css
            [ Tw.bg_color Theme.gray_200
            , Tw.py_2
            , Tw.px_4
            , Tw.rounded_full
            , Css.hover
                [ Tw.bg_color Theme.gray_300
                ]
            ]
        , Events.onClick onClickMsg
        ]
        [ text title ]

linkButton : String -> String -> Html msg
linkButton title url =
    a
        [ css
            [ Tw.bg_color Theme.gray_200
            , Tw.py_2
            , Tw.px_4
            , Tw.rounded_full
            , Css.hover
                [ Tw.bg_color Theme.gray_300
                ]
            ]
        , Attr.href url
        ]
        [ text title ]


modalDialog : String -> List (Html msg) -> msg -> Html msg
modalDialog title content onClose =
    div
        [ css
            [ Tw.fixed
            , Tw.inset_0
            , Tw.bg_color Theme.gray_900
            , Tw.bg_opacity_50
            , Tw.flex
            , Tw.justify_center
            , Tw.items_center
            , Tw.z_50
            ]
        ]
        [ div
            [ css
                [ Tw.bg_color Theme.white
                , Tw.rounded_lg
                , Tw.shadow_lg
                , Tw.p_8
                , Tw.w_96
                ]
            ]
            [ div
                [ css
                    [ Tw.flex
                    , Tw.justify_between
                    , Tw.items_center
                    , Tw.border_b
                    , Tw.pb_4
                    ]
                ]
                [ h2
                    [ css
                        [ Tw.text_lg
                        , Tw.font_semibold
                        , Tw.text_color Theme.gray_800
                        ]
                    ]
                    [ text title ]
                , button
                    [ css
                        [ Tw.text_color Theme.gray_400
                        , Css.hover
                            [ Tw.text_color Theme.gray_600
                            ]
                        ]
                    , Attr.id "close-modal"
                    , Events.onClick onClose
                    ]
                    [ text " ✕ " ]
                ]
            , div
                []
                content
            ]
        ]

viewInteractions : Styles msg -> BrowserEnv -> Interactions -> Html msg
viewInteractions styles browserEnv interactions =
    let
        bookmarkIcon =
            if interactions.isBookmarked then
                Icon.MaterialIcon Icon.MaterialOutlineBookmarkAdded 30 Icon.Inherit
            else
                Icon.MaterialIcon Icon.MaterialOutlineBookmarkAdd 30 Icon.Inherit
    in
    div
        [ css
            [ Tw.justify_start
            , Tw.items_start
            , Tw.gap_6
            , Tw.inline_flex
            ]
        ]
        [ viewReactions styles (Icon.FeatherIcon FeatherIcons.messageSquare) (Maybe.map String.fromInt interactions.notes)
        , viewReactions styles (Icon.FeatherIcon FeatherIcons.edit) (Maybe.map String.fromInt interactions.reactions)
        , viewReactions styles (Icon.FeatherIcon FeatherIcons.repeat) (Maybe.map String.fromInt interactions.reposts)
        , viewReactions styles (Icon.FeatherIcon FeatherIcons.zap) (Maybe.map (formatZapNum browserEnv) interactions.zaps)
        , viewReactions styles bookmarkIcon (Maybe.map String.fromInt interactions.bookmarks)
        ]
                
viewReactions : Styles msg -> Icon -> Maybe String -> Html msg
viewReactions styles icon maybeCount =
    div
        (styles.colorStyleLabel ++
        [ css
            [ Tw.rounded_3xl
            , Tw.justify_center
            , Tw.items_center
            , Tw.gap_1
            , Tw.flex
            ]
        ])
        [ div
            [ css
                [ Tw.w_5
                , Tw.h_5
                , Tw.px_0_dot_5
                , Tw.py_0_dot_5
                , Tw.justify_center
                , Tw.items_center
                , Tw.flex
                ]
            ]
            [ Icon.view icon]
        , div
            [ ] [ text (maybeCount |> Maybe.withDefault "0" ) ]
        ]

formatZapNum : BrowserEnv -> Int -> String
formatZapNum browserEnv bigNum =
    browserEnv.formatNumber "0 a" <| toFloat bigNum
