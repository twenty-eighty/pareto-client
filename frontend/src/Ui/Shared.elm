module Ui.Shared exposing (..)

import BrowserEnv exposing (BrowserEnv)
import Css
import Erl
import Html.Styled as Html exposing (Html, a, button, div, h2, text)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events as Events exposing (..)
import Nostr.ConfigCheck as ConfigCheck
import Nostr.Nip19 exposing (NIP19Type(..))
import Pareto
import Svg.Loaders
import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw
import Ui.Styles exposing (Styles, Theme, darkMode, stylesForTheme)


emptyHtml : Html msg
emptyHtml =
    text ""


extendUrlForScaling : Int -> String -> String
extendUrlForScaling width urlString =
    let
        parsed =
            urlString
                |> Erl.parse
    in
    if isNip96Server parsed then
        parsed
            -- add NIP-96 scaling parameter
            |> Erl.addQuery "w" (String.fromInt width)
            |> Erl.toString

    else
        urlString


countBadge : Int -> String
countBadge count =
    case count of
        1 ->
            "①"

        2 ->
            "②"

        3 ->
            "③"

        4 ->
            "④"

        5 ->
            "⑤"

        6 ->
            "⑥"

        7 ->
            "⑦"

        8 ->
            "⑧"

        9 ->
            "⑨"

        10 ->
            "⑩"

        11 ->
            "⑪"

        12 ->
            "⑫"

        13 ->
            "⑬"

        14 ->
            "⑭"

        15 ->
            "⑮"

        16 ->
            "⑯"

        17 ->
            "⑰"

        18 ->
            "⑱"

        19 ->
            "⑲"

        20 ->
            "⑳"

        otherNumber ->
            "(" ++ String.fromInt otherNumber ++ ")"


viewConfigIssues : BrowserEnv -> String -> List ConfigCheck.Issue -> Html msg
viewConfigIssues browserEnv title issues =
    case issues of
        [] ->
            emptyHtml

        profileIssues ->
            Html.div
                [ css
                    [ Tw.flex
                    , Tw.flex_col
                    , Tw.mb_4
                    ]
                ]
                [ Html.span [ css [ Tw.font_bold ] ] [ Html.text title ]
                , profileIssues
                    |> List.map (ConfigCheck.issueText browserEnv)
                    |> List.map viewIssueText
                    |> Html.ul
                        [ css
                            [ Tw.list_inside
                            ]
                        ]
                ]


viewIssueText : ConfigCheck.IssueText -> Html msg
viewIssueText { message, explanation, solution } =
    Html.li
        [ css
            [ Tw.list_disc
            ]
        ]
        [ Html.span [ css [ Tw.italic ] ] [ Html.text message ]
        , Html.text <| " - " ++ explanation
        , Html.p [ css [ Tw.text_sm ] ] [ Html.text solution ]
        ]


isNip96Server : Erl.Url -> Bool
isNip96Server url =
    List.member (String.join "." url.host)
        [ Pareto.paretoNip96Server
        ]


pageLoadingIndicator : Html msg
pageLoadingIndicator =
    Svg.Loaders.rings []
        |> Html.fromUnstyled


thinBorderButton : Styles msg -> msg -> String -> Html msg
thinBorderButton styles onClickMsg title =
    button
        [ css
            [ Tw.py_2
            , Tw.px_4
            , Tw.rounded_full
            , Tw.bg_color styles.color4
            , Css.hover
                [ Tw.bg_color styles.color4 ]
            , darkMode
                [ Tw.bg_color styles.color4DarkMode
                , Css.hover [ Tw.bg_color styles.color4DarkMode ]
                ]
            ]
        , Events.onClick onClickMsg
        ]
        [ text title ]


linkButton : Styles msg -> String -> String -> Html msg
linkButton styles title url =
    a
        [ css
            [ Tw.py_2
            , Tw.px_4
            , Tw.rounded_full
            , Tw.bg_color styles.color4
            , Css.hover
                [ Tw.bg_color styles.color4 ]
            , darkMode [ Tw.bg_color styles.color4DarkMode, Css.hover [ Tw.bg_color styles.color4DarkMode ] ]
            ]
        , Attr.href url
        ]
        [ text title ]


modalDialog : Theme -> String -> List (Html msg) -> msg -> Html msg
modalDialog theme title content onClose =
    let
        styles =
            stylesForTheme theme
    in
    div
        (css
            [ Tw.fixed
            , Tw.inset_0
            , Tw.bg_opacity_50
            , Tw.flex
            , Tw.justify_center
            , Tw.items_center
            , Tw.z_50
            ]
            :: styles.colorStyleBackground
        )
        [ div
            (styles.colorStyleBackground
                ++ [ css
                        [ Tw.rounded_lg
                        , Tw.shadow_lg
                        , Tw.drop_shadow_md
                        , Tw.backdrop_blur_md
                        , Tw.shadow_color styles.color2
                        , darkMode
                            [ Tw.shadow_color styles.color2DarkMode
                            ]
                        , Tw.p_8
                        , Tw.max_w_sm
                        , Bp.sm
                            [ Tw.max_w_md
                            ]
                        , Bp.md
                            [ Tw.max_w_lg
                            ]
                        ]
                   ]
            )
            [ div
                [ css
                    [ Tw.flex
                    , Tw.justify_between
                    , Tw.items_center
                    , Tw.border_b
                    , Tw.pb_4
                    , Tw.gap_4
                    ]
                ]
                [ h2
                    [ css
                        [ Tw.text_lg
                        , Tw.font_semibold
                        , Tw.text_color styles.color4
                        , darkMode [ Tw.text_color styles.color4DarkMode ]
                        ]
                    ]
                    [ text title ]
                , button
                    ([ css
                        [ Css.hover
                            [ Tw.text_color styles.color2 ]
                        , darkMode
                            [ Css.hover
                                [ Tw.text_color styles.color2DarkMode ]
                            ]
                        ]
                     , Attr.id "close-modal"
                     , Events.onClick onClose
                     ]
                        ++ styles.colorStyleGrayscaleText
                    )
                    [ text " ✕ " ]
                ]
            , div
                [ css
                    [ Tw.max_h_96
                    , Tw.overflow_y_auto
                    ]
                ]
                content
            ]
        ]
