module Ui.Shared exposing (..)

import BrowserEnv exposing (BrowserEnv)
import Color
import Components.Icon as Icon exposing (Icon)
import Css
import Erl
import FeatherIcons
import Html.Styled as Html exposing (Html, a, button, div, h2, text)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events as Events exposing (..)
import Nostr.Reactions exposing (Interactions)
import Pareto
import Svg.Loaders
import Tailwind.Breakpoints as Bp
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw
import Ui.Styles exposing (Styles, Theme, stylesForTheme)


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


isNip96Server : Erl.Url -> Bool
isNip96Server url =
    List.member (String.join "." url.host)
        [ Pareto.paretoNip96Server
        ]


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


modalDialog : Theme -> String -> List (Html msg) -> msg -> Html msg
modalDialog theme title content onClose =
    let
        styles =
            stylesForTheme theme
    in
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
            (styles.colorStyleBackground
                ++ [ css
                        [ Tw.rounded_lg
                        , Tw.shadow_lg
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


type alias Actions msg =
    { addBookmark : Maybe msg
    , removeBookmark : Maybe msg
    , addReaction : Maybe msg
    , removeReaction : Maybe msg
    }


viewInteractions : Styles msg -> BrowserEnv -> Actions msg -> Interactions -> Html msg
viewInteractions styles browserEnv actions interactions =
    let
        ( bookmarkIcon, bookmarkMsg ) =
            if interactions.isBookmarked then
                ( Icon.MaterialIcon Icon.MaterialOutlineBookmarkAdded 30 Icon.Inherit, actions.removeBookmark )

            else
                ( Icon.MaterialIcon Icon.MaterialOutlineBookmarkAdd 30 Icon.Inherit, actions.addBookmark )

        ( reactionIcon, reactionMsg ) =
            case interactions.reaction of
                Just _ ->
                    ( Icon.MaterialIcon Icon.MaterialFavorite 30 (Icon.Color (Color.fromRgba { red = 1.0, green = 0.0, blue = 0.0, alpha = 1.0 })), actions.removeReaction )

                Nothing ->
                    ( Icon.MaterialIcon Icon.MaterialFavoriteBorder 30 Icon.Inherit, actions.addReaction )
    in
    div
        [ css
            [ Tw.justify_start
            , Tw.items_start
            , Tw.gap_6
            , Tw.inline_flex
            ]
        ]
        [ viewReactions styles (Icon.FeatherIcon FeatherIcons.messageSquare) Nothing (Maybe.map String.fromInt interactions.notes)
        , viewReactions styles reactionIcon reactionMsg (Maybe.map String.fromInt interactions.reactions)
        , viewReactions styles (Icon.FeatherIcon FeatherIcons.repeat) Nothing (Maybe.map String.fromInt interactions.reposts)
        , viewReactions styles (Icon.FeatherIcon FeatherIcons.zap) Nothing (Maybe.map (formatZapNum browserEnv) interactions.zaps)
        , viewReactions styles bookmarkIcon bookmarkMsg (Maybe.map String.fromInt interactions.bookmarks)
        ]


viewReactions : Styles msg -> Icon -> Maybe msg -> Maybe String -> Html msg
viewReactions styles icon maybeMsg maybeCount =
    let
        onClickAttr =
            case maybeMsg of
                Just msg ->
                    [ Events.onClick msg, css [ Tw.cursor_pointer ] ]

                Nothing ->
                    []
    in
    div
        (styles.colorStyleLabel
            ++ [ css
                    [ Tw.rounded_3xl
                    , Tw.justify_center
                    , Tw.items_center
                    , Tw.gap_1
                    , Tw.flex
                    ]
               ]
        )
        [ div
            (onClickAttr
                ++ [ css
                        [ Tw.w_5
                        , Tw.h_5
                        , Tw.px_0_dot_5
                        , Tw.py_0_dot_5
                        , Tw.justify_center
                        , Tw.items_center
                        , Tw.flex
                        ]
                   ]
            )
            [ Icon.view icon ]
        , div
            []
            [ text (maybeCount |> Maybe.withDefault "0") ]
        ]


formatZapNum : BrowserEnv -> Int -> String
formatZapNum browserEnv milliSats =
    browserEnv.formatNumber "0 a" <| toFloat (milliSats // 1000)
