module Ui.Shared exposing (..)

import BrowserEnv exposing (BrowserEnv)
import Color
import Components.Icon as Icon exposing (Icon)
import Css
import FeatherIcons
import Html.Styled as Html exposing (Html, a, button, div, h2, text)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events as Events exposing (..)
import Json.Encode as Encode
import Nostr.Nip19 as Nip19 exposing (NIP19Type(..))
import Nostr.Reactions exposing (Interactions)
import Nostr.Types exposing (PubKey)
import Set exposing (Set)
import Svg.Loaders
import Tailwind.Breakpoints as Bp
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw
import Ui.Styles exposing (Styles, Theme, stylesForTheme)


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


type alias PreviewData msg =
    { pubKey : PubKey
    , noteId : Maybe String
    , relays : Set String
    , actions : Actions msg
    , interactions : Interactions
    }


viewInteractions : Styles msg -> BrowserEnv -> PreviewData msg -> Html msg
viewInteractions styles browserEnv previewData =
    let
        actions =
            previewData.actions

        interactions =
            previewData.interactions

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
        [ viewReactions styles (Icon.FeatherIcon FeatherIcons.messageSquare) Nothing (Maybe.map String.fromInt interactions.notes) previewData
        , viewReactions styles reactionIcon reactionMsg (Maybe.map String.fromInt interactions.reactions) previewData
        , viewReactions styles (Icon.FeatherIcon FeatherIcons.repeat) Nothing (Maybe.map String.fromInt interactions.reposts) previewData
        , viewReactions styles (Icon.FeatherIcon FeatherIcons.zap) Nothing (Maybe.map (formatZapNum browserEnv) interactions.zaps) previewData
        , viewReactions styles bookmarkIcon bookmarkMsg (Maybe.map String.fromInt interactions.bookmarks) previewData
        ]


viewReactions : Styles msg -> Icon -> Maybe msg -> Maybe String -> PreviewData msg -> Html msg
viewReactions styles icon maybeMsg maybeCount previewData =
    let
        onClickAttr =
            case maybeMsg of
                Just msg ->
                    [ Events.onClick msg, css [ Tw.cursor_pointer ] ]

                Nothing ->
                    []

        maybeNpub =
            Nip19.encode (Npub previewData.pubKey)
                |> Result.toMaybe

        nostrZapAttrs =
            case ( icon, maybeNpub ) of
                ( Icon.FeatherIcon featherIcon, Just npub ) ->
                    if featherIcon == FeatherIcons.zap then
                        [ Attr.id ("zap-button-" ++ npub)
                        , Attr.attribute "data-npub" npub
                        , Attr.attribute "data-relays" (Set.foldl (\r acc -> r ++ "," ++ acc) "" previewData.relays)
                        , Attr.attribute "data-button-color" "#334155"
                        , css [ Tw.cursor_pointer ]
                        ]
                            ++ Maybe.withDefault [] (Maybe.map (\noteId -> [ Attr.attribute "data-note-id" noteId ]) previewData.noteId)

                    else
                        []

                _ ->
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
                ++ nostrZapAttrs
                ++ [ css
                        [ Tw.w_5
                        , Tw.h_5
                        , Tw.px_0_dot_5
                        , Tw.py_0_dot_5
                        , Tw.justify_center
                        , Tw.items_center
                        , Tw.flex
                        , Tw.cursor_pointer
                        ]
                   ]
            )
            [ Icon.view icon
            , Html.node "js-zap-component"
                ((maybeNpub
                    |> Maybe.map
                        (\npub ->
                            [ Attr.property "buttonId" (Encode.string ("zap-button-" ++ npub))
                            ]
                        )
                 )
                    |> Maybe.withDefault [ Attr.hidden True ]
                )
                []
            ]
        , div
            []
            [ text (maybeCount |> Maybe.withDefault "0") ]
        ]


formatZapNum : BrowserEnv -> Int -> String
formatZapNum browserEnv milliSats =
    browserEnv.formatNumber "0 a" <| toFloat (milliSats // 1000)
