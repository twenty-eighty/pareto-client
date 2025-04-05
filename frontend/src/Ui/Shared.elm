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
import Json.Encode as Encode
import Nostr
import Nostr.Nip19 as Nip19 exposing (NIP19Type(..))
import Nostr.Reactions exposing (Interactions)
import Nostr.Relay exposing (websocketUrl)
import Nostr.Types exposing (PubKey)
import Pareto exposing (defaultRelays)
import Set exposing (Set)
import Svg.Loaders
import Tailwind.Breakpoints as Bp
import Tailwind.Theme as Theme
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
                [ css
                    [ Tw.max_h_96
                    , Tw.overflow_y_scroll
                    ]
                ]
                content
            ]
        ]


type alias Actions msg =
    { addBookmark : Maybe msg
    , removeBookmark : Maybe msg
    , addReaction : Maybe msg
    , removeReaction : Maybe msg
    , addRepost : Maybe msg
    , startComment : Maybe msg
    }


type alias PreviewData msg =
    { pubKey : PubKey
    , maybeNip19Target : Maybe String
    , zapRelays : Set String
    , actions : Actions msg
    , interactions : Interactions
    }



{-
   Extends the given relays with the inbox relays of the pub-key.
-}


extendedZapRelays : Set String -> Nostr.Model -> Maybe PubKey -> Set String
extendedZapRelays zapRelays nostrModel maybePubKey =
    let
        pubKeyRelays =
            maybePubKey
                |> Maybe.map (pubkeyRelays nostrModel)
                |> Maybe.withDefault Set.empty

        defaultRelays =
            Set.fromList nostrModel.defaultRelays |> Set.map websocketUrl

        candidateRelays =
            Set.union zapRelays pubKeyRelays
    in
    if Set.size candidateRelays == Set.size zapRelays || Set.size candidateRelays == Set.size pubKeyRelays then
        Set.union candidateRelays defaultRelays

    else
        candidateRelays


pubkeyRelays : Nostr.Model -> PubKey -> Set String
pubkeyRelays nostrModel pubKey =
    pubKey
        |> Nostr.getNip65RelaysForPubKey nostrModel
        |> List.map (\( _, relay ) -> websocketUrl relay.urlWithoutProtocol)
        |> Set.fromList


viewInteractions : Styles msg -> BrowserEnv -> PreviewData msg -> String -> Html msg
viewInteractions styles browserEnv previewData instanceId =
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

        ( repostIcon, repostMsg ) =
            case interactions.repost of
                Just _ ->
                    ( Icon.MaterialIcon Icon.MaterialRepeatOn 30 Icon.Inherit
                      -- disable reposting if done already by user
                    , Nothing
                    )

                Nothing ->
                    ( Icon.MaterialIcon Icon.MaterialRepeat 30 Icon.Inherit
                    , actions.addRepost
                    )
    in
    div
        [ css
            [ Tw.justify_start
            , Tw.items_start
            , Tw.gap_6
            , Tw.inline_flex
            ]
        ]
        [ viewReactions styles (Icon.FeatherIcon FeatherIcons.messageSquare) actions.startComment (Maybe.map String.fromInt interactions.notes) previewData instanceId
        , viewReactions styles reactionIcon reactionMsg (Maybe.map String.fromInt interactions.reactions) previewData instanceId
        , viewReactions styles repostIcon repostMsg (Maybe.map String.fromInt interactions.reposts) previewData instanceId
        , viewReactions styles (Icon.FeatherIcon FeatherIcons.zap) Nothing (Maybe.map (formatZapNum browserEnv) interactions.zaps) previewData instanceId
        , viewReactions styles bookmarkIcon bookmarkMsg (Maybe.map String.fromInt interactions.bookmarks) previewData instanceId
        ]


viewReactions : Styles msg -> Icon -> Maybe msg -> Maybe String -> PreviewData msg -> String -> Html msg
viewReactions styles icon maybeMsg maybeCount previewData instanceId =
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
        [ if icon == Icon.FeatherIcon FeatherIcons.zap then
            zapButton previewData.pubKey previewData.maybeNip19Target previewData.zapRelays instanceId

          else
            div
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


zapButton : PubKey -> Maybe String -> Set String -> String -> Html msg
zapButton pubKey maybeNip19Target zapRelays instanceId =
    let
        maybeNip19TargetAttr =
            maybeNip19Target
                |> Maybe.map
                    (\nip19Target ->
                        if String.startsWith "note" nip19Target then
                            [ Attr.attribute "data-note-id" nip19Target ]

                        else
                            [ Attr.attribute "data-naddr" nip19Target ]
                    )

        maybeNpub =
            Nip19.encode (Npub pubKey) |> Result.toMaybe

        ( nostrZapAttributes, zapComponent ) =
            maybeNpub
                |> Maybe.map
                    (\npub ->
                        ( [ Attr.id ("zap-button-" ++ instanceId)
                          , Attr.attribute "data-npub" npub
                          , Attr.attribute "data-relays" (zapRelays |> Set.toList |> String.join ",")
                          , Attr.attribute "data-button-color" "#334155"
                          ]
                            ++ Maybe.withDefault [] maybeNip19TargetAttr
                        , Html.node "js-zap-component"
                            [ Attr.property "buttonId" (Encode.string ("zap-button-" ++ instanceId)) ]
                            []
                        )
                    )
                |> Maybe.withDefault ( [], emptyHtml )
    in
    button
        (nostrZapAttributes
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
        [ Icon.view (Icon.FeatherIcon FeatherIcons.zap), zapComponent ]
