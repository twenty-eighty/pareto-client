module Ui.ShortNote exposing (..)

import BrowserEnv exposing (BrowserEnv)
import Dict exposing (Dict)
import Html.Styled as Html exposing (Html, a, article, aside, br, button, div, h1, h3, h4, img, main_, p, span, text)
import Html.Styled.Attributes as Attr exposing (class, css, href)
import Html.Styled.Events as Events exposing (..)
import Nostr
import Nostr.Profile exposing (Author, Profile, ProfileValidation(..))
import Nostr.Reactions exposing (Interactions)
import Nostr.ShortNote exposing (ShortNote)
import Nostr.Types exposing (EventId, PubKey)
import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw
import Tailwind.Theme as Theme
import Ui.Profile exposing (defaultProfileImage, profileDisplayName, validationIcon)
import Ui.Styles exposing (Styles, Theme)
import Ui.Styles exposing (stylesForTheme)


type alias ShortNotesViewData msg =
    { theme : Theme
    , browserEnv : BrowserEnv
    , nostr : Nostr.Model
    , userPubKey : Maybe PubKey
    , onBookmark : Maybe ((EventId -> msg), (EventId -> msg)) -- msgs for adding/removing a bookmark
    }

type alias ShortNoteViewData =
    { author : Author
    , interactions : Interactions
    }


viewShortNote : ShortNotesViewData msg -> ShortNoteViewData -> ShortNote -> Html msg
viewShortNote shortNotesViewData shortNoteViewData shortNote =
    let
        styles =
            stylesForTheme shortNotesViewData.theme
    in
    div
        [ Attr.class "animated"
        ]
        [ a
            [ Attr.attribute "data-event-bech32" "note1qqqqskvlgq7p7cqh8ygqvet68re2sre7l90ud56llkz6mxjmht0qjxjv6f"
            , Attr.href "/e/note1qqqqskvlgq7p7cqh8ygqvet68re2sre7l90ud56llkz6mxjmht0qjxjv6f"
            , Attr.attribute "link" ""
            , css
                [ Tw.relative
                , Tw.flex
                , Tw.flex_col
                , Tw.px_5
                , Tw.py_3
                ]
            ]
            [ div
                [ Attr.class "_header_qj1dj_35"
                , css
                    [ Tw.flex
                    , Tw.flex_col
                    , Tw.justify_between
                    ]
                ]
                [ div
                    [ Attr.class "_repostedBy_qj1dj_42"
                    ]
                    [ div
                        [ Attr.class "_repostIcon_qj1dj_287"
                        ]
                        []
                    , span []
                        [ a
                            [ Attr.href "/dergigi"
                            , Attr.class "inactive"
                            , Attr.attribute "link" ""
                            ]
                            [ text "Gigi and 5 others" ]
                        , span []
                            [ text "Reposted" ]
                        ]
                    ]
                ]
            , div
                [ Attr.class "_content_qj1dj_162"
                , Attr.attribute "grid-template-columns" "50px 1fr"
                , css
                    [ Tw.grid
                    , Tw.grid_cols_2
                    ]
                ]
                [ div
                    [ Attr.class "_leftSide_qj1dj_166"
                    , css
                        [ Tw.box_border
                        , Tw.cursor_pointer
                        , Tw.block
                        , Tw.w_11
                        , Tw.h_11
                        , Tw.mr_2
                        , Tw.border_0
                        ]
                    ]
                    [ a
                        [ Attr.href "/utxo"
                        , Attr.class "inactive"
                        , Attr.attribute "link" ""
                        ]
                        [ div
                            [ Attr.class "_vsAvatar_1sd2j_321   "
                            , Attr.attribute "data-user" "e2ccf7cf20403f3f2a4a55b328f0de3be38558a7d5f33632fdaaefc726c1c8eb"
                            , css
                                [ Tw.relative
                                , Tw.box_border
                                , Tw.cursor_pointer
                                , Tw.block
                                , Tw.w_11
                                , Tw.h_11
                                , Tw.p_1
                                , Tw.border_0
                                ]
                            ]
                            [ div
                                [ Attr.class "_missingBack_1sd2j_118 "
                                , css
                                    [ Tw.box_border
                                    , Tw.cursor_pointer
                                    , Tw.block
                                    , Tw.w_10
                                    , Tw.h_10
                                    , Tw.border_0
                                    ]
                                ]
                                [ img
                                    [ Attr.alt "avatar"
                                    , Attr.src "https://primal.b-cdn.net/media-cache?s=s&a=1&u=https%3A%2F%2Fi.nostr.build%2F6G6wW.gif"
                                    , css
                                        [ Tw.rounded_full
                                        , Tw.object_cover
                                        , Tw.box_content
                                        , Tw.overflow_hidden
                                        , Tw.w_10
                                        , Tw.h_10
                                        , Tw.border_0
                                        ]
                                    ]
                                    []
                                ]
                            ]
                        ]
                    ]
                , div
                    [ Attr.class "_rightSide_qj1dj_175"
                    ]
                    [ div
                        [ Attr.class "_authorInfo_qj1dj_231"
                        ]
                        [ span
                            [ Attr.class "_userName_qj1dj_245"
                            ]
                            [ text "utxo the webmaster 🧑‍💻" ]
                        , div
                            [ Attr.attribute "data-user" "e2ccf7cf20403f3f2a4a55b328f0de3be38558a7d5f33632fdaaefc726c1c8eb"
                            , Attr.class "_verificationIcon_1ga43_10"
                            ]
                            [ span
                                [ Attr.class "_verifiedIcon_1ga43_1"
                                ]
                                []
                            ]
                        , span
                            [ Attr.class "_verification_qj1dj_271"
                            , Attr.title "utxo.one"
                            ]
                            [ text "utxo.one" ]
                        , span
                            [ Attr.class "_time_qj1dj_255"
                            , Attr.title "12/23/2024, 2:59:13 PM"
                            ]
                            [ div
                                [ Attr.class "_ellipsisIcon_qj1dj_264"
                                ]
                                []
                            , text "4 hr." ]
                        ]
                    , div
                        [ Attr.class "_upRightFloater_qj1dj_107"
                        ]
                        [ div
                            [ Attr.class "_context_qj1dj_314"
                            ]
                            [ button
                                [ Attr.class "_contextButton_qj1dj_314"
                                ]
                                [ div
                                    [ Attr.class "_contextIcon_qj1dj_330"
                                    ]
                                    []
                                ]
                            ]
                        ]
                    , div
                        [ Attr.class "_message_qj1dj_60"
                        ]
                        [ div
                            [ Attr.id "note_note1qqqqskvlgq7p7cqh8ygqvet68re2sre7l90ud56llkz6mxjmht0qjxjv6f"
                            , Attr.class "_parsedNote_9gwql_1 "
                            ]
                            [ viewContent styles shortNote.content ]
                        ]
                    , div
                        [ Attr.class "_zapHighlightsCompact_qj1dj_570"
                        ]
                        [ div
                            [ Attr.class "_zapWrap_qj1dj_583 "
                            , Attr.attribute "data-index" "0"
                            , Attr.style "z-index" "12"
                            ]
                            [ button
                                [ Attr.class "_topZap_qj1dj_445 "
                                , Attr.style "z-index" "12"
                                ]
                                [ div
                                    [ Attr.class "_microAvatar_1sd2j_133   "
                                    , Attr.attribute "data-user" "dd840e433b978795bb35a408bc61ef7e99688ba0b166f8cb4c7cebcb5318ecb0"
                                    ]
                                    [ div
                                        [ Attr.class "_missingBack_1sd2j_118 "
                                        ]
                                        [ img
                                            [ Attr.alt "avatar"
                                            , Attr.src "https://primal.b-cdn.net/media-cache?s=s&a=1&u=https%3A%2F%2Fi.postimg.cc%2FQxdLv3dN%2F24d43056ae19f57cd5e8f08438d073a80e011902dfe0495c30440154da0dbdca.gif"
                                            ]
                                            []
                                        ]
                                    ]
                                , div
                                    [ Attr.class "_topZapIcon_qj1dj_490"
                                    ]
                                    []
                                , div
                                    [ Attr.class "_amount_qj1dj_493"
                                    ]
                                    [ text "3,333" ]
                                , div
                                    [ Attr.class "_description_qj1dj_499"
                                    ]
                                    []
                                ]
                            ]
                        , div
                            [ Attr.class "_zapWrap_qj1dj_583 "
                            , Attr.attribute "data-index" "1"
                            , Attr.style "z-index" "11"
                            ]
                            [ button
                                [ Attr.class "_topZap_qj1dj_445 _compact_qj1dj_612"
                                , Attr.style "z-index" "11"
                                ]
                                [ div
                                    [ Attr.class "_microAvatar_1sd2j_133 _legend_1sd2j_1 _legend_1sd2j_1 _legend_GOLD_1sd2j_1 undefined _legend_glow_GOLD_1sd2j_55 "
                                    , Attr.attribute "data-user" "386058f50fb3ab679f9bcae74d731dea693874688d3064a504ef5f0fd5cdecb9"
                                    ]
                                    [ div
                                        [ Attr.class "_missingBack_1sd2j_118 "
                                        ]
                                        [ img
                                            [ Attr.alt "avatar"
                                            , Attr.src "https://primal.b-cdn.net/media-cache?s=s&a=1&u=https%3A%2F%2Fm.primal.net%2FMkIg.jpg"
                                            ]
                                            []
                                        ]
                                    ]
                                ]
                            ]
                        , div
                            [ Attr.class "_zapWrap_qj1dj_583 "
                            , Attr.attribute "data-index" "2"
                            , Attr.style "z-index" "10"
                            ]
                            [ button
                                [ Attr.class "_topZap_qj1dj_445 _compact_qj1dj_612"
                                , Attr.style "z-index" "10"
                                ]
                                [ div
                                    [ Attr.class "_microAvatar_1sd2j_133   "
                                    , Attr.attribute "data-user" "ff2bfabafea9d5c3b00fe6a285139239012680f3cbf43ac5532a8736a95f8070"
                                    ]
                                    [ div
                                        [ Attr.class "_missingBack_1sd2j_118 "
                                        ]
                                        [ img
                                            [ Attr.alt "avatar"
                                            , Attr.src "https://primal.b-cdn.net/media-cache?s=s&a=1&u=https%3A%2F%2Fm.primal.net%2FKjPX.jpg"
                                            ]
                                            []
                                        ]
                                    ]
                                ]
                            ]
                        , div
                            [ Attr.class "_zapWrap_qj1dj_583 "
                            , Attr.attribute "data-index" "3"
                            , Attr.style "z-index" "9"
                            ]
                            [ button
                                [ Attr.class "_topZap_qj1dj_445 _compact_qj1dj_612"
                                , Attr.style "z-index" "9"
                                ]
                                [ div
                                    [ Attr.class "_microAvatar_1sd2j_133   "
                                    , Attr.attribute "data-user" "6e468422dfb74a5738702a8823b9b28168abab8655faacb6853cd0ee15deee93"
                                    ]
                                    [ div
                                        [ Attr.class "_missingBack_1sd2j_118 "
                                        ]
                                        [ img
                                            [ Attr.alt "avatar"
                                            , Attr.src "https://primal.b-cdn.net/media-cache?s=s&a=1&u=https%3A%2F%2Fdergigi.com%2Fassets%2Fimages%2Favatars%2F09-xmas.png"
                                            ]
                                            []
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    , div
                        [ Attr.class "_footer_qj1dj_448"
                        ]
                        [ div
                            [ Attr.class "_footer_drm5v_28 _short_drm5v_55"
                            ]
                            [ button
                                [ Attr.id "btn_reply_000008599f403c1f6017391006657a38f2a80f3ef95fc6d35ffd85ad9a5bbade"
                                , Attr.class "_stat_drm5v_81 "
                                ]
                                [ div
                                    [ Attr.class "_replyType_drm5v_111 "
                                    ]
                                    [ div
                                        [ Attr.class "_icon_drm5v_100 "
                                        , Attr.style "visibility" "visible"
                                        ]
                                        []
                                    , div
                                        [ Attr.class "_statNumber_drm5v_174"
                                        ]
                                        [ text "4" ]
                                    ]
                                ]
                            , button
                                [ Attr.id "btn_zap_000008599f403c1f6017391006657a38f2a80f3ef95fc6d35ffd85ad9a5bbade"
                                , Attr.class "_stat_drm5v_81 "
                                ]
                                [ div
                                    [ Attr.class "_zapType_drm5v_153 "
                                    ]
                                    [ div
                                        [ Attr.class "_icon_drm5v_100 "
                                        , Attr.style "visibility" "visible"
                                        ]
                                        []
                                    , div
                                        [ Attr.class "_statNumber_drm5v_174"
                                        ]
                                        [ text "5,259" ]
                                    ]
                                ]
                            , button
                                [ Attr.id "btn_like_000008599f403c1f6017391006657a38f2a80f3ef95fc6d35ffd85ad9a5bbade"
                                , Attr.class "_stat_drm5v_81 "
                                ]
                                [ div
                                    [ Attr.class "_likeType_drm5v_90 "
                                    ]
                                    [ div
                                        [ Attr.class "_icon_drm5v_100 "
                                        , Attr.style "visibility" "visible"
                                        ]
                                        []
                                    , div
                                        [ Attr.class "_statNumber_drm5v_174"
                                        ]
                                        [ text "12" ]
                                    ]
                                ]
                            , button
                                [ Attr.id "btn_repost_000008599f403c1f6017391006657a38f2a80f3ef95fc6d35ffd85ad9a5bbade"
                                , Attr.class "_stat_drm5v_81 "
                                , Attr.title "6"
                                ]
                                [ div
                                    [ Attr.class "_repostType_drm5v_132"
                                    ]
                                    [ div
                                        [ Attr.style "visibility" "visible"
                                        , Attr.class "_icon_drm5v_100 "
                                        ]
                                        []
                                    , div
                                        [ Attr.class "_statNumber_drm5v_174"
                                        ]
                                        [ text "6" ]
                                    , div
                                        [ Attr.id "repost_menu_000008599f403c1f6017391006657a38f2a80f3ef95fc6d35ffd85ad9a5bbade"
                                        , Attr.class "_contextMenuOptions_13zfd_43 _noteFooter_13zfd_59"
                                        , Attr.attribute "data-open" "false"
                                        ]
                                        [ button
                                            [ Attr.class "_contextOption_uwsv9_1   "
                                            ]
                                            [ span []
                                                [ text "Repost Note" ]
                                            , div
                                                [ Attr.style "mask" "url(\"/assets/feed_repost-ced4c2cd.svg\") 0px center / 100% no-repeat"
                                                ]
                                                []
                                            ]
                                        , button
                                            [ Attr.class "_contextOption_uwsv9_1   "
                                            ]
                                            [ span []
                                                [ text "Quote Note" ]
                                            , div
                                                [ Attr.style "mask" "url(\"/assets/quote-bec292c0.svg\") 0px center / 100% no-repeat"
                                                ]
                                                []
                                            ]
                                        ]
                                    ]
                                ]
                            , div
                                [ Attr.class "_bookmarkFoot_drm5v_34"
                                ]
                                [ div
                                    [ Attr.class "_bookmark_1ymqz_1"
                                    ]
                                    [ button
                                        [ Attr.type_ "button"
                                        , Attr.class "_ghost_hbp0p_278  _right_1ymqz_33 "
                                        ]
                                        [ div
                                            [ Attr.class "_emptyBookmark_1ymqz_1 "
                                            ]
                                            []
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]

viewContent : Styles msg -> Maybe String -> Html msg
viewContent styles maybeDescription =
    case maybeDescription of
        Just description ->
            p
                (styles.colorStyleGrayscaleText ++ styles.textStyleBody ++
                [ css
                    [ Tw.mb_4
                    ]
                ])
                ( formattedContent description )

        Nothing ->
            div [][]


formattedContent : String -> List (Html msg)
formattedContent content =
    content
    |> String.split "\n"
    |> List.map (\line -> text line) 
    |> List.intersperse (br [][])