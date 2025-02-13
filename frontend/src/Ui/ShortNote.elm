module Ui.ShortNote exposing (..)

import BrowserEnv exposing (BrowserEnv)
import Html.Styled as Html exposing (Html, a, br, button, div, p, span, text)
import Html.Styled.Attributes as Attr exposing (css)
import Nostr
import Nostr.Nip19 as Nip19
import Nostr.Profile exposing (Author(..), ProfileValidation(..), profileDisplayName)
import Nostr.Reactions exposing (Interactions)
import Nostr.ShortNote exposing (ShortNote)
import Nostr.Types exposing (EventId, PubKey)
import Tailwind.Utilities as Tw
import Ui.Links exposing (linkElementForAuthor)
import Ui.Profile
import Ui.Shared exposing (Actions)
import Ui.Styles exposing (Styles, Theme, stylesForTheme)
import Url


type alias ShortNotesViewData msg =
    { theme : Theme
    , browserEnv : BrowserEnv
    , nostr : Nostr.Model
    , userPubKey : Maybe PubKey
    , onBookmark : Maybe ( EventId -> msg, EventId -> msg ) -- msgs for adding/removing a bookmark
    }


type alias ShortNoteViewData msg =
    { author : Author
    , actions : Actions msg
    , interactions : Interactions
    }


viewShortNote : ShortNotesViewData msg -> ShortNoteViewData msg -> ShortNote -> Html msg
viewShortNote shortNotesViewData shortNoteViewData shortNote =
    let
        styles =
            stylesForTheme shortNotesViewData.theme

        ( authorText, maybeProfile, validationStatus ) =
            case shortNoteViewData.author of
                AuthorPubkey pubKey ->
                    ( pubKey, Nothing, ValidationUnknown )

                AuthorProfile profile profileValidation ->
                    ( profileDisplayName profile.pubKey profile, Just profile, profileValidation )

        noteUrl =
            shortNote.id
                |> Nip19.Note
                |> Nip19.encode
                |> Result.toMaybe
                |> Maybe.withDefault ""
    in
    div
        [ Attr.class "animated"
        ]
        [ a
            [ Attr.href noteUrl
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
                , css
                    [ Tw.flex
                    , Tw.flex_row
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
                    [ Ui.Profile.viewProfileImageSmall (linkElementForAuthor shortNoteViewData.author validationStatus) maybeProfile validationStatus

                    {-
                       a
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
                    -}
                    ]
                , div
                    [ Attr.class "_rightSide_qj1dj_175"
                    ]
                    [ div
                        [ Attr.class "_authorInfo_qj1dj_231"
                        , css
                            [ Tw.flex
                            , Tw.flex_row
                            , Tw.gap_2
                            ]
                        ]
                        [ span
                            [ Attr.class "_userName_qj1dj_245"
                            ]
                            [ text authorText ]
                        , div
                            [ Attr.attribute "data-user" "e2ccf7cf20403f3f2a4a55b328f0de3be38558a7d5f33632fdaaefc726c1c8eb"
                            , Attr.class "_verificationIcon_1ga43_10"
                            ]
                            [ Ui.Profile.validationIcon 24 validationStatus
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
                            , text <| BrowserEnv.formatDate shortNotesViewData.browserEnv shortNote.createdAt
                            ]
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
                        [ Attr.class "_footer_qj1dj_448"
                        ]
                        [ Ui.Shared.viewInteractions styles shortNotesViewData.browserEnv shortNoteViewData.actions shortNoteViewData.interactions
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
                (styles.colorStyleGrayscaleText
                    ++ styles.textStyleBody
                    ++ [ css
                            [ Tw.mb_4
                            ]
                       ]
                )
                (formattedContent description)

        Nothing ->
            div [] []


formattedContent : String -> List (Html msg)
formattedContent content =
    content
        |> String.split "\n"
        |> List.map
            (\line ->
                if isImageUrl line then
                    Html.img
                        [ Attr.src line
                        ]
                        []

                else
                    text line
            )
        |> List.intersperse (br [] [])


isImageUrl : String -> Bool
isImageUrl line =
    line
        |> Url.fromString
        |> Maybe.map
            (\url ->
                String.endsWith "jpeg" url.path
                    || String.endsWith "jpg" url.path
                    || String.endsWith "png" url.path
                    || String.endsWith "webp" url.path
            )
        |> Maybe.withDefault False
