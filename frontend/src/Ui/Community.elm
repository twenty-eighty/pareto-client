module Ui.Community exposing (..)

import BrowserEnv exposing (BrowserEnv)
import Dict exposing (Dict)
import Html.Styled as Html exposing (Html, a, article, aside, button, div, h1, h3, h4, img, main_, p, span, text)
import Html.Styled.Attributes as Attr exposing (class, css, href)
import Html.Styled.Events as Events exposing (..)
import Nostr.Community exposing (Community, Image, Moderator, communityName)
import Nostr.Profile exposing (Profile, ProfileValidation(..), profileDisplayName)
import Nostr.Types exposing (PubKey)
import Tailwind.Breakpoints as Bp
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw
import Ui.Profile
import Ui.Styles exposing (fontFamilyUnbounded)


viewCommunity : BrowserEnv -> Dict PubKey Profile -> Community -> Html msg
viewCommunity browserEnv profiles community =
    div
        [ css
            [ Tw.flex
            , Tw.items_center
            , Tw.justify_center
            , Tw.min_h_screen
            , Tw.bg_color Theme.gray_100
            ]
        ]
        [ div
            [ css
                [ Tw.bg_color Theme.white
                , Tw.p_6
                , Tw.rounded_lg
                , Tw.shadow_lg
                , Tw.max_w_3xl
                , Tw.space_y_2
                ]
            ]
            [ viewImage community.image
            , viewName <| communityName community
            , viewSummary community.description
            , viewModerators browserEnv profiles community.moderators
            ]
        ]


viewImage : Maybe Image -> Html msg
viewImage maybeImage =
    case maybeImage of
        Just image ->
            div
                [ css
                    [ Tw.relative
                    , Tw.mb_4
                    ]
                ]
                [ img
                    [ Attr.src image.url
                    , Attr.alt "Post Image"
                    , css
                        [ Tw.rounded_lg
                        , Tw.w_full
                        , Tw.object_cover
                        ]
                    ]
                    []
                ]

        Nothing ->
            div [] []


viewName : String -> Html msg
viewName name =
    h1
        [ css
            [ Tw.text_4xl
            , Tw.font_bold
            , Tw.text_color Theme.gray_900
            , Tw.mb_2
            ]
        , fontFamilyUnbounded
        ]
        [ text name
        ]


viewSummary : Maybe String -> Html msg
viewSummary maybeDescription =
    case maybeDescription of
        Just description ->
            p
                [ css
                    [ Tw.text_color Theme.gray_600
                    , Tw.text_sm
                    , Tw.mb_4
                    ]
                ]
                [ text description ]

        Nothing ->
            div [] []


viewModerators : BrowserEnv -> Dict PubKey Profile -> List Moderator -> Html msg
viewModerators browserEnv profiles moderators =
    if not (List.isEmpty moderators) then
        h3
            [ css
                [ Tw.text_lg
                , Tw.font_bold
                , Tw.text_color Theme.gray_900
                , Tw.mb_2
                ]
            , fontFamilyUnbounded
            ]
            [ text "Moderators"

            -- TODO: get actual validation status
            , List.map (\moderator -> viewModerator (Dict.get moderator.pubKey profiles) ValidationUnknown moderator) moderators
                |> div []
            ]

    else
        div [] []


viewModerator : Maybe Profile -> ProfileValidation -> Moderator -> Html msg
viewModerator maybeProfile validationStatus moderator =
    case maybeProfile of
        Just profile ->
            viewProfile profile validationStatus moderator

        Nothing ->
            viewPubKey moderator.pubKey


viewProfile : Profile -> ProfileValidation -> Moderator -> Html msg
viewProfile profile validationStatus moderator =
    div
        [ css
            [ Tw.flex
            , Tw.items_center
            , Tw.space_x_2
            , Tw.mb_4
            ]
        ]
        [ Ui.Profile.viewProfileImage (div []) (Just profile) validationStatus
        , div []
            [ h4
                [ css
                    [ Tw.text_sm
                    , Tw.font_semibold
                    , Tw.text_color Theme.gray_800
                    ]
                ]
                [ text (profileDisplayName moderator.pubKey profile) ]
            ]
        ]


viewPubKey : String -> Html msg
viewPubKey pubKey =
    h4
        [ css
            [ Tw.text_sm
            , Tw.font_semibold
            , Tw.text_color Theme.gray_800
            ]
        ]
        [ text pubKey ]
