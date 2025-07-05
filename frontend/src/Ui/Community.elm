module Ui.Community exposing (..)

import BrowserEnv exposing (BrowserEnv, Environment)
import Dict exposing (Dict)
import Html.Styled as Html exposing (Html, div, h1, h3, h4, img, p, text)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events exposing (..)
import Nostr.Community exposing (Community, Image, Moderator, communityName)
import Nostr.Profile exposing (Profile, ProfileValidation(..), profileDisplayName)
import Nostr.Types exposing (PubKey)
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw
import Ui.Profile
import Ui.Shared exposing (emptyHtml)
import Ui.Styles exposing (Styles, Theme(..), fontFamilyUnbounded)


viewCommunity : BrowserEnv -> Dict PubKey Profile -> Community -> Html msg
viewCommunity browserEnv profiles community =
    let
        styles =
            Ui.Styles.stylesForTheme ParetoTheme
    in
    div
        (css
            [ Tw.flex
            , Tw.items_center
            , Tw.justify_center
            , Tw.min_h_screen
            ]
            :: styles.colorStyleBackground
        )
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
            , viewName styles <| communityName community
            , viewSummary styles community.description
            , viewModerators browserEnv.environment styles profiles community.moderators
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
            emptyHtml


viewName : Styles msg -> String -> Html msg
viewName styles name =
    h1
        ([ css
            [ Tw.text_4xl
            , Tw.font_bold
            , Tw.mb_2
            ]
         , fontFamilyUnbounded
         ]
            ++ styles.colorStyleGrayscaleTitle
        )
        [ text name
        ]


viewSummary : Styles msg -> Maybe String -> Html msg
viewSummary styles maybeDescription =
    case maybeDescription of
        Just description ->
            p
                (css
                    [ Tw.text_sm
                    , Tw.mb_4
                    ]
                    :: styles.colorStyleGrayscaleText
                )
                [ text description ]

        Nothing ->
            emptyHtml


viewModerators : Environment -> Styles msg -> Dict PubKey Profile -> List Moderator -> Html msg
viewModerators environment styles profiles moderators =
    if not (List.isEmpty moderators) then
        h3
            ([ css
                [ Tw.text_lg
                , Tw.font_bold
                , Tw.mb_2
                ]
             , fontFamilyUnbounded
             ]
                ++ styles.colorStyleGrayscaleText
            )
            [ text "Moderators"

            -- TODO: get actual validation status
            , List.map (\moderator -> viewModerator environment (Dict.get moderator.pubKey profiles) ValidationUnknown moderator) moderators
                |> div []
            ]

    else
        emptyHtml


viewModerator : Environment -> Maybe Profile -> ProfileValidation -> Moderator -> Html msg
viewModerator environment maybeProfile validationStatus moderator =
    case maybeProfile of
        Just profile ->
            viewProfile environment profile validationStatus moderator

        Nothing ->
            viewPubKey moderator.pubKey


viewProfile : Environment -> Profile -> ProfileValidation -> Moderator -> Html msg
viewProfile environment profile validationStatus moderator =
    let
        styles =
            Ui.Styles.stylesForTheme ParetoTheme
    in
    div
        [ css
            [ Tw.flex
            , Tw.items_center
            , Tw.space_x_2
            , Tw.mb_4
            ]
        ]
        [ Ui.Profile.viewProfileImage environment (div []) (Just profile) validationStatus
        , div []
            [ h4
                (css
                    [ Tw.text_sm
                    , Tw.font_semibold
                    ]
                    :: styles.colorStyleGrayscaleTitle
                )
                [ text (profileDisplayName moderator.pubKey profile) ]
            ]
        ]


viewPubKey : String -> Html msg
viewPubKey pubKey =
    let
        styles =
            Ui.Styles.stylesForTheme ParetoTheme
    in
    h4
        (css
            [ Tw.text_sm
            , Tw.font_semibold
            ]
            :: styles.colorStyleGrayscaleText
        )
        [ text pubKey ]
