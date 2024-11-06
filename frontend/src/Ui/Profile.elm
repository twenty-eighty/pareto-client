module Ui.Profile exposing (..)

import BrowserEnv exposing (BrowserEnv)
import Graphics
import Html.Styled as Html exposing (Html, Attribute, a, article, aside, button, div, h2, h3, h4, img, main_, p, span, text)
import Html.Styled.Attributes as Attr exposing (class, css, href)
import Html.Styled.Events as Events exposing (..)
import Http
import Nostr.Profile exposing (Profile, ProfileValidation(..), profileDisplayName)
import Nostr.Shared exposing (httpErrorToString)
import Nostr.Types exposing (PubKey)
import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw
import Tailwind.Theme as Theme
import Time
import Ui.Links exposing (linkElementForProfile, linkElementForProfilePubKey)

defaultProfileImage : String
defaultProfileImage =
    "/images/avatars/placeholder_01.png"


viewProfileSmall : Profile -> ProfileValidation -> Html msg
viewProfileSmall profile validationStatus =
        div
            [ css
                [ Tw.flex
                , Tw.flex_col
                , Tw.space_y_2
                , Tw.mb_4
                ]
            ]
            [ div
                [ css
                    [ Tw.flex
                    , Tw.flex_row
                    , Tw.items_center
                    , Tw.space_x_2
                    , Tw.mb_4
                    ]
                ]
                [ viewProfileImageSmall (linkElementForProfile profile) profile.picture validationStatus
                , h2
                    [ css
                        [ Tw.text_sm
                        , Tw.font_semibold
                        , Tw.text_color Theme.gray_800
                        ]
                    ]
                    [ text (profileDisplayName profile.pubKey profile) ]
                ]
            ]



viewProfile : Profile -> ProfileValidation -> Html msg
viewProfile profile validationStatus =
        div
            [ css
                [ Tw.flex
                , Tw.flex_col
                , Tw.space_y_2
                , Tw.mb_4
                ]
            ]
            [ viewBanner profile.banner
            , div
                [ css
                    [ Tw.flex
                    , Tw.flex_row
                    , Tw.items_center
                    , Tw.space_x_2
                    , Tw.mb_4
                    ]
                ]
                [ viewProfileImage (div [ css [ Tw.flex_none ]]) profile.picture validationStatus
                , div
                    [ css
                        [ Tw.flex
                        , Tw.flex_col
                        , Tw.space_y_2
                        , Tw.flex_grow
                        , Tw.mb_4
                        ]
                    ]
                    [ h2
                        [ css
                            [ Tw.text_2xl
                            , Tw.text_color Theme.gray_800
                            ]
                        ]
                        [ text (profileDisplayName profile.pubKey profile) ]
                    , p
                        [ css
                            [ Tw.text_color Theme.gray_600
                            , Tw.text_sm
                            , Tw.mb_4
                            , Tw.w_auto
                            ]
                        ]
                        [ text (profile.about |> Maybe.withDefault "") ]
                    ]
                ]
            ]

viewBanner : Maybe String -> Html msg
viewBanner maybeImage =
    case maybeImage of
        Just image ->
            div
                [ css
                    [ Tw.relative
                    , Tw.mb_4
                    ]
                ]
                [ img
                    [ Attr.src image
                    , Attr.alt "Banner Image"
                    , css
                        [ Tw.rounded_lg
                        , Tw.w_full
                        , Tw.object_cover
                        ]
                    ]
                    []
                ]

        Nothing ->
            div [][]

viewProfilePubKey : PubKey -> Html msg
viewProfilePubKey pubKey =
            div
                [ css
                    [ Tw.flex
                    , Tw.items_center
                    , Tw.space_x_2
                    , Tw.mb_4
                    ]
                ]
                [ viewProfileImage (linkElementForProfilePubKey pubKey) (Just defaultProfileImage) ValidationUnknown
                , h2
                    [ css
                        [ Tw.text_sm
                        , Tw.font_semibold
                        , Tw.text_color Theme.gray_800
                        ]
                    ]
                    [ text pubKey ]
                ]

viewProfileImage : (List (Html msg) -> Html msg) -> Maybe String -> ProfileValidation -> Html msg
viewProfileImage linkElement maybeImage validationStatus =
    let
        image =
            maybeImage
            |> Maybe.withDefault defaultProfileImage
    in
    div
        [ css
            [ Tw.relative
            ]
        ]
        [ linkElement
            [ img
                [ Attr.src image
                , Attr.alt "Avatar"
                , css
                    [ Tw.min_w_28
                    , Tw.min_h_28
                    , Tw.max_w_28
                    , Tw.max_h_28
                    , Tw.p_1
                    , Tw.rounded_full
                    ]
                ]
                []
            ]
        , div 
            [ css
                [  Tw.absolute
                , Tw.top_0
                , Tw.right_0
                , Tw.text_color Theme.gray_400
                , Tw.w_4
                , Tw.h_4
                ]
            ]
            [ validationIcon 24 validationStatus
            ]
        ]

validationIcon : Int -> ProfileValidation -> Html msg
validationIcon width validation =
    case validation of
        ValidationUnknown ->
            Graphics.featherMehIcon width

        ValidationPending ->
            Graphics.featherMehIcon width

        ValidationNameMissing ->
            Graphics.featherFrownIcon width

        ValidationNotMatchingPubKey ->
            Graphics.featherFrownIcon width

        ValidationNetworkError _ ->
            Graphics.featherFrownIcon width

        ValidationSucceeded ->
            Graphics.featherSmileIcon width


validationTooltipText : ProfileValidation -> String
validationTooltipText status =
    case status of
        ValidationUnknown ->
            "Unknown profile validation status"

        ValidationPending ->
            "Validating profile..."

        ValidationNameMissing ->
            "Error validating profile: name missing in nostr.json"

        ValidationNotMatchingPubKey ->
            "Error validating profile: public key on server doesn't match"

        ValidationNetworkError error ->
            "Network error validating profile: " ++ httpErrorToString error

        ValidationSucceeded ->
            "Profile validated successfully"


viewProfileImageSmall : (List (Html msg) -> Html msg) -> Maybe String -> ProfileValidation -> Html msg
viewProfileImageSmall linkElement maybeImage validationStatus =
    let
        image =
            maybeImage
            |> Maybe.withDefault defaultProfileImage
    in
    div
        [ css
            [ Tw.relative
            ]
        ]
        [ linkElement
            [ img
                [ Attr.src image
                , Attr.alt "Avatar"
                , css
                    [ Tw.w_10
                    , Tw.h_10
                    , Tw.p_1
                    , Tw.rounded_full
                    ]
                ]
                []
            ]
        , div 
            [ css
                [ Tw.absolute
                , Tw.top_0
                , Tw.right_0
                , Tw.text_color Theme.gray_400
                , Tw.max_w_2
                , Tw.max_h_2
                ]
            ]
            [ validationIcon 16 validationStatus
            ]
        ]

timeParagraph : BrowserEnv -> Maybe Time.Posix -> Html msg
timeParagraph browserEnv maybePublishedAt =
    case maybePublishedAt of
        Just publishedAt ->
            p
                [ css
                    [ Tw.text_xs
                    , Tw.text_color Theme.gray_500
                    ]
                ]
                [ text <| BrowserEnv.formatDate browserEnv publishedAt ]

        Nothing ->
            div [][]
