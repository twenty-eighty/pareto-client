module Ui.ArticleFigma exposing (..)

import BrowserEnv exposing (BrowserEnv)
import Html.Styled as Html exposing (Html, a, article, aside, button, div, h2, h3, h4, img, main_, p, span, text)
import Html.Styled.Attributes as Attr exposing (class, css, href)
import Html.Styled.Events as Events exposing (..)
import Nostr.Article exposing (Article)
import Nostr.Profile exposing (Author, Profile, ProfileValidation(..), profileDisplayName)
import Nostr.Reactions exposing (Interactions)
import Nostr.Types exposing (PubKey)
import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw
import Tailwind.Theme as Theme
import Ui.Links exposing (linkElementForProfile, linkElementForProfilePubKey)
import Ui.Profile
import Ui.Shared exposing (fontFamilyInter, fontFamilyUnbounded, fontFamilySourceSerifPro)
import Time

viewArticlePreview : BrowserEnv -> Author -> Article -> Interactions -> Bool -> Html msg
viewArticlePreview browserEnv author article interactions displayAuthor =
    div
        [ css
            [ Tw.h_56
            , Tw.pb_6
            , Tw.border_b
            , Tw.border_color Theme.gray_200
            , Tw.flex_col
            , Tw.justify_start
            , Tw.items_start
            , Tw.gap_2
            , Tw.inline_flex
            ]
        ]
        [ div
            [ css
                [ Tw.justify_start
                , Tw.items_center
                , Tw.gap_2
                , Tw.inline_flex
                ]
            ]
            [ img
                [ css
                    [ Tw.w_8
                    , Tw.h_8
                    , Tw.rounded_3xl
                    ]
                , Attr.src "https://via.placeholder.com/32x32"
                ]
                []
            , div
                [ css
                    [ Tw.justify_start
                    , Tw.items_start
                    , Tw.gap_2
                    , Tw.flex
                    ]
                ]
                [ div
                    [ css
                        [ Tw.text_color Theme.zinc_900
                        , Tw.text_sm
                        , Tw.font_normal
                        ]
                    , fontFamilyInter
                    ]
                    [ text "Tony" ]
                , div
                    [ css
                        [ Tw.text_color Theme.zinc_500
                        , Tw.text_sm
                        , Tw.font_normal
                        ]
                    , fontFamilyInter
                    ]
                    [ text "Apr. 15" ]
                ]
            ]
        , div
            [ css
                [ Tw.justify_start
                , Tw.items_start
                , Tw.gap_6
                , Tw.inline_flex
                ]
            ]
            [ div
                [ css
                    [ Tw.flex_col
                    , Tw.justify_start
                    , Tw.items_start
                    , Tw.gap_4
                    , Tw.inline_flex
                    ]
                ]
                [ div
                    [ css
                        [ Tw.flex_col
                        , Tw.justify_start
                        , Tw.items_start
                        , Tw.gap_3
                        , Tw.flex
                        ]
                    ]
                    [ div
                        [ css
                            [ Tw.text_color Theme.zinc_900
                            , Tw.text_2xl
                            , Tw.font_semibold
                            ]
                        , fontFamilyUnbounded
                        ]
                        [ text <| Maybe.withDefault "" article.title ]
                    , div
                        [ css
                            [ Tw.w_96
                            , Tw.text_color Theme.zinc_500
                            , Tw.text_lg
                            , Tw.font_normal
                            , Tw.leading_relaxed
                            ]
                        , fontFamilySourceSerifPro
                        ]
                        [ text <| Maybe.withDefault "" article.summary ]
                    ]
                , viewHashTags article.hashtags
                ]
            , img
                [ css
                    [ Tw.w_60
                    , Tw.h_40
                    , Tw.rounded_2xl
                    ]
                , Attr.src <| Maybe.withDefault "" article.image
                ]
                []
            ]
        ]
    
viewHashTags : List String -> Html msg
viewHashTags hashTags =
    (List.map viewTag hashTags)
    |> div
        [ css
            [ Tw.space_x_2
            ]
        ]

viewTag : String -> Html msg
viewTag hashTag =
    div
        [ css
            [ Tw.px_4
            , Tw.py_2
            , Tw.bg_color Theme.neutral_100
            , Tw.rounded_3xl
            , Tw.justify_center
            , Tw.items_center
            , Tw.gap_2
            , Tw.inline_flex
            ]
        ]
        [ div
            [ css
                [ Tw.text_color Theme.zinc_900
                , Tw.text_sm
                , Tw.font_medium
                ]
            , fontFamilyInter
            ]
            [ text hashTag ]
        ]
viewAuthorAndDate : BrowserEnv -> Maybe Time.Posix -> PubKey -> Nostr.Profile.Author -> Html msg
viewAuthorAndDate browserEnv published articlePubKey author =
    case author of
        Nostr.Profile.AuthorPubkey pubKey ->
            div
                [ css
                    [ Tw.flex
                    , Tw.items_center
                    , Tw.space_x_2
                    , Tw.mb_4
                    ]
                ]
                [ viewProfilePubKey pubKey
                , timeParagraph browserEnv published
                ]

        Nostr.Profile.AuthorProfile profile ->
            div
                [ css
                    [ Tw.flex
                    , Tw.items_center
                    , Tw.space_x_2
                    , Tw.mb_4
                    ]
                ]
                [ viewProfileSmall profile
                , timeParagraph browserEnv published
                ]

viewProfileSmall : Profile -> Html msg
viewProfileSmall profile =
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
                [ viewProfileImageSmall (linkElementForProfile profile) profile.picture profile.valid
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
                [ viewProfileImage (linkElementForProfilePubKey pubKey) (Just Ui.Profile.defaultProfileImage) ValidationUnknown
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
            |> Maybe.withDefault Ui.Profile.defaultProfileImage
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
            [ Ui.Profile.validationIcon 24 validationStatus
            ]
        ]

viewProfileImageSmall : (List (Html msg) -> Html msg) -> Maybe String -> ProfileValidation -> Html msg
viewProfileImageSmall linkElement maybeImage validationStatus =
    let
        image =
            maybeImage
            |> Maybe.withDefault Ui.Profile.defaultProfileImage
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
            [ Ui.Profile.validationIcon 16 validationStatus
            ]
        ]