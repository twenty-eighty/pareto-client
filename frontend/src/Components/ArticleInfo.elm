module Components.ArticleInfo exposing (..)

import BrowserEnv exposing (BrowserEnv)
import Components.Icon as Icon exposing (Icon)
import Dict
import FeatherIcons
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attr exposing (..)
import Nostr
import Nostr.Article exposing (Article, publishedTime)
import Nostr.Profile exposing (Author)
import Nostr.Reactions exposing (Interactions)
import Tailwind.Breakpoints as Bp exposing (..)
import Tailwind.Theme as Theme exposing (..)
import Tailwind.Utilities as Tw exposing (..)
import TextStats exposing (TextStats)
import Ui.Profile
import Ui.Styles exposing (Styles)



{- Article Info Component -}
-- VIEW


view : Styles msg -> Author -> Article -> BrowserEnv -> Interactions -> Nostr.Model -> Html msg
view styles author article browserEnv interactions nostr =
    let
        ( profile, pubKey ) =
            case author of
                Nostr.Profile.AuthorPubkey pk ->
                    ( Nothing, pk )

                Nostr.Profile.AuthorProfile p _ ->
                    ( Just p, p.pubKey )

        profileImage =
            profile |> Ui.Profile.profilePicture 48

        profileName =
            profile |> Maybe.map (\p -> Nostr.Profile.profileDisplayName p.pubKey p) |> Maybe.withDefault pubKey

        articlePublishedDate =
            BrowserEnv.formatDate browserEnv (publishedTime article.createdAt article.publishedAt)

        articleStats =
            TextStats.compute article.language article.content

        articlesFromAuthor =
            Dict.get pubKey nostr.articlesByAuthor |> Maybe.map List.length |> Maybe.withDefault 0

        followersFromAuthor =
            Dict.get pubKey nostr.followLists |> Maybe.map List.length |> Maybe.withDefault 0
    in
    aside
        [ css
            [ Tw.overflow_x_clip
            , Tw.text_sm
            , Tw.font_semibold
            , Tw.tracking_wide
            , Tw.bg_color Theme.white
            , Tw.w_1over5
            , Tw.max_w_60
            , Tw.h_screen
            , Tw.text_color Theme.slate_300
            , Bp.lg
                [ Tw.inline ]
            , Tw.hidden
            ]
        ]
        [ section
            [ css
                [ Tw.flex
                , Tw.flex_col
                , Tw.items_start
                , Tw.px_7
                , Tw.pt_6
                , Tw.pb_96
                , Tw.w_full
                , Tw.h_full
                , Tw.bg_color Theme.slate_500
                ]
            ]
            {- Profile Section -}
            [ viewProfileImage profileImage
            , h2
                [ css
                    [ Tw.mt_3
                    , Tw.leading_4
                    ]
                ]
                [ text profileName ]
            , div
                [ css
                    [ Tw.mt_1 ]
                ]
                [ viewAuthorStat "Beiträge" articlesFromAuthor
                , viewAuthorStat "Follower" followersFromAuthor
                ]
            , {- Article Info Section -}
              h3
                [ css
                    [ Tw.mt_5
                    , Tw.text_xl
                    , Tw.tracking_wider
                    ]
                ]
                [ text "Artikel Info" ]
            , div
                [ css
                    [ Tw.text_xs
                    , Tw.tracking_wide
                    , Tw.text_color Theme.slate_400
                    ]
                ]
                [ text articlePublishedDate ]
            , viewTags <| List.filter (\hashtag -> not (String.isEmpty hashtag)) <| article.hashtags
            , viewArticleStats articleStats
            , viewInteractions browserEnv interactions
            ]
        ]


viewProfileImage : String -> Html msg
viewProfileImage profileImage =
    img
        [ Attr.src profileImage
        , Attr.alt "Profile avatar"
        , css
            [ Tw.object_contain
            , Tw.w_11
            , Tw.rounded_3xl
            , Tw.aspect_square
            ]
        ]
        []


viewAuthorStat : String -> Int -> Html msg
viewAuthorStat stat counter =
    div
        [ css
            [ Tw.flex
            , Tw.flex_wrap
            , Tw.neg_mx_4
            , Tw.neg_mb_4
            , Tw.text_xs
            , Tw.tracking_wide
            , Tw.text_color Theme.slate_400
            , Bp.md
                [ Tw.mb_0
                ]
            ]
        ]
        [ div
            [ css
                [ Tw.w_full
                , Tw.px_4
                , Tw.mb_4
                , Bp.md
                    [ Tw.w_1over3
                    , Tw.mb_0
                    ]
                ]
            ]
            [ text <| String.fromInt counter ]
        , div
            [ css
                [ Tw.w_full
                , Tw.px_4
                , Tw.mb_4
                , Bp.md
                    [ Tw.w_2over3
                    , Tw.mb_0
                    ]
                ]
            ]
            [ text stat ]
        ]


viewTags : List String -> Html msg
viewTags tags =
    ul
        [ css
            [ Tw.mt_3
            , Tw.w_full
            ]
        ]
        (tags
            |> List.map
                (\tag ->
                    li
                        [ css
                            [ Tw.leading_none
                            ]
                        ]
                        [ text <| "# " ++ tag ]
                )
        )


viewArticleStats : TextStats -> Html msg
viewArticleStats textStats =
    let
        toHtml label value =
            div
                [ css
                    [ Tw.flex
                    , Tw.gap_1_dot_5
                    , Tw.text_sm
                    , Tw.tracking_wide
                    , Tw.leading_none
                    , Tw.text_color Theme.slate_400
                    ]
                ]
                [ dt
                    [ css
                        [ Tw.grow ]
                    ]
                    [ text <| label ]
                , dd []
                    [ text <| value ]
                ]

        roundToTwoDecimals num =
            toFloat (round (num * 100)) / 100
    in
    dl
        [ css
            [ Tw.w_auto
            , Tw.mt_4
            ]
        ]
        [ toHtml "Reading Time:" (String.fromFloat <| roundToTwoDecimals textStats.readingTime)
        , toHtml "Speaking Time:" (String.fromFloat <| roundToTwoDecimals textStats.speakingTime)
        , toHtml "Sentences:" (String.fromInt textStats.sentences)
        , toHtml "Words:" (String.fromInt textStats.words)
        , toHtml "Characters:" (String.fromInt textStats.characters)
        ]


viewInteractions : BrowserEnv -> Interactions -> Html msg
viewInteractions browserEnv interactions =
    let
        renderInteraction : Icon -> Maybe Int -> Html msg
        renderInteraction icon maybeValue =
            div
                [ css
                    [ Tw.flex
                    , Tw.h_6
                    , Tw.gap_1_dot_5
                    ]
                ]
                [ div
                    [ css
                        [ Tw.w_6
                        , Tw.h_6
                        , Tw.justify_center
                        , Tw.items_center
                        , Tw.flex
                        ]
                    ]
                    [ Icon.view icon
                    ]
                , span [ css [ Tw.py_0_dot_5 ] ]
                    [ text <| Maybe.withDefault "0" <| Maybe.map String.fromInt maybeValue ]
                ]

        likkesIcon =
            Icon.MaterialIcon Icon.MaterialFavoriteBorder 30 Icon.Inherit

        bookmarkIcon =
            Icon.MaterialIcon Icon.MaterialOutlineBookmarkAdded 30 Icon.Inherit

        repostIcon =
            Icon.MaterialIcon Icon.MaterialRepeat 30 Icon.Inherit

        itemsList =
            [ ( Icon.FeatherIcon FeatherIcons.messageSquare, Just (List.length interactions.articleComments) )
            , ( likkesIcon, interactions.reactions )
            , ( repostIcon, interactions.reposts )
            , ( Icon.FeatherIcon FeatherIcons.zap, interactions.zaps |> Maybe.map (\v -> v // 1000) )
            , ( bookmarkIcon, interactions.bookmarks )
            ]
    in
    div
        [ css
            [ Tw.flex
            , Tw.gap_6
            , Tw.self_stretch
            , Tw.mt_6
            , Tw.flex_wrap
            ]
        ]
        (itemsList |> List.map (\( url, value ) -> renderInteraction url value))
