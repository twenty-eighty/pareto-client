module Components.ArticleInfo exposing (..)

import BrowserEnv exposing (BrowserEnv)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attr exposing (..)
import Nostr.Article exposing (Article, publishedTime)
import Nostr.Profile exposing (Author)
import Tailwind.Breakpoints as Bp exposing (..)
import Tailwind.Theme as Theme exposing (..)
import Tailwind.Utilities as Tw exposing (..)
import Ui.Profile
import Ui.Styles exposing (Styles)



{- Article Info Component -}
-- VIEW


view : Styles msg -> Author -> Article -> BrowserEnv -> Html msg
view styles author article browserEnv =
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
    in
    aside
        [ css
            [ Tw.overflow_hidden
            , Tw.text_sm
            , Tw.font_semibold
            , Tw.tracking_wide
            , Tw.bg_color Theme.white
            , Tw.max_w_72
            , Tw.text_color Theme.slate_300
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
                , Tw.bg_color Theme.slate_500
                ]
            ]
            {- Profile Section -}
            [ img
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
                [ viewAuthorStat "Beiträge" 21
                , viewAuthorStat "Follower" 210
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
            , viewArticleStats [ { label = "Lesezeit", counter = 3453 }, { label = "Hörzeit", counter = 3453 }, { label = "Anzahl Worte", counter = 3453 }, { label = "Anzahl Sätze", counter = 333 }, { label = "Anzahl Zeichen", counter = 3 } ]
            , viewInteractions { likes = 5, comments = 0, reposts = 0, zaps = 142, bookmarks = 2 }
            ]
        ]


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


viewArticleStats : List { label : String, counter : Int } -> Html msg
viewArticleStats stats =
    dl
        [ css
            [ Tw.w_full
            , Tw.mt_4
            ]
        ]
        (stats
            |> List.map
                (\stat ->
                    div
                        [ css
                            [ Tw.flex
                            , Tw.gap_1_dot_5
                            , Tw.text_xs
                            , Tw.tracking_wide
                            , Tw.leading_none
                            , Tw.text_color Theme.slate_400
                            ]
                        ]
                        [ dt
                            [ css
                                [ Tw.grow
                                ]
                            ]
                            [ text <| stat.label ++ ":" ]
                        , dd []
                            [ text <| String.fromInt stat.counter ]
                        ]
                )
        )


viewInteractions : { likes : Int, comments : Int, reposts : Int, zaps : Int, bookmarks : Int } -> Html msg
viewInteractions interactions =
    div
        [ css
            [ Tw.flex
            , Tw.gap_8
            , Tw.self_stretch
            , Tw.mt_6
            , Tw.text_base
            , Tw.tracking_wider
            , Tw.leading_none
            , Tw.whitespace_nowrap
            ]
        ]
        [ div
            [ css
                [ Tw.flex
                , Tw.flex_1
                , Tw.gap_1_dot_5
                ]
            ]
            [ img
                [ Attr.src "https://cdn.builder.io/api/v1/image/assets/TEMP/efa51e07b048e900800fe1cf9dd9ce516d17661b?placeholderIfAbsent=true&apiKey=10f24aae08624aa4a3139b1ffec54639"
                , Attr.alt "Likes icon"
                , css
                    [ Tw.object_contain
                    , Tw.shrink_0
                    , Tw.my_auto
                    , Tw.aspect_square
                    , Tw.w_4
                    ]
                ]
                []
            , span []
                [ text "5" ]
            ]
        , div
            [ css
                [ Tw.flex
                , Tw.flex_1
                , Tw.gap_1_dot_5
                ]
            ]
            [ img
                [ Attr.src "https://cdn.builder.io/api/v1/image/assets/TEMP/ecd4859d1d71529d148449914d4864dba4335a09?placeholderIfAbsent=true&apiKey=10f24aae08624aa4a3139b1ffec54639"
                , Attr.alt "Comments icon"
                , css
                    [ Tw.object_contain
                    , Tw.shrink_0
                    , Tw.my_auto
                    , Tw.aspect_square
                    , Tw.w_4
                    ]
                ]
                []
            , span []
                [ text "0" ]
            ]
        , div
            [ css
                [ Tw.flex
                , Tw.flex_1
                , Tw.gap_1_dot_5
                ]
            ]
            [ img
                [ Attr.src "https://cdn.builder.io/api/v1/image/assets/TEMP/b03c504a0dbdc9fed19204aa20fbd4016d88c3b4?placeholderIfAbsent=true&apiKey=10f24aae08624aa4a3139b1ffec54639"
                , Attr.alt "Shares icon"
                , css
                    [ Tw.object_contain
                    , Tw.shrink_0
                    , Tw.my_auto
                    , Tw.aspect_square
                    , Tw.w_4
                    ]
                ]
                []
            , span []
                [ text "0" ]
            ]
        , div
            [ css
                [ Tw.flex
                , Tw.gap_1_dot_5
                ]
            ]
            [ img
                [ Attr.src "https://cdn.builder.io/api/v1/image/assets/TEMP/e6108ae064f959cd44a0e2018e3cc9572635b74c?placeholderIfAbsent=true&apiKey=10f24aae08624aa4a3139b1ffec54639"
                , Attr.alt "Views icon"
                , css
                    [ Tw.object_contain
                    , Tw.shrink_0
                    , Tw.my_auto
                    , Tw.aspect_square
                    , Tw.w_4
                    ]
                ]
                []
            , span []
                [ text "142" ]
            ]
        , div
            [ css
                [ Tw.flex
                , Tw.gap_1
                ]
            ]
            [ img
                [ Attr.src "https://cdn.builder.io/api/v1/image/assets/TEMP/2deb36a8261980cb9698c587ac53c7f07b9591ce?placeholderIfAbsent=true&apiKey=10f24aae08624aa4a3139b1ffec54639"
                , Attr.alt "Saves icon"
                , css
                    [ Tw.object_contain
                    , Tw.shrink_0
                    , Tw.my_auto
                    , Tw.aspect_square
                    , Tw.w_4
                    ]
                ]
                []
            , span []
                [ text "2" ]
            ]
        ]
