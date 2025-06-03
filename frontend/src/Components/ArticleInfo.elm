module Components.ArticleInfo exposing (..)

import BrowserEnv exposing (BrowserEnv)
import Components.InteractionButton exposing (InteractionObject)
import Components.Interactions as Interactions
import Components.SharingButtonDialog exposing (SharingInfo)
import Dict
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attr exposing (..)
import I18Next
import Nostr
import Nostr.Article exposing (Article, publishedTime)
import Nostr.Profile exposing (Author, shortenedPubKey)
import Nostr.Types exposing (Following(..), LoginStatus, RelayUrl)
import Set exposing (Set)
import Tailwind.Breakpoints as Bp exposing (..)
import Tailwind.Theme exposing (..)
import Tailwind.Utilities as Tw exposing (..)
import TextStats exposing (TextStats)
import Translations.ArticleInfo as Translations
import Ui.Article exposing (linkToHashtag)
import Ui.Links exposing (linkElementForAuthor)
import Ui.Profile
import Ui.Styles exposing (Styles)


type alias ArticleInfoData msg =
    { browserEnv : BrowserEnv
    , model : Maybe Interactions.Model
    , toMsg : Interactions.Msg msg -> msg
    , theme : Ui.Styles.Theme
    , interactionObject : InteractionObject
    , nostr : Nostr.Model
    , loginStatus : LoginStatus
    , shareInfo : SharingInfo
    , zapRelays : Set RelayUrl
    }



{- Article Info Component -}
-- VIEW


view : Styles msg -> Author -> Article -> ArticleInfoData msg -> Html msg
view styles author article articleInfoData =
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
            profile |> Maybe.map (\p -> Nostr.Profile.profileDisplayName p.pubKey p) |> Maybe.withDefault (shortenedPubKey 6 pubKey)

        articlePublishedDate =
            BrowserEnv.formatDate articleInfoData.browserEnv (publishedTime article.createdAt article.publishedAt)

        articleStats =
            TextStats.compute article.language article.content

        articlesFromAuthor =
            Dict.get pubKey articleInfoData.nostr.articlesByAuthor |> Maybe.map List.length |> Maybe.withDefault 0

        followLinks =
            Nostr.isAuthor articleInfoData.nostr article.author

        followersFromAuthor =
            articleInfoData.nostr.followLists
                |> Dict.filter
                    (\_ followings ->
                        followings
                            |> List.any
                                (\f ->
                                    case f of
                                        FollowingPubKey fpk ->
                                            fpk.pubKey == pubKey

                                        FollowingHashtag _ ->
                                            False
                                )
                    )
                |> Dict.size
    in
    aside
        [ css
            [ Tw.text_sm
            , Tw.font_semibold
            , Tw.tracking_wide
            , Tw.text_color styles.color1
            , Tw.bg_color styles.color3
            , Ui.Styles.darkMode [ Tw.bg_color styles.color4 ]
            , Bp.lg
                [ Tw.block
                , Tw.max_w_60
                , Tw.h_screen
                ]
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
                ]
            ]
            {- Profile Section -}
            [ linkElementForAuthor followLinks
                author
                [ viewProfileImage profileImage
                , h2
                    [ css
                        [ Tw.mt_3
                        , Tw.leading_4
                        ]
                    ]
                    [ text profileName ]
                ]
            , div
                [ css
                    [ Tw.mt_1 ]
                ]
                [ viewAuthorStat styles (Translations.numberOfArticles [ articleInfoData.browserEnv.translations ]) articlesFromAuthor
                , viewAuthorStat styles (Translations.followers [ articleInfoData.browserEnv.translations ]) followersFromAuthor
                ]
            , {- Article Info Section -}
              h3
                [ css
                    [ Tw.mt_5
                    , Tw.text_xl
                    , Tw.tracking_wider
                    ]
                ]
                [ text (Translations.title [ articleInfoData.browserEnv.translations ]) ]
            , div
                [ css
                    [ Tw.text_xs
                    , Tw.tracking_wide
                    , Tw.text_color styles.color2
                    ]
                ]
                [ text articlePublishedDate ]
            , viewTags articleInfoData.browserEnv.translations <| List.filter (\hashtag -> not (String.isEmpty hashtag)) <| article.hashtags
            , viewArticleStats styles articleStats articleInfoData.browserEnv
            , viewInteractions articleInfoData
            ]
        ]


viewProfileImage : String -> Html msg
viewProfileImage profileImage =
    img
        [ Attr.src profileImage
        , Attr.alt "Profile avatar"
        , css
            [ Tw.w_11
            , Tw.h_11
            , Tw.rounded_3xl
            , Tw.aspect_square
            ]
        ]
        []


viewAuthorStat : Styles msg -> String -> Int -> Html msg
viewAuthorStat styles stat counter =
    div
        [ css
            [ Tw.flex
            , Tw.flex_wrap
            , Tw.neg_mx_4
            , Tw.neg_mb_4
            , Tw.text_xs
            , Tw.tracking_wide
            , Tw.text_color styles.color2
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
                    [ Tw.w_2over3
                    , Tw.mb_0
                    ]
                ]
            ]
            [ text stat ]
        , div
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
        ]


viewTags : I18Next.Translations -> List String -> Html msg
viewTags translations tags =
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
                            , Tw.my_1_dot_5
                            ]
                        ]
                        [ a
                            [ href (linkToHashtag tag)
                            , Attr.rel "nofollow"
                            , Attr.attribute "aria-label" (Translations.linkToHashtagAriaLabel [ translations ] { hashtag = tag })
                            ]
                            [ text <| "# " ++ tag ]
                        ]
                )
        )


viewArticleStats : Styles msg -> TextStats -> BrowserEnv -> Html msg
viewArticleStats styles textStats browserEnv =
    let
        toHtml label value =
            div
                [ css
                    [ Tw.flex
                    , Tw.gap_1_dot_5
                    , Tw.text_sm
                    , Tw.tracking_wide
                    , Tw.leading_none
                    , Tw.text_color styles.color2
                    , Tw.my_1_dot_5
                    ]
                ]
                [ dt
                    [ css
                        [ Tw.flex_1 ]
                    ]
                    [ text <| label ]
                , dd []
                    [ text <| value ]
                ]

        readingTime =
            browserEnv.formatNumber "0.0" textStats.readingTime

        speakingTime =
            browserEnv.formatNumber "0.0" textStats.speakingTime

        sentences =
            browserEnv.formatNumber "0,0" (toFloat textStats.sentences)

        words =
            browserEnv.formatNumber "0,0" (toFloat textStats.words)

        characters =
            browserEnv.formatNumber "0,0" (toFloat textStats.characters)
    in
    dl
        [ css
            [ Tw.w_auto
            , Tw.mt_4
            ]
        ]
        [ toHtml (Translations.readingTime [ browserEnv.translations ]) (Translations.timeUnit [ browserEnv.translations ] { minutes = readingTime })
        , toHtml (Translations.speakingTime [ browserEnv.translations ]) (Translations.timeUnit [ browserEnv.translations ] { minutes = speakingTime })
        , toHtml (Translations.sentences [ browserEnv.translations ]) sentences
        , toHtml (Translations.words [ browserEnv.translations ]) words
        , toHtml (Translations.characters [ browserEnv.translations ]) characters
        ]


viewInteractions : ArticleInfoData msg -> Html msg
viewInteractions articleInfoData =
    Interactions.new
        { browserEnv = articleInfoData.browserEnv
        , model = articleInfoData.model
        , toMsg = articleInfoData.toMsg
        , theme = articleInfoData.theme
        , interactionObject = articleInfoData.interactionObject
        , nostr = articleInfoData.nostr
        , loginStatus = articleInfoData.loginStatus
        }
        |> Interactions.withInteractionElements
            [ Interactions.CommentButtonElement Nothing
            , Interactions.LikeButtonElement
            , Interactions.RepostButtonElement
            , Interactions.ZapButtonElement "0" articleInfoData.zapRelays
            , Interactions.BookmarkButtonElement
            , Interactions.ShareButtonElement articleInfoData.shareInfo
            ]
        |> Interactions.view
