module Ui.Article exposing (..)

import BrowserEnv exposing (BrowserEnv)
import Components.Button as Button
import Components.Icon as Icon
import Components.ZapDialog as ZapDialog
import Css
import Dict
import Html.Styled as Html exposing (Html, a, article, div, h2, h3, img, summary, text)
import Html.Styled.Attributes as Attr exposing (css, href)
import Html.Styled.Events as Events exposing (..)
import LinkPreview exposing (LoadedContent)
import Locale
import Markdown
import Nostr
import Nostr.Article exposing (Article, addressComponentsForArticle, nip19ForArticle, publishedTime)
import Nostr.Event exposing (AddressComponents, Kind(..), Tag(..), TagReference(..))
import Nostr.Nip19 exposing (NIP19Type(..))
import Nostr.Nip27 exposing (GetProfileFunction)
import Nostr.Profile exposing (Author(..), Profile, ProfileValidation(..), profileDisplayName, shortenedPubKey)
import Nostr.Reactions exposing (Interactions)
import Nostr.Relay exposing (websocketUrl)
import Nostr.Types exposing (EventId, PubKey)
import Route
import Route.Path
import Set
import Tailwind.Breakpoints as Bp
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw
import Time
import Translations.Posts
import Ui.Links exposing (linkElementForProfile, linkElementForProfilePubKey)
import Ui.Profile
import Ui.Shared exposing (Actions, emptyHtml, extendedZapRelays)
import Ui.Styles exposing (Styles, Theme(..), darkMode, fontFamilyInter, fontFamilyRobotoMono, fontFamilyUnbounded)


type alias ArticlePreviewsData msg =
    { theme : Theme
    , browserEnv : BrowserEnv
    , nostr : Nostr.Model
    , userPubKey : Maybe PubKey
    , onBookmark : Maybe ( AddressComponents -> msg, AddressComponents -> msg ) -- msgs for adding/removing a bookmark
    , onReaction : Maybe (EventId -> PubKey -> AddressComponents -> msg)
    , onRepost : Maybe msg
    , onZap : Maybe (List ZapDialog.Recipient -> msg)
    }


type alias ArticlePreviewData msg =
    { author : Author
    , actions : Actions msg
    , interactions : Interactions
    , displayAuthor : Bool
    , loadedContent : Maybe (LoadedContent msg)
    }



-- single article


textStyleReactions : List (Html.Attribute msg)
textStyleReactions =
    [ css
        [ Tw.text_base
        , Tw.font_medium
        , Tw.tracking_normal
        ]
    , fontFamilyInter
    , Attr.style "line-height" "auto"
    ]


textStyleArticleHashtags : List (Html.Attribute msg)
textStyleArticleHashtags =
    [ css
        [ Tw.text_sm
        , Tw.font_medium
        , Tw.leading_snug
        ]
    , fontFamilyRobotoMono
    ]


textStyleArticleAuthor : List (Html.Attribute msg)
textStyleArticleAuthor =
    [ css
        [ Tw.text_sm
        , Tw.font_normal
        , Tw.leading_snug
        ]
    , fontFamilyRobotoMono
    ]


textStyleArticleDate : List (Html.Attribute msg)
textStyleArticleDate =
    [ css
        [ Tw.text_xs
        , Tw.font_normal
        , Tw.leading_tight
        ]
    , fontFamilyRobotoMono
    ]


colorStyleDate : List (Html.Attribute msg)
colorStyleDate =
    let
        styles =
            Ui.Styles.stylesForTheme ParetoTheme
    in
    [ css
        [ Tw.text_color styles.color3
        , darkMode
            [ Tw.text_color styles.color3DarkMode
            ]
        ]
    ]


colorStyleArticleHashtags : List (Html.Attribute msg)
colorStyleArticleHashtags =
    let
        styles =
            Ui.Styles.stylesForTheme ParetoTheme
    in
    [ css
        [ Tw.text_color styles.color4
        , darkMode
            [ Tw.text_color styles.color4DarkMode
            ]
        ]
    ]


viewArticle : ArticlePreviewsData msg -> ArticlePreviewData msg -> Article -> Html msg
viewArticle articlePreviewsData articlePreviewData article =
    let
        styles =
            Ui.Styles.stylesForTheme articlePreviewsData.theme

        getProfile =
            Nostr.getProfile articlePreviewsData.nostr

        langAttr =
            case article.language of
                Just language ->
                    [ Attr.lang (language |> Locale.languageToISOCode) ]

                Nothing ->
                    []

        contentMargins =
            [ css
                [ Tw.px_1
                , Bp.xxl
                    [ Tw.mx_28
                    ]
                , Bp.xl
                    [ Tw.mx_24
                    ]
                , Bp.lg
                    [ Tw.mx_20
                    ]
                , Bp.md
                    [ Tw.mx_10
                    ]
                , Bp.sm
                    [ Tw.px_5
                    ]
                ]
            ]

        articleRelays =
            article.relays |> Set.map websocketUrl

        previewData =
            { pubKey = article.author
            , maybeNip19Target = nip19ForArticle article
            , zapRelays = extendedZapRelays articleRelays articlePreviewsData.nostr articlePreviewsData.userPubKey
            , actions = articlePreviewData.actions
            , interactions = articlePreviewData.interactions
            }
    in
    Html.article
        (langAttr
            ++ [ css
                    [ Tw.flex_col
                    , Tw.justify_start
                    , Tw.items_center
                    , Tw.gap_12
                    , Tw.inline_flex
                    , Tw.px_2
                    , Bp.xxl
                        [ Tw.px_40
                        ]
                    , Bp.xl
                        [ Tw.px_20
                        ]
                    , Bp.lg
                        [ Tw.px_10
                        ]
                    , Bp.md
                        [ Tw.px_5
                        ]
                    , Bp.sm
                        [ Tw.px_3
                        ]
                    ]
               ]
        )
        [ div
            [ css
                [ Tw.self_stretch
                , Tw.flex_col
                , Tw.justify_center
                , Tw.items_center
                , Tw.gap_4
                , Tw.flex
                ]
            ]
            [ div
                (css
                    [ Tw.flex_col
                    , Tw.justify_start
                    , Tw.items_start
                    , Tw.gap_4
                    , Tw.inline_flex
                    ]
                    :: contentMargins
                )
                [ viewTags article
                , div
                    [ css
                        [ Tw.flex_col
                        , Tw.justify_start
                        , Tw.items_start
                        , Tw.gap_6
                        , Tw.flex
                        ]
                    ]
                    [ Html.h1
                        (styles.textStyleH1Article
                            ++ styles.colorStyleGrayscaleTitle
                            ++ [ css
                                    [ Tw.max_w_screen_sm
                                    , Bp.sm
                                        [ Tw.max_w_prose
                                        ]
                                    ]
                               ]
                        )
                        [ text <| Maybe.withDefault "" article.title ]
                    , Html.summary
                        (styles.textStyleH2
                            ++ styles.colorStyleGrayscaleSummary
                            ++ [ css
                                    [ Tw.max_w_screen_sm
                                    , Bp.sm
                                        [ Tw.max_w_prose
                                        , Tw.list_none
                                        ]
                                    ]
                               ]
                        )
                        [ text <| Maybe.withDefault "" article.summary ]
                    , viewAuthorAndDate styles articlePreviewsData.browserEnv article.publishedAt article.createdAt articlePreviewData.author
                    ]
                ]
            , viewArticleImage article.image
            , div
                (css
                    [ Tw.flex_col
                    , Tw.justify_start
                    , Tw.items_start
                    , Tw.gap_4
                    , Tw.flex
                    , Tw.mb_20
                    ]
                    :: styles.colorStyleGrayscaleMuted
                    ++ textStyleReactions
                    ++ contentMargins
                )
                [ Ui.Shared.viewInteractions styles articlePreviewsData.browserEnv previewData "1"
                , viewContent styles articlePreviewData.loadedContent getProfile article.content
                , Ui.Shared.viewInteractions styles articlePreviewsData.browserEnv previewData "2"
                ]
            ]

        -- , viewArticleComments styles
        ]


viewArticleImage : Maybe String -> Html msg
viewArticleImage maybeImage =
    case maybeImage of
        Just image ->
            div
                [ css
                    [ Tw.relative
                    , Tw.mb_4
                    ]
                ]
                [ img
                    [ Attr.src (Ui.Shared.extendUrlForScaling 384 image)
                    , Attr.alt "Post Image"
                    , css
                        [ Tw.rounded_lg
                        , Tw.w_full
                        , Tw.max_h_96
                        , Tw.object_cover
                        ]
                    ]
                    []
                ]

        Nothing ->
            emptyHtml


viewTitle : Maybe String -> Html msg
viewTitle maybeTitle =
    case maybeTitle of
        Just title ->
            h3
                [ css
                    [ Tw.text_4xl
                    , Tw.font_bold
                    , Tw.text_color Theme.gray_900
                    , Tw.mb_2
                    ]
                , fontFamilyUnbounded
                ]
                [ text title
                ]

        Nothing ->
            emptyHtml


viewSummary : Styles msg -> Maybe String -> Html msg
viewSummary _ maybeSummary =
    case maybeSummary of
        Just summary ->
            Html.summary
                [ css
                    [ Tw.text_color Theme.gray_600
                    , Tw.text_sm
                    , Tw.mb_4
                    , Tw.list_none
                    ]
                ]
                [ text summary ]

        Nothing ->
            emptyHtml


viewTags : Article -> Html msg
viewTags article =
    article.hashtags
        |> List.map removeHashTag
        |> List.map viewTag
        |> List.intersperse (text " / ")
        |> div
            (textStyleArticleHashtags ++ colorStyleArticleHashtags)


removeHashTag : String -> String
removeHashTag hashTag =
    if String.startsWith "#" hashTag then
        String.dropLeft 1 hashTag

    else
        hashTag


viewTag : String -> Html msg
viewTag tag =
    a
        [ href ("/t/" ++ tag)
        ]
        [ text tag ]


viewAuthorAndDate : Styles msg -> BrowserEnv -> Maybe Time.Posix -> Time.Posix -> Nostr.Profile.Author -> Html msg
viewAuthorAndDate styles browserEnv published createdAt author =
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
                [ Ui.Profile.viewProfilePubKey pubKey
                , timeParagraph styles browserEnv published createdAt
                ]

        Nostr.Profile.AuthorProfile profile validationStatus ->
            div
                [ css
                    [ Tw.flex
                    , Tw.items_center
                    , Tw.space_x_2
                    , Tw.mb_4
                    ]
                ]
                [ viewArticleProfileSmall profile validationStatus
                , div
                    [ css
                        [ Tw.h_11
                        , Tw.left_16
                        , Tw.top_1
                        ]
                    ]
                    [ div
                        (textStyleArticleAuthor
                            ++ styles.colorStyleLinks
                            ++ [ css
                                    [ Tw.left_0
                                    , Tw.top_0
                                    ]
                               ]
                        )
                        [ text <| profileDisplayName profile.pubKey profile ]
                    , viewArticleTime styles browserEnv published createdAt
                    ]
                ]


viewArticleProfileSmall : Profile -> ProfileValidation -> Html msg
viewArticleProfileSmall profile validationStatus =
    let
        linkElement =
            linkElementForProfile profile validationStatus
    in
    div
        [ css
            [ Tw.relative
            ]
        ]
        [ linkElement
            [ div
                []
                [ img
                    [ Attr.src <| Ui.Profile.profilePicture 48 (Just profile)
                    , Attr.alt "Avatar"
                    , css
                        [ Tw.min_w_12
                        , Tw.min_h_12
                        , Tw.max_w_12
                        , Tw.max_h_12
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
                    , Tw.w_3
                    , Tw.h_2
                    ]
                ]
                [ Ui.Profile.validationIcon 16 validationStatus ]
            ]
        ]


viewArticleTime : Styles msg -> BrowserEnv -> Maybe Time.Posix -> Time.Posix -> Html msg
viewArticleTime styles browserEnv maybePublishedAt createdAt =
    div
        (textStyleArticleDate
            ++ colorStyleDate
            ++ [ css
                    [ Tw.left_0
                    , Tw.top_5
                    ]
               ]
        )
        [ text <| BrowserEnv.formatDate browserEnv (publishedTime createdAt maybePublishedAt) ]


viewContent : Styles msg -> Maybe (LoadedContent msg) -> GetProfileFunction -> String -> Html msg
viewContent styles loadedContent fnGetProfile content =
    article
        [ css
            [ Tw.flex_col
            , Tw.justify_start
            , Tw.gap_10
            , Tw.max_w_96
            , Bp.sm
                [ Tw.max_w_prose
                ]
            ]
        ]
        [ viewContentMarkdown styles loadedContent fnGetProfile content
        ]


viewContentMarkdown : Styles msg -> Maybe (LoadedContent msg) -> GetProfileFunction -> String -> Html msg
viewContentMarkdown styles loadedContent fnGetProfile content =
    case Markdown.markdownViewHtml styles loadedContent fnGetProfile content of
        Ok html ->
            html

        Err error ->
            div [] [ text <| "Error rendering Markdown: " ++ error ]


viewArticleComments : Styles msg -> Html msg
viewArticleComments styles =
    div
        [ css
            [ Tw.self_stretch
            , Tw.flex_col
            , Tw.justify_start
            , Tw.items_start
            , Tw.gap_6
            , Tw.flex
            ]
        ]
        [ div
            [ css
                [ Tw.justify_start
                , Tw.items_center
                , Tw.gap_3
                , Tw.inline_flex
                ]
            ]
            [ div
                (styles.textStyleH2 ++ styles.colorStyleGrayscaleTitle)
                [ text "Comments" ]
            , div
                (textStyleArticleAuthor ++ styles.colorStyleGrayscaleMuted)
                [ text "(0)" ]
            ]
        , div
            [ css
                [ Tw.flex_col
                , Tw.justify_end
                , Tw.items_end
                , Tw.gap_4
                , Tw.flex
                ]
            ]
            [ div
                [ css
                    [ Tw.flex_col
                    , Tw.justify_start
                    , Tw.items_start
                    , Tw.gap_4
                    , Tw.flex
                    ]
                ]
                [ div
                    [ css
                        [ Tw.w_96
                        , Tw.h_28
                        , Tw.relative
                        ]
                    ]
                    [ div
                        [ css
                            [ Tw.w_96
                            , Tw.h_28
                            , Tw.left_0
                            , Tw.top_0
                            , Tw.bg_color Theme.gray_100
                            , Tw.rounded_xl
                            ]
                        ]
                        []
                    , div
                        (styles.textStyleBody
                            ++ styles.colorStyleGrayscaleMuted
                            ++ [ css
                                    [ Tw.left_4
                                    , Tw.top_3
                                    ]
                               ]
                        )
                        [ text "Comment" ]
                    ]
                ]
            , div
                [ css
                    [ Tw.self_stretch
                    , Tw.px_6
                    , Tw.py_3
                    , Tw.bg_color Theme.blue_600
                    , Tw.rounded_xl
                    , Tw.justify_center
                    , Tw.items_center
                    , Tw.gap_2_dot_5
                    , Tw.inline_flex
                    ]
                ]
                [ div
                    (textStyleReactions ++ styles.colorStyleInverse)
                    [ text "Post Comment" ]
                ]
            ]
        ]


viewArticleInternal : Styles msg -> Maybe (LoadedContent msg) -> GetProfileFunction -> BrowserEnv -> Article -> Html msg
viewArticleInternal styles loadedContent fnGetProfile _ article =
    div
        [ css
            [ Tw.flex
            , Tw.items_center
            , Tw.justify_center
            , Tw.min_h_screen
            ]
        ]
        [ div
            [ css
                [ Tw.p_6
                , Tw.rounded_lg
                , Tw.shadow_lg
                , Tw.max_w_3xl
                , Bp.xxl
                    [ Tw.w_max
                    ]
                , Bp.xl
                    [ Tw.w_max
                    ]
                , Bp.lg
                    [ Tw.w_max
                    ]
                ]
            ]
            [ viewArticleImage article.image
            , div
                (styles.textStyleH1Article
                    ++ styles.colorStyleGrayscaleTitle
                    ++ [ css
                            [-- Tw.w_96
                            ]
                       ]
                )
                [ text <| Maybe.withDefault "" article.title ]
            , div
                (styles.textStyleH4Article
                    ++ styles.colorStyleGrayscaleText
                    ++ [ css
                            [-- Tw.w_96
                            ]
                       ]
                )
                [ text <| Maybe.withDefault "" article.summary ]
            , viewContent styles loadedContent fnGetProfile article.content
            ]
        ]



-- article previews


viewArticlePreviewList : ArticlePreviewsData msg -> ArticlePreviewData msg -> Article -> Html msg
viewArticlePreviewList articlePreviewsData articlePreviewData article =
    let
        styles =
            Ui.Styles.stylesForTheme articlePreviewsData.theme

        ( textWidthAttr, hashtagsHeightAttr ) =
            case article.image of
                Just _ ->
                    ( [ Tw.w_80
                      , Bp.xxl
                            [ Css.property "width" "600px"
                            ]
                      , Bp.xl
                            [ Css.property "width" "400px"
                            ]
                      , Bp.lg
                            [ Css.property "width" "340px"
                            ]
                      , Bp.md
                            [ Css.property "width" "340px"
                            ]
                      , Bp.sm
                            [ Css.property "width" "320px"
                            ]
                      ]
                    , Css.property "height" "40px"
                    )

                Nothing ->
                    ( [ Tw.w_80
                      , Bp.xxl
                            [ Css.property "width" "950px"
                            ]
                      , Bp.xl
                            [ Css.property "width" "700px"
                            ]
                      , Bp.lg
                            [ Css.property "width" "600px"
                            ]
                      , Bp.md
                            [ Css.property "width" "540px"
                            ]
                      , Bp.sm
                            [ Css.property "width" "460px"
                            ]
                      ]
                    , Css.property "height" "auto"
                    )

        summaryText =
            case article.summary of
                Just summary ->
                    if String.trim summary == "" then
                        article.content
                            |> Markdown.summaryFromContent
                            |> Maybe.withDefault ""

                    else
                        summary

                Nothing ->
                    article.content

        summaryLinesAttr =
            if List.length article.hashtags < 1 then
                [ Tw.line_clamp_5 ]

            else
                [ Tw.line_clamp_3 ]

        hasInvalidTags =
            article.otherTags
                |> List.filter
                    (\tag ->
                        case tag of
                            InvalidTag _ ->
                                True

                            _ ->
                                False
                    )
                |> List.length
                |> (\length -> length > 0)

        invalidTagIndicator =
            if articlePreviewsData.browserEnv.environment == BrowserEnv.Development && hasInvalidTags then
                div [] [ text "-> has invalid tags <-" ]

            else
                emptyHtml
    in
    div
        [ css
            [ Tw.pb_6
            , Tw.border_b
            , Tw.border_color Theme.gray_200
            , Tw.flex
            , Tw.flex_col
            , Tw.justify_start
            , Tw.gap_3
            , Tw.px_6
            , Bp.xxl
                [ Css.property "width" "1024px"
                ]
            , Bp.xl
                [ Css.property "width" "800px"
                ]
            , Bp.lg
                [ Css.property "width" "720px"
                ]
            , Bp.md
                [ Css.property "width" "640px"
                ]
            , Bp.sm
                [ Tw.h_64
                , Css.property "width" "550px"
                ]
            ]
        ]
        [ viewAuthorAndDatePreview articlePreviewsData articlePreviewData article
        , invalidTagIndicator
        , div
            [ css
                [ Tw.self_stretch
                , Tw.justify_between
                , Tw.items_start
                , Tw.flex
                , Tw.flex_col
                , Tw.gap_4
                , Bp.sm
                    [ Tw.inline_flex
                    , Tw.flex_row_reverse
                    , Tw.gap_10
                    ]
                ]
            ]
            [ previewListImage article
            , div
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
                        , Tw.mb_2
                        , Bp.sm
                            [ Tw.w_80
                            ]
                        ]
                    ]
                    [ viewTitlePreview styles article.title (linkToArticle article) textWidthAttr
                    , div
                        (styles.colorStyleGrayscaleText
                            ++ styles.textStyleBody
                            ++ [ css
                                    (summaryLinesAttr ++ textWidthAttr)
                               ]
                        )
                        [ text summaryText ]
                    , viewHashTags styles article.hashtags (hashtagsHeightAttr :: textWidthAttr)
                    ]
                ]
            ]
        ]


linkToArticle : Article -> Maybe String
linkToArticle article =
    nip19ForArticle article |> Maybe.map (\naddr -> "/a/" ++ naddr)


viewTitlePreview : Styles msg -> Maybe String -> Maybe String -> List Css.Style -> Html msg
viewTitlePreview styles maybeTitle maybeLinkTarget textWidthAttr =
    case ( maybeTitle, maybeLinkTarget ) of
        ( Just title, Just linkUrl ) ->
            a
                (styles.colorStyleGrayscaleTitle
                    ++ styles.textStyleH2
                    ++ [ css
                            (Tw.line_clamp_2 :: textWidthAttr)
                       , href linkUrl
                       ]
                )
                [ text title ]

        ( Just title, Nothing ) ->
            div
                (styles.colorStyleGrayscaleTitle
                    ++ styles.textStyleH2
                    ++ [ css
                            (Tw.line_clamp_2 :: textWidthAttr)
                       ]
                )
                [ text title ]

        ( Nothing, _ ) ->
            emptyHtml


viewHashTags : Styles msg -> List String -> List Css.Style -> Html msg
viewHashTags styles hashTags widthAttr =
    if List.length hashTags > 0 then
        hashTags
            |> List.map viewHashTag
            |> List.intersperse (text " / ")
            |> div
                (css
                    (widthAttr
                        ++ [ Tw.space_x_2
                           , Tw.mb_2
                           , Tw.gap_2
                           , Tw.line_clamp_1
                           , Tw.text_clip
                           ]
                    )
                    :: (textStyleArticleHashtags ++ colorStyleArticleHashtags)
                )

    else
        emptyHtml


viewHashTag : String -> Html msg
viewHashTag hashTag =
    a
        [ css [ Tw.inline_block ]
        , href ("/t/" ++ hashTag)
        ]
        [ text hashTag ]


viewArticlePreviewBigPicture : ArticlePreviewsData msg -> ArticlePreviewData msg -> Article -> Html msg
viewArticlePreviewBigPicture articlePreviewsData articlePreviewData article =
    let
        styles =
            Ui.Styles.stylesForTheme articlePreviewsData.theme
    in
    div
        [ css
            [ Tw.flex_col
            , Tw.justify_start
            , Tw.items_start
            , Tw.gap_4
            , Tw.inline_flex
            ]
        ]
        [ previewBigPictureImage article
        , div
            [ css
                [ Tw.flex_col
                , Tw.justify_start
                , Tw.items_start
                , Tw.gap_3
                , Tw.flex
                ]
            ]
            [ div
                (styles.textStyleH4
                    ++ styles.colorStyleGrayscaleTitle
                    ++ [ css
                            [ Tw.w_96
                            ]
                       ]
                )
                [ text <| Maybe.withDefault "" article.title ]
            , viewAuthorAndDatePreview articlePreviewsData articlePreviewData article
            ]
        ]


previewListImage : Article -> Html msg
previewListImage article =
    case article.image of
        Just image ->
            div
                [ css
                    [ Tw.w_80
                    , Tw.h_44
                    , Tw.bg_color Theme.gray_300
                    , Tw.overflow_hidden
                    , Tw.relative
                    ]
                ]
                [ img
                    [ Attr.src (Ui.Shared.extendUrlForScaling 384 image)
                    , Attr.style "top" "50%"
                    , Attr.style "left" "50%"
                    , Attr.style "object-fit" "cover"
                    , Attr.style "width" "100%"
                    , Attr.style "height" "100%"
                    , Attr.style "position" "absolute"
                    , Attr.style "transform" "translate(-50%, -50%)"
                    , Attr.attribute "loading" "lazy"
                    ]
                    []
                ]

        Nothing ->
            div [] []


previewBigPictureImage : Article -> Html msg
previewBigPictureImage article =
    case article.image of
        Just image ->
            div
                [ css
                    [ Tw.w_96
                    , Tw.h_60
                    , Tw.bg_color Theme.gray_300
                    , Tw.overflow_hidden
                    , Tw.relative
                    , Tw.rounded_xl
                    ]
                ]
                [ img
                    [ Attr.src image
                    , Attr.style "top" "50%"
                    , Attr.style "left" "50%"
                    , Attr.style "object-fit" "cover"
                    , Attr.style "width" "100%"
                    , Attr.style "height" "100%"
                    , Attr.style "position" "absolute"
                    , Attr.style "transform" "translate(-50%, -50%)"
                    , Attr.attribute "loading" "lazy"
                    ]
                    []
                ]

        Nothing ->
            div
                [ css
                    [ Tw.w_96
                    , Tw.h_60
                    , Tw.bg_color Theme.gray_300
                    , Tw.overflow_hidden
                    , Tw.relative
                    , Tw.rounded_xl
                    ]
                ]
                []


viewAuthorAndDatePreview : ArticlePreviewsData msg -> ArticlePreviewData msg -> Article -> Html msg
viewAuthorAndDatePreview articlePreviewsData articlePreviewData article =
    let
        styles =
            Ui.Styles.stylesForTheme articlePreviewsData.theme
    in
    case articlePreviewData.author of
        Nostr.Profile.AuthorPubkey pubKey ->
            div
                [ css
                    [ Tw.justify_start
                    , Tw.items_center
                    , Tw.gap_2
                    , Tw.inline_flex
                    ]
                ]
                [ viewProfilePubKey pubKey
                , timeParagraph styles articlePreviewsData.browserEnv article.publishedAt article.createdAt
                ]

        Nostr.Profile.AuthorProfile profile validationStatus ->
            div
                [ css
                    [ Tw.justify_between
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
                    [ viewProfileImageSmall (linkElementForProfile profile validationStatus) (Just profile) validationStatus
                    , div
                        [ css
                            [ Tw.justify_start
                            , Tw.items_start
                            , Tw.gap_2
                            , Tw.flex
                            ]
                        ]
                        [ div
                            (styles.colorStyleGrayscaleText ++ styles.textStyle14)
                            [ text (profileDisplayName profile.pubKey profile) ]
                        , timeParagraph styles articlePreviewsData.browserEnv article.publishedAt article.createdAt
                        ]
                    ]
                , viewArticleEditButton articlePreviewsData article profile.pubKey
                , viewArticleBookmarkButton articlePreviewsData articlePreviewData article
                ]


viewArticleEditButton : ArticlePreviewsData msg -> Article -> PubKey -> Html msg
viewArticleEditButton articlePreviewsData article articleAuthorPubKey =
    if articlePreviewsData.userPubKey == Just articleAuthorPubKey then
        Button.new
            { label = Translations.Posts.editDraftButtonLabel [ articlePreviewsData.browserEnv.translations ]
            , onClick = Nothing
            , theme = articlePreviewsData.theme
            }
            |> Button.withLink (editLink article)
            |> Button.view

    else
        emptyHtml


viewArticleBookmarkButton : ArticlePreviewsData msg -> ArticlePreviewData msg -> Article -> Html msg
viewArticleBookmarkButton articlePreviewsData articlePreviewData article =
    case
        ( articlePreviewsData.onBookmark
        , articlePreviewsData.userPubKey
        , addressComponentsForArticle article
        )
    of
        ( Just ( onAddBookmark, onRemoveBookmark ), Just _, Just addressComponents ) ->
            let
                ( bookmarkMsg, bookmarkIcon ) =
                    if articlePreviewData.interactions.isBookmarked then
                        ( onRemoveBookmark, Icon.MaterialIcon Icon.MaterialOutlineBookmarkAdded 30 Icon.Inherit )

                    else
                        ( onAddBookmark, Icon.MaterialIcon Icon.MaterialOutlineBookmarkAdd 30 Icon.Inherit )
            in
            div
                [ css
                    [ Tw.cursor_pointer
                    ]
                , Events.onClick <| bookmarkMsg addressComponents
                ]
                [ bookmarkIcon
                    |> Icon.view
                ]

        ( _, _, _ ) ->
            emptyHtml


editLink : Article -> Maybe String
editLink article =
    nip19ForArticle article
        |> Maybe.map (\nip19 -> Route.toString { path = Route.Path.Write, query = Dict.singleton "a" nip19, hash = Nothing })


timeParagraph : Styles msg -> BrowserEnv -> Maybe Time.Posix -> Time.Posix -> Html msg
timeParagraph styles browserEnv maybePublishedAt createdAt =
    div
        (colorStyleDate ++ styles.textStyle14)
        [ text <| BrowserEnv.formatDate browserEnv (publishedTime createdAt maybePublishedAt) ]


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
        [ viewProfileImageSmall (linkElementForProfilePubKey pubKey) Nothing ValidationUnknown
        , h2
            [ css
                [ Tw.text_sm
                , Tw.font_semibold
                , Tw.text_color Theme.gray_800
                , Tw.truncate
                ]
            ]
            [ text <| shortenedPubKey 6 pubKey ]
        ]


viewProfileImage : (List (Html msg) -> Html msg) -> Maybe Profile -> ProfileValidation -> Html msg
viewProfileImage linkElement maybeProfile validationStatus =
    div
        [ css
            [ Tw.relative
            ]
        ]
        [ linkElement
            [ img
                [ Attr.src <| Ui.Profile.profilePicture 112 maybeProfile
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
                [ Tw.absolute
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


viewProfileImageSmall : (List (Html msg) -> Html msg) -> Maybe Profile -> ProfileValidation -> Html msg
viewProfileImageSmall linkElement maybeProfile validationStatus =
    div
        [ css
            [ Tw.relative
            ]
        ]
        [ linkElement
            [ img
                [ css
                    [ Tw.w_8
                    , Tw.h_8
                    , Tw.rounded_3xl
                    ]
                , Attr.src <| Ui.Profile.profilePicture 32 maybeProfile
                , Attr.alt "profile image"
                , Attr.attribute "loading" "lazy"
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


viewTitleSummaryImagePreview : Styles msg -> Article -> Html msg
viewTitleSummaryImagePreview styles article =
    div
        [ css
            [ Tw.flex
            , Tw.flex_row
            , Tw.space_x_5
            ]
        ]
        [ div []
            [ viewTitlePreview styles article.title (linkToArticle article) []
            , viewSummary styles article.summary
            ]
        , viewArticleImage article.image
        ]
