module Ui.Article exposing (..)

import BrowserEnv exposing (BrowserEnv)
import Components.Button as Button
import Components.Icon as Icon exposing (Icon)
import Css
import Dict
import FeatherIcons
import Html.Styled as Html exposing (Html, a, article, aside, button, div, h2, h3, h4, img, label, main_, p, span, summary, text)
import Html.Styled.Attributes as Attr exposing (class, css, href)
import Html.Styled.Events as Events exposing (..)
import Markdown
import Nostr
import Nostr.Article exposing (Article, addressComponentsForArticle, nip19ForArticle)
import Nostr.Event exposing (AddressComponents, Kind(..), TagReference(..), numberForKind)
import Nostr.Nip19 as Nip19
import Nostr.Nip27 exposing (GetProfileFunction)
import Nostr.Profile exposing (Author, Profile, ProfileValidation(..))
import Nostr.Reactions exposing (Interactions)
import Nostr.Types exposing (PubKey)
import Route
import Route.Path
import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw
import Tailwind.Theme as Theme
import TailwindExtensions exposing (bp_xsl)
import Time
import Translations.Posts
import Ui.Links exposing (linkElementForProfile, linkElementForProfilePubKey)
import Ui.Profile exposing (profileDisplayName, shortenedPubKey)
import Ui.Styles exposing (Styles, Theme, darkMode, fontFamilyUnbounded, stylesForTheme)

-- single article

viewArticle : ArticlePreviewsData msg -> ArticlePreviewData -> Article -> Html msg
viewArticle articlePreviewsData articlePreviewData article =
    let
        styles =
            Ui.Styles.stylesForTheme articlePreviewsData.theme

        getProfile =
            Nostr.getProfile articlePreviewsData.nostr

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
    in
    div
        [ css
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
                (
                [ css
                    [ Tw.flex_col
                    , Tw.justify_start
                    , Tw.items_start
                    , Tw.gap_4
                    , Tw.inline_flex
                    ]
                ] ++ contentMargins)
                [ viewTags styles article
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
                        (styles.textStyleH1Article ++ styles.colorStyleGrayscaleTitle ++
                        [ css
                            [ Tw.max_w_screen_sm
                            , Bp.sm
                                [ Tw.max_w_prose
                                ]
                            ]
                        ])
                        [ text <| Maybe.withDefault "" article.title ]
                    , Html.summary
                        (styles.textStyleH4Article ++ styles.colorStyleGrayscaleText ++
                        [ css
                            [ Tw.max_w_screen_sm
                            , Bp.sm
                                [ Tw.max_w_prose
                                , Tw.list_none
                                ]
                            ]
                        ])
                        [ text <| Maybe.withDefault "" article.summary ]
                    , viewAuthorAndDate styles articlePreviewsData.browserEnv article.publishedAt article.createdAt articlePreviewData.author
                    ]
                ]
            , viewArticleImage article.image
            , div
                (styles.colorStyleGrayscaleMuted ++ styles.textStyleReactions ++
                [ css
                    [ Tw.flex_col
                    , Tw.justify_start
                    , Tw.items_start
                    , Tw.gap_4
                    , Tw.flex
                    , Tw.mb_4
                    ]
                ] ++ contentMargins)
                [ viewInteractions styles articlePreviewsData.browserEnv articlePreviewData.interactions
                , viewContent styles getProfile article.content
                , viewInteractions styles articlePreviewsData.browserEnv articlePreviewData.interactions
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
                    [ Attr.src image
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
            div [][]


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
            div [][]


viewSummary : Styles msg -> Maybe String -> Html msg
viewSummary styles maybeSummary =
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
            div [][]


viewTags : Styles msg -> Article -> Html msg
viewTags styles article =
    article.hashtags
    |> List.intersperse " / "
    |> List.map viewTag
    |> div
        (styles.textStyleArticleHashtags ++ styles.colorStyleArticleHashtags)


viewTag : String -> Html msg
viewTag tag =
    a
        [ href ("/t/" ++ tag )
        ]
        [ text tag ]


viewInteractions : Styles msg -> BrowserEnv -> Interactions -> Html msg
viewInteractions styles browserEnv interactions =
    let
        bookmarkIcon =
            if interactions.isBookmarked then
                Icon.MaterialIcon Icon.MaterialOutlineBookmarkAdded 30 Icon.Inherit
            else
                Icon.MaterialIcon Icon.MaterialOutlineBookmarkAdd 30 Icon.Inherit
    in
    div
        [ css
            [ Tw.justify_start
            , Tw.items_start
            , Tw.gap_6
            , Tw.inline_flex
            ]
        ]
        [ viewReactions styles (Icon.FeatherIcon FeatherIcons.messageSquare) (Maybe.map String.fromInt interactions.notes)
        , viewReactions styles (Icon.FeatherIcon FeatherIcons.edit) (Maybe.map String.fromInt interactions.reactions)
        , viewReactions styles (Icon.FeatherIcon FeatherIcons.repeat) (Maybe.map String.fromInt interactions.reposts)
        , viewReactions styles (Icon.FeatherIcon FeatherIcons.zap) (Maybe.map (formatZapNum browserEnv) interactions.zaps)
        , viewReactions styles bookmarkIcon (Maybe.map String.fromInt interactions.bookmarks)
        ]
                

viewReactions : Styles msg -> Icon -> Maybe String -> Html msg
viewReactions styles icon maybeCount =
    div
        (styles.colorStyleLabel ++
        [ css
            [ Tw.rounded_3xl
            , Tw.justify_center
            , Tw.items_center
            , Tw.gap_1
            , Tw.flex
            ]
        ])
        [ div
            [ css
                [ Tw.w_5
                , Tw.h_5
                , Tw.px_0_dot_5
                , Tw.py_0_dot_5
                , Tw.justify_center
                , Tw.items_center
                , Tw.flex
                ]
            ]
            [ Icon.view icon]
        , div
            [ ] [ text (maybeCount |> Maybe.withDefault "0" ) ]
        ]

formatZapNum : BrowserEnv -> Int -> String
formatZapNum browserEnv bigNum =
    browserEnv.formatNumber "0 a" <| toFloat bigNum

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
                        (styles.textStyleArticleAuthor ++ styles.colorStyleArticleHashtags ++
                        [ css
                            [ Tw.left_0
                            , Tw.top_0
                            ]
                        ])
                        [ text <| profileDisplayName profile.pubKey profile ]
                    , viewArticleTime styles browserEnv published createdAt
                    ]
                ]

viewArticleProfileSmall : Profile -> ProfileValidation -> Html msg
viewArticleProfileSmall profile validationStatus =
    let
        image =
            profile.picture
            |> Maybe.withDefault Ui.Profile.defaultProfileImage

        linkElement =
            linkElementForProfile profile
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
                    [ Attr.src image
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
    case maybePublishedAt of
        Just publishedAt ->
            div
                (styles.textStyleArticleDate ++ styles.colorStyleGrayscaleMuted ++
                [ css
                    [ Tw.left_0
                    , Tw.top_5
                    ]
                ])
                [ text <| BrowserEnv.formatDate browserEnv (publishedTime createdAt maybePublishedAt) ]

        Nothing ->
            div [][]

viewContent : Styles msg -> GetProfileFunction -> String -> Html msg
viewContent styles fnGetProfile content =
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
        [ viewContentMarkdown styles fnGetProfile content 
        ]

viewContentMarkdown : Styles msg -> GetProfileFunction -> String -> Html msg
viewContentMarkdown styles fnGetProfile content =
    case Markdown.markdownViewHtml styles fnGetProfile content of
        Ok html ->
            html

        Err error ->
            div [][]


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
                    (styles.textStyleArticleAuthor ++ styles.colorStyleArticleHashtags)
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
                            (styles.textStyleBody ++ styles.colorStyleGrayscaleMuted ++
                            [ css
                                [ Tw.left_4
                                , Tw.top_3
                                ]
                            ])
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
                        (styles.textStyleReactions ++ styles.colorStyleInverse)
                        [ text "Post Comment" ]
                    ]
                ]
            ]

viewArticleInternal : Styles msg -> GetProfileFunction -> BrowserEnv -> Article -> Html msg
viewArticleInternal styles fnGetProfile browserEnv article =
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
                (styles.textStyleH1Article ++ styles.colorStyleGrayscaleTitle ++
                [ css
                    [ -- Tw.w_96
                    ]
                ])
                [ text <| Maybe.withDefault "" article.title ]
            , div
                (styles.textStyleH4Article ++ styles.colorStyleGrayscaleText ++
                [ css
                    [ -- Tw.w_96
                    ]
                ])
                [ text <| Maybe.withDefault "" article.summary ]
            , viewContent styles fnGetProfile article.content
            ]
        ]

-- article previews

type alias ArticlePreviewsData msg =
    { theme : Theme
    , browserEnv : BrowserEnv
    , nostr : Nostr.Model
    , userPubKey : Maybe PubKey
    , onBookmark : Maybe ((AddressComponents -> msg), (AddressComponents -> msg)) -- msgs for adding/removing a bookmark
    }

type alias ArticlePreviewData =
    { author : Author
    , interactions : Interactions
    , displayAuthor : Bool
    }

viewArticlePreviewList : ArticlePreviewsData msg -> ArticlePreviewData -> Article -> Html msg
viewArticlePreviewList articlePreviewsData articlePreviewData article =
    let
        styles =
            Ui.Styles.stylesForTheme articlePreviewsData.theme

        textWidthAttr =
            case article.image of
                Just _ ->
                    [ Tw.w_80
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
                
                Nothing ->
                    [ Tw.w_80
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
    in
    div
        [ css
            [Tw.pb_6
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
                        (styles.colorStyleGrayscaleText ++ styles.textStyleBody ++ 
                        [ css
                            (summaryLinesAttr ++ textWidthAttr)
                        ])
                        [ text summaryText ]
                    , viewHashTags styles article.hashtags textWidthAttr
                    ]
                ]
            ]
        ]

linkToArticle : Article -> Maybe String
linkToArticle article =
    case Nip19.encode <| Nip19.NAddr 
            { identifier = article.identifier |> Maybe.withDefault ""
            , pubKey = article.author
            , kind = numberForKind article.kind
            , relays = Maybe.map (\urlWithoutProtocol -> [ "wss://" ++ urlWithoutProtocol ]) article.relay |> Maybe.withDefault []
            } of
        Ok encodedCoordinates ->
            Just <| "/a/" ++ encodedCoordinates

        Err error ->
            Nothing



viewTitlePreview : Styles msg -> Maybe String -> Maybe String -> List Css.Style -> Html msg
viewTitlePreview styles maybeTitle maybeLinkTarget textWidthAttr =
    case (maybeTitle, maybeLinkTarget) of
        (Just title, Just linkUrl) ->
            a
                (styles.colorStyleGrayscaleTitle ++ styles.textStyleH2 ++ 
                [ css
                    ([ Tw.line_clamp_2
                    ] ++ textWidthAttr)
                , href linkUrl
                ])
                [ text title ]

        (Just title, Nothing) ->
            div
                (styles.colorStyleGrayscaleTitle ++ styles.textStyleH2 ++ 
                [ css
                    ([ Tw.line_clamp_2
                    ] ++ textWidthAttr)
                ])
                [ text title ]

        (Nothing, _) ->
            div [][]


viewHashTags : Styles msg -> List String -> List Css.Style -> Html msg
viewHashTags styles hashTags widthAttr =
    if List.length hashTags > 0 then
        hashTags
        |> List.map (viewHashTag styles)
        |> div
            [ css
                (widthAttr ++ 
                [ Tw.space_x_2
                , Tw.mb_2
                , Tw.gap_2
                , Tw.line_clamp_1
                , Tw.text_clip
                ])
            ]
    else
        div [][]

viewHashTag : Styles msg -> String -> Html msg
viewHashTag styles hashTag =
    a
        [ css
            [ Tw.px_4
            , Tw.py_2
            , Tw.bg_color Theme.gray_300
            , Tw.rounded_3xl
            , Tw.inline_block
            , darkMode
                [ Tw.bg_color Theme.neutral_700
                ]
            ]
        , href ("/t/" ++ hashTag )
        ]
        [
        div
            (styles.colorStyleLabel ++ styles.textStyleUppercaseLabel ++
            [ css
                [ Tw.whitespace_nowrap
                ]
            ]
            )
            [ text hashTag ]
        ]

viewArticlePreviewBigPicture : ArticlePreviewsData msg -> ArticlePreviewData -> Article -> Html msg
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
                (styles.textStyleH4 ++ styles.colorStyleGrayscaleTitle ++
                [ css
                    [ Tw.w_96
                    ]
                ])
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
                    [ Attr.src image
                    , Attr.style "top" "50%"
                    , Attr.style "left" "50%"
                    , Attr.style "object-fit" "cover"
                    , Attr.style "width" "100%"
                    , Attr.style "height" "100%"
                    , Attr.style "position" "absolute"
                    , Attr.style "transform" "translate(-50%, -50%)"
                    ]
                    [
                    ]
                ]

        Nothing ->
            div [][]
    
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
                    ]
                    [
                    ]
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

viewAuthorAndDatePreview : ArticlePreviewsData msg -> ArticlePreviewData -> Article -> Html msg
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
                    [ viewProfileImageSmall (linkElementForProfile profile) profile.picture validationStatus
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
        div [][]


viewArticleBookmarkButton : ArticlePreviewsData msg -> ArticlePreviewData -> Article -> Html msg
viewArticleBookmarkButton articlePreviewsData articlePreviewData article =
    case
        ( articlePreviewsData.onBookmark
        , articlePreviewsData.userPubKey
        , addressComponentsForArticle article
        ) of
        (Just (onAddBookmark, onRemoveBookmark), Just userPubKey, Just addressComponents) ->
            let
                (bookmarkMsg, bookmarkIcon) =
                    if articlePreviewData.interactions.isBookmarked then
                        (onRemoveBookmark, Icon.MaterialIcon Icon.MaterialOutlineBookmarkAdded 30 Icon.Inherit)
                    else
                        (onAddBookmark, Icon.MaterialIcon Icon.MaterialOutlineBookmarkAdd 30 Icon.Inherit)
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
            div [][]


editLink : Article -> Maybe String
editLink article =
    nip19ForArticle article
    |> Maybe.map (\nip19 -> Route.toString { path = Route.Path.Write, query = Dict.singleton "a" nip19, hash = Nothing })


viewProfileSmall : Profile -> ProfileValidation -> Html msg
viewProfileSmall profile validationStatus =
    div
        [ css
            [ Tw.h_8
            , Tw.justify_start
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
--            , Attr.src image
            , Attr.alt "Profile image"
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
                    [ Tw.text_color Theme.gray_500
                    , Tw.text_sm
                    , Tw.font_normal
--                    , Tw.font_['Inter']
                    ]
                ]
                [ text "Vortex-948" ]
            , div
                [ css
                    [ Tw.text_color Theme.gray_400
                    , Tw.text_sm
                    , Tw.font_normal
--                    , Tw.font_['Inter']
                    ]
                ]
                [ text "Apr. 15" ]
            ]
        ]

timeParagraph : Styles msg -> BrowserEnv -> Maybe Time.Posix -> Time.Posix -> Html msg
timeParagraph styles browserEnv maybePublishedAt createdAt =
    div
        (styles.colorStyleGrayscaleMuted ++ styles.textStyle14)
        [ text <| BrowserEnv.formatDate browserEnv (publishedTime createdAt maybePublishedAt) ]

publishedTime : Time.Posix -> Maybe Time.Posix -> Time.Posix
publishedTime createdAt maybePublishedAt =
    case maybePublishedAt of
        Just publishedAt ->
            -- some clients produce(d) wrong article dates > year 55000.
            -- maybe missed a conversion from milliseconds to seconds
            if Time.toYear Time.utc publishedAt > 50000 then
                -- show event creation time in this case
                createdAt
            else
                publishedAt

        Nothing ->
            createdAt


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
                [ viewProfileImageSmall (linkElementForProfilePubKey pubKey) (Just Ui.Profile.defaultProfileImage) ValidationUnknown
                , h2
                    [ css
                        [ Tw.text_sm
                        , Tw.font_semibold
                        , Tw.text_color Theme.gray_800
                        , Tw.truncate
                        ]
                    ]
                    [ text <| shortenedPubKey pubKey ]
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
                [ css
                    [ Tw.w_8
                    , Tw.h_8
                    , Tw.rounded_3xl
                    ]
                , Attr.src image
                , Attr.alt "profile image"
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
