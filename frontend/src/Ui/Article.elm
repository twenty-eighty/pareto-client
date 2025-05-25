module Ui.Article exposing (..)

import BrowserEnv exposing (BrowserEnv)
import Components.ArticleComments as ArticleComments
import Components.Button as Button
import Components.Comment as Comment
import Components.Icon as Icon
import Components.InteractionButton as InteractionButton
import Components.Interactions
import Components.SharingButtonDialog as SharingButtonDialog
import Components.ZapDialog as ZapDialog
import Css
import Dict exposing (Dict)
import Html.Styled as Html exposing (Html, a, article, div, h2, h3, img, summary, text)
import Html.Styled.Attributes as Attr exposing (css, href)
import Html.Styled.Events as Events exposing (..)
import I18Next
import LinkPreview exposing (LoadedContent)
import Locale
import Markdown
import Nostr
import Nostr.Article exposing (Article, addressComponentsForArticle, nip19ForArticle, publishedTime)
import Nostr.Event exposing (AddressComponents, Kind(..), Tag(..), TagReference(..))
import Nostr.Nip05 exposing (nip05ToString)
import Nostr.Nip19 as Nip19 exposing (NIP19Type(..))
import Nostr.Nip22 exposing (CommentType)
import Nostr.Nip27 exposing (GetProfileFunction)
import Nostr.Profile exposing (Author(..), Profile, ProfileValidation(..), profileDisplayName, shortenedPubKey)
import Nostr.Reactions exposing (Interactions)
import Nostr.Relay exposing (websocketUrl)
import Nostr.Types exposing (EventId, LoginStatus, PubKey, loggedInPubKey, loggedInSigningPubKey)
import Pareto
import Route
import Route.Path
import Set
import Tailwind.Breakpoints as Bp
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw
import Time
import Translations.ArticleView as Translations
import Translations.Posts
import Ui.Interactions exposing (Actions, extendedZapRelays, viewInteractions, viewInteractionsNew)
import Ui.Links exposing (linkElementForProfile, linkElementForProfilePubKey)
import Ui.Profile
import Ui.Shared exposing (emptyHtml)
import Ui.Styles exposing (Styles, Theme(..), darkMode, fontFamilyInter, fontFamilyRobotoMono, fontFamilyUnbounded, print)
import Url


type alias ArticlePreviewsData msg =
    { theme : Theme
    , browserEnv : BrowserEnv
    , nostr : Nostr.Model
    , loginStatus : LoginStatus
    , onBookmark : Maybe ( AddressComponents -> msg, AddressComponents -> msg ) -- msgs for adding/removing a bookmark
    , commenting : Maybe ( Comment.Comment msg, CommentType -> msg )
    , onReaction : Maybe (EventId -> PubKey -> AddressComponents -> msg)
    , onRepost : Maybe msg
    , onZap : Maybe (List ZapDialog.Recipient -> msg)
    , articleToInteractionsMsg : InteractionButton.InteractionObject -> Components.Interactions.Msg msg -> msg
    , openCommentMsg : Maybe msg
    , sharing : Maybe ( SharingButtonDialog.Model, SharingButtonDialog.Msg -> msg )
    }


type alias ArticlePreviewData msg =
    { author : Author
    , actions : Actions msg
    , interactions : Interactions
    , articleInteractions : Components.Interactions.Model
    , displayAuthor : Bool
    , loadedContent : Maybe (LoadedContent msg)
    }


linkToHashtag : String -> String
linkToHashtag hashtag =
    "/t/" ++ Url.percentEncode hashtag



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

        maybeProfile =
            getProfile article.author

        validationStatus =
            Nostr.getProfileValidationStatus articlePreviewsData.nostr article.author
                |> Maybe.withDefault ValidationUnknown

        linkElement =
            maybeProfile
                |> Maybe.map (\profile -> linkElementForProfile profile validationStatus)
                |> Maybe.withDefault (linkElementForProfilePubKey article.author)

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


        interactionObject =
            InteractionButton.Article article.id ( article.kind, article.author, article.identifier |> Maybe.withDefault "" )

        previewData : Ui.Interactions.PreviewData msg
        previewData =
            { browserEnv = articlePreviewsData.browserEnv
            , loginStatus = articlePreviewsData.loginStatus
            , maybeNip19Target = nip19ForArticle article
            , zapRelays = extendedZapRelays articleRelays articlePreviewsData.nostr (articlePreviewsData.loginStatus |> loggedInPubKey)
            , actions = articlePreviewData.actions
            , interactions = articlePreviewData.interactions
            , interactionsModel = articlePreviewData.articleInteractions
            , interactionObject = interactionObject
            , toInteractionsMsg = articlePreviewsData.articleToInteractionsMsg interactionObject
            , openCommentMsg = articlePreviewsData.openCommentMsg
            , nostr = articlePreviewsData.nostr
            , sharing = articlePreviewsData.sharing
            , sharingInfo = sharingInfoForArticle article articlePreviewData.author
            , translations = articlePreviewsData.browserEnv.translations
            , theme = articlePreviewsData.theme
            }
    in
    div
        [ css
            [ Tw.flex
            , Tw.flex_wrap
            , Tw.w_full
            , Tw.neg_mb_4
            , Bp.md [ Tw.mb_0 ]
            ]
        ]
        [ div
            [ css
                [ Tw.flex_1
                ]
            ]
            [ div
                [ css
                    [ Tw.relative
                    , Tw.flex
                    , Tw.flex_col
                    , Tw.items_center
                    , print
                        [ Tw.hidden
                        ]
                    ]
                ]
                [ Ui.Profile.viewBanner (getProfile article.author |> Maybe.andThen .banner)
                , div
                    [ css
                        [ Tw.absolute
                        , Tw.top_3over4
                        , Tw.z_10
                        ]
                    ]
                    [ Ui.Profile.viewProfileImage linkElement maybeProfile ValidationUnknown ]
                ]
            , Html.article
                (langAttr
                    ++ [ css
                            [ Tw.flex_col
                            , Tw.justify_start
                            , Tw.items_center
                            , Tw.gap_12
                            , Tw.inline_flex
                            , Tw.px_2
                            , Tw.my_20
                            , Tw.w_full

                            -- switch off ligatures - Inter font doesn't have ligatures
                            , Css.property "font-variant-ligatures" "none"
                            , Css.property "font-feature-settings" "\"liga\" 0"
                            , print
                                [ Tw.my_4
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
                        , Tw.mb_20
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
                        [ div [ css [ Bp.lg [ Tw.hidden ] ] ] [ viewTags articlePreviewsData.browserEnv.translations article ]
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
                            , Tw.mb_2
                            , Tw.flex
                            ]
                            :: styles.colorStyleGrayscaleMuted
                            ++ textStyleReactions
                            ++ contentMargins
                        )
                        [ viewInteractionsNew previewData "1"
                        , viewContent styles articlePreviewData.loadedContent getProfile article.content
                        , viewInteractionsNew previewData "2"
                        ]
                    , case articlePreviewsData.commenting of
                        Just ( comment, _ ) ->
                            div
                                [ 
                                ]
                                [ comment
                                    |> Comment.view
                                ]

                        Nothing ->
                            emptyHtml
                    , div
                        [ css
                            [ Tw.mt_2 ]
                        ]
                        [ ArticleComments.new
                            { browserEnv = articlePreviewsData.browserEnv
                            , nostr = articlePreviewsData.nostr
                            , articleComments = articlePreviewData.interactions.articleComments
                            , articleCommentComments = articlePreviewData.interactions.articleCommentComments
                            , interactions = Dict.empty
                            , toInteractionsMsg = articlePreviewsData.articleToInteractionsMsg
                            , loginStatus = articlePreviewsData.loginStatus
                            , theme = articlePreviewsData.theme
                            }
                            |> ArticleComments.view
                        ]
                    ]
                ]
            ]
        ]


sharingInfoForArticle : Article -> Author -> SharingButtonDialog.SharingInfo
sharingInfoForArticle article author =
    { url =
        linkToArticle author article
            |> Maybe.map (\relativeUrl -> Pareto.applicationUrl ++ relativeUrl)
            |> Maybe.withDefault ""
    , title = Maybe.withDefault "" article.title
    , text = Maybe.withDefault "" article.summary
    , hashtags = article.hashtags
    }


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
                    [ Attr.src (Ui.Links.scaledImageLink 384 image)
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


viewTitle : Styles msg -> Maybe String -> Html msg
viewTitle styles maybeTitle =
    case maybeTitle of
        Just title ->
            h3
                ([ css
                    [ Tw.text_4xl
                    , Tw.font_bold
                    , Tw.mb_2
                    ]
                 , fontFamilyUnbounded
                 ]
                    ++ styles.colorStyleGrayscaleTitle
                )
                [ text title
                ]

        Nothing ->
            emptyHtml


viewSummary : Styles msg -> Maybe String -> Html msg
viewSummary styles maybeSummary =
    case maybeSummary of
        Just summary ->
            Html.summary
                (css
                    [ Tw.text_sm
                    , Tw.mb_4
                    , Tw.list_none
                    ]
                    :: styles.colorStyleGrayscaleSummary
                )
                [ text summary ]

        Nothing ->
            emptyHtml


viewTags : I18Next.Translations -> Article -> Html msg
viewTags translations article =
    article.hashtags
        |> List.map removeHashTag
        |> List.map (viewHashTag translations)
        |> List.intersperse (text " / ")
        |> div
            (textStyleArticleHashtags ++ colorStyleArticleHashtags)


removeHashTag : String -> String
removeHashTag hashTag =
    if String.startsWith "#" hashTag then
        String.dropLeft 1 hashTag

    else
        hashTag


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
                    , Bp.lg [ Tw.hidden ]
                    , print
                        [ Tw.block
                        ]
                    ]
                ]
                [ Ui.Profile.viewProfilePubKey styles pubKey
                , timeParagraph styles browserEnv published createdAt
                ]

        Nostr.Profile.AuthorProfile profile validationStatus ->
            div
                [ css
                    [ Tw.flex
                    , Tw.flex_row
                    , Tw.w_full
                    , Tw.items_center
                    , Tw.space_x_2
                    , Tw.mb_4
                    , Bp.lg [ Tw.hidden ]
                    , print
                        [ Tw.block
                        ]
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
                        [ linkElementForProfile profile validationStatus [ text <| profileDisplayName profile.pubKey profile ] ]
                    , viewArticleTime browserEnv published createdAt
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
            , Tw.w_auto
            , print
                [ Tw.hidden
                ]
            ]
        ]
        [ linkElement
            [ div
                [ css
                    [ Tw.w_12
                    ]
                ]
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
                    , Tw.w_3
                    , Tw.h_2
                    ]
                ]
                [ Ui.Profile.validationIcon 16 validationStatus ]
            ]
        ]


viewArticleTime : BrowserEnv -> Maybe Time.Posix -> Time.Posix -> Html msg
viewArticleTime browserEnv maybePublishedAt createdAt =
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

        articleUrl =
            linkToArticle articlePreviewData.author article
    in
    div
        (css
            [ Tw.pb_6
            , Tw.border_b
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
                [ Tw.h_auto
                , Css.property "width" "550px"
                ]
            ]
            :: styles.colorStyleBorders
        )
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
                , Bp.lg
                    [ Tw.inline_flex
                    , Tw.flex_row_reverse
                    , Tw.gap_10
                    ]
                ]
            ]
            [ previewListImage articlePreviewsData.browserEnv.translations articleUrl article
            , div
                [ css
                    [ Tw.flex_col
                    , Tw.justify_start
                    , Tw.items_start
                    , Tw.gap_4
                    , Tw.inline_flex
                    , print [ Tw.block ]
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
                    [ viewTitlePreview articlePreviewsData.browserEnv.translations styles article.title articleUrl textWidthAttr
                    , viewListSummary styles textWidthAttr articleUrl articlePreviewsData.browserEnv.translations article summaryText
                    , viewHashTags articlePreviewsData.browserEnv.translations article.hashtags (hashtagsHeightAttr :: textWidthAttr)
                    ]
                ]
            ]
        ]


linkToArticle : Author -> Article -> Maybe String
linkToArticle author article =
    let
        articleRelays =
            article.relays
                |> Set.toList
                -- append max 5 relays so the link doesn't get infinitely long
                |> List.take 5
                |> List.map websocketUrl
    in
    case ( author, article.identifier ) of
        ( Nostr.Profile.AuthorProfile { nip05 } ValidationSucceeded, Just identifier ) ->
            Just <| "/u/" ++ (nip05 |> Maybe.map nip05ToString |> Maybe.withDefault "") ++ "/" ++ identifier

        ( _, Just identifier ) ->
            Nip19.NAddr
                { identifier = identifier
                , pubKey = article.author
                , kind = Nostr.Event.numberForKind article.kind
                , relays = articleRelays
                }
                |> Nip19.encode
                |> Result.toMaybe
                |> Maybe.map (\naddr -> "/a/" ++ naddr)

        ( _, Nothing ) ->
            NEvent
                { id = article.id
                , author = Just article.author
                , kind = Just <| Nostr.Event.numberForKind article.kind
                , relays = articleRelays
                }
                |> Nip19.encode
                |> Result.toMaybe
                |> Maybe.map (\nevent -> "/a/" ++ nevent)


viewTitlePreview : I18Next.Translations -> Styles msg -> Maybe String -> Maybe String -> List Css.Style -> Html msg
viewTitlePreview translations styles maybeTitle maybeLinkTarget textWidthAttr =
    case ( maybeTitle, maybeLinkTarget ) of
        ( Just title, Just linkUrl ) ->
            a
                (styles.colorStyleGrayscaleTitle
                    ++ styles.textStyleH2
                    ++ [ css
                            (Tw.line_clamp_2 :: textWidthAttr)
                       , href linkUrl
                       , Attr.attribute "aria-label" (Translations.linkToArticleAriaLabel [ translations ] { title = title })
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


viewListSummary : Styles msg -> List Css.Style -> Maybe String -> I18Next.Translations -> Article -> String -> Html msg
viewListSummary styles textWidthAttr articleUrl translations article summaryText =
    let
        summaryLinesAttr =
            if List.length article.hashtags < 1 then
                [ Tw.line_clamp_5 ]

            else
                [ Tw.line_clamp_3 ]
    in
    a
        [ href (articleUrl |> Maybe.withDefault "")
        , Attr.attribute "aria-label" (Translations.linkToArticleAriaLabel [ translations ] { title = article.title |> Maybe.withDefault article.id })
        ]
        [ div
            (styles.colorStyleGrayscaleText
                ++ styles.textStyleBody
                ++ [ css
                        (summaryLinesAttr ++ textWidthAttr)
                   ]
            )
            [ text summaryText ]
        ]


viewHashTags : I18Next.Translations -> List String -> List Css.Style -> Html msg
viewHashTags translations hashTags widthAttr =
    if List.length hashTags > 0 then
        hashTags
            |> List.map (viewHashTag translations)
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


viewHashTag : I18Next.Translations -> String -> Html msg
viewHashTag translations hashtag =
    a
        [ css [ Tw.inline_block ]
        , href (linkToHashtag hashtag)
        , Attr.attribute "aria-label" (Translations.linkToHashtagAriaLabel [ translations ] { hashtag = hashtag })
        ]
        [ text hashtag ]


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
        [ previewBigPictureImage article articlePreviewsData
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


previewListImage : I18Next.Translations -> Maybe String -> Article -> Html msg
previewListImage translations articleUrl article =
    case article.image of
        Just image ->
            a
                [ href (articleUrl |> Maybe.withDefault "")
                , Attr.attribute "aria-label" (Translations.linkToArticleAriaLabel [ translations ] { title = article.title |> Maybe.withDefault article.id })
                ]
                [ div
                    [ css
                        [ Tw.w_80
                        , Tw.h_44
                        , Tw.overflow_hidden
                        , Tw.relative
                        ]
                    ]
                    [ img
                        [ Attr.src (Ui.Links.scaledImageLink 384 image)
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
                ]

        Nothing ->
            div [] []


previewBigPictureImage : Article -> ArticlePreviewsData msg -> Html msg
previewBigPictureImage article articlePreviewData =
    let
        styles =
            Ui.Styles.stylesForTheme articlePreviewData.theme
    in
    case article.image of
        Just image ->
            div
                [ css
                    [ Tw.w_96
                    , Tw.h_60
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
                (css
                    [ Tw.w_96
                    , Tw.h_60
                    , Tw.overflow_hidden
                    , Tw.relative
                    , Tw.rounded_xl
                    ]
                    :: styles.colorStyleBackground
                )
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
                [ viewProfilePubKey articlePreviewsData.browserEnv.translations pubKey
                , timeParagraph styles articlePreviewsData.browserEnv article.publishedAt article.createdAt
                ]

        Nostr.Profile.AuthorProfile profile validationStatus ->
            let
                linkElementWrapper =
                    linkElementForProfile profile validationStatus
            in
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
                    [ viewProfileImageSmall articlePreviewsData.browserEnv.translations linkElementWrapper (Just profile) validationStatus
                    , div
                        [ css
                            [ Tw.justify_start
                            , Tw.items_start
                            , Tw.gap_2
                            , Tw.flex
                            ]
                        ]
                        [ linkElementWrapper
                            [ div
                                (styles.colorStyleGrayscaleText ++ styles.textStyle14)
                                [ text (profileDisplayName profile.pubKey profile) ]
                            ]
                        , timeParagraph styles articlePreviewsData.browserEnv article.publishedAt article.createdAt
                        ]
                    ]
                , viewArticleEditButton articlePreviewsData article profile.pubKey
                , viewArticleBookmarkButton articlePreviewsData articlePreviewData article
                ]


viewArticleEditButton : ArticlePreviewsData msg -> Article -> PubKey -> Html msg
viewArticleEditButton articlePreviewsData article articleAuthorPubKey =
    if (articlePreviewsData.loginStatus |> loggedInSigningPubKey) == Just articleAuthorPubKey then
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
        , articlePreviewsData.loginStatus |> loggedInSigningPubKey
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


viewProfilePubKey : I18Next.Translations -> PubKey -> Html msg
viewProfilePubKey translations pubKey =
    div
        [ css
            [ Tw.flex
            , Tw.items_center
            , Tw.space_x_2
            , Tw.mb_4
            ]
        ]
        [ viewProfileImageSmall translations (linkElementForProfilePubKey pubKey) Nothing ValidationUnknown
        , h2
            [ css
                [ Tw.text_sm
                , Tw.font_semibold
                , Tw.text_color Theme.green_500
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
                , Tw.w_4
                , Tw.h_4
                ]
            ]
            [ Ui.Profile.validationIcon 24 validationStatus
            ]
        ]


viewProfileImageSmall : I18Next.Translations -> (List (Html msg) -> Html msg) -> Maybe Profile -> ProfileValidation -> Html msg
viewProfileImageSmall translations linkElement maybeProfile validationStatus =
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
                , Attr.attribute "aria-label" (Translations.linkToProfileAriaLabel [ translations ] { author = maybeProfile |> Maybe.map (\profile -> profileDisplayName profile.pubKey profile) |> Maybe.withDefault "" })
                , Attr.attribute "loading" "lazy"
                ]
                []
            ]
        , div
            [ css
                [ Tw.absolute
                , Tw.top_0
                , Tw.right_0
                , Tw.max_w_2
                , Tw.max_h_2
                ]
            ]
            [ Ui.Profile.validationIcon 16 validationStatus
            ]
        ]


viewTitleSummaryImagePreview : I18Next.Translations -> Styles msg -> Author -> Article -> Html msg
viewTitleSummaryImagePreview translations styles author article =
    div
        [ css
            [ Tw.flex
            , Tw.flex_row
            , Tw.space_x_5
            ]
        ]
        [ div []
            [ viewTitlePreview translations styles article.title (linkToArticle author article) []
            , viewSummary styles article.summary
            ]
        , viewArticleImage article.image
        ]
