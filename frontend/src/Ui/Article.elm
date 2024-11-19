module Ui.Article exposing (..)

import BrowserEnv exposing (BrowserEnv)
import Css
import Graphics
import Html.Styled as Html exposing (Html, a, article, aside, button, div, h2, h3, h4, img, main_, p, span, text)
import Html.Styled.Attributes as Attr exposing (class, css, href)
import Html.Styled.Events as Events exposing (..)
import Markdown
import Nostr.Article exposing (Article)
import Nostr.Event exposing (Kind(..), TagReference(..), numberForKind)
import Nostr.Nip19 as Nip19
import Nostr.Profile exposing (Author, Profile, ProfileValidation(..))
import Nostr.Reactions exposing (Interactions)
import Nostr.Types exposing (PubKey)
import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw
import Tailwind.Theme as Theme
import Ui.Links exposing (linkElementForProfile, linkElementForProfilePubKey)
import Ui.Profile exposing (profileDisplayName, shortenedPubKey)
import Ui.Shared
import Ui.Styles exposing (Styles, darkMode, fontFamilyInter, fontFamilyUnbounded, fontFamilySourceSerifPro)
import Time

-- single article

viewArticle : Styles msg -> BrowserEnv -> Author -> Article -> Interactions -> Html msg
viewArticle styles browserEnv author article interactions =
    div
        [ css
            [ Tw.flex_col
            , Tw.justify_start
            , Tw.items_center
            , Tw.gap_12
            , Tw.inline_flex
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
                [ css
                    [ Tw.mx_48
                    , Tw.flex_col
                    , Tw.justify_start
                    , Tw.items_start
                    , Tw.gap_4
                    , Tw.inline_flex
                    ]
                ]
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
                    [ div
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
                    , viewAuthorAndDate styles browserEnv article.publishedAt author
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
                    , Tw.mx_48
                    ]
                ])
                [ viewInteractions styles browserEnv interactions
                , viewContent styles article.content
                , viewInteractions styles browserEnv interactions
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
            p
                [ css
                    [ Tw.text_color Theme.gray_600
                    , Tw.text_sm
                    , Tw.mb_4
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
    div
        [ css
            [ Tw.justify_start
            , Tw.items_start
            , Tw.gap_6
            , Tw.inline_flex
            ]
        ]
        [ viewReactions styles Graphics.commentsIcon (Maybe.map String.fromInt interactions.notes)
        , viewReactions styles Graphics.likeIcon (Maybe.map String.fromInt interactions.reactions)
        , viewReactions styles Graphics.repostIcon (Maybe.map String.fromInt interactions.reposts)
        , viewReactions styles Graphics.zapIcon (Maybe.map (formatZapNum browserEnv) interactions.zaps)
        ]
                

viewReactions : Styles msg -> Html msg -> Maybe String -> Html msg
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
            [ icon ]
        , div
            [ ] [ text (maybeCount |> Maybe.withDefault "0" ) ]
        ]

formatZapNum : BrowserEnv -> Int -> String
formatZapNum browserEnv bigNum =
    browserEnv.formatNumber "0 a" <| toFloat bigNum

viewAuthorAndDate : Styles msg -> BrowserEnv -> Maybe Time.Posix -> Nostr.Profile.Author -> Html msg
viewAuthorAndDate styles browserEnv published author =
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
                , timeParagraph styles browserEnv published
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
                        [ Tw.w_28
                        , Tw.h_11
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
                        [ text "Jack Whitmore" ]
                    , viewArticleTime styles browserEnv published
                    ]
                ]

viewArticleProfileSmall : Profile -> ProfileValidation -> Html msg
viewArticleProfileSmall profile validationStatus =
    let
        image =
            profile.picture
            |> Maybe.withDefault Ui.Profile.defaultProfileImage
    in
    div
        [ css
            [ Tw.relative
            ]
        ]
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
                , Tw.w_4
                , Tw.h_4
                ]
            ]
            [ Ui.Profile.validationIcon 24 validationStatus ]
        ]

viewArticleTime : Styles msg -> BrowserEnv -> Maybe Time.Posix -> Html msg
viewArticleTime styles browserEnv maybePublishedAt =
    case maybePublishedAt of
        Just publishedAt ->
            div
                (styles.textStyleArticleDate ++ styles.colorStyleGrayscaleMuted ++
                [ css
                    [ Tw.left_0
                    , Tw.top_5
                    ]
                ])
                [ text <| BrowserEnv.formatDate browserEnv publishedAt ]

        Nothing ->
            div [][]

viewContent : Styles msg -> String -> Html msg
viewContent styles content =
    div
        [ css
            [ Tw.flex_col
            , Tw.justify_start
            , Tw.items_start
            , Tw.gap_10
            , Tw.flex
            ]
        ]
        [ viewContentMarkdown styles content 
        ]

viewContentMarkdown : Styles msg -> String -> Html msg
viewContentMarkdown styles content =
    case Markdown.markdownViewHtml styles content of
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

viewArticleInternal : Styles msg -> BrowserEnv -> Article -> Html msg
viewArticleInternal styles browserEnv article =
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
            , viewContent styles article.content
            ]
        ]

-- article previews

viewArticlePreviewList : Styles msg -> BrowserEnv -> Author -> Article -> Interactions -> Bool -> Html msg
viewArticlePreviewList styles browserEnv author article interactions displayAuthor =
    let
        textWidthAttr =
            case article.image of
                Just _ ->
                    [ Tw.w_96 ]
                
                Nothing ->
                    []
    in
    div
        [ css
            [ Tw.h_64
            , Tw.pb_6
            , Tw.border_b
            , Tw.border_color Theme.gray_200
            , Tw.flex_col
            , Tw.justify_start
            , Tw.items_start
            , Tw.gap_2
            , Tw.inline_flex
            ]
        , Attr.style "width" "720px"
        , Attr.style "height" "266px"
        ]
        [ viewAuthorAndDatePreview styles browserEnv article.publishedAt author
        , div
            [ css
                [ Tw.self_stretch
                , Tw.justify_between
                , Tw.items_start
                , Tw.inline_flex
                ]
            ]
            [ div
                [ css
                    [ Tw.h_52
                    , Tw.flex_col
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
                    [ viewTitlePreview styles article.title (linkToArticle article) textWidthAttr
                    , div
                        (styles.colorStyleGrayscaleText ++ styles.textStyleBody ++ 
                        [ css
                            ([ Tw.line_clamp_3
                            ] ++ textWidthAttr)
                        ])
                        [ text <| Maybe.withDefault "" article.summary ]
                    , viewHashTags styles article.hashtags
                    ]
                ]
            , previewListImage article
            ]
        ]
    
linkToArticle : Article -> Maybe String
linkToArticle article =
    case Nip19.encode <| Nip19.NAddr 
            { identifier = article.identifier |> Maybe.withDefault ""
            , pubKey = article.author
            , kind = numberForKind article.kind
            , relays = []
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


viewHashTags : Styles msg -> List String -> Html msg
viewHashTags styles hashTags =
        hashTags
        |> List.take 3
        |> List.map (viewHashTag styles)
        |> div
            [ css
                [ Tw.h_10
                , Tw.justify_start
                , Tw.items_start
                , Tw.gap_2
                , Tw.inline_flex
                ]
            ]

viewHashTag : Styles msg -> String -> Html msg
viewHashTag styles hashTag =
    a
        [ css
            [ Tw.px_4
            , Tw.py_2
            , Tw.bg_color Theme.gray_300
            , Tw.rounded_3xl
            , Tw.justify_center
            , Tw.items_center
            , Tw.gap_2
            , Tw.inline_flex
            , darkMode
                [ Tw.bg_color Theme.neutral_700
                ]
            ]
        , href ("/t/" ++ hashTag )
        ]
        [
        div
            (styles.colorStyleLabel ++ styles.textStyleUppercaseLabel)
            [ text hashTag ]
        ]

viewArticlePreviewBigPicture : Styles msg -> BrowserEnv -> Author -> Article -> Interactions -> Bool -> Html msg
viewArticlePreviewBigPicture styles browserEnv author article interactions displayAuthor =
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
                , viewAuthorAndDatePreview styles browserEnv article.publishedAt author
                ]
            ]


previewListImage : Article -> Html msg
previewListImage article =
    case article.image of
        Just image ->
            div
                [ css
                    [ Tw.w_64
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

viewAuthorAndDatePreview : Styles msg -> BrowserEnv -> Maybe Time.Posix -> Nostr.Profile.Author -> Html msg
viewAuthorAndDatePreview styles browserEnv published author =
    case author of
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
                , timeParagraph styles browserEnv published
                ]

        Nostr.Profile.AuthorProfile profile validationStatus ->
            div
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
                    , timeParagraph styles browserEnv published
                    ]
                ]

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

timeParagraph : Styles msg -> BrowserEnv -> Maybe Time.Posix -> Html msg
timeParagraph styles browserEnv maybePublishedAt =
    case maybePublishedAt of
        Just publishedAt ->
            div
                (styles.colorStyleGrayscaleMuted ++ styles.textStyle14)
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

viewArticleDraftPreview : Styles msg -> BrowserEnv -> Article -> Html msg
viewArticleDraftPreview styles browserEnv article =
    div
        [ css
            [ Tw.flex
            , Tw.items_center
            , Tw.justify_center
            , Tw.mb_4
            ]
        ]
        [ div
            [ css
                [ Tw.p_6
                , Tw.rounded_lg
                , Tw.shadow_lg
                , Tw.min_w_96
                , Tw.max_w_3xl
                ]
            ]
            [ div
                [ css
                    [ Tw.flex
                    , Tw.flex_row
                    , Tw.justify_between
                    ]
                ]
                [ timeParagraph styles browserEnv article.publishedAt
                , editDraftButton article
                ]
            , viewTitleSummaryImagePreview styles article
            , viewTags styles article
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

editDraftButton : Article -> Html msg
editDraftButton article =
    editDraftLink article
    |> Maybe.map (Ui.Shared.linkButton "Edit")
    |> Maybe.withDefault (div [][])


editDraftLink : Article -> Maybe String
editDraftLink article =
    case Nip19.encode <| Nip19.NAddr 
            { identifier = article.identifier |> Maybe.withDefault ""
            , pubKey = article.author
            , kind = numberForKind article.kind
            , relays = []
            } of
        Ok encodedCoordinates ->
            Just <| "/write?a=" ++ encodedCoordinates

        Err error ->
            Nothing

     