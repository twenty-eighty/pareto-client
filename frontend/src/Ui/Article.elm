module Ui.Article exposing (..)

import BrowserEnv exposing (BrowserEnv)
import Css
import Graphics
import Html.Styled as Html exposing (Html, Attribute, a, article, aside, button, div, h2, h3, h4, img, main_, p, span, text)
import Html.Styled.Attributes as Attr exposing (class, css, href)
import Html.Styled.Events as Events exposing (..)
import Markdown
import Nostr.Article exposing (Article)
import Nostr.Event exposing (Kind(..), TagReference(..), numberForKind)
import Nostr.Nip19 as Nip19
import Nostr.Profile exposing (Author, Profile, ProfileValidation(..), profileDisplayName)
import Nostr.Reactions exposing (Interactions)
import Nostr.Types exposing (PubKey)
import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw
import Tailwind.Theme as Theme
import Time
import Ui.Links exposing (linkElementForAuthor, linkToAuthor, linkToProfile, linkToProfilePubKey)
import Ui.Profile exposing (timeParagraph)
import Ui.Shared exposing (fontFamilyUnbounded, fontFamilyInter)

viewArticle : BrowserEnv -> Author -> Article -> Interactions -> Html msg
viewArticle browserEnv author article interactions =
    div
        [ css
            [ Tw.flex
            , Tw.items_center
            , Tw.justify_center
            , Tw.min_h_screen
            , Tw.bg_color Theme.gray_100
            ]
        ]
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
            [ viewImage article.image
            , viewTitle article.title
            , viewSummary article.summary
            , viewTags article
            , viewInteractions browserEnv interactions
            , viewAuthorAndDate browserEnv article.publishedAt article.author author
            , viewContent article.content
            ]
        ]

viewArticleInternal : BrowserEnv -> Article -> Html msg
viewArticleInternal browserEnv article =
    div
        [ css
            [ Tw.flex
            , Tw.items_center
            , Tw.justify_center
            , Tw.min_h_screen
            , Tw.bg_color Theme.gray_100
            ]
        ]
        [ div
            [ css
                [ Tw.bg_color Theme.white
                , Tw.p_6
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
            [ viewImage article.image
            , viewTitle article.title
            , viewSummary article.summary
            , viewContent article.content
            ]
        ]

viewArticlePreview : BrowserEnv -> Author -> Article -> Interactions -> Bool -> Html msg
viewArticlePreview browserEnv author article interactions displayAuthor =
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
                [ Tw.bg_color Theme.white
                , Tw.p_6
                , Tw.rounded_lg
                , Tw.shadow_lg
                , Tw.max_w_3xl
                ]
            ]
            [ if displayAuthor then
                viewAuthorAndDate browserEnv article.publishedAt article.author author
              else
                timeParagraph browserEnv article.publishedAt
            , viewTitleSummaryImagePreview article
            , viewTags article
            , viewInteractions browserEnv interactions
            ]
        ]


viewArticleDraftPreview : BrowserEnv -> Article -> Html msg
viewArticleDraftPreview browserEnv article =
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
                [ Tw.bg_color Theme.white
                , Tw.p_6
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
                [ timeParagraph browserEnv article.publishedAt
                , editDraftButton article
                ]
            , viewTitleSummaryImagePreview article
            , viewTags article
            ]
        ]

editDraftButton : Article -> Html msg
editDraftButton article =
    editDraftLink article
    |> Maybe.map (Ui.Shared.linkButton "Edit")
    |> Maybe.withDefault (div [][])



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
                [ Ui.Profile.viewProfilePubKey pubKey
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
                [ Ui.Profile.viewProfileSmall profile
                , timeParagraph browserEnv published
                ]

viewTitleSummaryImagePreview : Article -> Html msg
viewTitleSummaryImagePreview article =
    div 
        [ css
            [ Tw.flex
            , Tw.flex_row
            , Tw.space_x_5
            ]
        ]
        [ div []
            [ viewTitlePreview article.title (linkToArticle article)
            , viewSummary article.summary
            ]
        , viewImagePreview article.image
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

viewTitlePreview : Maybe String -> Maybe String -> Html msg
viewTitlePreview maybeTitle maybeLinkTarget =
    case maybeTitle of
        Just title ->
            let
                linkElement =
                    case maybeLinkTarget of
                        Just linkTarget ->
                            a
                                [ href linkTarget
                                ]
                                [ text title ]
                        Nothing ->
                            text  title
            in
            h3
                [ css
                    [ Tw.text_2xl
                    , Tw.font_bold
                    , Tw.text_color Theme.gray_900
                    , Tw.mb_2
                    ]
                , fontFamilyUnbounded
                ]
                [ linkElement 
                ]

        Nothing ->
            div [][]

viewSummary : Maybe String -> Html msg
viewSummary maybeSummary =
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

viewSummaryPreview : Maybe String -> Html msg
viewSummaryPreview maybeSummary =
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

viewContent : String -> Html msg
viewContent content =
            p
                [ css
                    [ Tw.text_color Theme.gray_600
                    , Tw.text_sm
                    , Tw.mb_4
                    ]
                ]
                [ viewContentMarkdown content ]

viewContentMarkdown : String -> Html msg
viewContentMarkdown content =
    case Markdown.markdownViewHtml content of
        Ok html ->
            html

        Err error ->
            div [][]

viewImage : Maybe String -> Html msg
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

viewImagePreview : Maybe String -> Html msg
viewImagePreview maybeImage =
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
                        [ Tw.rounded_2xl
                        , Tw.object_cover
                        , Tw.max_w_60
                        , Tw.max_h_36
                        ]
                    ]
                    []
                ]

        Nothing ->
            div [][]

viewTags : Article -> Html msg
viewTags article =
    article.hashtags
    |> List.map viewTag
    |> div
        [ css
            [ Tw.flex
            , Tw.flex_row
            , Tw.flex_wrap
            , Tw.space_x_2
            ]
        ]

viewTag : String -> Html msg
viewTag tag =
    a
        [ css
            [ Tw.rounded_2xl
            , Tw.mb_4
            , Tw.bg_color Theme.gray_300
            , Tw.px_3
            , Tw.text_sm
            ]
        , href ("/t/" ++ tag )
        ]
        [ text tag ]

viewInteractions : BrowserEnv -> Interactions -> Html msg
viewInteractions browserEnv interactions =
    div
        [ css
            [ Tw.flex
            , Tw.items_center
            , Tw.justify_between
            , Tw.text_color Theme.gray_400
            , Tw.text_sm
            ]
        ]
        [ div
            [ css
                [ Tw.flex
                , Tw.space_x_6
                , Tw.mb_2
                ]
            ]
            [ viewReactions Graphics.zapIcon (Maybe.map (formatZapNum browserEnv) interactions.zaps)
            , viewReactions Graphics.repostIcon (Maybe.map String.fromInt interactions.reposts)
            , viewReactions Graphics.likeIcon (Maybe.map String.fromInt interactions.reactions)
            , viewReactions Graphics.commentsIcon (Maybe.map String.fromInt interactions.notes)
            , viewReactions Graphics.smallBookmarkIcon (Maybe.map String.fromInt interactions.bookmarks)
            ]
        ]

viewReactions : Html msg -> Maybe String -> Html msg
viewReactions icon maybeCount =
    span
        [ css
            [ Tw.flex
            , Tw.items_center
            , Tw.space_x_2
            , Tw.cursor_pointer
            , Css.hover
                [ Tw.text_color Theme.gray_600
                ]
            ]
        ]
        [ icon
        , span []
            [ text ( maybeCount |> Maybe.withDefault "0" ) ]
        ]

formatZapNum : BrowserEnv -> Int -> String
formatZapNum browserEnv bigNum =
    browserEnv.formatNumber "0 a" <| toFloat bigNum
