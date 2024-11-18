module Ui.View exposing (..)

-- this module connects the Nostr engine and the UI functions

import BrowserEnv exposing (BrowserEnv)
import Html.Styled as Html exposing (Html, div)
import Html.Styled.Attributes as Attr exposing (class, css, href)
import Nostr
import Nostr.Article exposing (Article)
import Nostr.Community exposing (Community)
import Tailwind.Utilities as Tw
import Tailwind.Theme as Theme
import Ui.Article
import Ui.ArticleOld
import Ui.Community
import Ui.ShortNote
import Ui.Styles exposing (Styles)
import Nostr.ShortNote exposing (ShortNote)

type ArticlePreviewType
    = ArticlePreviewList
    | ArticlePreviewBigPicture


viewArticle : BrowserEnv -> Nostr.Model -> Article -> Html msg
viewArticle browserEnv nostr article =
    Ui.ArticleOld.viewArticle browserEnv (Nostr.getAuthor nostr article.author) article (Nostr.getInteractions nostr article)

viewArticlePreviews : ArticlePreviewType -> Styles msg -> BrowserEnv -> Nostr.Model -> List Article -> Html msg
viewArticlePreviews previewType styles browserEnv nostr articles =
    case previewType of
        ArticlePreviewList ->
            viewArticlePreviewsList styles browserEnv nostr articles

        ArticlePreviewBigPicture ->
            viewArticlePreviewsBigPicture styles browserEnv nostr articles

viewArticlePreviewsList : Styles msg -> BrowserEnv -> Nostr.Model -> List Article -> Html msg
viewArticlePreviewsList styles browserEnv nostr articles =
    div
        [ css
            [ Tw.h_96
            , Tw.flex_col
            , Tw.justify_start
            , Tw.items_start
            , Tw.gap_8
            , Tw.inline_flex
            ]
        , Attr.style "width" "720px"
        ]
        [ div
            [ css
                [ Tw.self_stretch
                , Tw.h_96
                , Tw.flex_col
                , Tw.justify_start
                , Tw.items_start
                , Tw.gap_8
                , Tw.flex
                ]
            , Attr.style "width" "720px"
            ]
            ( articles
            |> List.take 20
            |> List.map (\article -> Ui.Article.viewArticlePreviewList styles browserEnv (Nostr.getAuthor nostr article.author) article (Nostr.getInteractions nostr article) True)
            )
        ]
    
viewArticlePreviewsBigPicture : Styles msg -> BrowserEnv -> Nostr.Model -> List Article -> Html msg
viewArticlePreviewsBigPicture styles browserEnv nostr articles =
    div
        [ css
            [ Tw.h_80
            , Tw.justify_start
            , Tw.items_start
            , Tw.gap_5
            , Tw.inline_flex
            ]
        ]
        ( articles
        |> List.take 20
        |> List.map (\article -> Ui.Article.viewArticlePreviewBigPicture styles browserEnv (Nostr.getAuthor nostr article.author) article (Nostr.getInteractions nostr article) True)
        )

viewCommunity : BrowserEnv -> Nostr.Model -> Community -> Html msg
viewCommunity browserEnv nostr community =
    Ui.Community.viewCommunity browserEnv nostr.profiles community

viewShortNote : Styles msg -> BrowserEnv -> Nostr.Model -> ShortNote -> Html msg
viewShortNote  styles browserEnv nostr shortNote =
    Ui.ShortNote.viewShortNote browserEnv nostr.profiles shortNote