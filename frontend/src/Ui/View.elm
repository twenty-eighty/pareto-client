module Ui.View exposing (..)

-- this module connects the Nostr engine and the UI functions

import Auth
import BrowserEnv exposing (BrowserEnv)
import Css
import Html.Styled as Html exposing (Html, div)
import Html.Styled.Attributes as Attr exposing (class, css, href)
import Nostr
import Nostr.Article exposing (Article)
import Nostr.Community exposing (Community)
import Nostr.ShortNote exposing (ShortNote)
import Nostr.Types exposing (PubKey)
import Tailwind.Breakpoints as Bp
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw
import Ui.Article
import Ui.ArticleOld
import Ui.Community
import Ui.ShortNote
import Ui.Styles exposing (Styles, Theme)

type ArticlePreviewType
    = ArticlePreviewList
    | ArticlePreviewBigPicture


viewArticle : Styles msg -> BrowserEnv -> Nostr.Model -> Article -> Html msg
viewArticle styles browserEnv nostr article =
    Ui.Article.viewArticle styles browserEnv (Nostr.getAuthor nostr article.author) article (Nostr.getInteractions nostr article)

viewArticlePreviews : ArticlePreviewType -> Theme -> BrowserEnv -> Nostr.Model -> Maybe PubKey -> List Article -> Html msg
viewArticlePreviews previewType theme browserEnv nostr maybeUserPubKey articles =
    case previewType of
        ArticlePreviewList ->
            viewArticlePreviewsList theme browserEnv nostr maybeUserPubKey articles

        ArticlePreviewBigPicture ->
            viewArticlePreviewsBigPicture theme browserEnv nostr articles

viewArticlePreviewsList : Theme -> BrowserEnv -> Nostr.Model -> Maybe PubKey -> List Article -> Html msg
viewArticlePreviewsList theme browserEnv nostr maybeUserPubKey articles =
    div
        [ css
            [ Tw.flex
            , Tw.justify_center
            ]
        ]
        [ div
            [ css
                [ Tw.flex
                , Tw.flex_col
                , Tw.gap_8
                ]
            ]
            ( articles
            |> List.take 20
            |> List.map (\article -> Ui.Article.viewArticlePreviewList theme browserEnv (Nostr.getAuthor nostr article.author) maybeUserPubKey article (Nostr.getInteractions nostr article) True)
            )
        ]
    
viewArticlePreviewsBigPicture : Theme -> BrowserEnv -> Nostr.Model -> List Article -> Html msg
viewArticlePreviewsBigPicture theme browserEnv nostr articles =
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
        |> List.map (\article -> Ui.Article.viewArticlePreviewBigPicture theme browserEnv (Nostr.getAuthor nostr article.author) article (Nostr.getInteractions nostr article) True)
        )

viewCommunity : BrowserEnv -> Nostr.Model -> Community -> Html msg
viewCommunity browserEnv nostr community =
    Ui.Community.viewCommunity browserEnv nostr.profiles community

viewShortNote : Styles msg -> BrowserEnv -> Nostr.Model -> ShortNote -> Html msg
viewShortNote styles browserEnv nostr shortNote =
    Ui.ShortNote.viewShortNote styles browserEnv nostr.profiles shortNote