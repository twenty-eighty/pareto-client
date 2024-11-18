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


viewArticle : BrowserEnv -> Nostr.Model -> Article -> Html msg
viewArticle browserEnv nostr article =
    Ui.ArticleOld.viewArticle browserEnv (Nostr.getAuthor nostr article.author) article (Nostr.getInteractions nostr article)

viewArticlePreviews : Styles msg -> BrowserEnv -> Nostr.Model -> List Article -> Html msg
viewArticlePreviews styles browserEnv nostr articles =
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
            |> List.map (\article -> Ui.Article.viewArticlePreview styles browserEnv (Nostr.getAuthor nostr article.author) article (Nostr.getInteractions nostr article) True)
            )
        ]
    

viewCommunity : BrowserEnv -> Nostr.Model -> Community -> Html msg
viewCommunity browserEnv nostr community =
    Ui.Community.viewCommunity browserEnv nostr.profiles community

viewShortNote : Styles msg -> BrowserEnv -> Nostr.Model -> ShortNote -> Html msg
viewShortNote  styles browserEnv nostr shortNote =
    Ui.ShortNote.viewShortNote browserEnv nostr.profiles shortNote