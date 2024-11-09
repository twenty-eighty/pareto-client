module Ui.View exposing (..)

-- this module connects the Nostr engine and the UI functions

import BrowserEnv exposing (BrowserEnv)
import Html.Styled as Html exposing (Html, div)
import Nostr
import Nostr.Article exposing (Article)
import Nostr.Community exposing (Community)
import Ui.ArticleFigma
import Ui.Article
import Ui.Community


viewArticle : BrowserEnv -> Nostr.Model -> Article -> Html msg
viewArticle browserEnv nostr article =
    Ui.Article.viewArticle browserEnv (Nostr.getAuthor nostr article.author) article (Nostr.getInteractions nostr article)

viewArticlePreviews : BrowserEnv -> Nostr.Model -> List Article -> Html msg
viewArticlePreviews browserEnv nostr articles =
    articles
    |> List.take 20
    |> List.map (\article -> Ui.ArticleFigma.viewArticlePreview browserEnv (Nostr.getAuthor nostr article.author) article (Nostr.getInteractions nostr article) True)
    |> div []


viewCommunity : BrowserEnv -> Nostr.Model -> Community -> Html msg
viewCommunity browserEnv nostr community =
    Ui.Community.viewCommunity browserEnv nostr.profiles community