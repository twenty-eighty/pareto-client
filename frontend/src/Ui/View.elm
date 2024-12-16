module Ui.View exposing (..)

-- this module connects the Nostr engine and the UI functions

import Auth
import BrowserEnv exposing (BrowserEnv)
import Components.RelayStatus as RelayStatus exposing (Purpose(..))
import Css
import Html.Styled as Html exposing (Html, div)
import Html.Styled.Attributes as Attr exposing (class, css, href)
import Nostr
import Nostr.Article exposing (Article)
import Nostr.Community exposing (Community)
import Nostr.Request exposing (RequestId)
import Nostr.ShortNote exposing (ShortNote)
import Nostr.Types exposing (PubKey)
import Tailwind.Breakpoints as Bp
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw
import Ui.Article exposing (ArticlePreviewData, ArticlePreviewsData)
import Ui.ArticleOld
import Ui.Community
import Ui.ShortNote
import Ui.Styles exposing (Styles, Theme)
import I18Next

type ArticlePreviewType
    = ArticlePreviewList
    | ArticlePreviewBigPicture


viewArticle : ArticlePreviewsData msg -> Article -> Html msg
viewArticle articlePreviewsData article =
    Ui.Article.viewArticle
        articlePreviewsData
        { author = Nostr.getAuthor articlePreviewsData.nostr article.author
        , interactions = Nostr.getInteractions articlePreviewsData.nostr articlePreviewsData.userPubKey article
        , displayAuthor = True
        }
        article

viewArticlePreviews : ArticlePreviewType -> ArticlePreviewsData msg -> List Article -> Html msg
viewArticlePreviews previewType articlePreviewsData articles =
    case previewType of
        ArticlePreviewList ->
            viewArticlePreviewsList articlePreviewsData articles

        ArticlePreviewBigPicture ->
            viewArticlePreviewsBigPicture articlePreviewsData articles

viewArticlePreviewsList : ArticlePreviewsData msg -> List Article -> Html msg
viewArticlePreviewsList articlePreviewsData articles =
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
            |> List.map (\article ->
                Ui.Article.viewArticlePreviewList
                    articlePreviewsData
                        { author = Nostr.getAuthor articlePreviewsData.nostr article.author
                        , interactions = Nostr.getInteractions articlePreviewsData.nostr articlePreviewsData.userPubKey article
                        , displayAuthor = True
                        }
                        article
                )
            )
        ]
    
viewArticlePreviewsBigPicture : ArticlePreviewsData msg -> List Article -> Html msg
viewArticlePreviewsBigPicture articlePreviewsData articles =
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
        |> List.map (\article ->
                Ui.Article.viewArticlePreviewBigPicture
                    articlePreviewsData
                    { author = Nostr.getAuthor articlePreviewsData.nostr article.author
                    , interactions = Nostr.getInteractions articlePreviewsData.nostr articlePreviewsData.userPubKey article
                    , displayAuthor = True
                    }
                    article
            )
        )

viewCommunity : BrowserEnv -> Nostr.Model -> Community -> Html msg
viewCommunity browserEnv nostr community =
    Ui.Community.viewCommunity browserEnv nostr.profiles community

viewShortNote : Styles msg -> BrowserEnv -> Nostr.Model -> ShortNote -> Html msg
viewShortNote styles browserEnv nostr shortNote =
    Ui.ShortNote.viewShortNote styles browserEnv nostr.profiles shortNote


viewRelayStatus : Theme -> I18Next.Translations -> Nostr.Model -> Purpose -> Maybe RequestId -> Html msg
viewRelayStatus theme translations nostr purpose requestId =
    let
        relays =
            Nostr.getRelaysForRequest nostr requestId
            |> List.filterMap (Nostr.getRelayData nostr)
    in
    RelayStatus.new
        { relays = relays
        , theme = theme
        , translations = translations
        , purpose = purpose
        }
        |> RelayStatus.view
        