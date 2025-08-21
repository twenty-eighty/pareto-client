module Ui.View exposing (..)

-- this module connects the Nostr engine and the UI functions

import BrowserEnv exposing (BrowserEnv)
import Components.Button as Button
import Components.Interactions
import Components.RelayStatus as RelayStatus exposing (Purpose(..))
import Dict
import Html.Styled as Html exposing (Html, div)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Keyed as Keyed
import Html.Styled.Lazy as Lazy
import I18Next
import LinkPreview exposing (LoadedContent)
import Nostr
import Nostr.Article exposing (Article, addressComponentsForArticle)
import Nostr.Community exposing (Community)
import Nostr.Request exposing (RequestId)
import Nostr.Types exposing (loggedInPubKey)
import Tailwind.Utilities as Tw
import Translations.View as Translations
import Ui.Article exposing (ArticlePreviewsData)
import Ui.Community
import Ui.Styles exposing (Theme)
import Ui.Shared exposing (emptyHtml)


type ArticlePreviewType
    = ArticlePreviewList
    | ArticlePreviewBigPicture


viewArticle : ArticlePreviewsData msg -> Maybe (LoadedContent msg) -> Components.Interactions.Model -> Article -> Html msg
viewArticle articlePreviewsData loadedContent articleInteractions article =
    let
        articleComponents =
            article
            |> addressComponentsForArticle
    in
    Ui.Article.viewArticle
        articlePreviewsData
        { author = Nostr.getAuthor articlePreviewsData.nostr article.author
        , articleComments = articleComponents |> Maybe.map (Nostr.getArticleComments articlePreviewsData.nostr (loggedInPubKey articlePreviewsData.loginStatus)) |> Maybe.withDefault []
        , articleCommentComments = articleComponents |> Maybe.map (Nostr.getArticleCommentComments articlePreviewsData.nostr) |> Maybe.withDefault Dict.empty
        , articleInteractions = articleInteractions
        , displayAuthor = True
        , loadedContent = loadedContent
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
    let
        loadMoreButton =
            articlePreviewsData.onLoadMore
                |> Maybe.map (\onLoadMore ->
                    Button.new
                        { label = Translations.loadMore [ articlePreviewsData.browserEnv.translations ]
                        , onClick = Just onLoadMore
                        ,theme = articlePreviewsData.theme
                        }
                        |> Button.withTypeSecondary
                        |> Button.view
                )
                |> Maybe.withDefault (emptyHtml)
    in
    div
        [ css
            [ Tw.flex
            , Tw.flex_col
            , Tw.justify_center
            ]
        ]
        [ Keyed.node "div"
            [ css
                [ Tw.flex
                , Tw.flex_col
                , Tw.justify_center
                , Tw.gap_8
                ]
            ]
            (articles
                |> List.map
                    (\article ->
                        ( -- unique identifier for Keyed.node
                          article.id
                        , Lazy.lazy3
                            Ui.Article.viewArticlePreviewList
                            articlePreviewsData
                            { author = Nostr.getAuthor articlePreviewsData.nostr article.author
                            , articleInteractions = Components.Interactions.init
                            , articleComments = []
                            , articleCommentComments = Dict.empty
                            , displayAuthor = True
                            , loadedContent = Nothing
                            }
                            article
                        )
                    )
            )
        , div
            [ css
                [ Tw.flex
                , Tw.justify_center
                , Tw.mt_8
                ]
            ]
            [ loadMoreButton ]
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
        (articles
            |> List.take 20
            |> List.map
                (\article ->
                    Ui.Article.viewArticlePreviewBigPicture
                        articlePreviewsData
                        { author = Nostr.getAuthor articlePreviewsData.nostr article.author
                        , articleInteractions = Components.Interactions.init
                        , articleComments = []
                        , articleCommentComments = Dict.empty
                        , displayAuthor = True
                        , loadedContent = Nothing
                        }
                        article
                )
        )


viewCommunity : BrowserEnv -> Nostr.Model -> Community -> Html msg
viewCommunity browserEnv nostr community =
    Ui.Community.viewCommunity browserEnv nostr.profiles community


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
