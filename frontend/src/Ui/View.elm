module Ui.View exposing (..)

-- this module connects the Nostr engine and the UI functions

import BrowserEnv exposing (BrowserEnv)
import Components.RelayStatus as RelayStatus exposing (Purpose(..))
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
import Tailwind.Utilities as Tw
import Ui.Article exposing (ArticlePreviewsData)
import Ui.Community
import Ui.Shared exposing (Actions)
import Ui.Styles exposing (Theme)


type ArticlePreviewType
    = ArticlePreviewList
    | ArticlePreviewBigPicture


viewArticle : ArticlePreviewsData msg -> Maybe (LoadedContent msg) -> Article -> Html msg
viewArticle articlePreviewsData loadedContent article =
    Ui.Article.viewArticle
        articlePreviewsData
        { author = Nostr.getAuthor articlePreviewsData.nostr article.author
        , actions = actionsFromArticlePreviewsData articlePreviewsData article
        , interactions = Nostr.getInteractions articlePreviewsData.nostr articlePreviewsData.userPubKey article
        , displayAuthor = True
        , loadedContent = loadedContent
        }
        article


actionsFromArticlePreviewsData : ArticlePreviewsData msg -> Article -> Actions msg
actionsFromArticlePreviewsData articlePreviewsData article =
    let
        maybeAddressComponents =
            addressComponentsForArticle article

        addReactionMsg =
            Maybe.map2
                (\addReaction addressComponents ->
                    addReaction article.id article.author addressComponents
                )
                articlePreviewsData.onReaction
                maybeAddressComponents
    in
    case ( articlePreviewsData.onBookmark, maybeAddressComponents ) of
        ( Just ( addArticleBookmark, removeArticleBookmark ), Just addressComponents ) ->
            { addBookmark = Just <| addArticleBookmark addressComponents
            , removeBookmark = Just <| removeArticleBookmark addressComponents
            , addReaction = addReactionMsg
            , removeReaction = Nothing
            , addRepost = articlePreviewsData.onRepost
            }

        ( _, _ ) ->
            { addBookmark = Nothing
            , removeBookmark = Nothing
            , addReaction = Nothing
            , removeReaction = Nothing
            , addRepost = Nothing
            }


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
        [ Keyed.node "div"
            [ css
                [ Tw.flex
                , Tw.flex_col
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
                            , actions =
                                actionsFromArticlePreviewsData articlePreviewsData article
                            , interactions = Nostr.getInteractions articlePreviewsData.nostr articlePreviewsData.userPubKey article
                            , displayAuthor = True
                            , loadedContent = Nothing
                            }
                            article
                        )
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
        (articles
            |> List.take 20
            |> List.map
                (\article ->
                    Ui.Article.viewArticlePreviewBigPicture
                        articlePreviewsData
                        { author = Nostr.getAuthor articlePreviewsData.nostr article.author
                        , actions =
                            actionsFromArticlePreviewsData articlePreviewsData article
                        , interactions = Nostr.getInteractions articlePreviewsData.nostr articlePreviewsData.userPubKey article
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
