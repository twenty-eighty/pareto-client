module Nostr.Nip22 exposing (..)

import Nostr.Article exposing (Article)
import Nostr.Event exposing (AddressComponents, Event, Kind(..), Tag(..), emptyEvent)
import Nostr.Types exposing (EventId, PubKey, RelayUrl)
import Set


type CommentType
    = CommentToArticle ArticleComment
    | CommentToArticleComment ArticleCommentComment


type alias ArticleComment =
    { pubKey : PubKey
    , rootAddress : AddressComponents
    , rootKind : Kind
    , rootPubKey : PubKey
    , rootRelay : Maybe RelayUrl
    , content : String
    }


type alias ArticleCommentComment =
    { pubKey : PubKey
    , parentEventId : EventId
    , parentKind : Kind
    , parentPubKey : PubKey
    , rootAddress : AddressComponents
    , rootKind : Kind
    , rootPubKey : PubKey
    , rootRelay : Maybe RelayUrl
    , content : String
    }


commentContent : CommentType -> String
commentContent comment =
    case comment of
        CommentToArticle articleComment ->
            articleComment.content

        CommentToArticleComment articleCommentComment ->
            articleCommentComment.content


setCommentContent : CommentType -> String -> CommentType
setCommentContent comment content =
    case comment of
        CommentToArticle articleComment ->
            CommentToArticle { articleComment | content = content }

        CommentToArticleComment articleCommentComment ->
            CommentToArticleComment { articleCommentComment | content = content }


commentValid : CommentType -> Bool
commentValid comment =
    case comment of
        CommentToArticle articleComment ->
            contentValid articleComment.content

        CommentToArticleComment articleCommentComment ->
            contentValid articleCommentComment.content


contentValid : String -> Bool
contentValid content =
    content /= ""


articleDraftComment : PubKey -> Article -> Maybe CommentType
articleDraftComment pubKey article =
    let
        firstRelay =
            article.relays
                |> Set.toList
                |> List.head
    in
    Nostr.Article.addressComponentsForArticle article
        |> Maybe.map
            (\addressComponents ->
                CommentToArticle
                    { pubKey = pubKey
                    , rootAddress = addressComponents
                    , rootKind = article.kind
                    , rootPubKey = article.author
                    , rootRelay = firstRelay
                    , content = ""
                    }
            )


articleCommentEvent : CommentType -> Event
articleCommentEvent comment =
    case comment of
        CommentToArticle articleComment ->
            let
                event =
                    emptyEvent articleComment.pubKey KindComment
            in
            { event
                | content = articleComment.content
                , tags =
                    [ KindTag articleComment.rootKind
                    , AddressTag articleComment.rootAddress articleComment.rootRelay
                    , PublicKeyTag articleComment.rootPubKey articleComment.rootRelay Nothing
                    , RootAddressTag articleComment.rootAddress articleComment.rootRelay
                    , RootKindTag articleComment.rootKind
                    , RootPubKeyTag articleComment.rootPubKey articleComment.rootRelay
                    ]
            }

        CommentToArticleComment articleCommentComment ->
            let
                event =
                    emptyEvent articleCommentComment.pubKey KindComment
            in
            { event
                | content = articleCommentComment.content
                , tags =
                    [ KindTag articleCommentComment.parentKind
                    , PublicKeyTag articleCommentComment.parentPubKey articleCommentComment.rootRelay Nothing
                    , EventIdTag articleCommentComment.parentEventId articleCommentComment.rootRelay
                    , RootAddressTag articleCommentComment.rootAddress articleCommentComment.rootRelay
                    , RootKindTag articleCommentComment.rootKind
                    , RootPubKeyTag articleCommentComment.rootPubKey articleCommentComment.rootRelay
                    ]
            }
