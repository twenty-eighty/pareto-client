module Nostr.Notifications exposing
    ( NotificationKind(..)
    , NotificationItem
    , forAuthorArticles
    , unreadCount
    )

{-| Build a chronological activity feed for a publisher's articles
from already-ingested reactions, comments, reposts, and zaps.
-}

import Dict exposing (Dict)
import Nostr.Article exposing (Article, addressComponentsForArticle)
import Nostr.Event exposing (buildAddress)
import Nostr.Nip18 exposing (Repost)
import Nostr.Nip22 as Nip22 exposing (CommentType(..))
import Nostr.Nutzaps exposing (Nutzap)
import Nostr.Reactions exposing (Reaction)
import Nostr.Types exposing (Address, EventId, PubKey)
import Nostr.Zaps exposing (ZapReceipt)
import Time exposing (Posix)


type NotificationKind
    = ReactionNotification
    | CommentNotification
    | RepostNotification
    | ZapNotification
    | NutzapNotification


type alias NotificationItem =
    { id : String
    , kind : NotificationKind
    , actorPubKey : PubKey
    , createdAt : Posix
    , article : Article
    , detail : Maybe String
    }


forAuthorArticles :
    { a
        | reactionsForAddress : Dict Address (Dict PubKey Reaction)
        , commentsByAddress : Dict Address (Dict EventId CommentType)
        , repostsByAddress : Dict Address (Dict PubKey Repost)
        , zapReceiptsAddress : Dict String (Dict String ZapReceipt)
        , nutzapsAddress : Dict String (Dict String Nutzap)
    }
    -> PubKey
    -> List Article
    -> List NotificationItem
forAuthorArticles store authorPubKey articles =
    articles
        |> List.concatMap (itemsForArticle store authorPubKey)
        |> List.sortBy (\item -> Time.posixToMillis item.createdAt)
        |> List.reverse


{-| Count notifications newer than `lastSeenMillis` (exclusive).
-}
unreadCount :
    { a
        | reactionsForAddress : Dict Address (Dict PubKey Reaction)
        , commentsByAddress : Dict Address (Dict EventId CommentType)
        , repostsByAddress : Dict Address (Dict PubKey Repost)
        , zapReceiptsAddress : Dict String (Dict String ZapReceipt)
        , nutzapsAddress : Dict String (Dict String Nutzap)
    }
    -> PubKey
    -> List Article
    -> Int
    -> Int
unreadCount store authorPubKey articles lastSeenMillis =
    forAuthorArticles store authorPubKey articles
        |> List.filter (\item -> Time.posixToMillis item.createdAt > lastSeenMillis)
        |> List.length


itemsForArticle :
    { a
        | reactionsForAddress : Dict Address (Dict PubKey Reaction)
        , commentsByAddress : Dict Address (Dict EventId CommentType)
        , repostsByAddress : Dict Address (Dict PubKey Repost)
        , zapReceiptsAddress : Dict String (Dict String ZapReceipt)
        , nutzapsAddress : Dict String (Dict String Nutzap)
    }
    -> PubKey
    -> Article
    -> List NotificationItem
itemsForArticle store authorPubKey article =
    case addressComponentsForArticle article of
        Nothing ->
            []

        Just addressComponents ->
            let
                address =
                    buildAddress addressComponents
            in
            List.concat
                [ reactionItems store.reactionsForAddress authorPubKey article address
                , commentItems store.commentsByAddress authorPubKey article address
                , repostItems store.repostsByAddress authorPubKey article address
                , zapItems store.zapReceiptsAddress authorPubKey article address
                , nutzapItems store.nutzapsAddress authorPubKey article address
                ]


reactionItems :
    Dict Address (Dict PubKey Reaction)
    -> PubKey
    -> Article
    -> Address
    -> List NotificationItem
reactionItems reactionsForAddress authorPubKey article address =
    Dict.get address reactionsForAddress
        |> Maybe.map Dict.values
        |> Maybe.withDefault []
        |> List.filter (\reaction -> reaction.pubKey /= authorPubKey)
        |> List.map
            (\reaction ->
                { id = "reaction:" ++ reaction.id
                , kind = ReactionNotification
                , actorPubKey = reaction.pubKey
                , createdAt = reaction.createdAt
                , article = article
                , detail = reactionContentPreview reaction.content
                }
            )


commentItems :
    Dict Address (Dict EventId CommentType)
    -> PubKey
    -> Article
    -> Address
    -> List NotificationItem
commentItems commentsByAddress authorPubKey article address =
    Dict.get address commentsByAddress
        |> Maybe.map Dict.values
        |> Maybe.withDefault []
        |> List.filter (\comment -> Nip22.commentPubKey comment /= authorPubKey)
        |> List.map
            (\comment ->
                { id = "comment:" ++ Nip22.commentEventId comment
                , kind = CommentNotification
                , actorPubKey = Nip22.commentPubKey comment
                , createdAt = commentCreatedAt comment
                , article = article
                , detail = Just (truncateText 120 (Nip22.commentContent comment))
                }
            )


repostItems :
    Dict Address (Dict PubKey Repost)
    -> PubKey
    -> Article
    -> Address
    -> List NotificationItem
repostItems repostsByAddress authorPubKey article address =
    Dict.get address repostsByAddress
        |> Maybe.map Dict.values
        |> Maybe.withDefault []
        |> List.filter (\repost -> repost.pubKey /= authorPubKey)
        |> List.map
            (\repost ->
                { id = "repost:" ++ address ++ ":" ++ repost.pubKey
                , kind = RepostNotification
                , actorPubKey = repost.pubKey
                , createdAt = repost.createdAt
                , article = article
                , detail = Nothing
                }
            )


zapItems :
    Dict String (Dict String ZapReceipt)
    -> PubKey
    -> Article
    -> Address
    -> List NotificationItem
zapItems zapReceiptsAddress authorPubKey article address =
    Dict.get address zapReceiptsAddress
        |> Maybe.map Dict.values
        |> Maybe.withDefault []
        |> List.filterMap
            (\receipt ->
                case receipt.pubkeySender of
                    Just sender ->
                        if sender == authorPubKey then
                            Nothing

                        else
                            Just
                                { id = "zap:" ++ receipt.id
                                , kind = ZapNotification
                                , actorPubKey = sender
                                , createdAt =
                                    receipt.createdAt
                                        |> Maybe.map (\seconds -> Time.millisToPosix (seconds * 1000))
                                        |> Maybe.withDefault article.createdAt
                                , article = article
                                , detail = receipt.amount |> Maybe.map formatSats
                                }

                    Nothing ->
                        Nothing
            )


nutzapItems :
    Dict String (Dict String Nutzap)
    -> PubKey
    -> Article
    -> Address
    -> List NotificationItem
nutzapItems nutzapsAddress authorPubKey article address =
    Dict.get address nutzapsAddress
        |> Maybe.map Dict.values
        |> Maybe.withDefault []
        |> List.filter (\nutzap -> nutzap.pubKey /= authorPubKey)
        |> List.map
            (\nutzap ->
                { id = "nutzap:" ++ nutzap.id
                , kind = NutzapNotification
                , actorPubKey = nutzap.pubKey
                , createdAt = nutzap.createdAt
                , article = article
                , detail = Just (String.fromInt nutzap.amount ++ " sats")
                }
            )


commentCreatedAt : CommentType -> Posix
commentCreatedAt comment =
    case comment of
        CommentToArticle articleComment ->
            articleComment.createdAt

        CommentToArticleComment articleCommentComment ->
            articleCommentComment.createdAt


reactionContentPreview : String -> Maybe String
reactionContentPreview content =
    case String.trim content of
        "" ->
            Nothing

        "+" ->
            Nothing

        other ->
            Just other


truncateText : Int -> String -> String
truncateText maxLen text =
    let
        trimmed =
            String.trim text
    in
    if String.length trimmed <= maxLen then
        trimmed

    else
        String.left (maxLen - 1) trimmed ++ "…"


formatSats : Int -> String
formatSats millisats =
    let
        sats =
            millisats // 1000
    in
    String.fromInt sats ++ " sats"
