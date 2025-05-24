module Nostr.Nip22 exposing (..)

import Css exposing (content)
import Html exposing (article)
import Nostr.Article exposing (Article)
import Nostr.Event exposing (AddressComponents, Event, Kind(..), Tag(..), TagReference(..), emptyEvent, numberForKind)
import Nostr.Nip19 exposing (NIP19Type(..))
import Nostr.Types exposing (EventId, PubKey, RelayUrl)
import Set
import Time exposing (Posix)


type CommentType
    = CommentToArticle ArticleComment
    | CommentToArticleComment ArticleCommentComment


type alias ArticleComment =
    { pubKey : PubKey
    , eventId : EventId
    , createdAt : Posix
    , rootEventId : Maybe EventId
    , rootAddress : AddressComponents
    , rootKind : Kind
    , rootPubKey : PubKey
    , rootRelay : Maybe RelayUrl
    , content : String
    }


type alias ArticleCommentComment =
    { pubKey : PubKey
    , eventId : EventId
    , createdAt : Posix
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


commentRootAddress : CommentType -> AddressComponents
commentRootAddress comment =
    case comment of
        CommentToArticle articleComment ->
            articleComment.rootAddress

        CommentToArticleComment articleCommentComment ->
            articleCommentComment.rootAddress


commentPubKey : CommentType -> PubKey
commentPubKey comment =
    case comment of
        CommentToArticle articleComment ->
            articleComment.pubKey

        CommentToArticleComment articleCommentComment ->
            articleCommentComment.pubKey


tagReference : CommentType -> TagReference
tagReference comment =
    case comment of
        CommentToArticle articleComment ->
            TagReferenceEventId articleComment.eventId

        CommentToArticleComment articleCommentComment ->
            TagReferenceEventId articleCommentComment.eventId


commentEventId : CommentType -> EventId
commentEventId comment =
    case comment of
        CommentToArticle articleComment ->
            articleComment.eventId

        CommentToArticleComment articleCommentComment ->
            articleCommentComment.eventId


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


nip19ForComment : CommentType -> NIP19Type
nip19ForComment comment =
    case comment of
        CommentToArticle articleComment ->
            NEvent
                { id = articleComment.eventId
                , author = Just articleComment.pubKey
                , kind = KindComment |> numberForKind |> Just
                , relays = articleComment.rootRelay |> Maybe.map List.singleton |> Maybe.withDefault []
                }

        CommentToArticleComment articleCommentComment ->
            NEvent
                { id = articleCommentComment.eventId
                , author = Just articleCommentComment.pubKey
                , kind = KindComment |> numberForKind |> Just
                , relays = articleCommentComment.rootRelay |> Maybe.map List.singleton |> Maybe.withDefault []
                }


articleCommentOfComment : CommentType -> Maybe ArticleComment
articleCommentOfComment comment =
    case comment of
        CommentToArticle articleComment ->
            Just articleComment

        _ ->
            Nothing


articleCommentCommentOfComment : CommentType -> Maybe ArticleCommentComment
articleCommentCommentOfComment comment =
    case comment of
        CommentToArticleComment articleCommentComment ->
            Just articleCommentComment

        _ ->
            Nothing


contentValid : String -> Bool
contentValid content =
    content /= ""


type alias CollectedFields =
    { parentEventId : Maybe EventId
    , parentKind : Maybe Kind
    , parentPubKey : Maybe PubKey
    , rootAddress : Maybe AddressComponents
    , rootKind : Maybe Kind
    , rootPubKey : Maybe PubKey
    , rootRelay : Maybe RelayUrl
    , content : Maybe String
    }


commentFromEvent : Event -> Maybe CommentType
commentFromEvent event =
    let
        collected : CollectedFields
        collected =
            event.tags
                |> List.foldl
                    (\tag acc ->
                        case tag of
                            KindTag parentKind ->
                                { acc | parentKind = Just parentKind }

                            PublicKeyTag parentPubKey rootRelay _ ->
                                { acc | parentPubKey = Just parentPubKey, rootRelay = rootRelay }

                            EventIdTag parentEventId rootRelay ->
                                { acc | parentEventId = Just parentEventId, rootRelay = rootRelay }

                            RootAddressTag rootAddress rootRelay ->
                                { acc | rootAddress = Just rootAddress, rootRelay = rootRelay }

                            RootKindTag rootKind ->
                                { acc | rootKind = Just rootKind }

                            RootPubKeyTag rootPubKey rootRelay ->
                                { acc | rootPubKey = Just rootPubKey, rootRelay = rootRelay }

                            _ ->
                                acc
                    )
                    { parentEventId = Nothing
                    , parentKind = Nothing
                    , parentPubKey = Nothing
                    , rootAddress = Nothing
                    , rootKind = Nothing
                    , rootPubKey = Nothing
                    , rootRelay = Nothing
                    , content = Nothing
                    }
    in
    case ( ( collected.parentEventId, collected.parentKind, collected.parentPubKey ), ( collected.rootAddress, collected.rootKind, collected.rootPubKey ) ) of
        ( ( maybeEventId, Just parentKind, Just parentPubKey ), ( Just rootAddress, Just rootKind, Just rootPubKey ) ) ->
            if rootKind == parentKind then
                CommentToArticle
                    { eventId = event.id
                    , createdAt = event.createdAt
                    , pubKey = event.pubKey
                    , rootAddress = rootAddress
                    , rootEventId = maybeEventId
                    , rootKind = rootKind
                    , rootPubKey = rootPubKey
                    , rootRelay = collected.rootRelay
                    , content = event.content
                    }
                    |> Just

            else
                case maybeEventId of
                    Just parentEventId ->
                        CommentToArticleComment
                            { eventId = event.id
                            , createdAt = event.createdAt
                            , pubKey = event.pubKey
                            , parentEventId = parentEventId
                            , parentKind = parentKind
                            , parentPubKey = parentPubKey
                            , rootAddress = rootAddress
                            , rootKind = rootKind
                            , rootPubKey = rootPubKey
                            , rootRelay = collected.rootRelay
                            , content = event.content
                            }
                            |> Just

                    _ ->
                        Nothing

        _ ->
            Nothing


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
                    { eventId = ""
                    , createdAt = Time.millisToPosix 0
                    , pubKey = pubKey
                    , rootAddress = addressComponents
                    , rootEventId = Just article.id
                    , rootKind = article.kind
                    , rootPubKey = article.author
                    , rootRelay = firstRelay
                    , content = ""
                    }
            )


articleCommentEvent : CommentType -> Event
articleCommentEvent comment =
    let
        protocolSafeRelayUrl urlString =
            if String.startsWith "wss://" urlString || String.startsWith "ws://" urlString then
                urlString

            else
                "wss://" ++ urlString

        createCommonPartCommentEvent cmt specificTags =
            let
                initialEvent =
                    emptyEvent cmt.rootPubKey KindComment

                maybeRelayUrl =
                    cmt.rootRelay |> Maybe.map protocolSafeRelayUrl

                eventCommonPart =
                    { initialEvent
                        | content = cmt.content
                        , tags =
                            [ RootAddressTag cmt.rootAddress maybeRelayUrl
                            , RootKindTag cmt.rootKind
                            , RootPubKeyTag cmt.rootPubKey maybeRelayUrl
                            ]
                    }
            in
            { eventCommonPart | tags = eventCommonPart.tags ++ specificTags }
    in
    case comment of
        CommentToArticle articleComment ->
            let
                rootRelay =
                    articleComment.rootRelay |> Maybe.map protocolSafeRelayUrl

                maybeEventIdTag =
                    Maybe.map (\eid -> EventIdTag eid rootRelay) articleComment.rootEventId
            in
            createCommonPartCommentEvent articleComment
                ([ KindTag articleComment.rootKind
                 , AddressTag articleComment.rootAddress rootRelay
                 , PublicKeyTag articleComment.rootPubKey rootRelay Nothing
                 ]
                    ++ (Maybe.map List.singleton maybeEventIdTag |> Maybe.withDefault [])
                )

        CommentToArticleComment articleCommentComment ->
            let
                rootRelay =
                    articleCommentComment.rootRelay |> Maybe.map protocolSafeRelayUrl
            in
            createCommonPartCommentEvent articleCommentComment
                [ KindTag articleCommentComment.parentKind
                , PublicKeyTag articleCommentComment.parentPubKey rootRelay Nothing
                , EventIdTag articleCommentComment.parentEventId rootRelay
                ]
