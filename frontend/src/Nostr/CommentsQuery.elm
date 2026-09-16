module Nostr.CommentsQuery exposing
    ( articleComments
    , textNoteCommentsForArticle
    , articleCommentsMerged
    , articleCommentComments
    )

{-| Comment lookups against the shared store dicts.
-}

import Dict exposing (Dict)
import Nostr.Event exposing (AddressComponents, buildAddress)
import Nostr.Nip10 exposing (TextNote)
import Nostr.Nip22 as Nip22 exposing (ArticleComment, ArticleCommentComment, CommentType(..), articleCommentCommentOfComment, articleCommentOfComment, commentFromTextNote)
import Nostr.Types exposing (Address, EventId)


articleComments :
    Dict Address (Dict EventId CommentType)
    -> AddressComponents
    -> List ArticleComment
articleComments commentsByAddress addressComponents =
    Dict.get (buildAddress addressComponents) commentsByAddress
        |> Maybe.map Dict.values
        |> Maybe.map (List.filterMap articleCommentOfComment)
        |> Maybe.withDefault []


textNoteCommentsForArticle :
    Dict EventId TextNote
    -> AddressComponents
    -> List CommentType
textNoteCommentsForArticle shortTextNotes addressComponents =
    shortTextNotes
        |> Dict.values
        |> List.filterMap
            (\textNote ->
                if textNote.rootAddressComponents == Just addressComponents then
                    commentFromTextNote textNote

                else
                    Nothing
            )


{-| Kind-22 comments plus text-note comments that reply to the article.
-}
articleCommentsMerged :
    Dict Address (Dict EventId CommentType)
    -> Dict EventId TextNote
    -> AddressComponents
    -> List ArticleComment
articleCommentsMerged commentsByAddress shortTextNotes addressComponents =
    let
        fromKind22 =
            articleComments commentsByAddress addressComponents

        fromTextNotes =
            textNoteCommentsForArticle shortTextNotes addressComponents
                |> List.filterMap
                    (\comment ->
                        case comment of
                            CommentToArticle commentValue ->
                                Just commentValue

                            _ ->
                                Nothing
                    )
    in
    fromKind22 ++ fromTextNotes


articleCommentComments :
    Dict Address (Dict EventId CommentType)
    -> Dict EventId TextNote
    -> AddressComponents
    -> Dict EventId (List ArticleCommentComment)
articleCommentComments commentsByAddress shortTextNotes addressComponents =
    let
        fromKind22 =
            Dict.get (buildAddress addressComponents) commentsByAddress
                |> Maybe.map
                    (\commentsDict ->
                        commentsDict
                            |> Dict.toList
                            |> List.filterMap (\( _, comment ) -> articleCommentCommentOfComment comment)
                            |> List.foldl insertCommentByParent Dict.empty
                    )
                |> Maybe.withDefault Dict.empty

        fromTextNotes =
            textNoteCommentsForArticle shortTextNotes addressComponents
                |> List.filterMap
                    (\comment ->
                        case comment of
                            CommentToArticleComment commentValue ->
                                Just commentValue

                            _ ->
                                Nothing
                    )
                |> List.foldl insertCommentByParent Dict.empty
    in
    Dict.union fromKind22 fromTextNotes


insertCommentByParent :
    ArticleCommentComment
    -> Dict EventId (List ArticleCommentComment)
    -> Dict EventId (List ArticleCommentComment)
insertCommentByParent comment acc =
    Dict.update comment.parentEventId
        (\maybeList ->
            case maybeList of
                Just commentList ->
                    Just (comment :: commentList)

                Nothing ->
                    Just [ comment ]
        )
        acc
