module Nostr.RelatedRequests exposing
    ( uniquePubKeys
    , missingPubKeys
    , profileRequestData
    , reactionsRequestData
    , deletionRequestData
    , commentFollowUps
    , shortNoteFollowUps
    , articlePrimaryFollowUps
    , articleDetailsReactionRequest
    , nip27PubKeys
    , nip05FollowUpRequests
    )

{-| Build follow-up `RequestData` for related kinds.
Adding them to a live request and calling ports stays in `Nostr`.
-}

import Dict exposing (Dict)
import Nostr.Article exposing (Article, addressComponentsForArticle, tagReference, uniqueArticleAuthors)
import Nostr.Event exposing (EventFilter, Kind(..), TagReference(..), emptyEventFilter)
import Nostr.EventFilters as EventFilters
import Nostr.Nip10 exposing (TextNote)
import Nostr.Nip19 exposing (NIP19Type(..))
import Nostr.Nip22 as Nip22 exposing (CommentType)
import Nostr.Request exposing (RequestData(..))
import Nostr.Types exposing (EventId, PubKey, RelayUrl)
import Set


uniquePubKeys : List PubKey -> List PubKey
uniquePubKeys pubKeys =
    pubKeys
        |> Set.fromList
        |> Set.toList


missingPubKeys : Dict PubKey a -> List PubKey -> List PubKey
missingPubKeys profiles pubKeys =
    pubKeys
        |> List.filter (\pubKey -> not (Dict.member pubKey profiles))


profileRequestData : Dict PubKey a -> List PubKey -> Maybe RequestData
profileRequestData profiles pubKeys =
    missingPubKeys profiles pubKeys
        |> EventFilters.forAuthors
        |> Maybe.map (RequestProfile Nothing)


reactionsRequestData : List TagReference -> Maybe RequestData
reactionsRequestData tagReferences =
    EventFilters.forReactions tagReferences
        |> Maybe.map RequestReactions


deletionRequestData : List TagReference -> Maybe RequestData
deletionRequestData tagReferences =
    EventFilters.forDeletionRequests tagReferences
        |> Maybe.map RequestDeletionRequests


commentFollowUps : Dict PubKey a -> List CommentType -> List RequestData
commentFollowUps profiles comments =
    [ comments
        |> List.map Nip22.commentPubKey
        |> uniquePubKeys
        |> profileRequestData profiles
    , comments
        |> List.map Nip22.tagReference
        |> reactionsRequestData
    ]
        |> List.filterMap identity


shortNoteFollowUps : Dict PubKey a -> List TextNote -> List RequestData
shortNoteFollowUps profiles shortNotes =
    [ shortNotes
        |> List.map .pubKey
        |> uniquePubKeys
        |> profileRequestData profiles
    , shortNotes
        |> List.map Nostr.Nip10.tagReference
        |> reactionsRequestData
    ]
        |> List.filterMap identity


articlePrimaryFollowUps : Dict PubKey a -> List Article -> List RequestData
articlePrimaryFollowUps profiles articles =
    [ articles
        |> uniqueArticleAuthors
        |> profileRequestData profiles
    , articles
        |> List.filterMap addressComponentsForArticle
        |> List.map TagReferenceCode
        |> deletionRequestData
    , articles
        |> List.map .id
        |> List.map TagReferenceEventId
        |> deletionRequestData
    ]
        |> List.filterMap identity


articleDetailsReactionRequest : List Article -> Maybe RequestData
articleDetailsReactionRequest articles =
    articles
        |> List.map tagReference
        |> reactionsRequestData


nip27PubKeys : List NIP19Type -> List PubKey
nip27PubKeys nip19List =
    nip19List
        |> List.filterMap
            (\nip27Ref ->
                case nip27Ref of
                    Npub pubKey ->
                        Just pubKey

                    NProfile { pubKey } ->
                        Just pubKey

                    Nsec _ ->
                        Nothing

                    Note _ ->
                        Nothing

                    NEvent _ ->
                        Nothing

                    NAddr _ ->
                        Nothing

                    NRelay _ ->
                        Nothing

                    Unknown _ ->
                        Nothing
            )
        |> uniquePubKeys


nip05FollowUpRequests :
    { pubKey : PubKey
    , identifier : Maybe String
    , relays : Maybe (List RelayUrl)
    , needsProfile : Bool
    , needsArticle : Bool
    }
    -> List RequestData
nip05FollowUpRequests { pubKey, identifier, relays, needsProfile, needsArticle } =
    let
        profileRequest =
            if needsProfile then
                Just
                    ({ emptyEventFilter | authors = Just [ pubKey ], kinds = Just [ KindUserMetadata ] }
                        |> RequestProfile relays
                    )

            else
                Nothing

        articleRequest =
            case ( needsArticle, identifier ) of
                ( True, Just id ) ->
                    Just
                        ({ emptyEventFilter
                            | authors = Just [ pubKey ]
                            , kinds = Just [ KindLongFormContent ]
                            , tagReferences = Just [ TagReferenceIdentifier id ]
                         }
                            |> RequestArticle relays
                        )

                _ ->
                    Nothing
    in
    [ profileRequest, articleRequest ]
        |> List.filterMap identity
