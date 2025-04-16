module Nostr.Nip68 exposing (..)

import MimeType exposing (MimeType)
import Nostr.Event exposing (Event, Kind(..), Tag(..), TagReference(..))
import Nostr.Types exposing (PubKey)
import Time


type alias PicturePost =
    { id : String
    , description : String
    , createdAt : Time.Posix
    , pubKey : PubKey
    , title : Maybe String
    , pictures : List Picture
    , contentWarning : Maybe String
    , taggedUsers : List PubKey
    , mimeTypes : List MimeType
    , hashes : List String
    , hashtags : List String
    , location : Maybe String
    , geohash : Maybe String
    , language : Maybe String
    }


type alias Picture =
    { url : String
    , mimeType : Maybe MimeType
    , blurHash : Maybe String
    , dim : Maybe ( Int, Int )
    , alt : Maybe String
    , x : Maybe String
    , fallbacks : List String
    }



{- -}


picturePostFromEvent : Event -> PicturePost
picturePostFromEvent event =
    event.tags
        |> List.foldl
            (\tag acc ->
                case tag of
                    ContentWarningTag contentWarning ->
                        { acc | contentWarning = Just contentWarning }

                    GeohashTag geohash ->
                        { acc | geohash = Just geohash }

                    HashTag hashtag ->
                        { acc | hashtags = acc.hashtags ++ [ hashtag ] }

                    ImageMetadataTag imageMetadata ->
                        { acc | pictures = acc.pictures ++ [ imageMetadata ] }

                    LabelTag language (Just "ISO-639-1") ->
                        { acc | language = Just language }

                    LocationTag location _ ->
                        { acc | location = Just location }

                    MentionTag maybeMimeType ->
                        case MimeType.parseMimeType maybeMimeType of
                            Just mimeType ->
                                { acc | mimeTypes = acc.mimeTypes ++ [ mimeType ] }

                            Nothing ->
                                acc

                    PublicKeyTag pubKey _ _ ->
                        { acc | taggedUsers = acc.taggedUsers ++ [ pubKey ] }

                    TitleTag title ->
                        { acc | title = Just title }

                    _ ->
                        acc
            )
            { id = event.id
            , description = event.content
            , createdAt = event.createdAt
            , pubKey = event.pubKey
            , title = Nothing
            , pictures = []
            , contentWarning = Nothing
            , taggedUsers = []
            , mimeTypes = []
            , hashes = []
            , hashtags = []
            , location = Nothing
            , geohash = Nothing
            , language = Nothing
            }


tagReference : PicturePost -> TagReference
tagReference picturePost =
    TagReferenceEventId picturePost.id
