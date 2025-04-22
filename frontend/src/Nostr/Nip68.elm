module Nostr.Nip68 exposing (..)

import Dict exposing (Dict)
import Locale exposing (Language(..))
import Locale exposing (languageToISOCode)
import MimeType exposing (MimeType)
import Nostr.Event exposing (Event, ImageMetadata, Kind(..), Tag(..), TagReference(..), emptyEvent)
import Nostr.Event as Event
import Nostr.Types exposing (PubKey, RelayUrl)
import Set exposing (Set)
import Time


type alias PicturePost =
    { id : String
    , description : String
    , createdAt : Time.Posix
    , pubKey : PubKey
    , title : Maybe String
    , pictures : List ImageMetadata
    , contentWarning : Maybe String
    , taggedUsers : List (PubKey, Maybe RelayUrl, Maybe String)
    , mimeTypes : List MimeType
    , hashes : List String
    , hashtags : List String
    , location : Maybe String
    , geohash : Maybe String
    , language : Maybe Language
    }



emptyPicturePost : PicturePost
emptyPicturePost =
    { id = ""
    , description = ""
    , createdAt = Time.millisToPosix 0
    , pubKey = ""
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
                        { acc | language = Locale.languageFromISOCode language }

                    LocationTag location _ ->
                        { acc | location = Just location }

                    MentionTag maybeMimeType ->
                        case MimeType.parseMimeType maybeMimeType of
                            Just mimeType ->
                                { acc | mimeTypes = acc.mimeTypes ++ [ mimeType ] }

                            Nothing ->
                                acc

                    PublicKeyTag pubKey maybeRelayUrl maybePetName_ ->
                        { acc | taggedUsers = acc.taggedUsers ++ [ ( pubKey, maybeRelayUrl, maybePetName_ ) ] }

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

picturePostEvent : PubKey -> PicturePost -> Event
picturePostEvent pubKey picturePost =
    let
        event =
            emptyEvent pubKey KindPicture

        mimeTypes =
            -- create list of unique mime types
            picturePost.pictures
                |> List.foldl (\picture acc ->
                    case picture.mimeType of
                        Just mimeType ->
                            Dict.insert (MimeType.toString mimeType) mimeType acc

                        Nothing ->
                            acc
                    ) Dict.empty
                |> Dict.values

        hashValues =
            picturePost.pictures
                |> List.foldl (\picture acc ->
                    case picture.x of
                        Just hash ->
                            Set.insert hash acc

                        Nothing ->
                            acc
                ) Set.empty
                |> Set.toList
    in
    { event
            | id = picturePost.id
            , content = picturePost.description
            , tags =
                []
                |> Event.addTitleTag picturePost.title
                |> Event.addPubKeyTags picturePost.taggedUsers
                |> Event.addHashtagsToTags picturePost.hashtags
                |> Maybe.withDefault identity (picturePost.language |> Maybe.map languageToISOCode |> Maybe.map (Event.addLabelTags "ISO-639-1"))
                |> Event.addImetaTags picturePost.pictures
                |> Event.addMimeTypeTags mimeTypes
                |> Event.addHashValueTags hashValues
        }


tagReference : PicturePost -> TagReference
tagReference picturePost =
    TagReferenceEventId picturePost.id
