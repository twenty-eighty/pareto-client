module Nostr.Nip94 exposing (..)

import Dict exposing (Dict)
import Http
import Json.Decode as Decode exposing (Decoder, andThen, bool, dict, fail, field, float, int, list, maybe, nullable, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import MimeType exposing (MimeType)
import Url



-- Type Definitions


type alias FileMetadata =
    { kind : Maybe Int
    , content : Maybe String
    , createdAt : Int
    , url : Maybe String
    , mimeType : Maybe MimeType
    , xHash : Maybe String
    , oxHash : Maybe String
    , size : Maybe Int
    , dim : Maybe ( Int, Int )
    , magnet : Maybe String
    , i : Maybe String
    , blurhash : Maybe String
    , thumb : Maybe Media
    , image : Maybe Media
    , summary : Maybe String
    , alt : Maybe String
    }


type alias Media =
    { uri : String
    , hash : Maybe String
    }


type alias Nip94Event =
    { tags : List (List String)
    , content : String
    }


isImage : FileMetadata -> Bool
isImage metaData =
    case metaData.mimeType of
        Just (MimeType.Image _) ->
            True

        _ ->
            False

isAudio : FileMetadata -> Bool
isAudio metaData =
    case metaData.mimeType of
        Just (MimeType.Audio _) ->
            True

        _ ->
            False

isVideo : FileMetadata -> Bool
isVideo metaData =
    case metaData.mimeType of
        Just (MimeType.Video _) ->
            True

        _ ->
            False




-- Decoders


fileMetadataDecoder : Decoder FileMetadata
fileMetadataDecoder =
    decodeRawEvent
        |> andThen fromRawEvent


type alias RawEvent =
    { kind : Maybe Int
    , content : Maybe String
    , createdAt : Int
    , tags : List (List String)
    }


decodeRawEvent : Decoder RawEvent
decodeRawEvent =
    succeed RawEvent
        |> optional "kind" (maybe int) Nothing
        |> required "content" (nullable string)
        |> required "created_at" int
        |> required "tags" (list (list string))


fromRawEvent : RawEvent -> Decoder FileMetadata
fromRawEvent rawEvent =
    let
        initialEvent =
            { kind = rawEvent.kind
            , content = rawEvent.content
            , createdAt = rawEvent.createdAt
            , url = Nothing
            , mimeType = Nothing
            , xHash = Nothing
            , oxHash = Nothing
            , size = Nothing
            , dim = Nothing
            , magnet = Nothing
            , i = Nothing
            , blurhash = Nothing
            , thumb = Nothing
            , image = Nothing
            , summary = Nothing
            , alt = Nothing
            }
    in
    succeed (parseTags rawEvent.tags initialEvent)


parseTags : List (List String) -> FileMetadata -> FileMetadata
parseTags tags file =
    List.foldl parseTag file tags


parseTag : List String -> FileMetadata -> FileMetadata
parseTag tag file =
    case tag of
        [ "url", urlValue ] ->
            { file | url = Just urlValue }

        [ "m", mimeTypeValue ] ->
            { file | mimeType = MimeType.parseMimeType mimeTypeValue }

        [ "x", xHashValue ] ->
            { file | xHash = Just xHashValue }

        [ "ox", oxHashValue ] ->
            { file | oxHash = Just oxHashValue }

        [ "size", sizeValue ] ->
            case String.toInt sizeValue of
                Just sizeInt ->
                    { file | size = Just sizeInt }

                Nothing ->
                    file

        [ "dim", dimValue ] ->
            parseDimensions dimValue file

        [ "magnet", magnetValue ] ->
            { file | magnet = Just magnetValue }

        [ "i", iValue ] ->
            { file | i = Just iValue }

        [ "blurhash", blurhashValue ] ->
            { file | blurhash = Just blurhashValue }

        [ "thumb", uri ] ->
            { file | thumb = Just { uri = uri, hash = Nothing } }

        [ "thumb", uri, hash ] ->
            { file | thumb = Just { uri = uri, hash = Just hash } }

        [ "image", uri ] ->
            { file | image = Just { uri = uri, hash = Nothing } }

        [ "image", uri, hash ] ->
            { file | image = Just { uri = uri, hash = Just hash } }

        [ "summary", summaryValue ] ->
            { file | summary = Just summaryValue }

        [ "alt", altValue ] ->
            { file | alt = Just altValue }

        _ ->
            file


parseDimensions : String -> FileMetadata -> FileMetadata
parseDimensions dimValue file =
    let
        dimensions =
            String.split "x" dimValue
    in
    case dimensions of
        [ widthStr, heightStr ] ->
            case ( String.toInt widthStr, String.toInt heightStr ) of
                ( Just width, Just height ) ->
                    { file | dim = Just ( width, height ) }

                _ ->
                    file

        _ ->
            file



-- JSON DECODERS


nip94EventDecoder : Decode.Decoder Nip94Event
nip94EventDecoder =
    Decode.map2 Nip94Event
        (Decode.field "tags" (Decode.list (Decode.list Decode.string)))
        (Decode.field "content" Decode.string)
