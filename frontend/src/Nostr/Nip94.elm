module Nostr.Nip94 exposing (..)

import Dict exposing (Dict)
import Http
import Json.Decode exposing (Decoder, andThen, bool, dict, fail, field, float, int, list, maybe, string, succeed)
import Json.Decode.Pipeline exposing (required, optional)
import Json.Decode as Decode
import Url


-- Type Definitions

type alias FileMetadata =
    { kind : Maybe Int
    , content : String
    , createdAt : Int
    , url : Maybe String
    , mimeType : Maybe String
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


-- Decoders

fileMetadataDecoder : Decoder FileMetadata
fileMetadataDecoder =
    decodeRawEvent
        |> andThen fromRawEvent


type alias RawEvent =
    { kind : Maybe Int
    , content : String
    , createdAt : Int
    , tags : List (List String)
    }


decodeRawEvent : Decoder RawEvent
decodeRawEvent =
    succeed RawEvent
        |> optional "kind" (maybe int) Nothing
        |> required "content" string
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
            { file | mimeType = Just mimeTypeValue }

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
