module Nostr.Nip96 exposing (..)

import Dict exposing (Dict)
import Http
import Json.Decode exposing (Decoder, andThen, bool, dict, fail, field, float, int, list, maybe, string, succeed)
import Json.Decode.Pipeline exposing (required, optional)
import Json.Decode as Decode
import Url


-- Type Definitions

type alias MediaTransformations =
    { image : List String
    , video : List String
    }


type alias Plan =
    { name : String
    , isNip98Required : Bool
    , url : Maybe String
    , maxByteSize : Float
    , fileExpiration : Maybe ( Int, Int )
    , mediaTransformations : Maybe (Dict String (List String))
    }


type alias ServerDescriptorData =
    { apiUrl : String
    , downloadUrl : String
    , supportedNips : Maybe (List Int)
    , tosUrl : Maybe String
    , contentTypes : List String
    , plans : Dict String Plan
    }

type alias ServerRedirection =
    { apiUrl : Maybe String
    , delegated_to_url : String
    }

type ServerDescResponse
    = ServerRedirect ServerRedirection
    | ServerDescriptor ServerDescriptorData

-- fetch server definition

fetchServerSpec : (Result Http.Error ServerDescResponse -> msg) -> String -> Cmd msg
fetchServerSpec toMsg url =
     Http.request
        { method = "GET"
        , headers =
            [ Http.header "Accept" "application/json"
            ]
        , url = url ++ "/.well-known/nostr/nip96.json"
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg serverSpecOrDelegationDecoder
        , timeout = Nothing
        , tracker = Nothing
        }

extendRelativeServerDescriptorUrls : String -> ServerDescriptorData -> ServerDescriptorData
extendRelativeServerDescriptorUrls url serverDesc =
    let
        extendedApiUrl =
            serverDesc.apiUrl
            |> extendRelativeUrl url

        extendedDownloadUrl =
            serverDesc.downloadUrl
            |> extendRelativeUrl url

        extendedTosUrl =
            serverDesc.tosUrl
            |> Maybe.map (extendRelativeUrl url)
    in
    { serverDesc | apiUrl = extendedApiUrl, downloadUrl = extendedDownloadUrl, tosUrl = extendedTosUrl }

extendRelativeUrl : String -> String -> String
extendRelativeUrl url path =
    let
        urlIsRelative =
            Url.fromString path
            |> Maybe.map (\_ -> False)
            |> Maybe.withDefault True
    in
    if urlIsRelative then
        url ++ path
    else
        path

-- Decoders

mediaTransformationsDecoder : Decoder (Dict String (List String))
mediaTransformationsDecoder =
    dict (list string)


fileExpirationDecoder : Decoder ( Int, Int )
fileExpirationDecoder =
    list int
        |> andThen
            (\list ->
                case list of
                    [ a, b ] ->
                        succeed ( a, b )

                    _ ->
                        fail "Expected a list of two integers"
            )


planDecoder : Decoder Plan
planDecoder =
    succeed Plan
        |> required "name" string
        |> required "is_nip98_required" bool
        |> optional "url" (maybe string) Nothing
        |> required "max_byte_size" float
        |> optional "file_expiration" (maybe fileExpirationDecoder) Nothing
        |> optional "media_transformations" (maybe mediaTransformationsDecoder) Nothing

plansDecoder : Decoder (Dict String Plan)
plansDecoder =
    dict planDecoder


serverSpecOrDelegationDecoder : Decoder ServerDescResponse
serverSpecOrDelegationDecoder =
    Decode.oneOf
        [ serverSpecDecoder |> Decode.map ServerDescriptor
        , serverRedirectionDecoder |> Decode.map ServerRedirect
        ]

serverSpecDecoder : Decoder ServerDescriptorData
serverSpecDecoder =
    succeed ServerDescriptorData
        |> required "api_url" string
        |> required "download_url" string
        |> optional "supported_nips" (maybe (list int)) Nothing
        |> optional "tos_url" (maybe string) Nothing
        |> required "content_types" (list string)
        |> required "plans" plansDecoder


serverRedirectionDecoder : Decoder ServerRedirection
serverRedirectionDecoder =
    succeed ServerRedirection
        |> optional "api_url" (maybe string) Nothing
        |> required "delegated_to_url" string


-- file list

fetchFileList : (Result Http.Error FileList -> msg) -> String -> String -> Cmd msg
fetchFileList toMsg authHeader url =
     Http.request
        { method = "GET"
        , headers =
            [ Http.header "Accept" "application/json"
            , Http.header "Authorization" authHeader
            ]
        , url = url ++ "?page=0&count=50"
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg fileListDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


-- Type Definitions

type alias FileList =
    { count : Int
    , total : Int
    , page : Int
    , files : List File
    }


-- Decoders

fileListDecoder : Decoder FileList
fileListDecoder =
    succeed FileList
        |> required "count" int
        |> required "total" int
        |> required "page" int
        |> required "files" (list fileDecoder)


-- Type Definitions

type alias File =
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

fileDecoder : Decoder File
fileDecoder =
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


fromRawEvent : RawEvent -> Decoder File
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


parseTags : List (List String) -> File -> File
parseTags tags file =
    List.foldl parseTag file tags


parseTag : List String -> File -> File
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


parseDimensions : String -> File -> File
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
