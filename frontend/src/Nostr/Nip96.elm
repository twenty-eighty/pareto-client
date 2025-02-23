module Nostr.Nip96 exposing (..)

import Dict exposing (Dict)
import File exposing (File)
import Http
import Json.Decode as Decode exposing (Decoder, andThen, bool, dict, fail, float, int, list, maybe, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Nostr.Nip94 as Nip94
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
        , url = urlWithoutTrailingSlash url ++ "/.well-known/nostr/nip96.json"
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
        urlWithoutTrailingSlash url ++ path

    else
        path


urlWithoutTrailingSlash : String -> String
urlWithoutTrailingSlash url =
    if String.endsWith "/" url then
        String.dropRight 1 url

    else
        url



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
    , files : List Nip94.FileMetadata
    }



-- Uploads


type alias FileUpload =
    { file : File
    , status : UploadStatus
    , caption : Maybe String
    , alt : Maybe String
    , mediaType : Maybe String
    , noTransform : Maybe Bool
    , uploadResponse : Maybe UploadResponse
    }


type alias UploadResponse =
    { status : String
    , message : Maybe String
    , processingUrl : Maybe String
    , fileMetadata : Maybe Nip94.FileMetadata
    }


type UploadStatus
    = Selected
    | Hashing
    | AwaitingAuthHeader String -- SHA256 Hash
    | ReadyToUpload String String -- SHA256 Hash, Auth Header
    | Uploading Float -- Progress percentage
    | Uploaded
    | Failed String -- Error message


uploadFile : String -> Int -> FileUpload -> (Result Http.Error UploadResponse -> msg) -> String -> Cmd msg
uploadFile apiUrl fileId fileUpload resultMsg authHeader =
    Http.request
        { method = "POST"
        , headers =
            [ Http.header "Authorization" authHeader ]
        , url = apiUrl
        , body = multipartBody fileUpload
        , expect = Http.expectJson resultMsg uploadResponseDecoder
        , timeout = Nothing
        , tracker = Just (String.fromInt fileId)
        }


multipartBody : FileUpload -> Http.Body
multipartBody upload =
    let
        expirationString =
            ""

        -- Empty string for no expiration
        -- Collect form fields
        formFields =
            [ Http.filePart "file" upload.file
            , Http.stringPart "size" (File.size upload.file |> String.fromInt)
            , Http.stringPart "content_type" (File.mime upload.file)
            , Http.stringPart "expiration" expirationString
            ]
                |> appendStringField "media_type" upload.mediaType
                |> appendStringField "alt" upload.alt
                |> appendStringField "caption" upload.caption
                |> appendBooleanField "no_transform" upload.noTransform
    in
    Http.multipartBody formFields


appendStringField : String -> Maybe String -> List Http.Part -> List Http.Part
appendStringField fieldName maybeFieldValue fields =
    case maybeFieldValue of
        Just fieldValue ->
            fields ++ [ Http.stringPart fieldName fieldValue ]

        Nothing ->
            fields


appendBooleanField : String -> Maybe Bool -> List Http.Part -> List Http.Part
appendBooleanField fieldName maybeFieldValue fields =
    case maybeFieldValue of
        Just True ->
            fields ++ [ Http.stringPart fieldName "true" ]

        Just False ->
            fields ++ [ Http.stringPart fieldName "false" ]

        Nothing ->
            fields



-- Decoders


fileListDecoder : Decoder FileList
fileListDecoder =
    succeed FileList
        |> required "count" int
        |> required "total" int
        |> required "page" int
        |> required "files" (list Nip94.fileMetadataDecoder)


uploadResponseDecoder : Decode.Decoder UploadResponse
uploadResponseDecoder =
    Decode.map4 UploadResponse
        (Decode.field "status" Decode.string)
        (Decode.maybe (Decode.field "message" Decode.string))
        (Decode.maybe (Decode.field "processing_url" Decode.string))
        (Decode.field "nip94_event" (Decode.nullable Nip94.fileMetadataDecoder))
