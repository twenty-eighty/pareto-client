module Nostr.Blossom exposing (..)

import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Time
import Nostr.Event exposing (Event, Tag(..))
import Nostr.Nip11 exposing (decodeUnixTime)
import Nostr.Nip94 as Nip94 exposing (FileMetadata)
import Nostr.Types exposing (PubKey)

type alias BlobDescriptor =
    { url : String
    , sha256 : String
    , size : Int
    , type_ : Maybe String
    , uploaded : Maybe Time.Posix
    , nip94 : Maybe FileMetadata
    }

userServerListFromEvent : Event -> (PubKey, List String)
userServerListFromEvent event =
    let
        userServerList =
            event.tags
            |> List.foldl (\tag serverList ->
                case tag of 
                    ServerTag url ->
                        serverList ++ [ url ]

                    _ ->
                        serverList
                    )
                []
    in
    (event.pubKey, userServerList )


fetchFileList : (Result Http.Error (List BlobDescriptor) -> msg) -> String -> String -> PubKey -> Cmd msg
fetchFileList toMsg authHeader url pubKey =
     Http.request
        { method = "GET"
        , headers =
            [ Http.header "Accept" "application/json"
            , Http.header "Authorization" authHeader
            ]
        , url = urlWithoutTrailingSlash url ++ "/list/" ++ pubKey
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg (Decode.list blobDescriptorDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }

urlWithoutTrailingSlash : String -> String
urlWithoutTrailingSlash url =
    if String.endsWith "/" url then
        String.dropRight 1 url
    else
        url

uploadFile : (Result Http.Error BlobDescriptor -> msg) -> String -> String -> Int -> String -> Cmd msg
uploadFile toMsg authHeader url contentLength mimeType =
     Http.request
        { method = "PUT"
        , headers =
            [ Http.header "Accept" "application/json"
            , Http.header "Authorization" authHeader
            , Http.header "Content-length" (contentLength |> String.fromInt)
            , Http.header "Content-type" mimeType
            ]
        , url = url ++ "/upload"
        , body = Http.emptyBody
        --, body = Http.bytesBody
        , expect = Http.expectJson toMsg blobDescriptorDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


blobDescriptorDecoder : Decode.Decoder BlobDescriptor
blobDescriptorDecoder =
    Decode.succeed BlobDescriptor
        |> Pipeline.required "url" Decode.string
        |> Pipeline.required "sha256" Decode.string
        |> Pipeline.required "size" Decode.int
        |> Pipeline.optional "type" (Decode.map Just Decode.string) Nothing
        |> Pipeline.optional "uploaded" (Decode.map Just decodeUnixTime) Nothing
        |> Pipeline.optional "nip94" (Decode.map Just metadataDecoder) Nothing

metadataDecoder : Decode.Decoder FileMetadata
metadataDecoder =
    Decode.succeed Nip94.FileMetadata
    |> Pipeline.hardcoded Nothing
    |> Pipeline.hardcoded ""
    |> Pipeline.hardcoded 0
    |> Pipeline.optional "url" (Decode.map Just Decode.string) Nothing
    |> Pipeline.optional "mimeType" (Decode.map Just Decode.string) Nothing
    |> Pipeline.optional "xHash" (Decode.map Just Decode.string) Nothing
    |> Pipeline.optional "oxHash" (Decode.map Just Decode.string) Nothing
    |> Pipeline.optional "size" (Decode.map Just sizeDecoder) Nothing
    |> Pipeline.hardcoded Nothing
    |> Pipeline.optional "magnet" (Decode.map Just Decode.string) Nothing
    |> Pipeline.optional "i" (Decode.map Just Decode.string) Nothing
    |> Pipeline.optional "blurhash" (Decode.map Just Decode.string) Nothing
    |> Pipeline.hardcoded Nothing
    |> Pipeline.hardcoded Nothing
    |> Pipeline.optional "summary" (Decode.map Just Decode.string) Nothing
    |> Pipeline.optional "alt" (Decode.map Just Decode.string) Nothing

sizeDecoder : Decode.Decoder Int
sizeDecoder =
    Decode.oneOf
        [ Decode.int
        , Decode.string
            |> Decode.andThen (\intStr ->
                case String.toInt intStr of
                    Just intValue ->
                        Decode.succeed intValue

                    Nothing ->
                        Decode.fail <| "Error converting size string to int: " ++ intStr


            )

        ]