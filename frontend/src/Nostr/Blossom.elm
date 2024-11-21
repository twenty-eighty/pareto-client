module Nostr.Blossom exposing (..)

import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Time
import Nostr.Event exposing (Event, Tag(..))
import Nostr.Nip11 exposing (decodeUnixTime)
import Nostr.Nip94 as Nip94 exposing (FileMetadata)
import Nostr.Types exposing (PubKey)
import Json.Decode as Decode

type alias BlobDescriptor =
    { url : String
    , sha256 : String
    , size : Int
    , type_ : Maybe String
    , uploaded : Time.Posix
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
        , url = url ++ "/list/" ++ pubKey
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg (Decode.list blobDescriptorDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }

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
        |> Pipeline.required "uploaded" decodeUnixTime
        |> Pipeline.optional "nip94" (Decode.map Just metadataDecoder) Nothing

metadataDecoder : Decode.Decoder FileMetadata
metadataDecoder =
    (Decode.list (Decode.list Decode.string))
    |> Decode.andThen (\tagList ->
        let
            initialEvent =
                { kind = Nothing
                , content = ""
                , createdAt = 0
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
        Decode.succeed (Nip94.parseTags tagList initialEvent)
    )
