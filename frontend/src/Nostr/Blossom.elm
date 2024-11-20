module Nostr.Blossom exposing (..)

import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Time
import Nostr.Event exposing (Event, Tag(..))
import Nostr.Nip11 exposing (decodeUnixTime)
import Nostr.Types exposing (PubKey)

type alias BlobDescriptor =
    { url : String
    , sha256 : String
    , size : Int
    , type_ : Maybe String
    , uploaded : Time.Posix
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