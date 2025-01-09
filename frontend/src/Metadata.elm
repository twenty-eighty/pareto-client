module Metadata exposing (..)

import Http
import Json.Decode exposing (Decoder, field, map3, maybe, string)

type alias OpenGraphMetadata =
    { title : Maybe String
    , description : Maybe String
    , image : Maybe String
    }


openGraphMetadataDecoder : Decoder OpenGraphMetadata
openGraphMetadataDecoder =
    map3 OpenGraphMetadata
        (field "title" (maybe string))
        (field "description" (maybe string))
        (field "image" (maybe string))


fetchOpenGraphMetadata : String -> (Result Http.Error OpenGraphMetadata -> msg) -> Cmd msg
fetchOpenGraphMetadata url gotDataMsg =
    let
        requestUrl =
            "http://localhost:4000/api/opengraph?url=" ++ url
    in
    Http.get
        { url = requestUrl
        , expect = Http.expectJson gotDataMsg openGraphMetadataDecoder
        }
