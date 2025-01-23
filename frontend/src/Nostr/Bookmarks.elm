module Nostr.Bookmarks exposing (..)

import Dict exposing (Dict)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as DecodePipeline
import Nostr.Types exposing (PubKey)
import Time


type alias Bookmark =
    { name : String
    , address : String
    }



{-
   ["REQ","kinds:3,10002,10000,-69",
       {
       "kinds":[3,10002,10000,30000,30003,10004],
       "authors":["8127df93d8453767aa11e74206f48aeea30d3d65a383c98d243b031fc7446afb"]
       }
   ]
-}


nostrBookmarkDecoder : Decode.Decoder Bookmark
nostrBookmarkDecoder =
    Decode.succeed Bookmark
        |> DecodePipeline.required "name" Decode.string
        |> DecodePipeline.required "address" Decode.string
