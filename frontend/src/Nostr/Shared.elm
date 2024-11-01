module Nostr.Shared exposing (..)

import Http

httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl url -> "Bad URL: " ++ url
        Http.Timeout -> "Network timeout"
        Http.NetworkError -> "Network error"
        Http.BadStatus status -> "Bad status: " ++ String.fromInt status
        Http.BadBody errString -> "Bad body: " ++ errString