module Nostr.Shared exposing (..)

import Http


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl url ->
            "Bad URL: " ++ url

        Http.Timeout ->
            "Network timeout"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus status ->
            "Bad status: " ++ String.fromInt status ++ " (" ++ httpTextForStatus status ++ ")"

        Http.BadBody errString ->
            "Bad body: " ++ errString


httpTextForStatus : Int -> String
httpTextForStatus status =
    case status of
        100 ->
            "Continue"

        101 ->
            "Switching Protocols"

        102 ->
            "Processing (WebDAV; RFC 2518)"

        103 ->
            "Early Hints (RFC 8297)"

        200 ->
            "OK"

        201 ->
            "Created"

        202 ->
            "Accepted"

        203 ->
            "Non-Authoritative Information"

        204 ->
            "No Content"

        205 ->
            "Reset Content"

        206 ->
            "Partial Content"

        207 ->
            "Multi-Status"

        208 ->
            "Already Reported"

        226 ->
            "IM Used"

        300 ->
            "Multiple Choices"

        301 ->
            "Moved Permanently"

        302 ->
            "Found (Previously \"Moved temporarily\")"

        303 ->
            "See Other (since HTTP/1.1)"

        304 ->
            "Not Modified"

        305 ->
            "Use Proxy (since HTTP/1.1)"

        306 ->
            "Switch Proxy"

        307 ->
            "Temporary Redirect (since HTTP/1.1)"

        308 ->
            "Permanent Redirect"

        400 ->
            "Bad Request"

        401 ->
            "Unauthorized"

        402 ->
            "Payment Required"

        403 ->
            "Forbidden"

        404 ->
            "Not Found"

        405 ->
            "Method Not Allowed"

        406 ->
            "Not Acceptable"

        407 ->
            "Proxy Authentication Required"

        408 ->
            "Request Timeout"

        409 ->
            "Conflict"

        410 ->
            "Gone"

        411 ->
            "Length Required"

        412 ->
            "Precondition Failed"

        413 ->
            "Payload Too Large"

        414 ->
            "URI Too Long"

        415 ->
            "Unsupported Media Type"

        416 ->
            "Range Not Satisfiable"

        417 ->
            "Expectation Failed"

        418 ->
            "I'm a teapot"

        419 ->
            "Page Expired"

        420 ->
            "Enhance Your Calm"

        421 ->
            "Misdirected Request"

        422 ->
            "Unprocessable Content"

        423 ->
            "Locked"

        424 ->
            "Failed Dependency"

        425 ->
            "Too Early"

        426 ->
            "Upgrade Required"

        428 ->
            "Precondition Required"

        429 ->
            "Too Many Requests"

        430 ->
            "Shopify Security Rejection"

        431 ->
            "Request Header Fields Too Large"

        449 ->
            "Retry With"

        450 ->
            "Blocked by Windows Parental Controls"

        451 ->
            "Unavailable For Legal Reasons"

        494 ->
            "Request header too large"

        495 ->
            "SSL Certificate Error"

        496 ->
            "SSL Certificate Required"

        497 ->
            "HTTP Request Sent to HTTPS Port"

        499 ->
            "Client Closed Request"

        500 ->
            "Internal Server Error"

        501 ->
            "Not Implemented"

        502 ->
            "Bad Gateway"

        503 ->
            "Service Unavailable"

        504 ->
            "Gateway Timeout"

        505 ->
            "HTTP Version Not Supported"

        506 ->
            "Variant Also Negotiates"

        507 ->
            "Insufficient Storage"

        508 ->
            "Loop Detected"

        510 ->
            "Not Extended"

        511 ->
            "Network Authentication Required"

        520 ->
            "Web Server Returned an Unknown Error"

        521 ->
            "Web Server Is Down"

        522 ->
            "Connection Timed Out"

        523 ->
            "Origin Is Unreachable"

        524 ->
            "A Timeout Occurred"

        525 ->
            "SSL Handshake Failed"

        526 ->
            "Invalid SSL Certificate"

        540 ->
            "Temporarily Disabled"

        598 ->
            "Network read timeout error"

        599 ->
            "Network Connect Timeout Error"

        783 ->
            "Unexpected Token"

        999 ->
            "Non-standard"

        _ ->
            "Unknown status code"


ensureHttps : String -> String
ensureHttps url =
    if String.startsWith "http://" url then
        "https://" ++ String.dropLeft 7 url

    else
        url
