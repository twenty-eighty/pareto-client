module LinkPreview exposing (LoadedContent, addLoadedContent, generatePreviewHtml)

import Dict exposing (Dict)
import Graphics
import Html.Styled as Html exposing (Html, div, img, a, text)
import Html.Styled.Attributes as Attr exposing (controls, css, href, src, alt, style, type_)
import Html.Styled.Events as Events
import Set exposing (Set)
import Tailwind.Utilities as Tw
import Tailwind.Theme as Theme
import Ui.Styles exposing (Styles)
import Url exposing (Url)
import Url.Parser exposing (Parser, (</>), s, string, parse)

type alias LoadedContent msg =
    { loadedUrls : Set String
    , addLoadedContentFunction : AddLoadedContent msg
    }

type alias AddLoadedContent msg =
    String -> msg

addLoadedContent : LoadedContent msg -> String -> LoadedContent msg
addLoadedContent loadedContent url =
    { loadedContent | loadedUrls = Set.insert url loadedContent.loadedUrls }

-- Function to generate preview HTML based on the link type
generatePreviewHtml : Maybe (LoadedContent msg) -> String -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
generatePreviewHtml loadedContent urlString linkAttr body =
    case Url.fromString urlString of
        Just url ->
            case detectLinkType url of
                YouTubeVideo videoId ->
                    generateYouTubePreview loadedContent url urlString videoId

                OdyseeVideo path ->
                    generateOdyseePreview loadedContent url urlString path

                RumbleVideo path ->
                    generateRumblePreview loadedContent url urlString path

                TwitterTweet tweetId ->
                    generateTwitterPreview urlString tweetId body

                VideoLink mimeType ->
                    generateVideoElement url mimeType

                AudioLink mimeType ->
                    generateAudioElement url mimeType

                ObjectLink mimeType ->
                    generateObjectElement url mimeType

                OtherLink ->
                    a (linkAttr ++ [ href urlString ]) body

        Nothing ->
            -- If URL parsing fails, just return a simple link
            a (linkAttr ++ [ href urlString ]) body


-- Define the types of links we can encounter


-- Define the types of links we can encounter
type LinkType
    = YouTubeVideo String
    | OdyseeVideo String
    | RumbleVideo String
    | TwitterTweet String
    | VideoLink String
    | AudioLink String
    | ObjectLink String
    | OtherLink


-- Function to detect the type of link and extract necessary IDs
detectLinkType : Url -> LinkType
detectLinkType url =
    if isYouTubeWatchUrl url then
        case getYouTubeVideoIdFromQuery url.query of
            Just videoId ->
                YouTubeVideo videoId

            Nothing ->
                OtherLink

    else if isYouTubeShortUrl url then
        case getYouTubeVideoIdFromPath url.path of
            Just videoId ->
                YouTubeVideo videoId

            Nothing ->
                OtherLink

    else if isOdyseeUrl url then
        OdyseeVideo url.path

    else if isRumbleUrl url then
        RumbleVideo url.path

    else if isTwitterStatusUrl url then
        case getTweetIdFromPath url.path of
            Just tweetId ->
                TwitterTweet tweetId

            Nothing ->
                OtherLink

    else if isVideohUrl url then
        case getVideoMimeTypeFromUrl url.path of
            Just mimeType ->
                VideoLink mimeType
            
            Nothing ->
                OtherLink

    else if isAudioUrl url then
        case getAudioMimeTypeFromUrl url.path of
            Just mimeType ->
                AudioLink mimeType
            
            Nothing ->
                OtherLink

    else if isObjectUrl url then
        case getObjectMimeTypeFromUrl url.path of
            Just mimeType ->
                ObjectLink mimeType
            
            Nothing ->
                OtherLink

    else
        OtherLink


-- Helper functions to identify and extract data from URLs

isYouTubeWatchUrl : Url -> Bool
isYouTubeWatchUrl url =
    let
        hosts =
            [ "www.youtube.com", "youtube.com", "m.youtube.com" ]
    in
    List.member url.host hosts && url.path == "/watch"


isVideohUrl : Url -> Bool
isVideohUrl url =
    getVideoMimeTypeFromUrl url.path /= Nothing

getVideoMimeTypeFromUrl : String -> Maybe String
getVideoMimeTypeFromUrl urlPath =
    [ ( "mp4", "video/mp4" )
    , ( "mov", "video/quicktime" )
    , ( "mkv", "video/x-matroska" )
    , ( "avi", "video/x-msvideo" )
    , ( "m4v", "video/x-m4v" )
    , ( "webm", "video/webm" )
    ]
    |> List.filterMap (\(extension, mimeType) ->
        if String.endsWith extension urlPath then
            Just mimeType
        else
            Nothing
    )
    |> List.head

isAudioUrl : Url -> Bool
isAudioUrl url =
    getAudioMimeTypeFromUrl url.path /= Nothing

getAudioMimeTypeFromUrl : String -> Maybe String
getAudioMimeTypeFromUrl urlPath =
    [ ( "mp3", "audio/mpeg" )
    , ( "wav", "audio/wav" )
    , ( "ogg", "audio/ogg" )
    ]
    |> List.filterMap (\(extension, mimeType) ->
        if String.endsWith extension urlPath then
            Just mimeType
        else
            Nothing
    )
    |> List.head

isObjectUrl : Url -> Bool
isObjectUrl url =
    getObjectMimeTypeFromUrl url.path /= Nothing

getObjectMimeTypeFromUrl : String -> Maybe String
getObjectMimeTypeFromUrl urlPath =
    [ ( "pdf", "application/pdf" )
    ]
    |> List.filterMap (\(extension, mimeType) ->
        if String.endsWith extension urlPath then
            Just mimeType
        else
            Nothing
    )
    |> List.head

getYouTubeVideoIdFromQuery : Maybe String -> Maybe String
getYouTubeVideoIdFromQuery maybeQuery =
    case maybeQuery of
        Just queryString ->
            let
                params =
                    parseQueryString queryString
            in
            Dict.get "v" params

        Nothing ->
            Nothing


isYouTubeShortUrl : Url -> Bool
isYouTubeShortUrl url =
    url.host == "youtu.be"


isOdyseeUrl : Url -> Bool
isOdyseeUrl url =
    url.host == "odysee.com"

isRumbleUrl : Url -> Bool
isRumbleUrl url =
    url.host == "rumble.com"

getYouTubeVideoIdFromPath : String -> Maybe String
getYouTubeVideoIdFromPath path =
    let
        pathSegments =
            String.split "/" (removeLeadingSlashes path)
    in
    case pathSegments of
        [ videoId ] ->
            Just videoId

        _ ->
            Nothing



isTwitterStatusUrl : Url -> Bool
isTwitterStatusUrl url =
    let
        hosts =
            [ "twitter.com", "www.twitter.com", "x.com", "www.x.com" ]
    in
    List.member url.host hosts



getTweetIdFromPath : String -> Maybe String
getTweetIdFromPath path =
    let
        pathSegments =
            String.split "/" (removeLeadingSlashes path)
    in
    case pathSegments of
        _ :: "status" :: tweetId :: _ ->
            Just tweetId

        _ ->
            Nothing


-- Helper function to remove leading slashes
removeLeadingSlashes : String -> String
removeLeadingSlashes str =
    case String.uncons str of
        Just ( '/', rest ) ->
            removeLeadingSlashes rest
        _ ->
            str


-- Function to parse the query string into a Dict
parseQueryString : String -> Dict String String
parseQueryString queryString =
    queryString
        |> String.split "&"
        |> List.filterMap (\param ->
            case String.split "=" param of
                [ key, value ] ->
                    Just ( key, value )

                [ key ] ->
                    Just ( key, "" )

                _ ->
                    Nothing
           )
        |> Dict.fromList


-- Function to generate YouTube preview HTML with a play button
generateYouTubePreview : Maybe (LoadedContent msg) -> Url -> String -> String -> Html msg
generateYouTubePreview maybeLoadedContent url urlString videoId =
    let
        thumbnailUrl =
            "https://img.youtube.com/vi/" ++ videoId ++ "/0.jpg"

        (showEmbedded, linkElement, clickAttr) =
            case maybeLoadedContent of
                Just loadedContent ->
                    (Set.member urlString loadedContent.loadedUrls
                    , Html.div
                    , [ Events.onClick (loadedContent.addLoadedContentFunction urlString)
                      , css
                            [ Tw.cursor_pointer
                            ]
                      ]
                    )

                Nothing ->
                    (False, Html.a, [ href urlString ])
    in
    if showEmbedded then
        Html.iframe
            [ Attr.width 560
            , Attr.height 315
            , Attr.src <| "https://www.youtube-nocookie.com/embed/" ++ videoId
            , Attr.title "YouTube video player"
            , Attr.attribute "frameborder" "0"
            , Attr.attribute "allow" "clipboard-write; encrypted-media; picture-in-picture; web-share"
            , Attr.attribute "referrerpolicy" "strict-origin-when-cross-origin"
            , Attr.attribute "allowfullscreen" ""
            ]
            []

    else
        videoThumbnailPreview linkElement clickAttr thumbnailUrl

     
-- Function to generate Odysee preview HTML with a play button
generateOdyseePreview : Maybe (LoadedContent msg) -> Url -> String -> String -> Html msg
generateOdyseePreview maybeLoadedContent url urlString path =
    let
        thumbnailUrl =
            "https://pareto.space/api/opengraph/image?url=" ++ Url.percentEncode urlString


        (showEmbedded, linkElement, clickAttr) =
            case maybeLoadedContent of
                Just loadedContent ->
                    (Set.member urlString loadedContent.loadedUrls
                    , Html.div
                    , [ Events.onClick (loadedContent.addLoadedContentFunction urlString)
                      , css
                            [ Tw.cursor_pointer
                            ]
                      ]
                    )

                Nothing ->
                    (False, Html.a, [ href urlString ])
    in
    if showEmbedded then
        Html.iframe
            [ Attr.width 560
            , Attr.height 315
            , Attr.src <| "https://odysee.com/$/embed" ++ path
            , Attr.attribute "allowfullscreen" ""
            ]
            []

    else
        videoThumbnailPreview linkElement clickAttr thumbnailUrl
    

-- Function to generate Rumble preview HTML with a play button
generateRumblePreview : Maybe (LoadedContent msg) -> Url -> String -> String -> Html msg
generateRumblePreview maybeLoadedContent url urlString path =
    let
        thumbnailUrl =
            "https://pareto.space/api/opengraph/image?url=" ++ Url.percentEncode urlString


        (showEmbedded, linkElement, clickAttr) =
            case maybeLoadedContent of
                Just loadedContent ->
                    (Set.member urlString loadedContent.loadedUrls
                    , Html.div
                    , [ Events.onClick (loadedContent.addLoadedContentFunction urlString)
                      , css
                            [ Tw.cursor_pointer
                            ]
                      ]
                    )

                Nothing ->
                    (False, Html.a, [ href urlString ])
    in
    if showEmbedded then
        Html.iframe
            [ Attr.width 560
            , Attr.height 315
            , Attr.src <| "https://pareto.space/api/rumble/embed?url=" ++ Url.percentEncode urlString
            , Attr.attribute "allowfullscreen" ""
            ]
            []

    else
        videoThumbnailPreview linkElement clickAttr thumbnailUrl
    

videoThumbnailPreview linkElement clickAttr thumbnailUrl =
        linkElement
            ([ css
                [ Tw.relative
                , Tw.block
                , Tw.w_96
                , Tw.h_72
                ]
            
            ] ++ clickAttr)
            [ div 
                [ css
                    [ Tw.absolute
                    , Tw.inset_0
                    ]
                ]
                [ img
                    [ src thumbnailUrl
                    , alt "YouTube Video"
                    , style "display" "block"
                    ] []
                ]
            , div
                [ css
                    [ Tw.text_color Theme.red_500
                    , Tw.absolute
                    , Tw.inset_0
                    , Tw.flex
                    , Tw.items_center
                    , Tw.justify_center
                    , Tw.opacity_90
                    ]
                ]
                [ Graphics.videoPlayIcon 100
                ]
            ]


-- Function to generate Twitter preview HTML
generateTwitterPreview : String -> String -> List (Html msg) -> Html msg
generateTwitterPreview url _ body =
    -- Embedding tweets requires external scripts, so we provide a link
        a [ href url ]
            body

generateVideoElement : Url -> String -> Html msg
generateVideoElement url mimeType =
    Html.video
        [ controls True
        ]
        [ Html.source
            [ src <| Url.toString url
            , type_ mimeType
            ]
            []
        , text "Your browser doesn't support the HTML video tag"
        ]

generateAudioElement : Url -> String -> Html msg
generateAudioElement url mimeType =
    Html.audio
        [ controls True
        ]
        [ Html.source
            [ src <| Url.toString url
            , type_ mimeType
            ]
            []
        , text "Your browser doesn't support the HTML video tag"
        ]

generateObjectElement : Url -> String -> Html msg
generateObjectElement url mimeType =
    Html.object
        [ Attr.attribute "data" <| Url.toString url
        , type_ mimeType
        , css
            [ Tw.w_full
            , Tw.h_96
            ]
        ]
        [ Html.p
            []
            [ text "Your browser doesn't support PDFs. You can download the PDF "
            , Html.a
                [ href <| Url.toString url
                ]
                [ text "here"
                ]
            , text "."
            ]
        ]