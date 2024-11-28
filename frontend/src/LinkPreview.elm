module LinkPreview exposing (generatePreviewHtml)

import Dict exposing (Dict)
import Graphics
import Html.Styled as Html exposing (Html, div, img, a, text)
import Html.Styled.Attributes exposing (css, href, src, alt, style)
import Maybe exposing (withDefault)
import String exposing (contains)
import Url exposing (Url)
import Url.Parser exposing (Parser, (</>), s, string, parse)
import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw
import Tailwind.Theme as Theme


-- Function to generate preview HTML based on the link type
generatePreviewHtml : String -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
generatePreviewHtml urlString linkAttr body =
    case Url.fromString urlString of
        Just url ->
            case detectLinkType url of
                YouTubeVideo videoId ->
                    generateYouTubePreview urlString videoId

                TwitterTweet tweetId ->
                    generateTwitterPreview urlString tweetId

                OtherLink ->
                    a [ href urlString ] body

        Nothing ->
            -- If URL parsing fails, just return a simple link
            a (linkAttr ++ [ href urlString ]) body


-- Define the types of links we can encounter


-- Define the types of links we can encounter
type LinkType
    = YouTubeVideo String
    | TwitterTweet String
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

    else if isTwitterStatusUrl url then
        case getTweetIdFromPath url.path of
            Just tweetId ->
                TwitterTweet tweetId

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
generateYouTubePreview : String -> String -> Html msg
generateYouTubePreview url videoId =
    let
        thumbnailUrl =
            "https://img.youtube.com/vi/" ++ videoId ++ "/0.jpg"
    in
    a
        [ css
            [ Tw.relative
            , Tw.block
            , Tw.w_96
            , Tw.h_72
            ]
        , href url
        ]
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
generateTwitterPreview : String -> String -> Html msg
generateTwitterPreview url _ =
    -- Embedding tweets requires external scripts, so we provide a link
    div []
        [ a [ href url ]
            [ text "View Tweet" ]
        ]