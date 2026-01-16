module LinkPreview exposing (LoadedContent, addLoadedContent, generatePreviewHtml)

import Erl
import Graphics
import Html.Styled as Html exposing (Html, a, div, img, text)
import Html.Styled.Attributes as Attr exposing (alt, controls, css, href, src, style, type_, value)
import Html.Styled.Events as Events
import List.Extra
import Oembed
import Regex exposing (Regex)
import Set exposing (Set)
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw
import Url
import Url.Parser exposing ((</>), string)


type alias LoadedContent msg =
    { loadedUrls : Set String
    , addLoadedContentFunction : AddLoadedContent msg
    }


type alias AddLoadedContent msg =
    String -> msg


addLoadedContent : LoadedContent msg -> String -> LoadedContent msg
addLoadedContent loadedContent url =
    { loadedContent | loadedUrls = Set.insert url loadedContent.loadedUrls }

openGraphImageUrl : String -> String
openGraphImageUrl urlString =
    "https://pareto.space/api/opengraph/image?url=" ++ Url.percentEncode urlString
    -- "http://localhost:4000/api/opengraph/image?url=" ++ Url.percentEncode urlString


type LinkType
    = YouTubeVideo String
    | OdyseeVideo
    | RumbleVideo
    | TelegramLink String (Maybe String)
    | TwitterTweet String
    | VideoLink String
    | AudioLink String
    | PodBeanLink String
    | ObjectLink String
    | PlainLink
    | EmbeddableLink String



-- Function to generate preview HTML based on the link type


generatePreviewHtml : Maybe (LoadedContent msg) -> String -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
generatePreviewHtml loadedContent urlString linkAttr body =
    let
        parsed =
            urlString
                |> Erl.parse

        sanitizedUrl =
            filterTrackingParams parsed

        linkType =
            detectLinkType sanitizedUrl urlString
    in
    case Url.fromString urlString of
        Just _ ->
            case linkType of
                YouTubeVideo videoId ->
                    generateYouTubePreview loadedContent urlString videoId

                OdyseeVideo ->
                    generateOdyseePreview loadedContent urlString

                RumbleVideo ->
                    generateRumblePreview loadedContent urlString

                TelegramLink groupId maybePostId ->
                    generateTelegramPreview urlString groupId maybePostId body

                TwitterTweet tweetId ->
                    generateTwitterPreview urlString tweetId body

                VideoLink mimeType ->
                    generateVideoElement loadedContent sanitizedUrl urlString mimeType

                AudioLink mimeType ->
                    generateAudioElement loadedContent sanitizedUrl urlString mimeType

                PodBeanLink iFrameUrl ->
                    generatePodbeanPreview loadedContent urlString iFrameUrl

                ObjectLink mimeType ->
                    generateObjectElement loadedContent sanitizedUrl urlString mimeType

                PlainLink ->
                    a (linkAttr ++ [ href <| Erl.toString sanitizedUrl ]) body

                EmbeddableLink originalUrl ->
                    generateGenericPreview loadedContent originalUrl linkAttr body

        Nothing ->
            -- If URL parsing fails, show regular link
            a (linkAttr ++ [ href urlString ]) body



-- Function to detect the type of link and extract necessary IDs


detectLinkType : Erl.Url -> String -> LinkType
detectLinkType url originalUrl =
    if isYouTubeUrl url then
        case getYouTubeVideoId url of
            Just videoId ->
                YouTubeVideo videoId

            Nothing ->
                PlainLink

    else if isOdyseeUrl url then
        OdyseeVideo

    else if isRumbleUrl url then
        RumbleVideo

    else if isTelegramUrl url then
        case getTelegramGroup url.path of
            Just ( groupId, maybePostId ) ->
                TelegramLink groupId maybePostId

            Nothing ->
                PlainLink

    else if isTwitterStatusUrl url then
        case getTweetIdFromPath url.path of
            Just tweetId ->
                TwitterTweet tweetId

            Nothing ->
                PlainLink

    else if isVideohUrl url then
        case getVideoMimeTypeFromUrl url.path of
            Just mimeType ->
                VideoLink mimeType

            Nothing ->
                PlainLink

    else if isAudioUrl url then
        case getAudioMimeTypeFromUrl url.path of
            Just mimeType ->
                AudioLink mimeType

            Nothing ->
                PlainLink

    else if isPodBeanUrl url then
        case parsePodbeanUrl url of
            Just iFrameUrl ->
                PodBeanLink iFrameUrl

            Nothing ->
                PlainLink

    else if isObjectUrl url then
        case getObjectMimeTypeFromUrl url.path of
            Just mimeType ->
                ObjectLink mimeType

            Nothing ->
                PlainLink

    else if isPlainLinkkUrl url then
        -- plain link must be tested before embeddable link to avoid trying to embed Facebook links
        PlainLink

    else if isEmbeddable originalUrl then
        EmbeddableLink originalUrl

    else
        PlainLink


filterTrackingParams : Erl.Url -> Erl.Url
filterTrackingParams url =
    let
        filteredQuery =
            url.query
                |> List.filter
                    (\( key, _ ) ->
                        not (Set.member key filteredParams)
                    )
    in
    { url | query = filteredQuery }


filteredParams : Set String
filteredParams =
    Set.fromList
        [ "dmcid"
        , "fbclid" -- Facebook
        , "fbc" -- Facebook
        , "f_tid"
        , "igshid" -- Instagram
        , "originalReferrer"
        , "ref_src" -- Twitter
        , "utm_source" -- Google
        , "utm_medium" -- Google
        , "utm_campaign" -- Google
        , "utm_term" -- Google
        , "utm_content" -- Google
        , "wt_zmc" -- Zeit
        , "xing_share" -- Xing
        ]



-- Helper functions to identify and extract data from URLs


isYouTubeUrl : Erl.Url -> Bool
isYouTubeUrl url =
    let
        hosts =
            [ [ "www", "youtube", "com" ]
            , [ "youtube", "com" ]
            , [ "m", "youtube", "com" ]
            , [ "youtu", "be" ]
            ]
    in
    List.member url.host hosts


-- don't embed Facebook content - requires App registration to use oEmbed


isPlainLinkkUrl : Erl.Url -> Bool
isPlainLinkkUrl url =
    let
        hosts =
            [ [ "www", "facebook", "com" ] ]
    in
    List.member url.host hosts


isEmbeddable : String -> Bool
isEmbeddable urlString =
    Oembed.matchesProvider oemProviders urlString


type alias DetectMimeTypeFromPathFunction =
    List String -> Maybe String


urlHasKnownMimeType : Erl.Url -> DetectMimeTypeFromPathFunction -> Bool
urlHasKnownMimeType url detectFn =
    detectFn url.path /= Nothing


isVideohUrl : Erl.Url -> Bool
isVideohUrl url =
    urlHasKnownMimeType url getVideoMimeTypeFromUrl


getVideoMimeTypeFromUrl : DetectMimeTypeFromPathFunction
getVideoMimeTypeFromUrl urlPath =
    [ ( "mp4", "video/mp4" )
    , ( "mov", "video/quicktime" )
    , ( "mkv", "video/x-matroska" )
    , ( "avi", "video/x-msvideo" )
    , ( "m4v", "video/x-m4v" )
    , ( "webm", "video/webm" )
    ]
        |> getMimeTypeFromUrl urlPath


getMimeTypeFromUrl : List String -> List ( String, String ) -> Maybe String
getMimeTypeFromUrl urlPath mimeMappingList =
    let
        maybeLastPathComponent =
            urlPath
                |> List.Extra.last
    in
    maybeLastPathComponent
        |> Maybe.andThen
            (\lastPathComponent ->
                mimeMappingList
                    |> List.filterMap
                        (\( extension, mimeType ) ->
                            if String.endsWith extension lastPathComponent then
                                Just mimeType

                            else
                                Nothing
                        )
                    |> List.head
            )


isAudioUrl : Erl.Url -> Bool
isAudioUrl url =
    urlHasKnownMimeType url getAudioMimeTypeFromUrl


getAudioMimeTypeFromUrl : DetectMimeTypeFromPathFunction
getAudioMimeTypeFromUrl urlPath =
    [ ( "mp3", "audio/mpeg" )
    , ( "wav", "audio/wav" )
    , ( "ogg", "audio/ogg" )
    ]
        |> getMimeTypeFromUrl urlPath


isObjectUrl : Erl.Url -> Bool
isObjectUrl url =
    urlHasKnownMimeType url getObjectMimeTypeFromUrl


getObjectMimeTypeFromUrl : DetectMimeTypeFromPathFunction
getObjectMimeTypeFromUrl urlPath =
    [ ( "pdf", "application/pdf" )
    ]
        |> getMimeTypeFromUrl urlPath


getYouTubeVideoId : Erl.Url -> Maybe String
getYouTubeVideoId url =
    case url.path of
        [ "embed", videoId ] ->
            Just videoId

        [ "watch" ] ->
            url.query
                |> List.filterMap
                    (\( key, value ) ->
                        if key == "v" then
                            Just value

                        else
                            Nothing
                    )
                |> List.head

        [ videoId ] ->
            Just videoId

        _ ->
            Nothing

isOdyseeUrl : Erl.Url -> Bool
isOdyseeUrl url =
    url.host == [ "odysee", "com" ]


isPodBeanUrl : Erl.Url -> Bool
isPodBeanUrl url =
    url.host == [ "www", "podbean", "com" ]


isRumbleUrl : Erl.Url -> Bool
isRumbleUrl url =
    url.host == [ "rumble", "com" ]


isTelegramUrl : Erl.Url -> Bool
isTelegramUrl url =
    let
        hosts =
            [ [ "t", "me" ]
            ]
    in
    List.member url.host hosts


isTwitterStatusUrl : Erl.Url -> Bool
isTwitterStatusUrl url =
    let
        hosts =
            [ [ "twitter", "com" ]
            , [ "www", "twitter", "com" ]
            , [ "x", "com" ]
            , [ "www", "x", "com" ]
            ]
    in
    List.member url.host hosts


getTelegramGroup : List String -> Maybe ( String, Maybe String )
getTelegramGroup path =
    case path of
        [] ->
            Nothing

        [ groupId ] ->
            Just ( groupId, Nothing )

        [ groupId, postId ] ->
            Just ( groupId, Just postId )

        _ ->
            Nothing


getTweetIdFromPath : List String -> Maybe String
getTweetIdFromPath path =
    case path of
        _ :: "status" :: tweetId :: _ ->
            Just tweetId

        _ ->
            Nothing



-- Function to generate YouTube preview HTML with a play button


generateYouTubePreview : Maybe (LoadedContent msg) -> String -> String -> Html msg
generateYouTubePreview maybeLoadedContent urlString videoId =
    let
        thumbnailUrl =
            "https://img.youtube.com/vi/" ++ videoId ++ "/0.jpg"

        -- disabled, yewtu.be seems to be not very reliable
        -- "https://yewtu.be/vi/" ++ videoId ++ "/maxres.jpg"
        ( showEmbedded, linkElement, clickAttr ) =
            case maybeLoadedContent of
                Just loadedContent ->
                    ( Set.member urlString loadedContent.loadedUrls
                    , Html.div
                    , [ Events.onClick (loadedContent.addLoadedContentFunction urlString)
                      , css
                            [ Tw.cursor_pointer
                            ]
                      ]
                    )

                Nothing ->
                    ( False, Html.a, [ href urlString ] )
    in
    if showEmbedded then
        Html.iframe
            [ Attr.width 560
            , Attr.height 315

            -- yewtu.be allows watching even with VPN and less data for YouTube.
            -- however, it's not very reliable
            -- , Attr.src <| "https://yewtu.be/embed/" ++ videoId
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


generateOdyseePreview : Maybe (LoadedContent msg) -> String -> Html msg
generateOdyseePreview maybeLoadedContent urlString =
    let
        odyseeNormalizedUrlString =
            urlString
                |> normalizeOdyseeUrl

        ( showEmbedded, linkElement, clickAttr ) =
            case maybeLoadedContent of
                Just loadedContent ->
                    ( Set.member odyseeNormalizedUrlString loadedContent.loadedUrls
                    , Html.div
                    , [ Events.onClick (loadedContent.addLoadedContentFunction odyseeNormalizedUrlString)
                      , css
                            [ Tw.cursor_pointer
                            ]
                      ]
                    )

                Nothing ->
                    ( False, Html.a, [ href odyseeNormalizedUrlString ] )
    in
    case ( showEmbedded, Url.fromString odyseeNormalizedUrlString ) of
        -- The Erl URL parser apparently doesn't deliver the correct path for Odysee URLS with path segments like "@abcd:7" - the ":7" is missing
        -- once this is fixed we could avoid parsing the URL here (again)
        ( True, Just url ) ->
            Html.iframe
                [ Attr.width 560
                , Attr.height 315
                , Attr.src <| "https://odysee.com/$/embed" ++ url.path
                , Attr.attribute "allowfullscreen" ""
                ]
                []

        ( _, _ ) ->
            videoThumbnailPreview linkElement clickAttr (openGraphImageUrl urlString)

-- Function to normalize Odysee URL strings by decoding percent-encoded text in the path
-- and removing /$/embed/ or /%24/embed/ prefixes
-- Example: "https://odysee.com/%24/embed/%40MWGFD%3A0%2Ftillenburg_katja%3Ae?autoplay=true"
-- becomes: "https://odysee.com/@MWGFD:0/tillenburg_katja:e?autoplay=true"
normalizeOdyseeUrl : String -> String
normalizeOdyseeUrl urlString =
    case Url.fromString urlString of
        Just url ->
            let
                decodedPath =
                    url.path
                        |> Url.percentDecode
                        |> Maybe.withDefault url.path
                
                -- Remove /$/embed/ prefix if present (after decoding)
                cleanedPath =
                    if String.startsWith "/$/embed/" decodedPath then
                        String.dropLeft 7 decodedPath
                    else
                        decodedPath
                
                normalizedUrl =
                    { url | path = cleanedPath }
            in
            Url.toString normalizedUrl
            
        Nothing ->
            urlString




-- Function to generate Podbean preview HTML with a play button


generatePodbeanPreview : Maybe (LoadedContent msg) -> String -> String -> Html msg
generatePodbeanPreview maybeLoadedContent urlString iFrameUrl =
    let
        thumbnailUrl =
            openGraphImageUrl urlString

        ( showEmbedded, linkElement, clickAttr ) =
            case maybeLoadedContent of
                Just loadedContent ->
                    ( Set.member urlString loadedContent.loadedUrls
                    , Html.div
                    , [ Events.onClick (loadedContent.addLoadedContentFunction urlString)
                      , css
                            [ Tw.cursor_pointer
                            ]
                      ]
                    )

                Nothing ->
                    ( False, Html.a, [ href urlString ] )
    in
    if showEmbedded then
        Html.iframe
            [ Attr.attribute "allowtransparency" "true"
            , Attr.height 300
            , Attr.style "border" "none"
            , Attr.style "min-width" "min(100%, 430px)"
            , Attr.style "height" "300px"
            , Attr.attribute "scrolling" "no"
            , Attr.attribute "data-name" "pb-iframe-player"
            , Attr.src iFrameUrl
            , Attr.attribute "loading" "lazy"
            , Attr.attribute "allowfullscreen" ""
            ]
            []

    else
        videoThumbnailPreview linkElement clickAttr thumbnailUrl


parsePodbeanUrl : Erl.Url -> Maybe String
parsePodbeanUrl inputUrl =
    inputUrl.path
        |> String.join "/"
        |> extractPodcastId
        |> Maybe.map buildPodBeanIframeUrl


extractPodcastId : String -> Maybe String
extractPodcastId path =
    let
        podBeanRegex =
            regex "pb-([a-zA-Z0-9]+-[a-zA-Z0-9]+)"
    in
    case Regex.find podBeanRegex path of
        match :: _ ->
            -- Remove the leading "pb-" by taking only the captured group
            case List.head match.submatches of
                Just (Just id) ->
                    Just (id ++ "-pb")

                _ ->
                    Nothing

        _ ->
            Nothing


buildPodBeanIframeUrl : String -> String
buildPodBeanIframeUrl podcastId =
    "https://www.podbean.com/player-v2/?from=embed&i="
        ++ podcastId
        ++ "&square=1&share=1&download=1&fonts=Courier%20New&skin=1&font-color=&rtl=0&logo_link=&btn-skin=ff6d00&size=300"



-- Function to generate generic oEmbed preview HTML with a play button


generateGenericPreview : Maybe (LoadedContent msg) -> String -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
generateGenericPreview maybeLoadedContent urlString linkAttr body =
    let
        thumbnailUrl =
            openGraphImageUrl urlString

        ( showEmbedded, linkElement, clickAttr ) =
            case maybeLoadedContent of
                Just loadedContent ->
                    ( Set.member urlString loadedContent.loadedUrls
                    , Html.div
                    , [ Events.onClick (loadedContent.addLoadedContentFunction urlString)
                      , css
                            [ Tw.cursor_pointer
                            ]
                      ]
                    )

                Nothing ->
                    ( False, Html.a, [ href urlString ] )
    in
    if showEmbedded then
        case Oembed.view oemProviders (Just { maxWidth = 300, maxHeight = 600 }) urlString of
            Just embedHtml ->
                div
                    [ css
                        [ Tw.w_full
                        , Tw.h_full
                        ]
                    ]
                    [ embedHtml
                        |> Html.fromUnstyled
                    ]

            Nothing ->
                a (linkAttr ++ [ href urlString ]) body

    else
        videoThumbnailPreview linkElement clickAttr thumbnailUrl



-- Function to generate Rumble preview HTML with a play button


generateRumblePreview : Maybe (LoadedContent msg) -> String -> Html msg
generateRumblePreview maybeLoadedContent urlString =
    let
        thumbnailUrl =
            rumbleThumbnailUrl urlString

        ( showEmbedded, linkElement, clickAttr ) =
            case maybeLoadedContent of
                Just loadedContent ->
                    ( Set.member urlString loadedContent.loadedUrls
                    , Html.div
                    , [ Events.onClick (loadedContent.addLoadedContentFunction urlString)
                      , css
                            [ Tw.cursor_pointer
                            ]
                      ]
                    )

                Nothing ->
                    ( False, Html.a, [ href urlString ] )
    in
    if showEmbedded then
        Html.iframe
            [ Attr.width 560
            , Attr.height 315
            , Attr.src <| rumbleProxyUrl urlString
            , Attr.attribute "allowfullscreen" ""
            ]
            []

    else
        videoThumbnailPreview linkElement clickAttr thumbnailUrl

rumbleProxyUrl : String -> String
rumbleProxyUrl urlString =
    "https://pareto.space/api/rumble/oembed/embed?url=" ++ Url.percentEncode urlString
    -- "http://localhost:4444/api/rumble/oembed/embed?url=" ++ Url.percentEncode urlString

rumbleThumbnailUrl : String -> String
rumbleThumbnailUrl urlString =
    "https://pareto.space/api/rumble/oembed/thumbnail?url=" ++ Url.percentEncode urlString
    -- "http://localhost:4444/api/rumble/oembed/thumbnail?url=" ++ Url.percentEncode urlString

videoThumbnailPreview : (List (Html.Attribute msg) -> List (Html msg) -> Html msg) -> List (Html.Attribute msg) -> String -> Html msg
videoThumbnailPreview linkElement clickAttr thumbnailUrl =
    linkElement
        (css
            [ Tw.relative
            , Tw.block
            , Tw.h_40
            , Tw.w_60
            ]
            :: clickAttr
        )
        [ div
            [ css
                [ Tw.inset_0
                , Tw.h_40
                , Tw.overflow_hidden
                ]
            ]
            [ img
                [ src thumbnailUrl
                , alt "Thumbnail"
                , css
                    [ Tw.w_full
                    , Tw.h_full
                    , Tw.object_cover
                    ]
                ]
                []
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



-- Function to generate Telegram post HTML


generateTelegramPreview : String -> String -> Maybe String -> List (Html msg) -> Html msg
generateTelegramPreview urlString groupId maybePostId body =
    case maybePostId of
        Just postId ->
            div []
                [ Html.blockquote
                    [ Attr.attribute "data-telegram-post" (groupId ++ "/" ++ postId)
                    , Attr.attribute "data-width" "100%"
                    , Attr.class "telegram-post"
                    ]
                    []
                , Html.node "script-element"
                    [ Attr.attribute "src" "https://telegram.org/js/telegram-widget.js?22"
                    ]
                    []
                ]

        Nothing ->
            -- TODO: Show group info with OpenGraph data from https://t.me/<group>
            a [ href urlString ]
                body



-- Function to generate Twitter preview HTML


generateTwitterPreview : String -> String -> List (Html msg) -> Html msg
generateTwitterPreview url _ body =
    case Oembed.view oemProviders (Just { maxWidth = 300, maxHeight = 600 }) url of
        Just tweetHtml ->
            div
                [ css
                    [ Tw.w_96
                    , Tw.h_full
                    ]
                ]
                [ tweetHtml
                    |> Html.fromUnstyled
                ]

        Nothing ->
            a [ href url ]
                body


oemProviders : List Oembed.Provider
oemProviders =
    [ { url = "https://publish.x.com/oembed"
      , schemes = [ regex "https://x\\.com/.*/status/.*", regex "https://.*\\.x\\.com/.*/status/.*" ]
      }
    , { url = "https://tube.theplattform.net/services/oembed"
      , schemes = [ regex "https://tube\\.theplattform\\.net" ]
      }

    -- see https://developers.facebook.com/docs/plugins/oembed
    , { url = "https://www.facebook.com/oembed_page"
      , schemes = [ regex "https://www\\.facebook\\.com/" ]
      }
    , { url = "https://www.facebook.com/oembed_post"
      , schemes = [ regex "https://www\\.facebook\\.com/.*/posts/.*", regex "https://www\\.facebook\\.com/photos/.*", regex "https://www\\.facebook\\.com/.*/photos/.*", regex "https://www\\.facebook\\.com/photo\\.php.*", regex "https://www\\.facebook\\.com/photo\\.php", regex "https://www\\.facebook\\.com/.*/activity/.*", regex "https://www\\.facebook\\.com/permalink\\.php", regex "https://www\\.facebook\\.com/media/set\\?set=.*", regex "https://www\\.facebook\\.com/questions/.*", regex "https://www\\.facebook\\.com/notes/.*/.*/.*" ]
      }
    , { url = "https://www.facebook.com/oembed_video"
      , schemes = [ regex "https://www\\.facebook\\.com/.*/videos/.*", regex "https://www\\.facebook\\.com/video\\.php" ]
      }
    , { url = "https://rumble.com/api/Media/oembed.json"
      , schemes = [ regex "https://rumble\\.com/.*" ]
      }
    , { url = "https://rutube.ru/api/oembed"
      , schemes = [ regex "https://rutube\\.ru/video/.*" ]
      }
    ]


regex : String -> Regex
regex string =
    string
        |> Regex.fromString
        |> Maybe.withDefault Regex.never


generateVideoElement : Maybe (LoadedContent msg) -> Erl.Url -> String -> String -> Html msg
generateVideoElement maybeLoadedContent url urlString mimeType =
    let
        ( showEmbedded, linkElement, clickAttr ) =
            case maybeLoadedContent of
                Just loadedContent ->
                    ( Set.member urlString loadedContent.loadedUrls
                    , Html.div
                    , [ Events.onClick (loadedContent.addLoadedContentFunction urlString)
                      , css
                            [ Tw.cursor_pointer
                            ]
                      ]
                    )

                Nothing ->
                    ( False, Html.a, [ href urlString ] )
    in
    if showEmbedded then
        Html.video
            [ controls True
            ]
            [ Html.source
                [ src <| Erl.toString url
                , type_ mimeType
                ]
                []
            , text "Your browser doesn't support the HTML video tag"
            ]

    else
        videoThumbnailPreview linkElement clickAttr "/images/video-placeholder.jpeg"


generateAudioElement : Maybe (LoadedContent msg) -> Erl.Url -> String -> String -> Html msg
generateAudioElement maybeLoadedContent url urlString mimeType =
    let
        ( showEmbedded, linkElement, clickAttr ) =
            case maybeLoadedContent of
                Just loadedContent ->
                    ( Set.member urlString loadedContent.loadedUrls
                    , Html.div
                    , [ Events.onClick (loadedContent.addLoadedContentFunction urlString)
                      , css
                            [ Tw.cursor_pointer
                            ]
                      ]
                    )

                Nothing ->
                    ( False, Html.a, [ href urlString ] )
    in
    if showEmbedded then
        Html.audio
            [ controls True
            ]
            [ Html.source
                [ src <| Erl.toString url
                , type_ mimeType
                ]
                []
            , text "Your browser doesn't support the HTML audio tag"
            ]

    else
        genericThumbnailPreview linkElement clickAttr "/images/audio-placeholder.jpeg"


generateObjectElement : Maybe (LoadedContent msg) -> Erl.Url -> String -> String -> Html msg
generateObjectElement maybeLoadedContent url urlString mimeType =
    let
        ( showEmbedded, linkElement, clickAttr ) =
            case maybeLoadedContent of
                Just loadedContent ->
                    ( Set.member urlString loadedContent.loadedUrls
                    , Html.div
                    , [ Events.onClick (loadedContent.addLoadedContentFunction urlString)
                      , css
                            [ Tw.cursor_pointer
                            ]
                      ]
                    )

                Nothing ->
                    ( False, Html.a, [ href urlString ] )
    in
    if showEmbedded then
        Html.object
            [ Attr.attribute "data" <| Erl.toString url
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
                    [ href <| Erl.toString url
                    ]
                    [ text "here"
                    ]
                , text "."
                ]
            ]

    else
        genericThumbnailPreview linkElement clickAttr "/images/pdf-placeholder.jpeg"


genericThumbnailPreview : (List (Html.Attribute msg) -> List (Html msg) -> Html msg) -> List (Html.Attribute msg) -> String -> Html msg
genericThumbnailPreview linkElement clickAttr thumbnailUrl =
    linkElement
        (css
            [ Tw.relative
            , Tw.block
            , Tw.h_40
            , Tw.w_64
            ]
            :: clickAttr
        )
        [ div
            [ css
                [ Tw.absolute
                , Tw.inset_0
                , Tw.h_40
                ]
            ]
            [ img
                [ src thumbnailUrl
                , alt "Thumbnail"
                , style "display" "block"
                ]
                []
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
