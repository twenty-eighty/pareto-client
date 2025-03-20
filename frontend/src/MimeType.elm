{-
The original package was published on https://github.com/danyx23/elm-mimetype

Copyright (c) 2015, Daniel Bachler
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.

* Neither the name of elm-mimetype nor the names of its
  contributors may be used to endorse or promote products derived from
  this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

module MimeType exposing
    ( MimeType(..)
    , mimeTypeDecoder, parseMimeType, toString
    , MimeText(..), MimeImage(..), MimeAudio(..), MimeVideo(..), MimeApp(..)
    )

{-| This modules provides the union type MimeType to model some of the most common
mime types and a parsing function that tries to parse a MimeType. The possible values for
MimeType are all union types as well that specify the Sub-type. It was originally developed to
classify files dropped into the browser via the HTML5 Drag and Drop api.

This library ATM provides only an incomplete, somewhat arbitrary mapping of the most common
browser mime types.
See <https://code.google.com/p/chromium/codesearch#chromium/src/net/base/mime_util.cc&l=201>
for a full list of Mime types as implemented in chromium.


# Mime type

@docs MimeType


# Parsing function & toString

@docs parseMimeType, toString


# Subtypes

@docs MimeText, MimeImage, MimeAudio, MimeVideo, MimeApp

-}

import Json.Decode as Decode


{-| Models the most common image subtypes
-}
type MimeImage
    = Jpeg
    | Png
    | Gif
    | Ico
    | Svg
    | Webp
    | OtherImage String


{-| Models the most common audio subtypes
-}
type MimeAudio
    = Mp3
    | Ogg
    | Wav
    | OtherAudio String


{-| Models the most common video subtypes
-}
type MimeVideo
    = Mp4
    | Mpeg
    | Quicktime
    | Avi
    | Webm
    | OtherVideo String


{-| Models the most common text subtypes
-}
type MimeText
    = PlainText
    | Html
    | Css
    | Xml
    | Json
    | OtherText String


{-| Models the most common app subtypes
-}
type MimeApp
    = Word
    | WordXml
    | Excel
    | ExcelXml
    | PowerPoint
    | PowerPointXml
    | Pdf
    | Binary
    | OtherApp String


{-| Models the major types image, audio, video and text
with a subtype or OtherMimeType
-}
type MimeType
    = Image MimeImage
    | Audio MimeAudio
    | Video MimeVideo
    | Text MimeText
    | App MimeApp
    | OtherMimeType String


mimeTypeDecoder : Decode.Decoder MimeType
mimeTypeDecoder =
    Decode.string
    |> Decode.andThen (\mimeTypeString ->
            case parseMimeType mimeTypeString of
                Just mimeType ->
                    Decode.succeed mimeType

                Nothing ->
                    Decode.fail ("No mime type: " ++ mimeTypeString)
        )

{-| Tries to parse the Mime type from a string.

    -- normal use of a type/subtype that is modelled:
    parseMimeType "image/jpeg" == Just (Image Jpeg)

    -- use of a subtype that is not modelled ATM
    parseMimeType "image/tiff" == Just (Image OtherImage)

    -- use with an empty string
    parseMimeType "" == Nothing

    -- use with something else
    parseMimeType "bla" == Just OtherMimeType

-}
parseMimeType : String -> Maybe MimeType
parseMimeType mimeString =
    case String.toLower mimeString of
        "" ->
            Nothing

        "image/jpeg" ->
            Just <| Image Jpeg

        "image/png" ->
            Just <| Image Png

        "image/gif" ->
            Just <| Image Gif

        "image/x-icon" ->
            Just <| Image Ico

        "image/svg+xml" ->
            Just <| Image Svg

        "image/webp" ->
            Just <| Image Webp

        "audio/mp3" ->
            Just <| Audio Mp3

        "audio/mpeg" ->
            Just <| Audio Mp3

        -- firefox etc tag mp3 as audio/mpeg
        "audio/wav" ->
            Just <| Audio Wav

        "audio/ogg" ->
            Just <| Audio Ogg

        "video/mp4" ->
            Just <| Video Mp4

        "video/mpeg" ->
            Just <| Video Mpeg

        "video/quicktime" ->
            Just <| Video Quicktime

        "video/avi" ->
            Just <| Video Avi

        "video/webm" ->
            Just <| Video Webm

        "text/plain" ->
            Just <| Text PlainText

        "text/html" ->
            Just <| Text Html

        "text/css" ->
            Just <| Text Css

        "text/xml" ->
            Just <| Text Xml

        "application/json" ->
            Just <| Text Json

        "application/msword" ->
            Just <| App Word

        "application/vnd.openxmlformats-officedocument.wordprocessingml.document" ->
            Just <| App WordXml

        "application/vnd.ms-excel" ->
            Just <| App Excel

        "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" ->
            Just <| App ExcelXml

        "application/vnd.ms-powerpoint" ->
            Just <| App PowerPoint

        "application/vnd.openxmlformats-officedocument.presentationml.presentation" ->
            Just <| App PowerPointXml

        "application/pdf" ->
            Just <| App Pdf

        "application/octet-stream" ->
            Just <| App Binary

        lowerCaseMimeString ->
            if String.startsWith "image/" lowerCaseMimeString then
                Just <| Image (OtherImage (String.dropLeft (String.length "image/") lowerCaseMimeString))

            else if String.startsWith "audio/" lowerCaseMimeString then
                Just <| Audio (OtherAudio (String.dropLeft (String.length "audio/") lowerCaseMimeString))

            else if String.startsWith "video/" lowerCaseMimeString then
                Just <| Video (OtherVideo (String.dropLeft (String.length "video/") lowerCaseMimeString))

            else if String.startsWith "text/" lowerCaseMimeString then
                Just <| Text (OtherText (String.dropLeft (String.length "text/") lowerCaseMimeString))

            else
                Just (OtherMimeType lowerCaseMimeString)


{-| Transforms a MimeType back to a string represenation.
Note that this only works properly for correctly recognized
mime types at the moment. A future version of this library
will instead store the originally parsed mime type.

    toString (Image Jpeg) == "image/jpeg"

-}
toString : MimeType -> String
toString mimeType =
    case mimeType of
        Image img ->
            case img of
                Jpeg ->
                    "image/jpeg"

                Png ->
                    "image/png"

                Gif ->
                    "image/gif"

                Ico ->
                    "image/x-icon"

                Svg ->
                    "image/svg+xml"

                Webp ->
                    "image/webp"

                OtherImage type_ ->
                    "image/" ++ type_

        Audio audio ->
            case audio of
                Mp3 ->
                    "audio/mp3"

                Wav ->
                    "audio/wav"

                Ogg ->
                    "audio/ogg"

                OtherAudio type_ ->
                    "audio/" ++ type_

        Video video ->
            case video of
                Mp4 ->
                    "video/mp4"

                Mpeg ->
                    "video/mpeg"

                Quicktime ->
                    "video/quicktime"

                Avi ->
                    "video/avi"

                Webm ->
                    "video/webm"

                OtherVideo type_ ->
                    "video/" ++ type_

        Text text ->
            case text of
                PlainText ->
                    "text/plain"

                Html ->
                    "text/html"

                Css ->
                    "text/css"

                Xml ->
                    "text/xml"

                Json ->
                    "application/json"

                OtherText type_ ->
                    "text/" ++ type_

        App app ->
            case app of
                Word ->
                    "application/msword"

                WordXml ->
                    "application/vnd.openxmlformats-officedocument.wordprocessingml.document"

                Excel ->
                    "application/vnd.ms-excel"

                ExcelXml ->
                    "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"

                PowerPoint ->
                    "application/vnd.ms-powerpoint"

                PowerPointXml ->
                    "application/vnd.openxmlformats-officedocument.presentationml.presentation"

                Pdf ->
                    "application/pdf"

                Binary ->
                    "application/octet-stream"

                OtherApp type_ ->
                    "application/" ++ type_

        OtherMimeType type_ ->
            type_