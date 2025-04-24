module Ui.PicturePost exposing (..)

import BrowserEnv exposing (BrowserEnv)
import Html.Styled as Html exposing (Html, br, div, p, text)
import Html.Styled.Attributes as Attr exposing (css)
import Locale exposing (languageToISOCode)
import Nostr
import Nostr.Event exposing (ImageMetadata)
import Nostr.Nip68 exposing (PicturePost)
import Nostr.Profile exposing (Author(..), ProfileValidation(..), profileDisplayName)
import Nostr.Reactions exposing (Interactions)
import Nostr.Types exposing (EventId, PubKey)
import Tailwind.Utilities as Tw
import Ui.Links exposing (linkElementForAuthor)
import Ui.Profile
import Ui.Shared exposing (Actions, emptyHtml, extendedZapRelays, pubkeyRelays)
import Ui.Styles exposing (Styles, Theme, darkMode, stylesForTheme)
import Url


type alias PicturePostsViewData =
    { theme : Theme
    , browserEnv : BrowserEnv
    , nostr : Nostr.Model
    }


type alias PicturePostViewData =
    { author : Author
    }


viewPicturePost : PicturePostsViewData -> PicturePostViewData -> PicturePost -> Html msg
viewPicturePost picturePostsViewData picturePostViewData picturePost =
    let
        styles =
            stylesForTheme picturePostsViewData.theme

        ( authorText, maybeProfile, validationStatus ) =
            case picturePostViewData.author of
                AuthorPubkey pubKey ->
                    ( pubKey, Nothing, ValidationUnknown )

                AuthorProfile profile profileValidation ->
                    ( profileDisplayName profile.pubKey profile, Just profile, profileValidation )

        profileLinkElement =
            linkElementForAuthor picturePostViewData.author validationStatus
    in
    div
        [ css
            [ Tw.flex
            , Tw.flex_col
            , Tw.max_w_md
            , Tw.my_4
            , Tw.gap_2
            , Tw.border_b
            , Tw.border_color styles.color1
            , darkMode
                [ Tw.border_color styles.color1DarkMode
                ]
            ]
        , Attr.lang (picturePost.language |> Maybe.map languageToISOCode |> Maybe.withDefault "en")
        ]
        [ case maybeProfile of
            Just profile ->
                Ui.Profile.viewProfileSmall styles profile validationStatus

            Nothing ->
                emptyHtml
        , case picturePost.title of
            Just title ->
                text title

            Nothing ->
                emptyHtml
        , viewPictures picturePost.pictures
        , viewContent styles picturePost.description
        ]


viewPictures : List ImageMetadata -> Html msg
viewPictures imageMetadataList =
    case imageMetadataList of
        [] ->
            emptyHtml

        [ picture ] ->
            viewImage picture.url

        firstPicture :: pictureList ->
            viewImage firstPicture.url


viewImage : String -> Html msg
viewImage url =
    Html.img
        [ Attr.src url
        , Attr.attribute "loading" "lazy"
        , css
            [ Tw.rounded_sm
            ]
        ]
        []


viewContent : Styles msg -> String -> Html msg
viewContent styles description =
    p
        (styles.colorStyleGrayscaleText
            ++ styles.textStyleBody
            ++ [ css
                    [ Tw.mb_4
                    , Tw.overflow_hidden
                    , Tw.overflow_ellipsis
                    ]
               ]
        )
        (formattedContent description)


formattedContent : String -> List (Html msg)
formattedContent content =
    content
        |> String.split "\n"
        |> List.map
            (\line ->
                if isImageUrl line then
                    Html.img
                        [ Attr.src line
                        ]
                        []

                else
                    text line
            )
        |> List.intersperse (br [] [])


isImageUrl : String -> Bool
isImageUrl line =
    line
        |> Url.fromString
        |> Maybe.map
            (\url ->
                String.endsWith "jpeg" url.path
                    || String.endsWith "jpg" url.path
                    || String.endsWith "png" url.path
                    || String.endsWith "webp" url.path
            )
        |> Maybe.withDefault False
