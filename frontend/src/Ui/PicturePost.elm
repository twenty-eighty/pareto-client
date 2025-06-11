module Ui.PicturePost exposing (..)

import BrowserEnv exposing (BrowserEnv)
import Components.InteractionButton as InteractionButton
import Components.Interactions as Interactions
import Dict exposing (Dict)
import Html.Styled as Html exposing (Html, br, div, p, text)
import Html.Styled.Attributes as Attr exposing (css)
import Locale exposing (languageToISOCode)
import Nostr
import Nostr.Event exposing (ImageMetadata, Kind(..), numberForKind)
import Nostr.Nip19 as Nip19 exposing (NIP19Type(..))
import Nostr.Nip68 exposing (PicturePost)
import Nostr.Profile exposing (Author(..), ProfileValidation(..), profileDisplayName)
import Nostr.Types exposing (EventId, LoginStatus)
import Set
import Tailwind.Utilities as Tw
import Ui.Links
import Ui.Profile
import Ui.Shared exposing (emptyHtml)
import Ui.Styles exposing (Styles, Theme, darkMode, stylesForTheme)
import Url


type alias PicturePostsViewData =
    { browserEnv : BrowserEnv
    , nostr : Nostr.Model
    , interactions : Dict EventId Interactions.Model
    , loginStatus : LoginStatus
    , theme : Theme
    }


type alias PicturePostViewData msg =
    { author : Author
    , toInteractionsMsg : Interactions.Msg msg -> msg
    }


viewPicturePost : PicturePostsViewData -> PicturePostViewData msg -> PicturePost -> Html msg
viewPicturePost picturePostsViewData picturePostViewData picturePost =
    let
        styles =
            stylesForTheme picturePostsViewData.theme

        ( _, maybeProfile, validationStatus ) =
            case picturePostViewData.author of
                AuthorPubkey pubKey ->
                    ( pubKey, Nothing, ValidationUnknown )

                AuthorProfile profile profileValidation ->
                    ( profileDisplayName profile.pubKey profile, Just profile, profileValidation )

        viewInteractions =
            let
                shareButtonElement =
                    Nip19.NEvent { id = picturePost.id, author = Just picturePost.pubKey, kind = Just (KindPicture |> numberForKind), relays = picturePost.relays |> Maybe.withDefault [] }
                    |> Ui.Links.linkToNJump
                    |> Maybe.map (\url ->
                        [ Interactions.ShareButtonElement
                            { url = url
                            , title = picturePost.title |> Maybe.withDefault ""
                            , text = picturePost.description
                            , hashtags = []
                            }
                        ]
                    )
                    |> Maybe.withDefault []
            in
            Interactions.new
                { browserEnv = picturePostsViewData.browserEnv
                , model = Dict.get picturePost.id picturePostsViewData.interactions
                , toMsg = picturePostViewData.toInteractionsMsg
                , theme = picturePostsViewData.theme
                , interactionObject = InteractionButton.PicturePost picturePost.id picturePost.pubKey
                , nostr = picturePostsViewData.nostr
                , loginStatus = picturePostsViewData.loginStatus
                }
                |> Interactions.withInteractionElements
                    (
                    [ Interactions.LikeButtonElement
                    , Interactions.RepostButtonElement
                    , Interactions.ZapButtonElement "0" (picturePost.relays |> Maybe.map Set.fromList |> Maybe.withDefault Set.empty)
                    ] ++ shareButtonElement
                    )
                |> Interactions.view

        followLinks =
            Nostr.isAuthor picturePostsViewData.nostr picturePost.pubKey
    in
    div
        [ css
            [ Tw.flex
            , Tw.flex_col
            , Tw.max_w_md
            , Tw.my_6
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
                Ui.Profile.viewProfileSmall styles followLinks profile validationStatus

            Nothing ->
                emptyHtml
        , case picturePost.title of
            Just title ->
                text title

            Nothing ->
                emptyHtml
        , viewPictures picturePost.pictures
        , viewContent styles picturePost.description
        , viewInteractions
        ]


viewPictures : List ImageMetadata -> Html msg
viewPictures imageMetadataList =
    case imageMetadataList of
        [] ->
            emptyHtml

        [ picture ] ->
            viewImage picture.url

        -- TODO: show additional pictures in a gallery
        firstPicture :: _ ->
            viewImage firstPicture.url


viewImage : String -> Html msg
viewImage url =
    Html.img
        [ Attr.src <| Ui.Links.scaledImageLink 450 url
        , Attr.attribute "loading" "lazy"
        , css
            [ Tw.rounded_sm
            ]
        ]
        []


viewContent : Styles msg -> String -> Html msg
viewContent styles description =
    if description /= "" then
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
    else
        emptyHtml


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
