module Ui.ShortNote exposing (..)

import BrowserEnv exposing (BrowserEnv)
import Html.Styled as Html exposing (Html, br, div, p)
import Html.Styled.Attributes as Attr exposing (css)
import Nostr
import Nostr.Nip27 as Nip27 exposing (GetProfileFunction)
import Nostr.Profile exposing (Author(..), ProfileValidation(..), profileDisplayName)
import Nostr.Reactions exposing (Interactions)
import Nostr.ShortNote exposing (ShortNote)
import Nostr.Types exposing (EventId, PubKey)
import Tailwind.Utilities as Tw
import Ui.Interactions exposing (Actions)
import Ui.Profile
import Ui.Shared exposing (emptyHtml)
import Ui.Styles exposing (Styles, Theme, darkMode, stylesForTheme)
import Url


type alias ShortNotesViewData msg =
    { theme : Theme
    , browserEnv : BrowserEnv
    , nostr : Nostr.Model
    , userPubKey : Maybe PubKey
    , onBookmark : Maybe ( EventId -> msg, EventId -> msg ) -- msgs for adding/removing a bookmark
    }


type alias ShortNoteViewData msg =
    { author : Author
    , actions : Actions msg
    , interactions : Interactions
    }


viewShortNote : ShortNotesViewData msg -> ShortNoteViewData msg -> ShortNote -> Html msg
viewShortNote shortNotesViewData shortNoteViewData shortNote =
    let
        styles =
            stylesForTheme shortNotesViewData.theme

        ( _, maybeProfile, validationStatus ) =
            case shortNoteViewData.author of
                AuthorPubkey pubKey ->
                    ( pubKey, Nothing, ValidationUnknown )

                AuthorProfile profile profileValidation ->
                    ( profileDisplayName profile.pubKey profile, Just profile, profileValidation )

        followLinks =
            Nostr.isAuthor shortNotesViewData.nostr shortNote.pubKey
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
        ]
        [ case maybeProfile of
            Just profile ->
                Ui.Profile.viewProfileSmall styles followLinks profile validationStatus

            Nothing ->
                emptyHtml
        , viewContent styles (Nostr.getProfile shortNotesViewData.nostr) shortNote.content
        ]


viewContent : Styles msg -> GetProfileFunction -> Maybe String -> Html msg
viewContent styles fnGetProfile maybeDescription =
    case maybeDescription of
        Just description ->
            p
                (styles.colorStyleGrayscaleText
                    ++ styles.textStyleBody
                    ++ [ css
                            [ Tw.mb_4
                            ]
                       ]
                )
                (formattedContent styles fnGetProfile description)

        Nothing ->
            emptyHtml


formattedContent : Styles msg -> GetProfileFunction -> String -> List (Html msg)
formattedContent styles fnGetProfile content =
    content
        |> String.split "\n"
        |> List.map
            (\line ->
                if isImageUrl (String.trim line) then
                    Html.img
                        [ Attr.src line
                        ]
                        []

                else
                    Nip27.subsituteNostrLinks styles fnGetProfile line
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
