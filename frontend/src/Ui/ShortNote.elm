module Ui.ShortNote exposing (..)

import BrowserEnv exposing (BrowserEnv)
import Dict exposing (Dict)
import Html.Styled as Html exposing (Html, a, article, aside, br, button, div, h1, h3, h4, img, main_, p, span, text)
import Html.Styled.Attributes as Attr exposing (class, css, href)
import Html.Styled.Events as Events exposing (..)
import Nostr.Profile exposing (Profile, ProfileValidation(..))
import Nostr.ShortNote exposing (ShortNote)
import Nostr.Types exposing (PubKey)
import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw
import Tailwind.Theme as Theme
import Ui.Profile exposing (defaultProfileImage, profileDisplayName, validationIcon)
import Ui.Styles exposing (fontFamilyUnbounded, fontFamilyInter)

viewShortNote : BrowserEnv -> Dict PubKey Profile -> ShortNote -> Html msg
viewShortNote browserEnv profiles shortNote =
    div
        [ css
            [ Tw.flex
            , Tw.items_center
            , Tw.justify_center
            , Tw.min_h_screen
            , Tw.bg_color Theme.gray_100
            ]
        ]
        [ div
            [ css
                [ Tw.bg_color Theme.white
                , Tw.p_6
                , Tw.rounded_lg
                , Tw.shadow_lg
                , Tw.max_w_3xl
                , Tw.space_y_2
                ]
            ]
            [ viewContent shortNote.content
            ]
        ]

viewContent : Maybe String -> Html msg
viewContent maybeDescription =
    case maybeDescription of
        Just description ->
            p
                [ css
                    [ Tw.text_color Theme.gray_600
                    , Tw.text_sm
                    , Tw.mb_4
                    ]
                ]
                ( formattedContent description )
        Nothing ->
            div [][]


formattedContent : String -> List (Html msg)
formattedContent content =
    content
    |> String.split "\n"
    |> List.map (\line -> text line) 
    |> List.intersperse (br [][])