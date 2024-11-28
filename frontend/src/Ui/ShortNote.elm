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
import Ui.Styles exposing (Styles)

viewShortNote : Styles msg -> BrowserEnv -> Dict PubKey Profile -> ShortNote -> Html msg
viewShortNote styles browserEnv profiles shortNote =
    div
        [ css
            [ Tw.flex
            , Tw.justify_center
            , Tw.min_h_screen
            ]
        ]
        [ div
            [ css
                [ Tw.p_6
                , Tw.rounded_lg
                , Tw.shadow_lg
                , Tw.max_w_3xl
                ]
            ]
            [ viewContent styles shortNote.content
            ]
        ]

viewContent : Styles msg -> Maybe String -> Html msg
viewContent styles maybeDescription =
    case maybeDescription of
        Just description ->
            p
                (styles.colorStyleGrayscaleText ++ styles.textStyleBody ++
                [ css
                    [ Tw.mb_4
                    ]
                ])
                ( formattedContent description )

        Nothing ->
            div [][]


formattedContent : String -> List (Html msg)
formattedContent content =
    content
    |> String.split "\n"
    |> List.map (\line -> text line) 
    |> List.intersperse (br [][])