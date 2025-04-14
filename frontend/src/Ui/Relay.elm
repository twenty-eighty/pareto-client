module Ui.Relay exposing (..)

import Graphics
import Html.Styled as Html exposing (Html, div, object)
import Html.Styled.Attributes as Attr
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw
import Ui.Styles exposing (darkMode)


viewRelayImage : String -> Html msg
viewRelayImage imageUrl =
    object
        [ Attr.attribute "data" imageUrl
        , Attr.css
            [ Tw.h_5
            , Tw.w_5
            , Tw.object_cover
            , Tw.overflow_clip
            ]
        ]
        [ div
            [ Attr.css
                [ Tw.text_color Theme.black
                , Tw.bg_color Theme.white
                , darkMode [ Tw.text_color Theme.white, Tw.bg_color Theme.black ]
                , Tw.items_center
                , Tw.bg_clip_border
                , Tw.rounded_full
                , Tw.h_5
                , Tw.w_5
                , Tw.flex
                , Tw.justify_center
                ]
            ]
            [ Graphics.defaultRelayImage 14
            ]
        ]
