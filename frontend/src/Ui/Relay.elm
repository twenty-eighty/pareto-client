module Ui.Relay exposing (..)

import Graphics
import Html.Styled as Html exposing (Html, div, object)
import Html.Styled.Attributes as Attr
import Nostr.Relay exposing (Relay)
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw


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
                [ Tw.text_color Theme.white
                , Tw.bg_color Theme.slate_300
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
