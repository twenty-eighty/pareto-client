module Components.Icon exposing (Icon(..), view)

import FeatherIcons
import Html.Styled as Html exposing (Html, div)

type Icon
    = FeatherIcon FeatherIcons.Icon
    | DummyIcon
        {
        }

view : Icon -> Html msg
view icon =
    case icon of
        FeatherIcon featherIcon ->
            featherIcon
            |> FeatherIcons.toHtml []
            |> Html.fromUnstyled

        DummyIcon _ ->
            div [][]