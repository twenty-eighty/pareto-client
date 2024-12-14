module Components.Icon exposing (Coloring(..), Icon(..), MaterialIcon(..), view)

import Color exposing (Color)
import FeatherIcons
import Html.Styled as Html exposing (Html, div)
import Svg as UnstyledSvg
import Material.Icons
import Material.Icons.Outlined
import Material.Icons.Types

type Icon
    = FeatherIcon FeatherIcons.Icon
    | MaterialIcon MaterialIcon Int Coloring
    | DummyIcon
        {
        }

type Coloring
    = Color Color
    | Inherit


type MaterialIcon
    = MaterialBookmark
    | MaterialBookmarkAdd
    | MaterialBookmarkAdded
    | MaterialOutlineBookmark
    | MaterialOutlineBookmarkAdd
    | MaterialOutlineBookmarkAdded

view : Icon -> Html msg
view icon =
    case icon of
        FeatherIcon featherIcon ->
            featherIcon
            |> FeatherIcons.toHtml []
            |> Html.fromUnstyled

        MaterialIcon materialIcon size coloring ->
            viewMaterialIcon materialIcon size coloring

        DummyIcon _ ->
            div [][]


viewMaterialIcon : MaterialIcon -> Int -> Coloring -> Html msg
viewMaterialIcon materialIcon size coloring =
    let
        materialColoring =
            case coloring of
                Color color ->
                    Material.Icons.Types.Color color

                Inherit ->
                    Material.Icons.Types.Inherit
    in
    (svgForMaterialIcon materialIcon) size materialColoring
    |> Html.fromUnstyled

svgForMaterialIcon : MaterialIcon -> (Int -> Material.Icons.Types.Coloring -> UnstyledSvg.Svg msg)
svgForMaterialIcon materialIcon =
    case materialIcon of
        MaterialBookmark ->
            Material.Icons.bookmark

        MaterialBookmarkAdd ->
            Material.Icons.bookmark_add

        MaterialBookmarkAdded ->
            Material.Icons.bookmark_added

        MaterialOutlineBookmark ->
            Material.Icons.Outlined.bookmark

        MaterialOutlineBookmarkAdd ->
            Material.Icons.Outlined.bookmark_add

        MaterialOutlineBookmarkAdded ->
            Material.Icons.Outlined.bookmark_added