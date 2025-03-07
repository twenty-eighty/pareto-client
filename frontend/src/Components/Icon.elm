module Components.Icon exposing (Coloring(..), Icon(..), MaterialIcon(..), view, viewWithSize)

import Color exposing (Color)
import FeatherIcons
import Html.Styled as Html exposing (Html, div)
import Material.Icons
import Material.Icons.Outlined
import Material.Icons.Types
import Svg as UnstyledSvg


type Icon
    = FeatherIcon FeatherIcons.Icon
    | MaterialIcon MaterialIcon Int Coloring
    | DummyIcon {}


type Coloring
    = Color Color
    | Inherit


type MaterialIcon
    = MaterialBookmark
    | MaterialBookmarkAdd
    | MaterialBookmarkAdded
    | MaterialCheck
    | MaterialFavorite
    | MaterialFavoriteBorder
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
            div [] []


viewWithSize : Int -> Icon -> Html msg
viewWithSize size icon =
    case icon of
        FeatherIcon featherIcon ->
            featherIcon
                |> FeatherIcons.withSize (toFloat size)
                |> FeatherIcons.toHtml []
                |> Html.fromUnstyled

        MaterialIcon materialIcon _ coloring ->
            viewMaterialIcon materialIcon size coloring

        DummyIcon _ ->
            div [] []


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
    svgForMaterialIcon materialIcon size materialColoring
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

        MaterialCheck ->
            Material.Icons.check

        MaterialFavorite ->
            Material.Icons.favorite

        MaterialFavoriteBorder ->
            Material.Icons.favorite_border

        MaterialOutlineBookmark ->
            Material.Icons.Outlined.bookmark

        MaterialOutlineBookmarkAdd ->
            Material.Icons.Outlined.bookmark_add

        MaterialOutlineBookmarkAdded ->
            Material.Icons.Outlined.bookmark_added
