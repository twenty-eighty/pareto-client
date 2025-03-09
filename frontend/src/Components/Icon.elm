module Components.Icon exposing (Coloring(..), Icon(..), MaterialIcon(..), ParetoIcon(..), view, viewWithSize)

import Color exposing (Color)
import FeatherIcons
import Graphics
import Html.Styled as Html exposing (Html, div)
import Html.Styled.Attributes exposing (css)
import Material.Icons
import Material.Icons.Outlined
import Material.Icons.Types
import Svg as UnstyledSvg
import Tailwind.Color as TailwindColor
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw


type Icon
    = FeatherIcon FeatherIcons.Icon
    | MaterialIcon MaterialIcon Int Coloring
    | ParetoIcon ParetoIcon Int Coloring
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


type ParetoIcon
    = ParetoFollowed
    | ParetoGlobe
    | ParetoPeaceDove
    | ParetoCube


view : Icon -> Html msg
view icon =
    case icon of
        FeatherIcon featherIcon ->
            featherIcon
                |> FeatherIcons.toHtml []
                |> Html.fromUnstyled

        MaterialIcon materialIcon size coloring ->
            viewMaterialIcon materialIcon size coloring

        ParetoIcon paretoIcon size coloring ->
            viewParetoIcon paretoIcon size coloring

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

        ParetoIcon paretoIcon _ coloring ->
            viewParetoIcon paretoIcon size coloring

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


viewParetoIcon : ParetoIcon -> Int -> Coloring -> Html msg
viewParetoIcon paretoIcon size coloring =
    let
        coloredIcon =
            case coloring of
                Color color ->
                    \icon ->
                        div
                            [ css
                                [ Tw.text_color <| tailwindColorFromColor color ]
                            ]
                            [ icon ]

                Inherit ->
                    identity
    in
    svgForParetoIcon paretoIcon size
        |> coloredIcon


tailwindColorFromColor : Color -> Theme.Color
tailwindColorFromColor color =
    let
        rgba =
            Color.toRgba color

        oneToByte : Float -> Int
        oneToByte value =
            round (value * 255.0)
    in
    TailwindColor.arbitraryRgba (oneToByte rgba.red) (oneToByte rgba.green) (oneToByte rgba.blue) (rgba.alpha * 100.0)


svgForParetoIcon : ParetoIcon -> (Int -> Html msg)
svgForParetoIcon paretoIcon =
    case paretoIcon of
        ParetoCube ->
            Graphics.paretoCube

        ParetoFollowed ->
            Graphics.followedIcon

        ParetoGlobe ->
            Graphics.globeIcon

        ParetoPeaceDove ->
            Graphics.peaceDove
