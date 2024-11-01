module TailwindExtensions exposing (background, bp_2xl, bp_dark, bp_focus, bp_focus_visible, bp_placeholder, bp_xsl, cursorPointer, text_color, width_pix)

import Css
import Css.Media
import Html.Styled as Html
import Html.Styled.Attributes as Attr exposing (css)
import Tailwind.Utilities as Tw


bp_dark : List Css.Style -> Css.Style
bp_dark =
    Css.Media.withMediaQuery [ "(prefers-color-scheme: dark)" ]


bp_focus : List Css.Style -> Css.Style
bp_focus =
    Css.pseudoClass "focus"


bp_focus_visible : List Css.Style -> Css.Style
bp_focus_visible =
    Css.pseudoClass "focus-visible"


bp_placeholder : List Css.Style -> Css.Style
bp_placeholder =
    Css.pseudoClass "placeholder"


bp_xsl : List Css.Style -> Css.Style
bp_xsl =
    Css.Media.withMediaQuery [ "(max-width: 639px)" ]


bp_2xl : List Css.Style -> Css.Style
bp_2xl =
    Css.Media.withMediaQuery [ "(min-width: 1536px)" ]


text_color : String -> Css.Style
text_color color =
    Css.property "color" color


background : String -> Css.Style
background color =
    Css.property "background-color" color


cursorPointer : Css.Style
cursorPointer =
    Css.property "cursor" "pointer"


width_pix : Int -> Css.Style
width_pix pixels =
    Css.property "width" (String.fromInt pixels ++ "px")
