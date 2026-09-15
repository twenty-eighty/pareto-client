module Components.AppUpdateBanner exposing (view)

import Components.Button as Button
import Html.Styled as Html exposing (Html, div, span, text)
import Html.Styled.Attributes exposing (attribute, css)
import I18Next
import Tailwind.Utilities as Tw
import Translations.AppUpdate as Translations
import Ui.Shared exposing (emptyHtml)
import Ui.Styles exposing (Theme, print, stylesForTheme)


view :
    { theme : Theme
    , translations : I18Next.Translations
    , visible : Bool
    , onReload : msg
    }
    -> Html msg
view props =
    if not props.visible then
        emptyHtml

    else
        let
            styles =
                stylesForTheme props.theme
        in
        div
            (styles.colorStyleBackground
                ++ styles.colorStyleGrayscaleText
                ++ styles.colorStyleBorders
                ++ [ attribute "role" "status"
                   , attribute "aria-live" "polite"
                   , css
                        [ Tw.fixed
                        , Tw.top_0
                        , Tw.left_0
                        , Tw.right_0
                        , Tw.z_50
                        , Tw.flex
                        , Tw.flex_row
                        , Tw.items_center
                        , Tw.justify_center
                        , Tw.gap_4
                        , Tw.border_b
                        , Tw.px_4
                        , Tw.py_2
                        , print [ Tw.hidden ]
                        ]
                   ]
            )
            [ span [] [ text <| Translations.newVersionAvailable [ props.translations ] ]
            , Button.new
                { label = Translations.reloadButton [ props.translations ]
                , onClick = Just props.onReload
                , theme = props.theme
                }
                |> Button.withTypePrimary
                |> Button.withSizeSmall
                |> Button.view
            ]
