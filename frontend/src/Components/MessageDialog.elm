module Components.MessageDialog exposing
    ( MessageDialog, new
    , ButtonStyle(..)
    , view
    )

import Components.Button as Button
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, a, article, aside, button, div, h2, h3, h4, img, input, label, main_, p, span, strong, text)
import Html.Styled.Attributes as Attr exposing (class, classList, css, disabled, href, type_)
import Html.Styled.Events as Events exposing (..)
import Tailwind.Utilities as Tw
import Translations.MessageDialog as Translations
import Ui.Shared exposing (modalDialog)
import Ui.Styles exposing (Styles, Theme)

type MessageDialog identifier msg
     = Settings
        { onClick : identifier -> msg
        , onClose : msg
        , title : String
        , content : Html msg
        , buttons : List (ButtonData identifier)
        , theme : Theme
        }

type alias ButtonData identifier =
    { style : ButtonStyle
    , title : String
    , identifier : identifier
    }

type ButtonStyle
    = PrimaryButton
    | SecondaryButton



new :
    { onClick : identifier -> msg
    , onClose : msg
    , title : String
    , content : Html msg
    , buttons : List (ButtonData identifier)
    , theme : Theme
    }
    -> MessageDialog identifier msg
new props =
    Settings
        { onClick = props.onClick
        , onClose = props.onClose
        , title = props.title
        , content = props.content
        , buttons = props.buttons
        , theme = props.theme
        }


view : MessageDialog identifier msg -> Html msg
view (Settings settings) =
    let
        buttons =
            settings.buttons
            |> List.map (buttonFromDescription settings.theme settings.onClick)
            |> div []

        content =
            [
            div
                [ css
                    [ Tw.flex
                    , Tw.flex_col
                    , Tw.gap_2
                    , Tw.mt_4
                    ]
                ]
                [ settings.content
                , buttons
                ]
            ]
    in
    modalDialog
        settings.title
        content
        settings.onClose

buttonFromDescription : Theme -> (identifier -> msg) -> ButtonData identifier -> Html msg
buttonFromDescription theme onClick buttonData =
    case buttonData.style of
        PrimaryButton ->
            Button.new
                { label = buttonData.title
                , onClick = Just <| onClick buttonData.identifier
                , theme = theme
                }
                |> Button.withTypePrimary
                |> Button.view

        SecondaryButton ->
            Button.new
                { label = buttonData.title
                , onClick = Just <| onClick buttonData.identifier
                , theme = theme
                }
                |> Button.withTypeSecondary
                |> Button.view
