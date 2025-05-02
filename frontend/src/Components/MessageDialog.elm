module Components.MessageDialog exposing
    ( ButtonStyle(..)
    , MessageDialog
    , new
    , view
    )

import Components.Button as Button
import Components.ModalDialog as ModalDialog
import Html.Styled as Html exposing (Html)
import Ui.Styles exposing (Theme)


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
    ModalDialog.new
        { title = settings.title
        , content =
            [ settings.content
            ]
        , onClose = settings.onClose
        , theme = settings.theme
        , buttons =
            settings.buttons
                |> List.map (buttonFromDescription settings.theme settings.onClick)
        }
        |> ModalDialog.view

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
