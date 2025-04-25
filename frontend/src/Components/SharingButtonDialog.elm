module Components.SharingButtonDialog exposing
    ( Model
    , Msg
    , SharingButtonDialog
    , SharingInfo
    , init
    , new
    , update
    , view
    )

import BrowserEnv exposing (BrowserEnv)
import Components.EntryField as EntryField
import Components.Icon as Icon
import Components.ModalDialog as ModalDialog
import Json.Encode as Encode
import Json.Decode as Decode
import Effect exposing (Effect)
import FeatherIcons
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Events
import I18Next exposing (Translations)
import Ports
import QRCode
import SHA256
import Shared.Msg
import Svg.Attributes as SvgAttr
import Tailwind.Utilities as Tw
import Translations.SharingButtonDialog as Translations
import Ui.Styles exposing (Theme, stylesForTheme)
import Ui.Styles exposing (darkMode)

type SharingButtonDialog msg
    = Settings
        { model : Model
        , toMsg : Msg -> msg
        , browserEnv : BrowserEnv
        , sharingInfo : SharingInfo
        , translations : Translations
        , theme : Theme
        }

type alias SharingInfo =
    { url : String
    , title : String
    , text : String
    }

new :
    { model : Model
    , toMsg : Msg -> msg
    , browserEnv : BrowserEnv
    , sharingInfo : SharingInfo
    , translations : Translations
    , theme : Theme
    }
    -> SharingButtonDialog msg
new props =
    Settings
        { model = props.model
        , toMsg = props.toMsg
        , browserEnv = props.browserEnv
        , sharingInfo = props.sharingInfo
        , translations = props.translations
        , theme = props.theme
        }


type Model
    = Model
        { visible : Bool
        }


init : Model
init =
    Model
        { visible = False
        }


type Msg
    = UpdateVisible Bool
    | ShareLink SharingInfo
    | NoOp
    | ShowCopiedMessage

update :
    { msg : Msg
    , model : Model
    , toModel : Model -> model
    , toMsg : Msg -> msg
    , browserEnv : BrowserEnv
    }
    -> ( model, Effect msg )
update props =
    let
        (Model model) =
            props.model

        toParentModel : ( Model, Effect msg ) -> ( model, Effect msg )
        toParentModel ( innerModel, effect ) =
            ( props.toModel innerModel
            , effect
            )
    in
    toParentModel <|
        case props.msg of
            UpdateVisible visible ->
                ( Model { model | visible = visible }
                , Effect.none
                )

            ShareLink sharingInfo ->
                ( Model model
                , Ports.shareLink sharingInfo
                    |> Effect.sendCmd
                )

            NoOp ->
                ( Model model
                , Effect.none
                )

            ShowCopiedMessage ->
                ( Model { model | visible = False }
                , Effect.sendSharedMsg <| Shared.Msg.ShowAlert (Translations.copiedToClipboardAlertMessage [ props.browserEnv.translations ])
                )

view : SharingButtonDialog msg -> Html msg
view (Settings settings) =
    let
        (Model model) =
            settings.model

        dialog =
            if model.visible then
                viewDialog (Settings settings)
            else
                Html.text ""
    in
    if BrowserEnv.isNativeSharingAvailable settings.browserEnv then
        Html.button
            [ Attr.type_ "button"
            , Events.onClick (settings.toMsg (ShareLink settings.sharingInfo))
            ]
            [ Icon.FeatherIcon FeatherIcons.share
                |> Icon.view
            ]
    else
        Html.div
            [ Attr.css
                [ Tw.flex
                , Tw.flex_col
                , Tw.gap_2
                ]
            ]
            [ Html.button
                [ Attr.type_ "button"
                , Events.onClick (settings.toMsg (UpdateVisible True))
                ]
                [ Icon.FeatherIcon FeatherIcons.share
                    |> Icon.view
                ]
            , dialog
            ]

viewDialog : SharingButtonDialog msg -> Html msg
viewDialog (Settings settings) =
    let
        qrCode =
            settings.sharingInfo.url
                |> QRCode.fromString 
                |> Result.map
                    (\qrcode ->
                        qrcode
                            |> QRCode.toSvg [ SvgAttr.width "200px", SvgAttr.height "200px" ]
                            |> Html.fromUnstyled
                    )
                |> Result.withDefault (Html.text "")
    in
    ModalDialog.new
        { title = Translations.dialogTitle [ settings.translations ]
        , content =
            [ Html.div [ Attr.css [ Tw.flex, Tw.flex_col, Tw.gap_2 ] ]
                [ Html.div [ Attr.css [ Tw.flex, Tw.flex_row, Tw.items_end ] ]
                    [ qrCode
{-
                    , copyButton settings.theme
                        settings.sharingInfo.url
                        (settings.sharingInfo.url
                            |> SHA256.fromString
                            |> SHA256.toHex
                        )
                        |> Html.map settings.toMsg
-}
                    ]
{-
                , EntryField.new
                    { value = settings.sharingInfo.url
                    , onInput = \_ -> NoOp
                    , theme = settings.theme
                    }
                    |> EntryField.withReadOnly
                    |> EntryField.view
                    |> Html.map settings.toMsg
-}
                ]
            ]
        , onClose = settings.toMsg (UpdateVisible False)
        , theme = settings.theme
        }
        |> ModalDialog.view


copyButton : Theme -> String -> String -> Html Msg 
copyButton theme copyText uniqueId =
    let
        styles =
            stylesForTheme theme
        elementId =
            "copy-to-clipboard-" ++ uniqueId
    in
    Html.div
        [ Attr.css
            [ Tw.p_3
            , Tw.rounded_md
            ]
        ]
        [ Html.div
            [ Attr.css
                [ Tw.flex
                , Tw.flex_row
                , Tw.cursor_pointer
                , Tw.text_color styles.color4
                , darkMode [ Tw.text_color styles.color4DarkMode ]
                ]
            , Attr.id elementId
            ]
            [ Icon.FeatherIcon FeatherIcons.clipboard
                |> Icon.viewWithSize 50
            ]
        , Html.node "js-clipboard-component"
            [ Attr.property "buttonId" (Encode.string elementId)
            , Attr.property "copyContent" (Encode.string copyText)
            , Events.on "copiedToClipboard" (Decode.succeed ShowCopiedMessage)
            ]
            []
        ]