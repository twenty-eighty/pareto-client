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
import Components.Icon as Icon
import Components.ModalDialog as ModalDialog
import Effect exposing (Effect)
import FeatherIcons
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Events
import I18Next exposing (Translations)
import Ports
import QRCode
import Svg.Attributes as SvgAttr
import Tailwind.Breakpoints as Bp
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw
import Translations.SharingButtonDialog as Translations
import Ui.Styles exposing (Theme)


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


init : { } -> Model
init props =
    Model
        { visible = False
        }


type Msg
    = UpdateVisible Bool
    | ShareLink SharingInfo

update :
    { msg : Msg
    , model : Model
    , toModel : Model -> model
    , toMsg : Msg -> msg
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
                            |> QRCode.toSvg [ SvgAttr.width "100px", SvgAttr.height "100px" ]
                            |> Html.fromUnstyled
                    )
                |> Result.withDefault (Html.text "")
    in
    ModalDialog.new
        { title = Translations.dialogTitle [ settings.translations ]
        , content =
            [ Html.div []
                [ qrCode
                ]
            ]
        , onClose = settings.toMsg (UpdateVisible False)
        , theme = settings.theme
        }
        |> ModalDialog.view
