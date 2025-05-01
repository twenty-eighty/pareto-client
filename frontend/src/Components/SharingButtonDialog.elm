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
import Components.Button as Button
import Components.EntryField as EntryField
import Components.Icon as Icon
import Components.ModalDialog as ModalDialog
import Json.Encode as Encode
import Json.Decode as Decode
import Effect exposing (Effect)
import FeatherIcons
import Graphics
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
import Url.Builder

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
    , hashtags : List String
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
        { state : State
        }

type State
    = Hidden
    | ShowingQrcode
    | ShowingSocials

type SocialMedia
    = Facebook
    | Twitter
    | LinkedIn
    | Reddit
    | Telegram
    | WhatsApp

init : Model
init =
    Model
        { state = Hidden
        }


type Msg
    = ShareLink SharingInfo
    | NoOp
    | UpdateState State
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
            ShareLink sharingInfo ->
                ( Model model
                , Ports.shareLink { url = sharingInfo.url, title = sharingInfo.title, text = sharingInfo.text }
                    |> Effect.sendCmd
                )

            NoOp ->
                ( Model model
                , Effect.none
                )

            UpdateState state ->
                ( Model { model | state = state }
                , Effect.none
                )

            ShowCopiedMessage ->
                ( Model { model | state = Hidden }
                , Effect.sendSharedMsg <| Shared.Msg.ShowAlert (Translations.copiedToClipboardAlertMessage [ props.browserEnv.translations ])
                )

shareSocialLink : SharingInfo -> SocialMedia -> String
shareSocialLink sharingInfo socialMedia =
    case socialMedia of
        Facebook ->
            Url.Builder.crossOrigin "https://www.facebook.com"
                [ "sharer", "sharer.php" ]
                [ Url.Builder.string "u" sharingInfo.url ]

        Twitter ->
            Url.Builder.crossOrigin "https://twitter.com"
                [ "intent", "tweet" ]
                [ Url.Builder.string "url" sharingInfo.url
                , Url.Builder.string "text" (sharingInfo.title ++ ": " ++ sharingInfo.text)
                , Url.Builder.string "hashtags" (String.join "," sharingInfo.hashtags)
                ]

        LinkedIn ->
            Url.Builder.crossOrigin "https://www.linkedin.com"
                [ "shareArticle" ]
                [ Url.Builder.string "mini" "true"
                , Url.Builder.string "url" sharingInfo.url
                ]

        Reddit ->
            Url.Builder.crossOrigin "https://www.reddit.com"
                [ "submit" ]
                [ Url.Builder.string "url" sharingInfo.url
                , Url.Builder.string "title" (sharingInfo.title ++ ": " ++ sharingInfo.text)
                ]

        Telegram ->
            Url.Builder.crossOrigin "https://t.me"
                [ "share", "url" ]
                [ Url.Builder.string "url" sharingInfo.url
                , Url.Builder.string "text" (sharingInfo.title ++ ": " ++ sharingInfo.text)
                ]

        WhatsApp ->
            Url.Builder.crossOrigin "https://wa.me"
                [ "?text=" ]
                [ Url.Builder.string "text" (sharingInfoToText sharingInfo)
                ]

sharingInfoToText : SharingInfo -> String
sharingInfoToText sharingInfo =
    sharingInfo.url
        ++ "\n"
        ++ sharingInfo.title
        ++ "\n"
        ++ sharingInfo.text

view : SharingButtonDialog msg -> Html msg
view (Settings settings) =
    let
        (Model model) =
            settings.model

        dialog =
            case model.state of
                Hidden ->
                    Html.text ""

                ShowingQrcode ->
                    viewDialog (Settings settings)

                ShowingSocials ->
                    viewSocialDialog (Settings settings)

        buttonMsg =
            if BrowserEnv.isNativeSharingAvailable settings.browserEnv then
                ShareLink settings.sharingInfo
            else
                UpdateState ShowingQrcode
    
        button =
            Html.button
                [ Attr.type_ "button"
                , Events.onClick (settings.toMsg buttonMsg)
                ]
                [ Icon.FeatherIcon FeatherIcons.share2
                    |> Icon.viewWithSize 16
                ]
    in
    if BrowserEnv.isNativeSharingAvailable settings.browserEnv then
        button
    else
        Html.div
            [ Attr.css
                [ Tw.flex
                , Tw.flex_col
                , Tw.gap_2
                ]
            ]
            [ button
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
        , buttons =
            [ Button.new
                { label = Translations.showSocialDialogButtonTitle [ settings.translations ]
                , onClick = Just (settings.toMsg (UpdateState ShowingSocials))
                , theme = settings.theme
                }
                |> Button.view
            ]
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
        , onClose = settings.toMsg (UpdateState Hidden)
        , theme = settings.theme
        }
        |> ModalDialog.view


viewSocialDialog : SharingButtonDialog msg -> Html msg
viewSocialDialog (Settings settings) =
    ModalDialog.new
        { title = Translations.dialogTitle [ settings.translations ]
        , buttons =
            [ Button.new
                { label = Translations.backButtonTitle [ settings.translations ]
                , onClick = Just (settings.toMsg (UpdateState ShowingQrcode))
                , theme = settings.theme
                }
                |> Button.view
            ]
        , content =
            [ Html.div
                [ Attr.css
                    [ Tw.flex
                    , Tw.flex_col
                    , Tw.items_center
                    , Tw.gap_2
                    ]
                ]
                [ Button.new
                    { label = Translations.twitterButtonTitle [ settings.translations ]
                    , onClick = Nothing
                    , theme = settings.theme
                    }
                    |> Button.withNewTabLink (shareSocialLink settings.sharingInfo Twitter)
                    |> Button.withIconLeft (Icon.FeatherIcon FeatherIcons.twitter)
                    |> Button.withTypeSecondary
                    |> Button.view
                , Button.new
                    { label = Translations.facebookButtonTitle [ settings.translations ]
                    , onClick = Nothing
                    , theme = settings.theme
                    }
                    |> Button.withNewTabLink (shareSocialLink settings.sharingInfo Facebook)
                    |> Button.withIconLeft (Icon.FeatherIcon FeatherIcons.facebook)
                    |> Button.withTypeSecondary
                    |> Button.view
                , Button.new
                    { label = Translations.linkedinButtonTitle [ settings.translations ]
                    , onClick = Nothing
                    , theme = settings.theme
                    }
                    |> Button.withNewTabLink (shareSocialLink settings.sharingInfo LinkedIn)
                    |> Button.withIconLeft (Icon.FeatherIcon FeatherIcons.linkedin)
                    |> Button.withTypeSecondary
                    |> Button.view
                , Button.new
                    { label = Translations.redditButtonTitle [ settings.translations ]
                    , onClick = Nothing
                    , theme = settings.theme
                    }
                    |> Button.withNewTabLink (shareSocialLink settings.sharingInfo Reddit)
                    |> Button.withContentLeft (Graphics.redditIcon 20)
                    |> Button.withTypeSecondary
                    |> Button.view
                , Button.new
                    { label = Translations.telegramButtonTitle [ settings.translations ]
                    , onClick = Nothing
                    , theme = settings.theme
                    }
                    |> Button.withNewTabLink (shareSocialLink settings.sharingInfo Telegram)
                    |> Button.withContentLeft (Graphics.telegramIcon 20)
                    |> Button.withTypeSecondary
                    |> Button.view
                , Button.new
                    { label = Translations.whatsappButtonTitle [ settings.translations ]
                    , onClick = Nothing
                    , theme = settings.theme
                    }
                    |> Button.withNewTabLink (shareSocialLink settings.sharingInfo WhatsApp)
                    |> Button.withContentLeft (Graphics.whatsappIcon 20)
                    |> Button.withTypeSecondary
                    |> Button.view
                ]
            ]
        , onClose = settings.toMsg (UpdateState Hidden)
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