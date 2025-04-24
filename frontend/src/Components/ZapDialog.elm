module Components.ZapDialog exposing (Model, Msg, Recipient, ZapDialog, hide, init, new, show, update, view)

import BrowserEnv exposing (BrowserEnv)
import Components.Button as Button
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, div, ul)
import Html.Styled.Attributes as Attr exposing (css)
import Http
import Nostr
import Nostr.Send exposing (SendRequest(..))
import Nostr.Types exposing (PubKey, RelayUrl)
import Nostr.Zaps exposing (Lud16, PayRequest, fetchPayRequest)
import Shared.Model exposing (Model)
import Shared.Msg exposing (Msg)
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw
import Translations.ZapDialog as Translations
import Ui.Shared exposing (emptyHtml)
import Ui.Styles exposing (Theme)


type Msg msg
    = CloseDialog
    | ConfigureRelaysClicked
    | PublishClicked (List RelayUrl -> msg)
    | ReceivedPayRequest (Result Http.Error PayRequest)


type Model msg
    = Model
        { state : DialogState
        }


type DialogState
    = DialogHidden
    | ZapAmountSelection ZapAmountSelectionData


type alias Recipient =
    { imageUrl : String
    , name : String
    , part : Int
    , lud16 : Lud16
    }


type alias ZapAmountSelectionData =
    { amount : Int
    , comment : String
    , recipients : List Recipient
    }


type ZapDialog msg
    = Settings
        { model : Model msg
        , toMsg : Msg msg -> msg
        , onPublish : List RelayUrl -> msg
        , nostr : Nostr.Model
        , pubKey : PubKey
        , browserEnv : BrowserEnv
        , theme : Theme
        }


new :
    { model : Model msg
    , toMsg : Msg msg -> msg
    , onPublish : List RelayUrl -> msg
    , nostr : Nostr.Model
    , pubKey : PubKey
    , browserEnv : BrowserEnv
    , theme : Theme
    }
    -> ZapDialog msg
new props =
    Settings
        { model = props.model
        , toMsg = props.toMsg
        , onPublish = props.onPublish
        , nostr = props.nostr
        , pubKey = props.pubKey
        , browserEnv = props.browserEnv
        , theme = props.theme
        }


init : {} -> Model msg
init props =
    Model
        { state = DialogHidden
        }


show : Model msg -> (Msg msg -> msg) -> List Recipient -> ( Model msg, Effect msg )
show (Model model) toMsg recipients =
    ( Model { model | state = ZapAmountSelection { amount = 21, comment = "", recipients = recipients } }
    , requestPayRequests recipients
        |> Effect.sendCmd
        |> Effect.map toMsg
    )


requestPayRequests : List Recipient -> Cmd (Msg msg)
requestPayRequests recipients =
    recipients
        |> List.map
            (\recipient ->
                fetchPayRequest ReceivedPayRequest recipient.lud16
            )
        |> Cmd.batch


hide : Model msg -> Model msg
hide (Model model) =
    Model { model | state = DialogHidden }


update :
    { msg : Msg msg
    , model : Model msg
    , toModel : Model msg -> model
    , toMsg : Msg msg -> msg
    , nostr : Nostr.Model
    }
    -> ( model, Effect msg )
update props =
    let
        (Model model) =
            props.model

        toParentModel : ( Model msg, Effect msg ) -> ( model, Effect msg )
        toParentModel ( innerModel, effect ) =
            ( props.toModel innerModel
            , effect
            )
    in
    toParentModel <|
        case props.msg of
            CloseDialog ->
                ( Model { model | state = DialogHidden }
                , Effect.none
                )

            ConfigureRelaysClicked ->
                ( Model model
                , Effect.none
                )

            PublishClicked msg ->
                ( Model model
                , Effect.none
                )

            ReceivedPayRequest (Ok payRequest) ->
                ( Model model
                , Effect.none
                )

            ReceivedPayRequest (Err httpError) ->
                ( Model model
                , Effect.none
                )


view : ZapDialog msg -> Html msg
view dialog =
    let
        (Settings settings) =
            dialog

        (Model model) =
            settings.model
    in
    case model.state of
        DialogHidden ->
            emptyHtml

        ZapAmountSelection zapAmountSelectionData ->
            Ui.Shared.modalDialog
                settings.theme
                (Translations.dialogTitle [ settings.browserEnv.translations ])
                [ viewZapDialog dialog zapAmountSelectionData ]
                CloseDialog
                |> Html.map settings.toMsg


viewZapDialog : ZapDialog msg -> ZapAmountSelectionData -> Html (Msg msg)
viewZapDialog (Settings settings) data =
    let
        (Model model) =
            settings.model

        relays =
            Nostr.getWriteRelaysForPubKey settings.nostr settings.pubKey
    in
    div
        [ css
            [ Tw.my_4
            , Tw.flex
            , Tw.flex_col
            , Tw.justify_start
            , Tw.gap_2
            ]
        ]
        [ recipientsSection (Settings settings) data.recipients
        , div
            []
            [ Button.new
                { label = Translations.closeButtonTitle [ settings.browserEnv.translations ]
                , onClick = Just CloseDialog
                , theme = settings.theme
                }
                |> Button.withTypeSecondary
                |> Button.view
            , Button.new
                { label = Translations.zapButtonTitle [ settings.browserEnv.translations ]
                , onClick = Just <| PublishClicked settings.onPublish
                , theme = settings.theme
                }
                |> Button.withTypePrimary
                |> Button.view
            ]
        ]


recipientsSection : ZapDialog msg -> List Recipient -> Html (Msg msg)
recipientsSection (Settings settings) recipients =
    let
        (Model model) =
            settings.model

        styles =
            Ui.Styles.stylesForTheme settings.theme
    in
    viewRecipients (Settings settings) recipients


viewRecipients : ZapDialog msg -> List Recipient -> Html (Msg msg)
viewRecipients (Settings settings) recipients =
    div []
        [ ul
            [ Attr.style "list-style-type" "disc"
            , css
                [ Tw.text_base
                , Tw.text_color Theme.purple_900
                , Tw.mb_2
                , Tw.flex
                , Tw.flex_col
                , Tw.gap_y_2
                ]
            ]
            (List.map (viewRecipient settings.theme) recipients)
        ]


viewRecipient : Theme -> Recipient -> Html (Msg msg)
viewRecipient _ _ =
    div
        []
        []
