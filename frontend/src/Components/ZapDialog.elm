module Components.ZapDialog exposing (ZapDialog, Recipient, Model, Msg, new, init, update, view, show, hide)

import BrowserEnv exposing (BrowserEnv)
import Components.Button as Button
import Components.Checkbox as Checkbox
import Components.Icon as Icon
import Dict exposing (Dict)
import Effect exposing (Effect)
import FeatherIcons
import Html.Styled as Html exposing (Html, a, button, div, form, h2, img, input, label, li, p, span, text, ul)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events as Events exposing (..)
import Nostr
import Nostr.Relay exposing (Relay)
import Nostr.RelayListMetadata exposing (RelayMetadata, eventWithRelayList, extendRelayList)
import Nostr.Send exposing (SendRequest(..))
import Nostr.Types exposing (PubKey, RelayUrl)
import Pareto
import Svg.Styled as Svg exposing (svg, path)
import Tailwind.Utilities as Tw
import Tailwind.Theme as Theme
import Translations.ZapDialog as Translations
import Shared.Msg exposing (Msg)
import Shared.Model exposing (Model)
import Ui.Shared
import Ui.Styles exposing (Theme, fontFamilyUnbounded, fontFamilyInter)
import Nostr exposing (getReadRelayUrlsForPubKey)
import Css exposing (readWrite)
import Nostr.Types exposing (RelayRole(..))

type Msg msg
    = CloseDialog
    | ConfigureRelaysClicked
    | PublishClicked (List RelayUrl -> msg)


type Model msg =
    Model
        { state : DialogState
        }

type DialogState
    = DialogHidden
    | ZapAmountSelection ZapAmountSelectionData

type alias Recipient =
    { imageUrl : String
    , name : String
    , part : Int
    }

type alias ZapAmountSelectionData =
    { amount : Int
    , comment : String
    }

type ZapDialog msg
    = Settings
        { model : Model msg
        , toMsg : Msg msg -> msg
        , onPublish : List RelayUrl -> msg
        , recipients : List Recipient
        , nostr : Nostr.Model
        , pubKey : PubKey
        , browserEnv : BrowserEnv
        , theme : Theme
        }

new :
    { model : Model msg
    , toMsg : Msg msg -> msg
    , onPublish : List RelayUrl -> msg
    , recipients : List Recipient
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
        , recipients = props.recipients
        , nostr = props.nostr
        , pubKey = props.pubKey
        , browserEnv = props.browserEnv
        , theme = props.theme
        }

init : { } -> Model msg
init props =
    Model
        { state = DialogHidden
        }

show : Model msg -> Model msg
show (Model model) =
    Model { model | state = ZapAmountSelection { amount = 21, comment = "" } }

hide : Model msg -> Model msg
hide (Model model) =
    Model { model | state = DialogHidden }

update :
    { msg : Msg msg
    , model : Model msg
    , toModel : Model msg -> model
    , toMsg : Msg msg -> msg
    , nostr : Nostr.Model
    , pubKey : PubKey
    }
    -> ( model, Effect msg )
update props  = 
    let
        (Model model) =
            props.model

        toParentModel : (Model msg, Effect msg) -> (model, Effect msg)
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
                let
                    allListRelays = 
                        Nostr.getRelayListForPubKey props.nostr props.pubKey
                        |> extendRelayList Pareto.defaultOutboxRelays
                in
                ( Model model
                , sendRelayListCmd props.pubKey allListRelays
                )

            PublishClicked msg ->
                ( Model model
                , Effect.none
                )

sendRelayListCmd : PubKey -> List RelayMetadata -> Effect msg
sendRelayListCmd pubKey relays =
    let
        relaysWithProtocol =
            relays
            |> List.map (\relay ->
                    { relay | url = "wss://" ++ relay.url}
                )

        relayUrls =
            relays
            |> List.filterMap (\relay ->
                if relay.role == WriteRelay || relay.role == ReadWriteRelay then
                    Just relay.url
                else
                    Nothing
            )
    in
    eventWithRelayList pubKey relaysWithProtocol
    |> SendRelayList relayUrls
    |> Shared.Msg.SendNostrEvent
    |> Effect.sendSharedMsg


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
            div [][]

        ZapAmountSelection zapAmountSelectionData ->
            Ui.Shared.modalDialog
                (Translations.dialogTitle [ settings.browserEnv.translations ])
                [ viewZapDialog dialog zapAmountSelectionData]
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
    div [ css
            [ Tw.my_4
            , Tw.flex
            , Tw.flex_col
            , Tw.justify_start
            , Tw.gap_2
            ]
        ]
        [ recipientsSection (Settings settings) settings.recipients
        , div
            [
            ]
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
viewRecipient theme recipient =
    div
        []
        []