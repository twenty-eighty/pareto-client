module Components.EmailSubscriptionDialog exposing (EmailSubscriptionDialog, Model, Msg, Recipient, hide, init, new, show, update, view)

import BrowserEnv exposing (BrowserEnv)
import Components.Button as Button
import Components.Icon as Icon
import Dict exposing (Dict)
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, a, button, div, form, h2, img, input, label, li, p, span, text, ul)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events as Events exposing (..)
import Http
import Nostr
import Nostr.Relay exposing (Relay)
import Nostr.RelayListMetadata exposing (RelayMetadata, eventWithRelayList, extendRelayList)
import Nostr.Send exposing (SendRequest(..))
import Nostr.Types exposing (PubKey, RelayUrl)
import Nostr.Zaps exposing (Lud16, PayRequest, fetchPayRequest)
import Pareto
import Shared.Model exposing (Model)
import Shared.Msg exposing (Msg)
import Svg.Styled as Svg exposing (path, svg)
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw
import Translations.EmailSubscriptionDialog as Translations
import Ui.Shared
import Ui.Styles exposing (Theme, fontFamilyInter, fontFamilyUnbounded)


type Msg msg
    = CloseDialog
    | ConfigureRelaysClicked
    | SubscribeClicked
    | ReceivedPayRequest (Result Http.Error PayRequest)


type Model
    = Model
        { state : DialogState
        }


type DialogState
    = DialogHidden
    | DialogVisible EmailSubscriptionData


type alias Recipient =
    { imageUrl : String
    , name : String
    , part : Int
    , lud16 : Lud16
    }


type alias EmailSubscriptionData =
    { email : Maybe String
    }


type EmailSubscriptionDialog msg
    = Settings
        { model : Model
        , toMsg : Msg msg -> msg
        , nostr : Nostr.Model
        , pubKey : PubKey
        , browserEnv : BrowserEnv
        , theme : Theme
        }


new :
    { model : Model
    , toMsg : Msg msg -> msg
    , nostr : Nostr.Model
    , pubKey : PubKey
    , browserEnv : BrowserEnv
    , theme : Theme
    }
    -> EmailSubscriptionDialog msg
new props =
    Settings
        { model = props.model
        , toMsg = props.toMsg
        , nostr = props.nostr
        , pubKey = props.pubKey
        , browserEnv = props.browserEnv
        , theme = props.theme
        }


init : {} -> Model
init props =
    Model
        { state = DialogHidden
        }


show : Model -> Model
show (Model model) =
    Model { model | state = DialogVisible { email = Nothing } }


requestPayRequests : List Recipient -> Cmd (Msg msg)
requestPayRequests recipients =
    recipients
        |> List.map
            (\recipient ->
                fetchPayRequest ReceivedPayRequest recipient.lud16
            )
        |> Cmd.batch


hide : Model -> Model
hide (Model model) =
    Model { model | state = DialogHidden }


update :
    { msg : Msg msg
    , model : Model
    , toModel : Model -> model
    , toMsg : Msg msg -> msg
    , nostr : Nostr.Model
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
            CloseDialog ->
                ( Model { model | state = DialogHidden }
                , Effect.none
                )

            ConfigureRelaysClicked ->
                ( Model model
                , Effect.none
                )

            SubscribeClicked ->
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


view : EmailSubscriptionDialog msg -> Html msg
view dialog =
    let
        (Settings settings) =
            dialog

        (Model model) =
            settings.model
    in
    case model.state of
        DialogHidden ->
            div [] []

        DialogVisible emailSubscriptionData ->
            Ui.Shared.modalDialog
                settings.theme
                (Translations.dialogTitle [ settings.browserEnv.translations ])
                [ viewZapDialog dialog emailSubscriptionData ]
                CloseDialog
                |> Html.map settings.toMsg


viewZapDialog : EmailSubscriptionDialog msg -> EmailSubscriptionData -> Html (Msg msg)
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
        [ recipientsSection (Settings settings)
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
                { label = Translations.subscribeButtonTitle [ settings.browserEnv.translations ]
                , onClick = Just <| SubscribeClicked
                , theme = settings.theme
                }
                |> Button.withTypePrimary
                |> Button.view
            ]
        ]


recipientsSection : EmailSubscriptionDialog msg -> Html (Msg msg)
recipientsSection (Settings settings) =
    let
        (Model model) =
            settings.model

        styles =
            Ui.Styles.stylesForTheme settings.theme
    in
    div [] []



-- viewRecipients (Settings settings) recipients


viewRecipients : EmailSubscriptionDialog msg -> List Recipient -> Html (Msg msg)
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
