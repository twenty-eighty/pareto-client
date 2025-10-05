module Components.SendNewsletterDialog exposing (Model, Msg(..), SendNewsletterDialog, hide, init, new, show, subscriptions, update, view)

import BrowserEnv exposing (BrowserEnv, Environment(..))
import Components.Button as Button
import Components.Checkbox as Checkbox
import Components.CriteriaBuilder as CriteriaBuilder
import Components.Dropdown as Dropdown
import Components.Icon as Icon
import Components.ModalDialog as ModalDialog
import Dict exposing (Dict)
import Effect exposing (Effect)
import FeatherIcons
import Html.Styled as Html exposing (Html, div, h2, li, p, text, ul)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events exposing (..)
import I18Next
import Json.Decode as Decode
import Newsletters.ContactDatabase as ContactDatabase
import Nostr
import Nostr.Event exposing (Kind(..))
import Nostr.External
import Nostr.Relay as Relay exposing (Relay)
import Nostr.RelayListMetadata exposing (RelayMetadata, eventWithRelayList, extendRelayList)
import Nostr.Send exposing (SendRequest(..))
import Nostr.Types exposing (IncomingMessage, PubKey, RelayRole(..), RelayUrl)
import Pareto
import Ports
import Shared.Model exposing (Model)
import Shared.Msg exposing (Msg)
import Newsletters.Subscribers as Subscribers
import Tailwind.Utilities as Tw
import Translations.SendNewsletterDialog as Translations
import Ui.Shared exposing (emptyHtml)
import Ui.Styles exposing (Theme(..), darkMode, fontFamilyInter)


type Msg msg
    = ShowDialog
    | CloseDialog
    | SendClicked (NewsletterInfo -> msg)
    | ReceivedMessage IncomingMessage

type alias NewsletterInfo =
    { tags : List String
    }


type Model
    = Model
        { criteriaBuilder : CriteriaBuilder.Model
        , errors : List String
        , tags : List String
        , state : DialogState
        }


type DialogState
    = DialogHidden
    | DialogPreparation
    | DialogSending
    | DialogSent
    | DialogError String



type SendNewsletterDialog msg
    = Settings
        { model : Model
        , toMsg : Msg msg -> msg
        , pubKey : PubKey
        , browserEnv : BrowserEnv
        , theme : Theme
        }


new :
    { model : Model
    , toMsg : Msg msg -> msg
    , pubKey : PubKey
    , browserEnv : BrowserEnv
    , theme : Theme
    }
    -> SendNewsletterDialog msg
new props =
    Settings
        { model = props.model
        , toMsg = props.toMsg
        , pubKey = props.pubKey
        , browserEnv = props.browserEnv
        , theme = props.theme
        }


init : { pubKey : PubKey } -> ( Model, Effect (Msg msg) )
init props =
    ( Model
        { criteriaBuilder = CriteriaBuilder.init {}
        , errors = []
        , state = DialogHidden
        , tags = []
        }
    , ContactDatabase.loadContactTags props.pubKey
        |> Effect.sendCmd
    )

hide : Model -> Model
hide (Model model) =
    Model { model | state = DialogHidden }


show : Model -> Model
show (Model model) =
    Model { model | state = DialogPreparation }


update :
    { msg : Msg msg
    , model : Model
    , toModel : Model -> model
    , toMsg : Msg msg -> msg
    , nostr : Nostr.Model
    , pubKey : PubKey
    , testMode : BrowserEnv.TestMode
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
            ShowDialog ->
                ( Model { model | state = DialogPreparation }
                , Subscribers.load props.nostr props.pubKey
                    |> Effect.sendSharedMsg
                )

            CloseDialog ->
                ( Model { model | state = DialogHidden }
                , Effect.none
                )

            SendClicked msg ->
                let
                    effect =
                        { tags = model.tags }
                            |> msg
                            |> Effect.sendMsg
                in
                ( Model model
                , effect
                )

            ReceivedMessage message ->
                updateWithMessage (Model model) props.pubKey message


sendRelayListCmd : PubKey -> List RelayMetadata -> Effect msg
sendRelayListCmd pubKey relays =
    let
        relaysWithProtocol =
            relays
                |> List.map
                    (\relay ->
                        { relay | url = "wss://" ++ relay.url }
                    )

        relayUrls =
            relays
                |> List.filterMap
                    (\relay ->
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


updateWithMessage : Model -> PubKey -> IncomingMessage -> ( Model, Effect msg )
updateWithMessage (Model model) userPubKey message =
    case message.messageType of
        "contactTags" ->
            case Decode.decodeValue (Decode.field "tags" (Decode.list Decode.string)) message.value of
                Ok tags ->
                    let
                        errors =
                            []
                    in
                    ( Model { model | tags = tags, errors = errors }, Effect.none )

                Err error ->
                    ( Model { model | errors = ("Error receiving tags: " ++ Decode.errorToString error) :: model.errors }, Effect.none )

        _ ->
            ( Model model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> (Msg msg -> msg) -> Sub msg
subscriptions _ toMsg =
    Ports.receiveMessage ReceivedMessage
        |> Sub.map toMsg



-- VIEW


view : SendNewsletterDialog msg -> Html msg
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

        DialogPreparation ->
            viewPreparationDialog dialog

        DialogSending ->
            viewSendingDialog dialog

        DialogSent ->
            viewSentDialog dialog
            
        DialogError error ->
            viewErrorDialog dialog error


viewPreparationDialog : SendNewsletterDialog msg -> Html msg
viewPreparationDialog dialog =
    let
        (Settings settings) =
            dialog
    in
    ModalDialog.new
        { title = Translations.dialogTitle [ settings.browserEnv.translations ]
        , content =
            [ viewSendNewsletterDialog dialog
            ]
        , onClose = CloseDialog
        , theme = settings.theme
        , buttons =
            [ {- Button.new
                { label = Translations.sendButtonTitle [ settings.browserEnv.translations ]
                , onClick = Just <| SendClicked settings.onPublish
                , theme = settings.theme
                }
                |> Button.withTypePrimary
                |> Button.withDisabled (numberOfRecipients settings.model < 1)
                |> Button.view
                -}
            ]
        }
        |> ModalDialog.view
        |> Html.map settings.toMsg

numberOfRecipients : Model -> Int
numberOfRecipients (Model model) =
    0


viewSendingDialog : SendNewsletterDialog msg -> Html msg
viewSendingDialog dialog =
    let
        (Settings settings) =
            dialog
    in
    ModalDialog.new
        { title = Translations.dialogTitle [ settings.browserEnv.translations ]
        , content = [ viewSendNewsletterDialog dialog ]
        , onClose = CloseDialog
        , theme = settings.theme
        , buttons = [ ]
        }
        |> ModalDialog.view
        |> Html.map settings.toMsg


viewSentDialog : SendNewsletterDialog msg -> Html msg
viewSentDialog dialog =
    let
        (Settings settings) =
            dialog
    in
    ModalDialog.new
        { title = Translations.dialogTitle [ settings.browserEnv.translations ]
        , content = [ viewSendNewsletterDialog dialog ]
        , onClose = CloseDialog
        , theme = settings.theme
        , buttons = [ ]
        }
        |> ModalDialog.view
        |> Html.map settings.toMsg

viewErrorDialog : SendNewsletterDialog msg -> String -> Html msg
viewErrorDialog dialog error =
    let
        (Settings settings) =
            dialog
    in
    ModalDialog.new
        { title = Translations.dialogTitle [ settings.browserEnv.translations ]
        , content = [ viewSendNewsletterDialog dialog ]
        , onClose = CloseDialog
        , theme = settings.theme
        , buttons = [  ]
        }
        |> ModalDialog.view
        |> Html.map settings.toMsg

viewSendNewsletterDialog : SendNewsletterDialog msg -> Html (Msg msg)
viewSendNewsletterDialog (Settings settings) =
    let
        (Model model) =
            settings.model

        activeSubscribersCount =
            1
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
        [ 
        ]

