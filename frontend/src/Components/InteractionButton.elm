module Components.InteractionButton exposing
    ( InteractionButton, new
    , ClickAction(..)
    , view
    , init, update, Model, Msg
    , subscriptions
    , InteractionObject(..)
    , InteractionParams
    , withLabel
    , withAttributes
    , withOnClickAction
    )

{-|


## Basic usage

@docs InteractionButton, new
@docs view

## State management

@docs init, update, Model, Msg

## Modifiers

@docs withIsReacted
@docs withOnReact
@docs withSubscription

-}

import Components.InteractionIcon as InteractionIcon
import Components.Icon exposing (Icon)
import Effect exposing (Effect)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attr exposing (css)
import I18Next
import Nostr
import Nostr.Event exposing (AddressComponents)
import Nostr.External
import Nostr.Send exposing (SendRequestId, SendRequest)
import Nostr.Types exposing (EventId, PubKey, IncomingMessage)
import Ports
import Shared.Msg
import Tailwind.Utilities as Tw
import Translations.InteractionButton as Translations
import Ui.Shared exposing (emptyHtml)
import Ui.Styles

type InteractionObject
    = Article EventId AddressComponents
    | Comment EventId PubKey
    | PicturePost EventId PubKey

type alias InteractionParams =
    { nostr : Nostr.Model
    , pubKey : PubKey
    , interactionObject : InteractionObject
    }

type ClickAction msg
    = NoAction
    | Send SendRequest
    | SendMsg msg

-- MODEL

type Model =
    Model
        { sendRequestId : Maybe SendRequestId
        }


init : Model
init =
    Model
        { sendRequestId = Nothing
        }


-- UPDATE

type Msg msg
    = Clicked (ClickAction msg)
    | ReceivedMessage IncomingMessage

update :
    { msg : Msg msg
    , model : Model
    , nostr : Nostr.Model
    , toModel : Model -> model
    , translations : I18Next.Translations
    } -> ( model, Effect msg )
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
            Clicked clickAction ->
                case clickAction of
                    NoAction ->
                        ( Model model, Effect.none )

                    Send sendRequest ->
                        ( Model
                            { sendRequestId = Just (Nostr.getLastSendRequestId props.nostr)
                            }
                        , sendRequest
                            |> Shared.Msg.SendNostrEvent
                            |> Effect.sendSharedMsg
                        )

                    SendMsg clickMsg ->
                        ( Model model, Effect.sendMsg clickMsg )
            
            ReceivedMessage message ->
                updateWithMessage (Model model) props.translations message


updateWithMessage : Model -> I18Next.Translations -> IncomingMessage -> ( Model, Effect msg )
updateWithMessage (Model model) translations message =
    case message.messageType of
        "events" ->
            case (model.sendRequestId, Nostr.External.decodeSendId message.value) of
                ( Just sendRequestId, Ok incomingSendRequestId ) ->
                    if sendRequestId == incomingSendRequestId then
                        ( Model { model | sendRequestId = Nothing }, Effect.none )
                    else
                        ( Model model, Effect.none )

                _ ->
                    ( Model model, Effect.none )

        "error" ->
            case (model.sendRequestId, Nostr.External.decodeSendId message.value, Nostr.External.decodeReason message.value) of
                ( Just sendRequestId, Ok incomingSendRequestId, Ok reason ) ->
                    if sendRequestId == incomingSendRequestId then
                        ( Model { model | sendRequestId = Nothing }
                        , Translations.errorPublishingInteraction [ translations ] { reason = reason }
                            |> Shared.Msg.ShowAlert
                            |> Effect.sendSharedMsg
                        )
                    else
                        ( Model model, Effect.none )

                _ ->
                    ( Model model, Effect.none )

        _ ->
            ( Model model, Effect.none )



-- SETTINGS


type InteractionButton msg
    = Settings
        { model : Model
        , onClickAction : Maybe (ClickAction msg)
        , unreactedIcon : Icon
        , reactedIcon : Icon
        , reacted : Bool
        , toMsg : Msg msg -> msg
        , theme : Ui.Styles.Theme
        , label : Maybe String
        , attributes : List (Html.Attribute msg)
        }

new : 
    { model : Model
    , unreactedIcon : Icon
    , reactedIcon : Icon
    , reacted : Bool
    , toMsg : Msg msg -> msg
    , theme : Ui.Styles.Theme
    } -> InteractionButton msg
new props =
    Settings
        { model = props.model
        , onClickAction = Nothing
        , unreactedIcon = props.unreactedIcon
        , reactedIcon = props.reactedIcon
        , reacted = props.reacted
        , toMsg = props.toMsg
        , theme = props.theme
        , label = Nothing
        , attributes = []
        }

withLabel : String -> InteractionButton msg -> InteractionButton msg
withLabel label (Settings settings) =
    Settings { settings | label = Just label }


withOnClickAction : ClickAction msg -> InteractionButton msg -> InteractionButton msg
withOnClickAction clickAction (Settings settings) =
    Settings { settings | onClickAction = Just clickAction }


withAttributes : List (Html.Attribute msg) -> InteractionButton msg -> InteractionButton msg
withAttributes attributes (Settings settings) =
    Settings { settings | attributes = attributes }

subscriptions : Model -> Sub (Msg msg)
subscriptions (Model model) =
    case model.sendRequestId of
        Just _ ->
            Ports.receiveMessage ReceivedMessage 
        
        Nothing ->
            Sub.none


-- VIEW


view : InteractionButton msg -> Html msg
view (Settings settings) =
    let
        (Model model) =
            settings.model

        currentIcon =
            if settings.reacted then
                settings.reactedIcon
            else
                settings.unreactedIcon

        onClick =
            settings.onClickAction
            |> Maybe.map Clicked
    in
    div
        [ css
            [ Tw.flex
            , Tw.flex_row
            , Tw.items_center
            ]
        ]
        [ InteractionIcon.new
            { icon = currentIcon
            , actionInProgress = model.sendRequestId /= Nothing
            , onClick = onClick
            , theme = settings.theme
            }
            |> InteractionIcon.map settings.toMsg
            |> InteractionIcon.withAttributes settings.attributes
            |> InteractionIcon.view
        , settings.label
            |> Maybe.map (\label ->
                Html.span [ css [ Tw.text_left ] ] [ Html.text label ]
            )
            |> Maybe.withDefault emptyHtml
        ]


