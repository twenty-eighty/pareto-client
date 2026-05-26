module Components.InteractionButton exposing
    ( InteractionButton, new
    , view
    , init, update, Model, Msg
    , ClickAction(..), InteractionObject(..), InteractionParams, eventIdOfInteractionObject, mapAction, pubKeyOfInteractionObject, subscriptions, withAttributes, withLabel, withOnClickAction, withReactIcon, withTestAttribute
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

import Components.Icon exposing (Icon)
import Components.InteractionIcon as InteractionIcon
import Effect exposing (Effect)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (css)
import I18Next
import Nostr
import Nostr.Event exposing (AddressComponents)
import Nostr.External
import Nostr.Send exposing (SendRequest, SendRequestId)
import Nostr.Types exposing (EventId, IncomingMessage, PubKey)
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
    | BatchAction (List (ClickAction msg))
    | Send SendRequest
    | SendMsg msg


pubKeyOfInteractionObject : InteractionObject -> PubKey
pubKeyOfInteractionObject interactionObject =
    case interactionObject of
        Article _ ( _, pubKey, _ ) ->
            pubKey

        Comment _ pubKey ->
            pubKey

        PicturePost _ pubKey ->
            pubKey


eventIdOfInteractionObject : InteractionObject -> EventId
eventIdOfInteractionObject interactionObject =
    case interactionObject of
        Article eventId _ ->
            eventId

        Comment eventId _ ->
            eventId

        PicturePost eventId _ ->
            eventId


mapAction : (msg1 -> msg2) -> ClickAction msg1 -> ClickAction msg2
mapAction toMsg clickAction =
    case clickAction of
        NoAction ->
            NoAction

        BatchAction actions ->
            BatchAction (List.map (mapAction toMsg) actions)

        Send sendRequest ->
            Send sendRequest

        SendMsg msg ->
            SendMsg (toMsg msg)



-- MODEL


type Model
    = Model
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
            Clicked clickAction ->
                clickActionToEffect props.nostr (Model model) clickAction

            ReceivedMessage message ->
                updateWithMessage (Model model) props.translations message

clickActionToEffect : Nostr.Model -> Model -> ClickAction msg -> (Model, Effect msg)
clickActionToEffect nostr (Model model) clickAction =
    case clickAction of
        NoAction ->
            ( Model model, Effect.none )

        BatchAction actions ->
            let
                (finalModel, finalEffects) =
                    actions
                    |> List.foldl
                        (\action (innerModel, innerEffects) ->
                            let
                                (updatedModel, effect) = clickActionToEffect nostr innerModel action
                            in
                            (updatedModel, innerEffects ++ [effect])
                        )
                        (Model model, [])
            in
            ( finalModel, Effect.batch finalEffects )

        Send sendRequest ->
            ( Model
                { sendRequestId = Just (Nostr.getLastSendRequestId nostr)
                }
            , sendRequest
                |> Shared.Msg.SendNostrEvent
                |> Effect.sendSharedMsg
            )

        SendMsg clickMsg ->
            ( Model model, Effect.sendMsg clickMsg )



updateWithMessage : Model -> I18Next.Translations -> IncomingMessage -> ( Model, Effect msg )
updateWithMessage (Model model) translations message =
    case message.messageType of
        "published" ->
            case ( model.sendRequestId, Nostr.External.decodeSendId message.value ) of
                ( Just sendRequestId, Ok incomingSendRequestId ) ->
                    if sendRequestId == incomingSendRequestId then
                        ( Model { model | sendRequestId = Nothing }, Effect.none )

                    else
                        ( Model model, Effect.none )

                _ ->
                    ( Model model, Effect.none )

        "error" ->
            case ( model.sendRequestId, Nostr.External.decodeSendId message.value, Nostr.External.decodeReason message.value ) of
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
        , reactIcon : Maybe Icon
        , reactedIcon : Icon
        , reacted : Bool
        , testAttribute : String
        , theme : Ui.Styles.Theme
        , toMsg : Msg msg -> msg
        , label : Maybe String
        , attributes : List ( String, String )
        }
new :
    { model : Model
    , unreactedIcon : Icon
    , reactedIcon : Icon
    , reacted : Bool
    , theme : Ui.Styles.Theme
    , toMsg : Msg msg -> msg
    }
    -> InteractionButton msg
new props =
    Settings
        { model = props.model
        , onClickAction = Nothing
        , unreactedIcon = props.unreactedIcon
        , reactIcon = Nothing
        , reactedIcon = props.reactedIcon
        , reacted = props.reacted
        , testAttribute = "unnamed"
        , theme = props.theme
        , toMsg = props.toMsg
        , label = Nothing
        , attributes = []
        }


withLabel : Maybe String -> InteractionButton msg -> InteractionButton msg
withLabel label (Settings settings) =
    Settings { settings | label = label }


withOnClickAction : Maybe (ClickAction msg) -> InteractionButton msg -> InteractionButton msg
withOnClickAction clickAction (Settings settings) =
    Settings { settings | onClickAction = clickAction }


withReactIcon : Icon -> InteractionButton msg -> InteractionButton msg
withReactIcon icon (Settings settings) =
    Settings { settings | reactIcon = Just icon }


withAttributes : List ( String, String ) -> InteractionButton msg -> InteractionButton msg
withAttributes attributes (Settings settings) =
    Settings { settings | attributes = attributes }


withTestAttribute : String -> InteractionButton msg -> InteractionButton msg
withTestAttribute testAttribute (Settings settings) =
    Settings { settings | testAttribute = testAttribute }


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
                case ( settings.reactIcon, settings.onClickAction ) of
                    ( Just reactIcon, Just _ ) ->
                        reactIcon

                    _ ->
                        settings.unreactedIcon

        onClick =
            if settings.onClickAction /= Nothing then
                settings.onClickAction
                    |> Maybe.map Clicked

            else
                Nothing
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
            |> InteractionIcon.withAttributes settings.attributes
            |> InteractionIcon.withTestAttribute settings.testAttribute
            |> InteractionIcon.view
            |> Html.map settings.toMsg
        , settings.label
            |> Maybe.map
                (\label ->
                    Html.span [ css [ Tw.text_left ] ] [ Html.text label ]
                )
            |> Maybe.withDefault emptyHtml
        ]
