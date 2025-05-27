module Components.LikeButton exposing
    ( LikeButton, new
    , view
    , init, update, Model, Msg
    , subscriptions
    )

{-|


## Basic usage

@docs LikeButton, new
@docs view

## State management

@docs init, update, Model, Msg

-}

import Color
import Components.Icon as Icon
import Components.InteractionButton as InteractionButton exposing (InteractionObject(..))
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html)
import I18Next
import Nostr
import Nostr.Send exposing (SendRequest(..))
import Nostr.Types exposing (LoginStatus, PubKey, loggedInPubKey, loggedInSigningPubKey)
import Ui.Styles

-- MODEL

type Model =
    Model InteractionButton.Model


init : Model
init =
    Model InteractionButton.init


-- UPDATE

type Msg
    = InteractionButtonMsg (InteractionButton.Msg Msg)


update : { msg : Msg, model : Model, nostr : Nostr.Model, toModel : Model -> model, toMsg : Msg -> msg, translations : I18Next.Translations } -> ( model, Effect msg )
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
            InteractionButtonMsg interactionMsg ->
                let
                    (updatedModel, effect) =
                        InteractionButton.update
                            { msg = interactionMsg
                            , model = model
                            , nostr = props.nostr
                            , toModel = \interactionButtonModel -> Model interactionButtonModel
                            , translations = props.translations
                            }
                in
                ( updatedModel, effect |> Effect.map props.toMsg )


-- SETTINGS


type LikeButton msg
    = Settings
        { model : Model
        , interactionObject : InteractionObject
        , nostr : Nostr.Model
        , loginStatus : LoginStatus
        , toMsg : Msg -> msg
        , theme : Ui.Styles.Theme
        }


new : 
    { model : Model
    , interactionObject : InteractionObject
    , nostr : Nostr.Model
    , loginStatus : LoginStatus
    , toMsg : Msg -> msg
    , theme : Ui.Styles.Theme
    } -> LikeButton msg
new props =
    Settings
        { model = props.model
        , interactionObject = props.interactionObject
        , nostr = props.nostr
        , loginStatus = props.loginStatus
        , toMsg = props.toMsg
        , theme = props.theme
        }




-- VIEW


view : LikeButton msg -> Html msg
view (Settings settings) =
    let
        (Model model) =
            settings.model

        label =
            getReactionsCount settings.interactionObject settings.nostr
            |> String.fromInt

        reacted =
            settings.loginStatus
            |> loggedInPubKey
            |> Maybe.map (isLiked settings.interactionObject settings.nostr)
            |> Maybe.withDefault False

        clickAction =
            settings.loginStatus
            |> loggedInSigningPubKey
            |> Maybe.map (\pubKey ->
                if not reacted then
                    getSendRequest settings.interactionObject pubKey
                    |> InteractionButton.Send
                    |> Just
                else
                    Nothing
            )
            |> Maybe.withDefault Nothing
    in
    InteractionButton.new
        { model = model
        , unreactedIcon = Icon.MaterialIcon Icon.MaterialFavoriteBorder 20 Icon.Inherit
        , reactedIcon = Icon.MaterialIcon Icon.MaterialFavorite 20 (Icon.Color (Color.fromRgba { red = 1.0, green = 0.0, blue = 0.0, alpha = 1.0 }))
        , reacted = reacted
        , toMsg = InteractionButtonMsg
        , theme = settings.theme
        }
        |> InteractionButton.withOnClickAction clickAction
        |> InteractionButton.withLabel label
        |> InteractionButton.view 
        |> Html.map settings.toMsg


getSendRequest : InteractionObject -> PubKey -> SendRequest
getSendRequest interactionObject pubKey =
    case interactionObject of
        Article eventId (( _, articlePubKey, _ ) as addressComponents) ->
            SendReaction pubKey eventId articlePubKey (Just addressComponents)

        Comment eventId articlePubKey ->
            SendReaction pubKey eventId articlePubKey Nothing

        PicturePost eventId articlePubKey ->
            SendReaction pubKey eventId articlePubKey Nothing


getReactionsCount : InteractionObject -> Nostr.Model -> Int
getReactionsCount interactionObject nostr =
    case interactionObject of
        Article _ addressComponents ->
            Nostr.getReactionsCountForAddressComponents nostr addressComponents
                |> Maybe.withDefault 0

        Comment eventId _ ->
            Nostr.getReactionsCountForEventId nostr eventId
                |> Maybe.withDefault 0

        PicturePost eventId _ ->
            Nostr.getReactionsCountForEventId nostr eventId
                |> Maybe.withDefault 0

isLiked : InteractionObject -> Nostr.Model -> PubKey -> Bool
isLiked interactionObject nostr pubKey =
    case interactionObject of
        Article _ addressComponents ->
            Nostr.getReactionForArticle nostr pubKey addressComponents /= Nothing

        Comment eventId _ ->
            Nostr.getReactionForEventId nostr pubKey eventId /= Nothing

        PicturePost eventId _ ->
            Nostr.getReactionForEventId nostr pubKey eventId /= Nothing


subscriptions : Model -> Sub Msg
subscriptions (Model model) =
    InteractionButton.subscriptions model
    |> Sub.map InteractionButtonMsg