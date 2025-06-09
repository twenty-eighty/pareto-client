module Components.CommentButton exposing
    ( CommentButton, new
    , withClickedMsg
    , view
    , init, update, Model, Msg
    , subscriptions
    )

{-|


## Basic usage

@docs CommentButton, new
@docs view

## State management

@docs init, update, Model, Msg

-}

import Components.Icon as Icon
import Components.InteractionButton as InteractionButton exposing (InteractionObject(..))
import Dict
import Effect exposing (Effect)
import FeatherIcons
import Html.Styled as Html exposing (Html)
import I18Next
import Nostr
import Nostr.Send exposing (SendRequest(..))
import Nostr.Types exposing (LoginStatus, PubKey, loggedInPubKey, loggedInSigningPubKey)
import Ui.Styles
import Nostr.Event exposing (Kind(..))


-- MODEL


type Model =
    Model InteractionButton.Model


init : Model
init =
    Model InteractionButton.init


-- UPDATE

type Msg msg
    = InteractionButtonMsg (InteractionButton.Msg (Msg msg))
    | Clicked msg


update : { msg : Msg msg, model : Model, nostr : Nostr.Model, toModel : Model -> model, toMsg : Msg msg -> msg, translations : I18Next.Translations } -> ( model, Effect msg )
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

            Clicked clickedMsg ->
                ( Model model
                , Effect.sendMsg clickedMsg
                )



-- SETTINGS


type CommentButton msg
    = Settings
        { model : Model
        , interactionObject : InteractionObject
        , nostr : Nostr.Model
        , loginStatus : LoginStatus
        , clickedMsg : Maybe msg
        , toMsg : Msg msg -> msg
        , theme : Ui.Styles.Theme
        }


withClickedMsg : Maybe msg -> CommentButton msg -> CommentButton msg
withClickedMsg clickedMsg (Settings settings) =
    Settings { settings | clickedMsg = clickedMsg }

new : 
    { model : Model
    , interactionObject : InteractionObject
    , nostr : Nostr.Model
    , loginStatus : LoginStatus
    , toMsg : Msg msg -> msg
    , theme : Ui.Styles.Theme
    } -> CommentButton msg
new props =
    Settings
        { model = props.model
        , interactionObject = props.interactionObject
        , nostr = props.nostr
        , loginStatus = props.loginStatus
        , clickedMsg = Nothing
        , toMsg = props.toMsg
        , theme = props.theme
        }


-- VIEW


view : CommentButton msg -> Html msg
view (Settings settings) =
    let
        (Model model) =
            settings.model

        maybeUserPubKey =
            settings.loginStatus
            |> loggedInPubKey

        label =
            getCommentsCount settings.interactionObject maybeUserPubKey settings.nostr
            |> String.fromInt

        commented =
            maybeUserPubKey
            |> Maybe.map (hasComment settings.interactionObject maybeUserPubKey settings.nostr)
            |> Maybe.withDefault False

        clickAction : Maybe (InteractionButton.ClickAction (Msg msg))
        clickAction =
            if loggedInSigningPubKey settings.loginStatus /= Nothing then
                settings.clickedMsg
                |> Maybe.map (\clickedMsg ->
                    InteractionButton.SendMsg (Clicked clickedMsg)
                )
            else
                Nothing
    in
    InteractionButton.new
        { model = model
        , unreactedIcon = Icon.FeatherIcon FeatherIcons.messageSquare
        , reactedIcon = Icon.FeatherIcon FeatherIcons.messageSquare
        , reacted = commented
        , toMsg = InteractionButtonMsg
        , theme = settings.theme
        }
        |> InteractionButton.withLabel label
        |> InteractionButton.withOnClickAction clickAction
        |> InteractionButton.view 
        |> Html.map settings.toMsg


getCommentsCount : InteractionObject -> Maybe PubKey -> Nostr.Model -> Int
getCommentsCount interactionObject maybeUserPubKey nostr =
    case interactionObject of
        Article _ addressComponents ->
            List.length (Nostr.getArticleComments nostr maybeUserPubKey addressComponents)
            + List.length (Nostr.getArticleCommentComments nostr addressComponents |> Dict.values)

        Comment _ _ ->
            0

        PicturePost _ _ ->
            0

hasComment : InteractionObject -> Maybe PubKey -> Nostr.Model -> PubKey -> Bool
hasComment interactionObject maybeUserPubKey nostr pubKey =
    case interactionObject of
        Article _ addressComponents ->
            Nostr.getArticleComments nostr maybeUserPubKey addressComponents
            |> List.filter (\articleComment -> articleComment.pubKey == pubKey)
            |> List.isEmpty
            |> not

        Comment _ _ ->
            False

        PicturePost _ _ ->
            False


subscriptions : Model -> Sub (Msg msg)
subscriptions (Model model) =
    InteractionButton.subscriptions model
    |> Sub.map InteractionButtonMsg