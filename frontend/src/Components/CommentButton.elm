module Components.CommentButton exposing
    ( CommentButton, new
    , withOpenCommentMsg
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
    | OpenComment msg


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

            OpenComment openCommentMsg ->
                ( Model model, Effect.sendMsg openCommentMsg )


-- SETTINGS


type CommentButton msg
    = Settings
        { model : Model
        , interactionObject : InteractionObject
        , nostr : Nostr.Model
        , loginStatus : LoginStatus
        , openCommentMsg : Maybe msg
        , toMsg : Msg msg -> msg
        , theme : Ui.Styles.Theme
        }


withOpenCommentMsg : Maybe msg -> CommentButton msg -> CommentButton msg
withOpenCommentMsg openCommentMsg (Settings settings) =
    Settings { settings | openCommentMsg = openCommentMsg }

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
        , openCommentMsg = Nothing
        , toMsg = props.toMsg
        , theme = props.theme
        }


-- VIEW


view : CommentButton msg -> Html msg
view (Settings settings) =
    let
        (Model model) =
            settings.model

        label =
            getCommentsCount settings.interactionObject settings.nostr
            |> String.fromInt

        commented =
            settings.loginStatus
            |> loggedInPubKey
            |> Maybe.map (hasComment settings.interactionObject settings.nostr)
            |> Maybe.withDefault False

        clickAction : InteractionButton.ClickAction (Msg msg)
        clickAction =
            if loggedInSigningPubKey settings.loginStatus /= Nothing then
                settings.openCommentMsg
                |> Maybe.map (\openCommentMsg ->
                    InteractionButton.SendMsg (OpenComment openCommentMsg)
                )
                |> Maybe.withDefault InteractionButton.NoAction
            else
                InteractionButton.NoAction
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


getCommentsCount : InteractionObject -> Nostr.Model -> Int
getCommentsCount interactionObject nostr =
    case interactionObject of
        Article _ addressComponents ->
            List.length (Nostr.getArticleComments nostr addressComponents)
            + List.length (Nostr.getArticleCommentComments nostr addressComponents |> Dict.values)

        Comment _ _ ->
            0

        PicturePost _ _ ->
            0

hasComment : InteractionObject -> Nostr.Model -> PubKey -> Bool
hasComment interactionObject nostr pubKey =
    case interactionObject of
        Article _ addressComponents ->
            Nostr.getArticleComments nostr addressComponents
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