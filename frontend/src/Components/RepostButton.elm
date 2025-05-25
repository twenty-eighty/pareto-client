module Components.RepostButton exposing
    ( RepostButton, new
    , view
    , init, update, Model, Msg
    , subscriptions
    )

{-|

## Basic usage

@docs RepostButton, new
@docs view

## State management

@docs init, update, Model, Msg

-}

import Components.Icon as Icon
import Components.InteractionButton as InteractionButton exposing (InteractionObject(..))
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html)
import I18Next
import Nostr
import Nostr.Nip18 as Nip18
import Nostr.Send exposing (SendRequest(..))
import Nostr.Types exposing (LoginStatus, PubKey, loggedInPubKey, loggedInSigningPubKey)
import Ui.Styles
import Dict
import Nostr.Event exposing (Kind(..))

-- MODEL

type Model =
    Model InteractionButton.Model


init : Model
init =
    Model InteractionButton.init


-- UPDATE

type Msg
    = InteractionButtonMsg (InteractionButton.Msg Msg)


update : { msg : Msg, model : Model, nostr : Nostr.Model, toModel : Model -> model, translations : I18Next.Translations, toMsg : Msg -> msg } -> ( model, Effect msg )
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


type RepostButton msg
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
    } -> RepostButton msg
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


view : RepostButton msg -> Html msg
view (Settings settings) =
    let
        (Model model) =
            settings.model

        label =
            getRepostsCount settings.interactionObject settings.nostr
            |> String.fromInt

        reposted =
            settings.loginStatus
            |> loggedInPubKey
            |> Maybe.map (hasRepost settings.interactionObject settings.nostr)
            |> Maybe.withDefault False

        clickAction =
            settings.loginStatus
            |> loggedInSigningPubKey
            |> Maybe.map (\pubKey ->
                getSendRequest settings.interactionObject pubKey
                |> InteractionButton.Send
            )
            |> Maybe.withDefault InteractionButton.NoAction
    in
    InteractionButton.new
        { model = model
        , unreactedIcon = Icon.MaterialIcon Icon.MaterialRepeat 30 Icon.Inherit
        , reactedIcon = Icon.MaterialIcon Icon.MaterialRepeatOn 30 Icon.Inherit
        , reacted = reposted
        , toMsg = InteractionButtonMsg
        , theme = settings.theme
        }
        |> InteractionButton.withLabel label
        |> InteractionButton.withOnClickAction clickAction
        |> InteractionButton.view 
        |> Html.map settings.toMsg


getSendRequest : InteractionObject -> PubKey -> SendRequest
getSendRequest interactionObject pubKey =
    case interactionObject of
        Article eventId (( _, articlePubKey, _ ) as addressComponents) ->
            Nip18.repostEvent pubKey eventId articlePubKey KindLongFormContent (Just addressComponents) Nothing
            |> SendRepost []

        Comment eventId articlePubKey ->
            Nip18.repostEvent pubKey eventId articlePubKey KindLongFormContent Nothing Nothing
            |> SendRepost []

        PicturePost eventId articlePubKey ->
            Nip18.repostEvent pubKey eventId articlePubKey KindLongFormContent Nothing Nothing
            |> SendRepost []


getRepostsCount : InteractionObject -> Nostr.Model -> Int
getRepostsCount interactionObject nostr =
    case interactionObject of
        Article _ addressComponents ->
            Nostr.getRepostsCountForAddressComponents nostr addressComponents
                |> Maybe.withDefault 0

        Comment eventId _ ->
            Nostr.getRepostsCountForEventId nostr eventId
                |> Maybe.withDefault 0

        PicturePost eventId _ ->
            Nostr.getRepostsCountForEventId nostr eventId
                |> Maybe.withDefault 0

hasRepost : InteractionObject -> Nostr.Model -> PubKey -> Bool
hasRepost interactionObject nostr pubKey =
    case interactionObject of
        Article _ addressComponents ->
            Nostr.getRepostsForArticle nostr addressComponents
            |> Maybe.map (Dict.member pubKey)
            |> Maybe.withDefault False

        Comment eventId _ ->
            Nostr.getRepostsForEventId nostr eventId
            |> Maybe.map (Dict.member pubKey)
            |> Maybe.withDefault False

        PicturePost eventId _ ->
            Nostr.getRepostsForEventId nostr eventId
            |> Maybe.map (Dict.member pubKey)
            |> Maybe.withDefault False


subscriptions : Model -> Sub Msg
subscriptions (Model model) =
    InteractionButton.subscriptions model
    |> Sub.map InteractionButtonMsg