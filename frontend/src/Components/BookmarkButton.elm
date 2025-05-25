module Components.BookmarkButton exposing
    ( BookmarkButton, new
    , view
    , init, update, Model, Msg
    , subscriptions
    )

{-|


## Basic usage

@docs BookmarkButton, new
@docs view

## State management

@docs init, update, Model, Msg

-}


import Components.Icon as Icon
import Components.InteractionButton as InteractionButton exposing (InteractionObject(..))
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html)
import I18Next
import Nostr exposing (areAddressComponentsBookmarked, isEventIdBookmarked)
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


type BookmarkButton msg
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
    } -> BookmarkButton msg
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


view : BookmarkButton msg -> Html msg
view (Settings settings) =
    let
        (Model model) =
            settings.model

        label =
            getBookmarksCount settings.interactionObject settings.nostr
            |> String.fromInt

        isBookmarked =
            settings.loginStatus
                |> loggedInPubKey
                |> Maybe.map (hasBookmark settings.interactionObject settings.nostr)
                |> Maybe.withDefault False

        clickAction =
            settings.loginStatus
            |> loggedInSigningPubKey
            |> Maybe.map (\pubKey ->
                if isBookmarked then
                    getAddBookmarkRequest settings.interactionObject pubKey
                else
                    getRemoveBookmarkRequest settings.interactionObject pubKey
            )
            |> Maybe.map InteractionButton.Send
            |> Maybe.withDefault InteractionButton.NoAction
    in
    InteractionButton.new
        { model = model
        , unreactedIcon = Icon.MaterialIcon Icon.MaterialOutlineBookmarkAdd 30 Icon.Inherit
        , reactedIcon = Icon.MaterialIcon Icon.MaterialOutlineBookmarkAdded 30 Icon.Inherit
        , reacted = isBookmarked
        , toMsg = InteractionButtonMsg
        , theme = settings.theme
        }
        |> InteractionButton.withLabel label
        |> InteractionButton.withOnClickAction clickAction
        |> InteractionButton.view 
        |> Html.map settings.toMsg


getAddBookmarkRequest : InteractionObject -> PubKey -> SendRequest
getAddBookmarkRequest interactionObject pubKey =
    case interactionObject of
        Article _ addressComponents ->
            SendBookmarkListWithArticle pubKey addressComponents

        Comment eventId _ ->
            SendBookmarkListWithShortNote pubKey eventId

        PicturePost eventId _ ->
            SendBookmarkListWithShortNote pubKey eventId


getRemoveBookmarkRequest : InteractionObject -> PubKey -> SendRequest
getRemoveBookmarkRequest interactionObject pubKey =
    case interactionObject of
        Article _ addressComponents ->
            SendBookmarkListWithoutArticle pubKey addressComponents

        Comment eventId _ ->
            SendBookmarkListWithoutShortNote pubKey eventId

        PicturePost eventId _ ->
            SendBookmarkListWithoutShortNote pubKey eventId


getBookmarksCount : InteractionObject -> Nostr.Model -> Int
getBookmarksCount interactionObject nostr =
    case interactionObject of
        Article _ addressComponents ->
            Nostr.getBookmarkListCountForAddressComponents nostr addressComponents

        Comment eventId _ ->
            Nostr.getBookmarkListCountForEventId nostr eventId

        PicturePost eventId _ ->
            Nostr.getBookmarkListCountForEventId nostr eventId


hasBookmark : InteractionObject -> Nostr.Model -> PubKey -> Bool
hasBookmark interactionObject nostr pubKey =
    case interactionObject of
        Article _ addressComponents ->
            areAddressComponentsBookmarked nostr addressComponents pubKey

        Comment eventId _ ->
            isEventIdBookmarked nostr eventId pubKey

        PicturePost eventId _ ->
            isEventIdBookmarked nostr eventId pubKey


subscriptions : Model -> Sub Msg
subscriptions (Model model) =
    InteractionButton.subscriptions model
    |> Sub.map InteractionButtonMsg