module Components.Comment exposing (Comment, Model, Msg, hide, init, new, show, subscriptions, update, view)

import BrowserEnv exposing (BrowserEnv)
import Components.Button as Button
import Components.EntryField as EntryField
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, div)
import Html.Styled.Attributes exposing (css)
import Json.Decode as Decode
import Locale exposing (Language(..))
import Nostr
import Nostr.Event exposing (Kind(..))
import Nostr.External
import Nostr.Nip22 exposing (CommentType(..), articleCommentEvent, commentContent, setCommentContent)
import Nostr.Profile exposing (Profile, profileDisplayName)
import Nostr.Send exposing (SendRequest(..), SendRequestId)
import Nostr.Types exposing (IncomingMessage, PubKey)
import Ports
import Shared
import Shared.Model exposing (LoginStatus(..), Model)
import Shared.Msg exposing (Msg)
import Tailwind.Utilities as Tw
import Translations.Comment as Translations
import Ui.Shared exposing (emptyHtml)
import Ui.Styles exposing (Theme, stylesForTheme)


type Msg msg
    = CloseDialog
    | UpdateComment CommentType
    | PostClicked PubKey
    | ReceivedMessage IncomingMessage


type Model
    = Model
        { state : CommentState
        }


type CommentState
    = CommentHidden
    | CommentEditing CommentType
    | CommentSending SendRequestId CommentType
    | CommentSent CommentType
    | CommentSendError String CommentType


type Comment msg
    = Settings
        { model : Model
        , toMsg : Msg msg -> msg
        , nostr : Nostr.Model
        , profile : Profile
        , loginStatus : LoginStatus
        , browserEnv : BrowserEnv
        , theme : Theme
        }


new :
    { model : Model
    , toMsg : Msg msg -> msg
    , nostr : Nostr.Model
    , profile : Profile
    , loginStatus : LoginStatus
    , browserEnv : BrowserEnv
    , theme : Theme
    }
    -> Comment msg
new props =
    Settings
        { model = props.model
        , toMsg = props.toMsg
        , nostr = props.nostr
        , profile = props.profile
        , loginStatus = props.loginStatus
        , browserEnv = props.browserEnv
        , theme = props.theme
        }


init : {} -> Model
init _ =
    Model
        { state = CommentHidden
        }


show : Model -> CommentType -> Model
show (Model model) comment =
    Model { model | state = CommentEditing comment }


hide : Model -> Model
hide (Model model) =
    Model { model | state = CommentHidden }


type alias Props model msg =
    { msg : Msg msg
    , model : Model
    , toModel : Model -> model
    , toMsg : Msg msg -> msg
    , nostr : Nostr.Model
    }


update : Props model msg -> ( model, Effect msg )
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
                ( Model { model | state = CommentHidden }
                , Effect.none
                )

            UpdateComment comment ->
                ( Model { model | state = CommentEditing comment }, Effect.none )

            PostClicked pubKey ->
                case model.state of
                    CommentEditing commentData ->
                        updateWithSending props pubKey commentData

                    CommentSendError _ commentData ->
                        updateWithSending props pubKey commentData

                    _ ->
                        ( Model model, Effect.none )

            ReceivedMessage message ->
                ( updateWithMessage (Model model) message, Effect.none )


updateWithSending : Props model msg -> PubKey -> CommentType -> ( Model, Effect msg )
updateWithSending props pubKey commentData =
    let
        (Model model) =
            props.model
    in
    ( Model { model | state = CommentSending (Nostr.getLastSendRequestId props.nostr) commentData }
    , articleCommentEvent commentData
        |> SendComment (Nostr.getWriteRelayUrlsForPubKey props.nostr pubKey)
        |> Shared.Msg.SendNostrEvent
        |> Effect.sendSharedMsg
    )


updateWithMessage : Model -> IncomingMessage -> Model
updateWithMessage (Model model) message =
    case ( model.state, Nostr.External.decodeSendId message.value ) of
        ( CommentSending sendRequestId commentData, Ok sendId ) ->
            if sendRequestId == sendId then
                case message.messageType of
                    "published" ->
                        Model { model | state = CommentSent commentData }

                    "error" ->
                        case Nostr.External.decodeReason message.value of
                            Ok errorReason ->
                                Model { model | state = CommentSendError errorReason commentData }

                            Err decodingError ->
                                Model { model | state = CommentSendError (Decode.errorToString decodingError) commentData }

                    _ ->
                        Model model

            else
                Model model

        ( _, _ ) ->
            Model model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub (Msg msg)
subscriptions _ =
    Ports.receiveMessage ReceivedMessage



-- VIEW


view : Comment msg -> Html msg
view comment =
    let
        (Settings settings) =
            comment

        (Model model) =
            settings.model

        signingPubKey =
            Shared.loggedInSigningPubKey settings.loginStatus
    in
    case model.state of
        CommentHidden ->
            emptyHtml

        CommentEditing commentData ->
            viewComment comment commentData (Translations.postButtonText [ settings.browserEnv.translations ]) (signingPubKey |> Maybe.map PostClicked) Nothing
                |> Html.map settings.toMsg

        CommentSending _ commentData ->
            viewComment comment commentData (Translations.postingButtonText [ settings.browserEnv.translations ]) Nothing Nothing
                |> Html.map settings.toMsg

        CommentSent commentData ->
            viewComment comment commentData (Translations.postedButtonText [ settings.browserEnv.translations ]) Nothing Nothing
                |> Html.map settings.toMsg

        CommentSendError error commentData ->
            viewComment comment commentData (Translations.postButtonText [ settings.browserEnv.translations ]) (signingPubKey |> Maybe.map PostClicked) (Just error)
                |> Html.map settings.toMsg


viewComment : Comment msg -> CommentType -> String -> Maybe (Msg msg) -> Maybe String -> Html (Msg msg)
viewComment (Settings settings) draftComment postButtonText buttonMsg maybeError =
    let
        styles =
            stylesForTheme settings.theme

        errorMessage =
            maybeError
                |> Maybe.map
                    (\message ->
                        div
                            styles.colorStyleGrayscaleMuted
                            [ Html.text message
                            ]
                    )
                |> Maybe.withDefault emptyHtml
    in
    div
        [ css
            [ Tw.flex
            , Tw.flex_col
            , Tw.gap_2
            , Tw.self_stretch
            , Tw.max_w_full
            ]
        ]
        [ EntryField.new
            { value = commentContent draftComment
            , onInput = \content -> UpdateComment (setCommentContent draftComment content)
            , theme = settings.theme
            }
            |> EntryField.withLabel (Translations.commentLabel [ settings.browserEnv.translations ])
            |> EntryField.withPlaceholder (Translations.commentPlaceholder [ settings.browserEnv.translations ])
            |> EntryField.withRows 5
            |> EntryField.view
        , errorMessage
        , div
            [ css
                [ Tw.self_stretch
                , Tw.max_w_full
                ]
            ]
            [ Button.new
                { label = postButtonText
                , onClick = buttonMsg
                , theme = settings.theme
                }
                |> Button.view
            ]
        ]
