module Components.PicturePostDialog exposing (PicturePostDialog, Model, Msg, hide, init, new, show, subscriptions, update, view)

import BrowserEnv exposing (BrowserEnv)
import Components.Button as Button
import Components.Dropdown as Dropdown
import Components.EntryField as EntryField
import Components.HashtagEditor as HashtagEditor
import Components.MediaSelector as MediaSelector
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, div)
import Html.Styled.Attributes as Attr exposing (css)
import I18Next exposing (Translations)
import Json.Decode as Decode
import Locale exposing (Language(..), languageToString)
import Nostr
import Nostr.Event exposing (Kind(..), ImageMetadata)
import Nostr.External
import Nostr.Nip68 exposing (PicturePost, emptyPicturePost, picturePostEvent)
import Nostr.Send exposing (SendRequest(..), SendRequestId)
import Nostr.Types exposing (IncomingMessage, PubKey)
import Ports
import Shared
import Shared.Model exposing (LoginStatus(..), Model)
import Shared.Msg exposing (Msg)
import Tailwind.Utilities as Tw
import Translations.PicturePostDialog as Translations
import Ui.Shared exposing (emptyHtml)
import Ui.Styles exposing (Theme, stylesForTheme)


type Msg
    = CloseDialog
    | UpdatePicturePost PicturePost
    | PostClicked PubKey
    | ReceivedMessage IncomingMessage
    | OpenMediaSelector
    | MediaSelectorSent (MediaSelector.Msg Msg)
    | ImageSelected MediaSelector.UploadedFile
    | DropdownSent (Dropdown.Msg Language Msg)
    | LanguageChanged (Maybe Language)
    | HashtagEditorMsg HashtagEditor.Msg

type Model
    = Model
        { state : DialogState
        , hashtagEditor : HashtagEditor.Model
        , mediaSelector : MediaSelector.Model
        , pubKey : PubKey
        , languageSelection : Dropdown.Model Language
        }


type DialogState
    = DialogHidden
    | DialogEditing PicturePost
    | DialogSending SendRequestId PicturePost
    | DialogSent PicturePost
    | DialogSendError String PicturePost


type PicturePostDialog msg
    = Settings
        { model : Model
        , toMsg : Msg -> msg
        , nostr : Nostr.Model
        , loginStatus : LoginStatus
        , browserEnv : BrowserEnv
        , theme : Theme
        }


new :
    { model : Model
    , toMsg : Msg -> msg
    , nostr : Nostr.Model
    , loginStatus : LoginStatus
    , browserEnv : BrowserEnv
    , theme : Theme
    }
    -> PicturePostDialog msg
new props =
    Settings
        { model = props.model
        , toMsg = props.toMsg
        , nostr = props.nostr
        , loginStatus = props.loginStatus
        , browserEnv = props.browserEnv
        , theme = props.theme
        }


init :
    { nostr : Nostr.Model
    , pubKey : PubKey
    , language : Language
    } -> ( Model, Effect Msg )
init { nostr, pubKey, language } =
    let
        ( mediaSelector, mediaSelectorEffect ) =
            MediaSelector.init
                { selected = Nothing
                , toMsg = MediaSelectorSent
                , blossomServers = Nostr.getBlossomServers nostr pubKey
                , nip96Servers = Nostr.getNip96Servers nostr pubKey
                , displayType = MediaSelector.DisplayModalDialog False
                }
    in
    ( Model
        { state = DialogHidden
        , hashtagEditor = HashtagEditor.init { hashtags = [] }
        , mediaSelector = mediaSelector
        , pubKey = pubKey
        , languageSelection = Dropdown.init { selected = Just language }
        }
    , mediaSelectorEffect
    )


show : Model -> Model
show (Model model) =
    Model { model | state = DialogEditing emptyPicturePost}
        

hide : Model -> Model
hide (Model model) =
    Model { model | state = DialogHidden }


type alias Props model msg =
    { msg : Msg
    , model : Model
    , toModel : Model -> model
    , toMsg : Msg -> msg
    , nostr : Nostr.Model
    , browserEnv : BrowserEnv
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
                ( Model { model | state = DialogHidden }
                , Effect.none
                )

            UpdatePicturePost picturePost ->
                ( Model { model | state = DialogEditing picturePost }, Effect.none )


            PostClicked pubKey ->
                case model.state of
                    DialogEditing picturePostData ->
                        updateWithSending props pubKey picturePostData

                    DialogSendError _ picturePostData ->
                        updateWithSending props pubKey picturePostData

                    _ ->
                        ( Model model, Effect.none )

            ReceivedMessage message ->
                ( updateWithMessage (Model model) message, Effect.none )

            OpenMediaSelector ->
                ( Model { model | mediaSelector = MediaSelector.show model.mediaSelector }, Effect.none )

            MediaSelectorSent innerMsg ->
                let
                    ( newModel, mediaSelectorEffect ) =
                        MediaSelector.update
                            { pubKey = model.pubKey
                            , nostr = props.nostr
                            , msg = innerMsg
                            , model = model.mediaSelector
                            , toModel = \mediaSelector -> Model { model | mediaSelector = mediaSelector }
                            , toMsg = MediaSelectorSent
                            , browserEnv = props.browserEnv
                            }
                in
                ( newModel, Effect.map props.toMsg mediaSelectorEffect )

            DropdownSent innerMsg ->
                let
                    ( newModel, dropdownEffect ) =
                        Dropdown.update
                            { msg = innerMsg
                            , model = model.languageSelection
                            , toModel = \dropdown -> Model { model | languageSelection = dropdown }
                            , toMsg = DropdownSent
                            }
                in
                ( newModel, Effect.map props.toMsg dropdownEffect )


            LanguageChanged maybeLanguage ->
                case model.state of
                    DialogEditing picturePost ->
                        ( Model { model | state = DialogEditing { picturePost | language = maybeLanguage } }, Effect.none )

                    _ ->
                        ( Model model, Effect.none )


            ImageSelected uploadedFile ->
                case model.state of
                    DialogEditing picturePost ->
                        ( Model { model | state = DialogEditing (addPicturePostPicture picturePost uploadedFile) }
                        , Effect.none
                        )

                    _ ->
                        ( Model model, Effect.none )

            HashtagEditorMsg innerMsg ->
                let
                    ( newModel, dropdownEffect ) =
                        HashtagEditor.update
                            { msg = innerMsg
                            , model = model.hashtagEditor
                            , toModel = \hashtagEditor -> Model { model | hashtagEditor = hashtagEditor }
                            , toMsg = HashtagEditorMsg
                            }
                in
                ( newModel, Effect.map props.toMsg dropdownEffect )

addPicturePostPicture : PicturePost -> MediaSelector.UploadedFile -> PicturePost
addPicturePostPicture picturePost uploadedFile =
    { picturePost | pictures = picturePost.pictures ++ [ pictureFromUploadedFile uploadedFile ] }

pictureFromUploadedFile : MediaSelector.UploadedFile -> ImageMetadata
pictureFromUploadedFile uploadedFile =
    case uploadedFile of
        MediaSelector.BlossomFile blobDescriptor ->
            { url = blobDescriptor.url
            , mimeType = blobDescriptor.nip94 |> Maybe.andThen .mimeType
            , blurHash = blobDescriptor.nip94 |> Maybe.andThen .blurhash
            , dim = blobDescriptor.nip94 |> Maybe.andThen .dim
            , alt = blobDescriptor.nip94 |> Maybe.andThen .alt
            , x = Just blobDescriptor.sha256
            , fallbacks = blobDescriptor.nip94 |> Maybe.andThen .fallbacks |> Maybe.withDefault []
            }

        MediaSelector.Nip96File fileMetadata ->
            { url = fileMetadata.url |> Maybe.withDefault ""
            , mimeType = fileMetadata.mimeType
            , blurHash = fileMetadata.blurhash
            , dim = fileMetadata.dim
            , alt = fileMetadata.alt
            , x = fileMetadata.xHash
            , fallbacks = fileMetadata.fallbacks |> Maybe.withDefault []
            }


updateWithSending : Props model msg -> PubKey -> PicturePost -> ( Model, Effect msg )
updateWithSending props pubKey picturePost =
    let
        (Model model) =
            props.model

        picturePostWithHashtags =
            { picturePost | hashtags = model.hashtagEditor |> HashtagEditor.getHashtags }
    in
    ( Model { model | state = DialogSending (Nostr.getLastSendRequestId props.nostr) picturePostWithHashtags }
    , picturePostEvent model.pubKey picturePostWithHashtags
        |> SendComment (Nostr.getWriteRelayUrlsForPubKey props.nostr pubKey)
        |> Shared.Msg.SendNostrEvent
        |> Effect.sendSharedMsg
    )


updateWithMessage : Model -> IncomingMessage -> Model
updateWithMessage (Model model) message =
    case ( model.state, Nostr.External.decodeSendId message.value ) of
        ( DialogSending sendRequestId commentData, Ok sendId ) ->
            if sendRequestId == sendId then
                case message.messageType of
                    "published" ->
                        Model
                            { model
                                | state = DialogHidden

                                {- DialogSent commentData -}
                            }

                    "error" ->
                        case Nostr.External.decodeReason message.value of
                            Ok errorReason ->
                                Model { model | state = DialogSendError errorReason commentData }

                            Err decodingError ->
                                Model { model | state = DialogSendError (Decode.errorToString decodingError) commentData }

                    _ ->
                        Model model

            else
                Model model

        ( _, _ ) ->
            Model model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions (Model model) =
    Sub.batch
        [ Ports.receiveMessage ReceivedMessage
        , Sub.map MediaSelectorSent (MediaSelector.subscribe model.mediaSelector)
        ]



-- VIEW


view : PicturePostDialog msg -> Html msg
view picturePostDialog =
    let
        (Settings settings) =
            picturePostDialog

        (Model model) =
            settings.model

        signingPubKey =
            Shared.loggedInSigningPubKey settings.loginStatus

        postButtonMsg =
            signingPubKey |> Maybe.map PostClicked
    in
    case model.state of
        DialogHidden ->
            emptyHtml

        DialogEditing picturePost ->
            viewPicturePostDialog picturePostDialog picturePost (Translations.postButtonTitle [ settings.browserEnv.translations ]) postButtonMsg Nothing
                |> Html.map settings.toMsg

        DialogSending _ picturePost ->
            viewPicturePostDialog picturePostDialog picturePost (Translations.postingButtonText [ settings.browserEnv.translations ]) Nothing Nothing
                |> Html.map settings.toMsg

        DialogSent picturePost ->
            viewPicturePostDialog picturePostDialog picturePost (Translations.postedButtonText [ settings.browserEnv.translations ]) Nothing Nothing
                |> Html.map settings.toMsg

        DialogSendError error picturePost ->
            viewPicturePostDialog picturePostDialog picturePost (Translations.postButtonTitle [ settings.browserEnv.translations ]) postButtonMsg (Just error)
                |> Html.map settings.toMsg


viewPicturePostDialog : PicturePostDialog msg -> PicturePost -> String -> Maybe Msg -> Maybe String -> Html Msg
viewPicturePostDialog (Settings settings) picturePost postButtonText buttonMsg maybeError =
    let
        styles =
            stylesForTheme settings.theme

        (Model model) =
            settings.model
    in
    Ui.Shared.modalDialog settings.theme (Translations.dialogTitle [ settings.browserEnv.translations ])
        [ div
            [ css
                [ Tw.flex
                , Tw.flex_col
                , Tw.gap_2
                , Tw.self_stretch
                , Tw.h_auto
                , Tw.max_w_full
                ]
            ]
            [ EntryField.new
                { value = picturePost.title |> Maybe.withDefault ""
                , onInput = \content -> UpdatePicturePost (setPicturePostTitle picturePost content)
                , theme = settings.theme
                }
                |> EntryField.withLabel (Translations.titleLabel [ settings.browserEnv.translations ])
                |> EntryField.withPlaceholder (Translations.titlePlaceholder [ settings.browserEnv.translations ])
                |> EntryField.view
            , viewPictures settings.theme settings.browserEnv.translations picturePost
            , Dropdown.new
                { model = model.languageSelection
                , toMsg = DropdownSent
                , choices = Locale.defaultLanguages
                , allowNoSelection = True
                , toLabel = toLabel settings.browserEnv.translations
                }
                |> Dropdown.withOnChange LanguageChanged
                |> Dropdown.view
            , EntryField.new
                { value = picturePost.description
                , onInput = \content -> UpdatePicturePost (setPicturePostDescription picturePost content)
                , theme = settings.theme
                }
                --|> EntryField.withLabel (Translations.dialogTitle [ settings.browserEnv.translations ])
                --|> EntryField.withPlaceholder (Translations.dialogTitle [ settings.browserEnv.translations ])
                |> EntryField.withRows 5
                |> EntryField.view
            , HashtagEditor.new
                { model = model.hashtagEditor
                , toMsg = HashtagEditorMsg
                , translations = settings.browserEnv.translations
                , theme = settings.theme
                }
                |> HashtagEditor.view
            , div
                [ css
                    [ Tw.flex
                    , Tw.items_center
                    ]
                ]
                [ Button.new
                    { label = postButtonText
                    , onClick = buttonMsg
                    , theme = settings.theme
                    }
                    |> Button.withDisabled (List.isEmpty picturePost.pictures)
                    |> Button.view
                ]
            , MediaSelector.new
                { model = model.mediaSelector
                , toMsg = MediaSelectorSent
                , onSelected = Just ImageSelected
                , pubKey = model.pubKey
                , browserEnv = settings.browserEnv
                , theme = settings.theme
                }
                |> MediaSelector.view
            ]
       ]
       CloseDialog

toLabel : Translations -> Maybe Language -> String
toLabel translations maybeLanguage =
    case maybeLanguage of
        Just language ->
            languageToString translations language

        Nothing ->
            Translations.noLanguageText [ translations ]

setPicturePostDescription : PicturePost -> String -> PicturePost
setPicturePostDescription picturePost description =
    { picturePost | description = description }

setPicturePostTitle : PicturePost -> String -> PicturePost
setPicturePostTitle picturePost title =
    { picturePost | title = Just title }

viewPictures : Theme -> Translations -> PicturePost -> Html Msg
viewPictures theme translations picturePost =
    div
        [ css
            [ Tw.flex
            , Tw.flex_row
            , Tw.gap_2
            , Tw.overflow_x_auto
            , Tw.overflow_y_hidden
            , Tw.h_16
            ]
        ]
        [ div
            [ css
                [ Tw.flex
                , Tw.flex_row
                , Tw.gap_2
                ]
            ]
            (List.map viewPicture picturePost.pictures)
        , div
            [ css
                [ Tw.flex
                , Tw.items_center
                , Tw.justify_center
                ]
            ]
            [ Button.new
                { label = Translations.addPictureButtonLabel [ translations ]
                , onClick = Just OpenMediaSelector
                , theme = theme
                }
                |> Button.view
            ]
        ]

viewPicture : ImageMetadata -> Html msg
viewPicture imageMetadata =
    div
        [ css
            [ Tw.w_16
            , Tw.h_16
            , Tw.rounded_md
            ]
        ]
        [ Html.img
            [ Attr.src imageMetadata.url
            ]
            []
        ]
