module Components.PicturePostDialog exposing (PicturePostDialog, PostCategory(..), Model, Msg, hide, init, new, show, subscriptions, update, view, categoryToHashtag)

import BrowserEnv exposing (BrowserEnv)
import Components.Button as Button
import Components.Dropdown as Dropdown
import Components.EntryField as EntryField
import Components.HashtagEditor as HashtagEditor
import Components.MediaSelector as MediaSelector
import Components.ModalDialog as ModalDialog
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
import Nostr.Types exposing (IncomingMessage, LoginStatus, PubKey, loggedInSigningPubKey)
import Ports
import Shared.Model exposing (Model)
import Shared.Msg exposing (Msg)
import Tailwind.Utilities as Tw
import Translations.PicturePostDialog as Translations
import Ui.Shared exposing (emptyHtml)
import Ui.Styles exposing (Theme)


type Msg
    = CloseDialog
    | UpdatePicturePost PicturePost
    | PostClicked PubKey
    | ReceivedMessage IncomingMessage
    | OpenMediaSelector
    | MediaSelectorSent (MediaSelector.Msg Msg)
    | ImageSelected MediaSelector.UploadedFile
    | CategoryDropdownSent (Dropdown.Msg PostCategory Msg)
    | LanguageDropdownSent (Dropdown.Msg Language Msg)
    | LanguageChanged (Maybe Language)
    | HashtagEditorMsg HashtagEditor.Msg

type PostCategory
    = Art
    | Culture
    | Meme
    | Food
    | Animals
    | Nature
    | Peace
    | Science
    | Technology
    | Politics
    | Sports
    | Travel


type Model
    = Model
        { state : DialogState
        , hashtagEditor : HashtagEditor.Model
        , mediaSelector : MediaSelector.Model
        , pubKey : PubKey
        , categoryDropdown : Dropdown.Model PostCategory
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
    , category : Maybe PostCategory
    } -> ( Model, Effect Msg )
init { nostr, pubKey, language, category } =
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
        , categoryDropdown = Dropdown.init { selected = category }
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

            CategoryDropdownSent innerMsg ->
                let
                    ( newModel, dropdownEffect ) =
                        Dropdown.update
                            { msg = innerMsg
                            , model = model.categoryDropdown
                            , toModel = \dropdown -> Model { model | categoryDropdown = dropdown }
                            , toMsg = CategoryDropdownSent
                            }
                in
                ( newModel, Effect.map props.toMsg dropdownEffect )

            LanguageDropdownSent innerMsg ->
                let
                    ( newModel, dropdownEffect ) =
                        Dropdown.update
                            { msg = innerMsg
                            , model = model.languageSelection
                            , toModel = \dropdown -> Model { model | languageSelection = dropdown }
                            , toMsg = LanguageDropdownSent
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
                            , modifiedMsg = Nothing
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

        -- set a hashtag for the category
        categoryHashtag =
            model.categoryDropdown
                |> Dropdown.selectedItem
                |> Maybe.map categoryToHashtag
                |> Maybe.map List.singleton
                |> Maybe.withDefault []

        -- add the hashtags from the hashtag editor
        picturePostWithHashtags =
            { picturePost | hashtags = categoryHashtag ++ (model.hashtagEditor |> HashtagEditor.getHashtags) }
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
            loggedInSigningPubKey settings.loginStatus

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
        (Model model) =
            settings.model

        errorMessage =
            maybeError
            |> Maybe.map (\error ->
                div
                    [ css
                        [ Tw.mb_4
                        ]
                    ]
                    [ Html.text <| Translations.errorMessageText [ settings.browserEnv.translations ] { errorText = error }
                    ]
            )
            |> Maybe.withDefault emptyHtml
    in
    ModalDialog.new
        { title = Translations.dialogTitle [ settings.browserEnv.translations ]
        , content =
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
                , toMsg = LanguageDropdownSent
                , choices = Locale.defaultLanguages
                , allowNoSelection = True
                , toLabel = toLanguageLabel settings.browserEnv.translations
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
            , div [ css [ Tw.flex, Tw.flex_row, Tw.gap_2, Tw.items_baseline ] ]
                [ Html.text (Translations.categoryLabel [ settings.browserEnv.translations ])
                , Dropdown.new
                    { model = model.categoryDropdown
                    , toMsg = CategoryDropdownSent
                    , choices = [ Art, Meme, Food, Animals, Nature, Peace, Science, Technology, Politics, Culture, Sports, Travel ]
                    , allowNoSelection = True
                    , toLabel = toCategoryLabel settings.browserEnv.translations
                    }
                |> Dropdown.view
                ]
            , HashtagEditor.new
                { model = model.hashtagEditor
                , toMsg = HashtagEditorMsg
                , translations = settings.browserEnv.translations
                , theme = settings.theme
                }
                |> HashtagEditor.view
            , errorMessage
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
        , onClose = CloseDialog
        , theme = settings.theme
        , buttons =
            [ Button.new
                { label = postButtonText
                , onClick = buttonMsg
                , theme = settings.theme
                }
                |> Button.withDisabled (List.isEmpty picturePost.pictures)
                |> Button.view
            ]
        }
        |> ModalDialog.view

toLanguageLabel : Translations -> Maybe Language -> String
toLanguageLabel translations maybeLanguage =
    case maybeLanguage of
        Just language ->
            languageToString translations language

        Nothing ->
            Translations.noLanguageText [ translations ]

toCategoryLabel : Translations -> Maybe PostCategory -> String
toCategoryLabel translations maybeCategory =
    case maybeCategory of
        Just category ->
            categoryToString translations category

        Nothing ->
            Translations.otherCategoryText [ translations ]

categoryToString : Translations -> PostCategory -> String
categoryToString translations category =
    case category of
        Art ->
            Translations.artCategoryText [ translations ]

        Meme ->
            Translations.memeCategoryText [ translations ]

        Food ->
            Translations.foodCategoryText [ translations ]

        Animals ->
            Translations.animalsCategoryText [ translations ]

        Nature ->
            Translations.natureCategoryText [ translations ]

        Peace ->
            Translations.peaceCategoryText [ translations ]

        Science ->
            Translations.scienceCategoryText [ translations ]

        Technology ->
            Translations.technologyCategoryText [ translations ]

        Politics ->
            Translations.politicsCategoryText [ translations ]

        Culture ->
            Translations.cultureCategoryText [ translations ]

        Sports ->
            Translations.sportsCategoryText [ translations ]

        Travel ->
            Translations.travelCategoryText [ translations ]


categoryToHashtag : PostCategory -> String
categoryToHashtag category =
    case category of
        Art ->
            "art"

        Meme ->
            "meme"

        Food ->
            "food"

        Animals ->
            "animals"

        Nature ->
            "nature"

        Peace ->
            "peace"

        Science ->
            "science"

        Technology ->
            "technology"

        Politics ->
            "politics"

        Culture ->
            "culture"

        Sports ->
            "sports"

        Travel ->
            "travel"


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
            , Attr.alt "Selected picture"
            ]
            []
        ]
