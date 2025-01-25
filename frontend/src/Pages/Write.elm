module Pages.Write exposing (Model, Msg, page)

import Auth
import BrowserEnv exposing (BrowserEnv)
import Components.Button as Button
import Components.Dropdown
import Components.MediaSelector as MediaSelector exposing (UploadedFile(..))
import Components.MessageDialog as MessageDialog
import Components.PublishArticleDialog as PublishArticleDialog exposing (PublishArticleDialog)
import Css
import Dict exposing (Dict)
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, a, aside, button, code, div, h2, h3, h4, img, input, label, node, p, span, text, textarea)
import Html.Styled.Attributes as Attr exposing (class, css, style)
import Html.Styled.Events as Events exposing (..)
import Json.Decode as Decode
import Layouts
import LinkPreview exposing (LoadedContent)
import Locale exposing (Language(..), showLanguage)
import Milkdown.MilkdownEditor as Milkdown
import Nostr
import Nostr.Article exposing (Article, articleFromEvent)
import Nostr.DeletionRequest exposing (draftDeletionEvent)
import Nostr.Event as Event exposing (Event, Kind(..), Tag(..), numberForKind)
import Nostr.Nip19 as Nip19 exposing (NIP19Type(..))
import Nostr.Request exposing (RequestData(..), RequestId)
import Nostr.Send exposing (SendRequest(..), SendRequestId)
import Nostr.Types exposing (EventId, IncomingMessage, PubKey, RelayUrl)
import Page exposing (Page)
import Pareto
import Ports
import Route exposing (Route)
import Set
import Shared
import Shared.Msg
import Svg.Loaders as Loaders
import Tailwind.Breakpoints as Bp
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw
import Time
import Translations.Write as Translations
import Ui.Article
import Ui.Styles exposing (Theme, stylesForTheme)
import View exposing (View)


page : Auth.User -> Shared.Model -> Route () -> Page Model Msg
page user shared route =
    Page.new
        { init = init user shared route
        , update = update shared user
        , subscriptions = subscriptions
        , view = view user shared
        }
        |> Page.withLayout (toLayout shared.theme)


toLayout : Theme -> Model -> Layouts.Layout Msg
toLayout theme model =
    Layouts.Sidebar
        { styles = Ui.Styles.stylesForTheme theme }



-- INIT


type alias Model =
    { draftEventId : Maybe EventId -- event ID of draft (when editing one)
    , title : Maybe String
    , summary : Maybe String
    , image : Maybe String
    , content : Maybe String
    , milkdown : Milkdown.Model
    , identifier : Maybe String
    , tags : Maybe String
    , zapWeights : List ( PubKey, RelayUrl, Maybe Int )
    , otherTags : List Tag
    , now : Time.Posix
    , mediaSelector : MediaSelector.Model
    , imageSelection : Maybe ImageSelection
    , publishArticleDialog : PublishArticleDialog.Model Msg
    , articleState : ArticleState
    , editorMode : EditorMode
    , loadedContent : LoadedContent Msg
    , modalDialog : ModalDialog
    , languageSelection : Components.Dropdown.Model Language
    }


type EditorMode
    = Editor
    | Preview


type ModalDialog
    = NoModalDialog
    | PublishedDialog


type ArticleState
    = ArticleEmpty
    | ArticleModified
    | ArticleLoadingDraft RequestId
    | ArticleLoadingDraftError String
    | ArticleSavingDraft SendRequestId
    | ArticleDraftSaveError String
    | ArticleDraftSaved
    | ArticlePublishing SendRequestId
    | ArticlePublishingError String
    | ArticlePublished
    | ArticleDeletingDraft SendRequestId


type ImageSelection
    = ArticleImageSelection
    | MarkdownImageSelection


init : Auth.User -> Shared.Model -> Route () -> () -> ( Model, Effect Msg )
init user shared route () =
    let
        maybeNip19 =
            case Dict.get "a" route.query of
                Just address ->
                    Nip19.decode address
                        |> Result.toMaybe

                Nothing ->
                    Nothing

        maybeDraftId =
            maybeNip19
                |> Maybe.andThen
                    (\nip19 ->
                        case nip19 of
                            NAddr addrData ->
                                Just addrData

                            _ ->
                                Nothing
                    )

        maybeArticle =
            maybeNip19
                |> Maybe.andThen (\nip19 -> Nostr.getArticleForNip19 shared.nostr nip19)

        effect =
            case ( maybeArticle, maybeNip19 ) of
                ( Nothing, Just (NAddr naddrData) ) ->
                    Event.eventFilterForNaddr naddrData
                        |> RequestArticle (Just naddrData.relays)
                        |> Nostr.createRequest shared.nostr "Article described as NIP-19 for editing" []
                        |> Shared.Msg.RequestNostrEvents
                        |> Effect.sendSharedMsg

                ( _, _ ) ->
                    Effect.none

        ( mediaSelector, mediaSelectorEffect ) =
            MediaSelector.init
                { selected = Nothing
                , toMsg = MediaSelectorSent
                , blossomServers = Nostr.getBlossomServers shared.nostr user.pubKey
                , nip96Servers = Nostr.getNip96Servers shared.nostr user.pubKey
                , displayType = MediaSelector.DisplayModalDialog False
                }

        publishArticleDialog =
            PublishArticleDialog.init {}

        model =
            case maybeArticle of
                Just article ->
                    { draftEventId = Just article.id
                    , title = article.title
                    , summary = article.summary
                    , image = article.image
                    , content = Just article.content
                    , milkdown = Milkdown.init
                    , identifier = article.identifier
                    , tags = tagsString article.hashtags
                    , otherTags = article.otherTags
                    , zapWeights =
                        if List.length article.zapWeights > 0 then
                            article.zapWeights

                        else
                            defaultZapWeights user.pubKey
                    , now = Time.millisToPosix 0
                    , mediaSelector = mediaSelector
                    , imageSelection = Nothing
                    , publishArticleDialog = publishArticleDialog
                    , articleState = ArticleDraftSaved
                    , editorMode = Editor
                    , loadedContent = { loadedUrls = Set.empty, addLoadedContentFunction = AddLoadedContent }
                    , modalDialog = NoModalDialog
                    , languageSelection = Components.Dropdown.init { selected = article.language }
                    }

                Nothing ->
                    { draftEventId = Nothing
                    , title = Nothing
                    , summary = Nothing
                    , image = Nothing
                    , content = Nothing
                    , milkdown = Milkdown.init
                    , identifier = Nothing
                    , tags = Nothing
                    , otherTags = []
                    , zapWeights = defaultZapWeights user.pubKey
                    , now = Time.millisToPosix 0
                    , mediaSelector = mediaSelector
                    , imageSelection = Nothing
                    , publishArticleDialog = publishArticleDialog
                    , articleState = ArticleEmpty
                    , editorMode = Editor
                    , loadedContent = { loadedUrls = Set.empty, addLoadedContentFunction = AddLoadedContent }
                    , modalDialog = NoModalDialog
                    , languageSelection = Components.Dropdown.init { selected = Nothing }
                    }
    in
    ( model
    , Effect.batch
        [ mediaSelectorEffect
        , effect
        ]
    )


defaultZapWeights : PubKey -> List ( PubKey, RelayUrl, Maybe Int )
defaultZapWeights pubKey =
    if pubKey == Pareto.paretoPubKey then
        [ ( Pareto.paretoPubKey, Pareto.paretoRelay, Just 1 )
        ]

    else
        [ ( pubKey, Pareto.paretoRelay, Just 80 )
        , ( Pareto.paretoPubKey, Pareto.paretoRelay, Just 20 )
        ]


tagsString : List String -> Maybe String
tagsString tags =
    case tags of
        [] ->
            Nothing

        [ tag ] ->
            Just tag

        tagList ->
            Just <| String.join ", " tagList



-- UPDATE


type Msg
    = EditorChanged String
    | EditorFocused
    | EditorBlurred
    | EditorLoaded
    | ToggleEditorMode
    | AddLoadedContent String
    | UpdateTitle String
    | UpdateSubtitle String
    | UpdateTags String
    | SelectImage ImageSelection
    | ImageSelected ImageSelection MediaSelector.UploadedFile
    | Publish
    | PublishArticle (List RelayUrl)
    | SaveDraft
    | Now Time.Posix
    | OpenMediaSelector
    | MediaSelectorSent (MediaSelector.Msg Msg)
    | ReceivedPortMessage IncomingMessage
    | MilkdownSent (Milkdown.Msg Msg)
    | PublishArticleDialogSent (PublishArticleDialog.Msg Msg)
    | PublishedDialogButtonClicked PublishedDialogButton
    | DropdownSent (Components.Dropdown.Msg Language Msg)


type PublishedDialogButton
    = OkButton


update : Shared.Model -> Auth.User -> Msg -> Model -> ( Model, Effect Msg )
update shared user msg model =
    case msg of
        EditorChanged newContent ->
            if newContent == "" then
                ( { model | articleState = ArticleModified, content = Nothing }, Effect.none )

            else
                ( { model | articleState = ArticleModified, content = Just newContent }, Effect.none )

        EditorFocused ->
            ( model, Effect.none )

        EditorBlurred ->
            ( model, Effect.none )

        EditorLoaded ->
            ( model, Effect.none )

        ToggleEditorMode ->
            case model.editorMode of
                Editor ->
                    ( { model | editorMode = Preview }, Effect.none )

                Preview ->
                    ( { model | editorMode = Editor }, Effect.none )

        AddLoadedContent url ->
            ( { model | loadedContent = LinkPreview.addLoadedContent model.loadedContent url }, Effect.none )

        UpdateTitle title ->
            if title == "" then
                ( { model | articleState = ArticleModified, title = Nothing }, Effect.none )

            else
                ( { model | articleState = ArticleModified, title = Just <| filterTitleChars title }, Effect.none )

        UpdateSubtitle summary ->
            if summary == "" then
                ( { model | articleState = ArticleModified, summary = Nothing }, Effect.none )

            else
                ( { model | articleState = ArticleModified, summary = Just <| filterTitleChars summary }, Effect.none )

        UpdateTags tags ->
            if tags == "" then
                ( { model | articleState = ArticleModified, tags = Nothing }, Effect.none )

            else
                ( { model | articleState = ArticleModified, tags = Just tags }, Effect.none )

        SelectImage imageSelection ->
            ( { model
                | mediaSelector = MediaSelector.show model.mediaSelector
                , imageSelection = Just imageSelection
                , articleState = ArticleModified
              }
            , Effect.none
            )

        ImageSelected imageSelection uploadedFile ->
            case imageSelection of
                ArticleImageSelection ->
                    case uploadedFile of
                        BlossomFile blobDescriptor ->
                            ( { model | image = Just blobDescriptor.url }, Effect.none )

                        Nip96File fileMetadata ->
                            ( { model | image = fileMetadata.url }, Effect.none )

                MarkdownImageSelection ->
                    case uploadedFile of
                        BlossomFile blobDescriptor ->
                            case blobDescriptor.nip94 of
                                Just fileMetadata ->
                                    ( { model | milkdown = Milkdown.setSelectedImage model.milkdown blobDescriptor.url fileMetadata.content fileMetadata.alt }, Effect.none )

                                Nothing ->
                                    ( { model | milkdown = Milkdown.setSelectedImage model.milkdown blobDescriptor.url "" Nothing }, Effect.none )

                        Nip96File fileMetadata ->
                            case fileMetadata.url of
                                Just url ->
                                    ( { model | milkdown = Milkdown.setSelectedImage model.milkdown url fileMetadata.content fileMetadata.alt }, Effect.none )

                                Nothing ->
                                    ( model, Effect.none )

        Publish ->
            ( { model | publishArticleDialog = PublishArticleDialog.show model.publishArticleDialog }, Effect.none )

        PublishArticle relayUrls ->
            ( { model | articleState = ArticlePublishing (Nostr.getLastSendRequestId shared.nostr) }, sendPublishCmd shared model user relayUrls )

        SaveDraft ->
            ( { model | articleState = ArticleSavingDraft (Nostr.getLastSendRequestId shared.nostr) }, sendDraftCmd shared model user )

        Now now ->
            if model.identifier == Nothing then
                let
                    identifier =
                        now
                            |> Time.posixToMillis
                            |> String.fromInt
                in
                ( { model
                    | now = now
                    , identifier = Just identifier
                  }
                , Effect.none
                )

            else
                ( { model | now = now }, Effect.none )

        OpenMediaSelector ->
            ( { model | mediaSelector = MediaSelector.show model.mediaSelector }, Effect.none )

        MediaSelectorSent innerMsg ->
            MediaSelector.update
                { user = user
                , nostr = shared.nostr
                , msg = innerMsg
                , model = model.mediaSelector
                , toModel = \mediaSelector -> { model | mediaSelector = mediaSelector }
                , toMsg = MediaSelectorSent
                , browserEnv = shared.browserEnv
                }

        ReceivedPortMessage portMessage ->
            updateWithPortMessage shared model user portMessage

        MilkdownSent innerMsg ->
            let
                ( milkdown, cmd ) =
                    Milkdown.update innerMsg model.milkdown
            in
            ( { model | milkdown = milkdown }, Effect.sendCmd cmd )

        PublishArticleDialogSent innerMsg ->
            PublishArticleDialog.update
                { msg = innerMsg
                , model = model.publishArticleDialog
                , toModel = \publishArticleDialog -> { model | publishArticleDialog = publishArticleDialog }
                , toMsg = PublishArticleDialogSent
                , nostr = shared.nostr
                , pubKey = user.pubKey
                }

        PublishedDialogButtonClicked _ ->
            ( { model | modalDialog = NoModalDialog }, Effect.none )

        DropdownSent innerMsg ->
            Components.Dropdown.update
                { msg = innerMsg
                , model = model.languageSelection
                , toModel = \dropdown -> { model | languageSelection = dropdown }
                , toMsg = DropdownSent
                }


updateWithPortMessage : Shared.Model -> Model -> Auth.User -> IncomingMessage -> ( Model, Effect Msg )
updateWithPortMessage shared model user portMessage =
    case portMessage.messageType of
        "published" ->
            updateWithPublishedResults shared model user portMessage.value

        "events" ->
            -- TODO: This code has not been tested because the a=naddr1 query parameter
            -- is lost while logging in. After preserving the query parameters while logging
            -- in the article should appear here
            case
                ( Decode.decodeValue (Decode.field "requestId" Decode.int) portMessage.value
                , Decode.decodeValue (Decode.field "kind" Event.kindDecoder) portMessage.value
                , model.articleState
                )
            of
                ( Ok requestId, Ok KindDraftLongFormContent, ArticleLoadingDraft draftRequestId ) ->
                    if requestId == draftRequestId then
                        updateModelWithDraftRequest model portMessage.value

                    else
                        ( model, Effect.none )

                ( _, _, _ ) ->
                    ( model, Effect.none )

        _ ->
            ( model, Effect.none )


updateModelWithDraftRequest : Model -> Decode.Value -> ( Model, Effect Msg )
updateModelWithDraftRequest model value =
    case Decode.decodeValue (Decode.field "events" (Decode.list Event.decodeEvent)) value of
        Ok draftEvents ->
            let
                maybeDraft =
                    draftEvents
                        |> List.head
                        |> Maybe.map articleFromEvent
                        |> Maybe.andThen Result.toMaybe
            in
            case maybeDraft of
                Just draft ->
                    ( { model
                        | articleState = ArticleDraftSaved
                        , draftEventId = Just draft.id
                        , title = draft.title
                        , summary = draft.summary
                        , image = draft.image
                        , content = Just draft.content
                        , identifier = draft.identifier
                        , tags = tagsString draft.hashtags
                      }
                    , Effect.none
                    )

                Nothing ->
                    ( { model | articleState = ArticleLoadingDraftError "Can't load draft" }, Effect.none )

        Err error ->
            ( { model | articleState = ArticleLoadingDraftError (Decode.errorToString error) }, Effect.none )


updateWithPublishedResults : Shared.Model -> Model -> Auth.User -> Decode.Value -> ( Model, Effect Msg )
updateWithPublishedResults shared model user value =
    let
        receivedSendRequestId =
            Decode.decodeValue (Decode.field "sendId" Decode.int) value
                |> Result.toMaybe

        receivedDraftEventId =
            Decode.decodeValue (Decode.at [ "event", "id" ] Decode.string) value
                |> Result.toMaybe

        -- TODO: Error handling
        -- - not on enough relays published?
    in
    case model.articleState of
        ArticleSavingDraft sendRequestId ->
            if Just sendRequestId == receivedSendRequestId then
                ( { model
                    | articleState = ArticleDraftSaved
                    , draftEventId = receivedDraftEventId
                  }
                , Effect.none
                )

            else
                ( model, Effect.none )

        ArticlePublishing sendRequestId ->
            if Just sendRequestId == receivedSendRequestId then
                case model.draftEventId of
                    Just _ ->
                        ( { model
                            | articleState = ArticleDeletingDraft (Nostr.getLastSendRequestId shared.nostr)
                          }
                          -- after publishing article, delete draft
                        , sendDraftDeletionCmd shared model user
                        )

                    Nothing ->
                        ( { model
                            | articleState = ArticlePublished
                            , publishArticleDialog = PublishArticleDialog.hide model.publishArticleDialog
                            , modalDialog = PublishedDialog
                          }
                        , Effect.none
                        )

            else
                ( model, Effect.none )

        ArticleDeletingDraft sendRequestId ->
            if Just sendRequestId == receivedSendRequestId then
                ( { model
                    | articleState = ArticlePublished
                    , publishArticleDialog = PublishArticleDialog.hide model.publishArticleDialog
                    , modalDialog = PublishedDialog
                  }
                , Effect.none
                )

            else
                ( model, Effect.none )

        _ ->
            ( model, Effect.none )


sendPublishCmd : Shared.Model -> Model -> Auth.User -> List RelayUrl -> Effect Msg
sendPublishCmd shared model user relayUrls =
    eventWithContent shared model user KindLongFormContent
        |> SendLongFormArticle relayUrls
        |> Shared.Msg.SendNostrEvent
        |> Effect.sendSharedMsg


sendDraftCmd : Shared.Model -> Model -> Auth.User -> Effect Msg
sendDraftCmd shared model user =
    eventWithContent shared model user KindDraftLongFormContent
        |> SendLongFormDraft Pareto.defaultRelays
        |> Shared.Msg.SendNostrEvent
        |> Effect.sendSharedMsg


sendDraftDeletionCmd : Shared.Model -> Model -> Auth.User -> Effect Msg
sendDraftDeletionCmd shared model user =
    case model.draftEventId of
        Just draftEventId ->
            draftDeletionEvent user.pubKey shared.browserEnv.now draftEventId "Deleting draft after publishing article" model.identifier
                |> SendDeletionRequest (Nostr.getDraftRelayUrls shared.nostr draftEventId)
                |> Shared.Msg.SendNostrEvent
                |> Effect.sendSharedMsg

        Nothing ->
            Effect.none


eventWithContent : Shared.Model -> Model -> Auth.User -> Kind -> Event
eventWithContent shared model user kind =
    { pubKey = user.pubKey
    , createdAt = shared.browserEnv.now
    , kind = kind
    , tags =
        []
            |> Event.addTitleTag model.title
            |> Event.addSummaryTag model.summary
            |> Event.addImageTag model.image
            |> Event.addIdentifierTag model.identifier
            |> Event.addHashtagsToTags model.tags
            |> Event.addZapTags model.zapWeights
            |> Event.addClientTag Pareto.client Pareto.paretoPubKey Pareto.handlerIdentifier Pareto.paretoRelay
            |> Event.addAltTag (altText model.identifier user.pubKey kind [ Pareto.paretoRelay ])
    , content = model.content |> Maybe.withDefault ""
    , id = ""
    , sig = Nothing
    , relay = Nothing
    }


altText : Maybe String -> PubKey -> Kind -> List String -> String
altText maybeIdentifier pubKey kind relays =
    case maybeIdentifier of
        Just identifier ->
            case Nip19.encode (Nip19.NAddr { identifier = identifier, pubKey = pubKey, kind = numberForKind kind, relays = relays }) of
                Ok nip19 ->
                    "This is a long form article, you can read it in " ++ Pareto.applicationUrl ++ "/a/" ++ nip19

                Err _ ->
                    "This is a long form article, you can read it on " ++ Pareto.applicationUrl

        Nothing ->
            "This is a long form article, you can read it on " ++ Pareto.applicationUrl


filterTitleChars : String -> String
filterTitleChars title =
    title
        |> String.trimLeft
        |> String.replace "\n" ""



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 1000 Now
        , Sub.map MediaSelectorSent (MediaSelector.subscribe model.mediaSelector)
        , Ports.receiveMessage ReceivedPortMessage
        ]



-- VIEW


view : Auth.User -> Shared.Model -> Model -> View Msg
view user shared model =
    { title = Translations.pageTitle [ shared.browserEnv.translations ]
    , body =
        [ div
            [ css
                [ Tw.mx_10
                , Tw.mt_12
                , Tw.space_y_5
                , Bp.xl
                    [ Tw.mx_40
                    ]
                , Bp.md
                    [ Tw.mx_20
                    ]
                ]
            ]
            [ div
                [ css
                    [ Tw.flex
                    , Tw.flex_row
                    , Tw.gap_4
                    ]
                ]
                [ div
                    [ css
                        [ Tw.flex
                        , Tw.flex_col
                        , Tw.w_full
                        ]
                    ]
                    [ viewTitle shared.theme shared.browserEnv model
                    , viewSubtitle shared.theme shared.browserEnv model
                    ]
                , div
                    [ css
                        [ Tw.flex
                        , Tw.flex_col
                        , Tw.w_96
                        ]
                    ]
                    [ viewImage model
                    ]
                ]
            , viewEditor shared.theme shared.browserEnv model
            , Components.Dropdown.new
                { model = model.languageSelection
                , toMsg = DropdownSent
                , choices = [ English "US", German "DE" ]
                , toLabel = showLanguage
                }
                |> Components.Dropdown.view
            , viewTags shared.theme shared.browserEnv model
            , viewArticleState shared.browserEnv shared.theme model.articleState
            , saveButtons shared.browserEnv shared.theme model
            , viewMediaSelector user shared model
            ]
        , PublishArticleDialog.new
            { model = model.publishArticleDialog
            , toMsg = PublishArticleDialogSent
            , onPublish = PublishArticle
            , nostr = shared.nostr
            , pubKey = user.pubKey
            , browserEnv = shared.browserEnv
            , theme = shared.theme
            }
            |> PublishArticleDialog.view
        , viewModalDialog shared.theme shared.browserEnv model.modalDialog
        ]
    }


viewModalDialog : Theme -> BrowserEnv -> ModalDialog -> Html Msg
viewModalDialog theme browserEnv modalDialog =
    case modalDialog of
        NoModalDialog ->
            div [] []

        PublishedDialog ->
            MessageDialog.new
                { onClick = PublishedDialogButtonClicked
                , onClose = PublishedDialogButtonClicked OkButton
                , title = Translations.articlePublishedMessageBoxTitle [ browserEnv.translations ]
                , content = div [] [ text <| Translations.articlePublishedMessageBoxText [ browserEnv.translations ] ]
                , buttons =
                    [ { style = MessageDialog.PrimaryButton
                      , title = "Ok"
                      , identifier = OkButton
                      }
                    ]
                , theme = theme
                }
                |> MessageDialog.view


viewArticleState : BrowserEnv -> Theme -> ArticleState -> Html Msg
viewArticleState browserEnv theme articleState =
    let
        styles =
            stylesForTheme theme
    in
    case ( articleStateToString browserEnv articleState, articleStateProcessIndicator articleState ) of
        ( Just articleStateString, Just processIndicator ) ->
            div
                (styles.colorStyleGrayscaleMuted
                    ++ styles.textStyle14
                    ++ [ css
                            [ Tw.flex
                            , Tw.flex_row
                            , Tw.items_center
                            ]
                       ]
                )
                [ processIndicator
                , text articleStateString
                ]

        ( Just articleStateString, Nothing ) ->
            div
                (styles.colorStyleGrayscaleMuted
                    ++ styles.textStyle14
                    ++ []
                )
                [ text articleStateString
                ]

        ( Nothing, Just processIndicator ) ->
            processIndicator

        ( Nothing, Nothing ) ->
            div [] []


articleStateToString : BrowserEnv -> ArticleState -> Maybe String
articleStateToString browserEnv articleState =
    case articleState of
        ArticleEmpty ->
            Nothing

        ArticleModified ->
            Just <| Translations.articleModifiedState [ browserEnv.translations ]

        ArticleLoadingDraft _ ->
            Just <| Translations.articleLoadingDraftState [ browserEnv.translations ]

        ArticleLoadingDraftError error ->
            Just <| Translations.articleLoadingDraftError [ browserEnv.translations ] ++ ": " ++ error

        ArticleSavingDraft _ ->
            Just <| Translations.articleSavingDraftState [ browserEnv.translations ]

        ArticleDraftSaved ->
            Just <| Translations.articleDraftSavedState [ browserEnv.translations ]

        ArticleDraftSaveError error ->
            Just <| Translations.articleDraftSaveError [ browserEnv.translations ] ++ ": " ++ error

        ArticlePublishing _ ->
            Just <| Translations.articlePublishingState [ browserEnv.translations ]

        ArticlePublishingError error ->
            Just <| Translations.articlePublishingError [ browserEnv.translations ] ++ ": " ++ error

        ArticlePublished ->
            Just <| Translations.articlePublishedState [ browserEnv.translations ]

        ArticleDeletingDraft _ ->
            Just <| Translations.articleDeletingDraftState [ browserEnv.translations ]


articleStateProcessIndicator : ArticleState -> Maybe (Html Msg)
articleStateProcessIndicator articleState =
    case articleState of
        ArticleEmpty ->
            Nothing

        ArticleModified ->
            Nothing

        ArticleLoadingDraft _ ->
            Just <| (Loaders.rings [] |> Html.fromUnstyled)

        ArticleLoadingDraftError error ->
            Nothing

        ArticleSavingDraft _ ->
            Just <| (Loaders.rings [] |> Html.fromUnstyled)

        ArticleDraftSaved ->
            Nothing

        ArticleDraftSaveError error ->
            Nothing

        ArticlePublishing _ ->
            Just <| (Loaders.rings [] |> Html.fromUnstyled)

        ArticlePublishingError error ->
            Nothing

        ArticlePublished ->
            Nothing

        ArticleDeletingDraft _ ->
            Just <| (Loaders.rings [] |> Html.fromUnstyled)


viewMediaSelector : Auth.User -> Shared.Model -> Model -> Html Msg
viewMediaSelector user shared model =
    case model.imageSelection of
        Just imageSelection ->
            MediaSelector.new
                { model = model.mediaSelector
                , toMsg = MediaSelectorSent
                , onSelected = Just (ImageSelected imageSelection)
                , pubKey = user.pubKey
                , browserEnv = shared.browserEnv
                , theme = shared.theme
                }
                |> MediaSelector.view

        Nothing ->
            div [] []


viewTitle : Theme -> BrowserEnv -> Model -> Html Msg
viewTitle theme browserEnv model =
    let
        ( foreground, background ) =
            if browserEnv.darkMode then
                ( "white", "black" )

            else
                ( "black", "white" )
    in
    div
        [ css
            [ Tw.w_full
            ]
        ]
        [ node "auto-resize-textarea"
            [ Attr.attribute "value" (model.title |> Maybe.withDefault "")
            , Attr.attribute "color" foreground
            , Attr.attribute "backgroundcolor" background
            , Attr.attribute "fontfamily" "Inter"
            , Attr.attribute "fontsize" "36px"
            , Attr.attribute "fontWeight" "700"
            , Attr.attribute "placeholder" (Translations.editorTitlePlaceholderText [ browserEnv.translations ])
            , on "input-change" (Decode.map UpdateTitle decodeInputChange)
            ]
            []
        ]


viewSubtitle : Theme -> BrowserEnv -> Model -> Html Msg
viewSubtitle theme browserEnv model =
    let
        ( foreground, background ) =
            if browserEnv.darkMode then
                ( "white", "black" )

            else
                ( "black", "white" )
    in
    div
        [ css
            [ Tw.w_full
            ]
        ]
        [ node "auto-resize-textarea"
            [ Attr.attribute "value" (model.summary |> Maybe.withDefault "")
            , Attr.attribute "color" foreground
            , Attr.attribute "backgroundcolor" background
            , Attr.attribute "fontfamily" "Inter"
            , Attr.attribute "fontsize" "19px"
            , Attr.attribute "fontWeight" "700"
            , Attr.attribute "placeholder" (Translations.editorSubtitlePlaceholderText [ browserEnv.translations ])
            , on "input-change" (Decode.map UpdateSubtitle decodeInputChange)
            ]
            []
        ]


decodeInputChange : Decode.Decoder String
decodeInputChange =
    Decode.field "detail" (Decode.field "value" Decode.string)


viewImage : Model -> Html Msg
viewImage model =
    case model.image of
        Just image ->
            div
                [ css
                    [ Tw.max_w_72
                    ]
                ]
                [ img
                    [ Attr.src image
                    , Events.onClick (SelectImage ArticleImageSelection)
                    ]
                    []
                ]

        Nothing ->
            div
                [ css
                    [ Tw.w_48
                    , Tw.h_32
                    , Tw.bg_color Theme.slate_500
                    ]
                , Events.onClick (SelectImage ArticleImageSelection)
                ]
                []


viewEditor : Theme -> BrowserEnv -> Model -> Html Msg
viewEditor theme browserEnv model =
    case model.editorMode of
        Editor ->
            div
                [ css
                    [ Tw.w_full
                    ]
                ]
                [ milkdownEditor model.milkdown browserEnv (model.content |> Maybe.withDefault "")
                ]

        Preview ->
            let
                styles =
                    stylesForTheme theme
            in
            Ui.Article.viewContentMarkdown styles (Just model.loadedContent) (\_ -> Nothing) (Maybe.withDefault "" model.content)


milkdownEditor : Milkdown.Model -> BrowserEnv -> Milkdown.Content -> Html Msg
milkdownEditor milkdownModel browserEnv content =
    Milkdown.view
        MilkdownSent
        (Milkdown.defaults
            |> Milkdown.withContent content
            --          |> Milkdown.withPlaceholder "Start typing..."
            |> Milkdown.withDarkMode (milkDownDarkMode browserEnv.darkMode)
            |> Milkdown.onChange (Just EditorChanged)
            |> Milkdown.onFocus (Just EditorFocused)
            |> Milkdown.onBlur (Just EditorBlurred)
            |> Milkdown.onLoad (Just EditorLoaded)
            |> Milkdown.onFileRequest (Just <| SelectImage MarkdownImageSelection)
        )
        milkdownModel


milkDownDarkMode : Bool -> Milkdown.DarkMode
milkDownDarkMode darkModeActive =
    if darkModeActive then
        Milkdown.Dark

    else
        Milkdown.Light


viewTags : Theme -> BrowserEnv -> Model -> Html Msg
viewTags theme browserEnv model =
    let
        styles =
            stylesForTheme theme
    in
    div
        [ css
            [ Tw.w_full
            ]
        ]
        [ {- Label -}
          label
            [ Attr.for "entry-field"
            , css
                [ Tw.block
                , Tw.text_color Theme.gray_700
                , Tw.text_sm
                , Tw.font_medium
                , Tw.mb_2
                ]
            ]
            [ text <| Translations.tagsLabelText [ browserEnv.translations ]
            ]
        , {- Input Field -}
          input
            (styles.colorStyleBackground
                ++ [ Attr.type_ "text"
                   , Attr.id "entry-field"
                   , Attr.placeholder <| Translations.tagsPlaceholderText [ browserEnv.translations ]
                   , Attr.value (model.tags |> Maybe.withDefault "")
                   , Events.onInput UpdateTags
                   , css
                        [ Tw.w_full
                        , Tw.px_4
                        , Tw.py_2
                        , Tw.border
                        , Tw.border_color Theme.gray_300
                        , Tw.rounded_lg
                        , Tw.transition_all
                        , Tw.duration_200
                        , Css.focus
                            [ Tw.outline_none
                            , Tw.ring_2
                            , Tw.ring_color Theme.blue_500
                            , Tw.border_color Theme.blue_500
                            ]
                        ]
                   ]
            )
            []
        ]


saveButtons : BrowserEnv -> Theme -> Model -> Html Msg
saveButtons browserEnv theme model =
    div
        [ css
            [ Tw.flex
            , Tw.justify_between
            , Tw.mb_10
            ]
        ]
        [ previewButton browserEnv model theme
        , saveDraftButton browserEnv model theme
        , publishButton browserEnv model theme
        ]


publishButton : BrowserEnv -> Model -> Theme -> Html Msg
publishButton browserEnv model theme =
    Button.new
        { label = Translations.publishButtonTitle [ browserEnv.translations ]
        , onClick = Just Publish
        , theme = theme
        }
        |> Button.withDisabled (not <| articleReadyForPublishing model)
        |> Button.withTypePrimary
        |> Button.view


articleReadyForPublishing : Model -> Bool
articleReadyForPublishing model =
    (model.articleState == ArticleDraftSaved || model.articleState == ArticleModified)
        && model.title
        /= Nothing
        && model.summary
        /= Nothing
        && model.content
        /= Nothing
        && model.image
        /= Nothing
        && model.identifier
        /= Nothing


previewButton : BrowserEnv -> Model -> Theme -> Html Msg
previewButton browserEnv model theme =
    let
        buttonTitle =
            case model.editorMode of
                Editor ->
                    Translations.previewButtonTitle [ browserEnv.translations ]

                Preview ->
                    Translations.editButtonTitle [ browserEnv.translations ]
    in
    Button.new
        { label = buttonTitle
        , onClick = Just ToggleEditorMode
        , theme = theme
        }
        |> Button.withDisabled (model.content == Nothing)
        |> Button.withTypeSecondary
        |> Button.view


saveDraftButton : BrowserEnv -> Model -> Theme -> Html Msg
saveDraftButton browserEnv model theme =
    Button.new
        { label = Translations.saveDraftButtonTitle [ browserEnv.translations ]
        , onClick = Just SaveDraft
        , theme = theme
        }
        |> Button.withDisabled (not <| articleReadyForSaving model)
        |> Button.withTypeSecondary
        |> Button.view


articleReadyForSaving : Model -> Bool
articleReadyForSaving model =
    model.articleState == ArticleModified
