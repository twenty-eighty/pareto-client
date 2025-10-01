module Pages.Write exposing (Model, Msg, page)

import Auth
import BrowserEnv exposing (BrowserEnv)
import Components.Button as Button
import Components.Dropdown as Dropdown
import Components.HashtagEditor as HashtagEditor
import Components.MediaSelector as MediaSelector exposing (UploadedFile(..))
import Components.MessageDialog as MessageDialog
import Components.PublishArticleDialog as PublishArticleDialog exposing (PublishingInfo(..))
import Components.PublishDateDialog as PublishDateDialog
import Dict exposing (Dict)
import Effect exposing (Effect)
import Html.Styled as Html exposing (Html, div, img, label, node, text)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events as Events exposing (..)
import I18Next exposing (Translations)
import Json.Decode as Decode
import Layouts
import Layouts.Sidebar
import LinkPreview exposing (LoadedContent)
import Locale exposing (Language(..), languageToISOCode, languageToString)
import Markdown
import Milkdown.MilkdownEditor as Milkdown
import Nostr
import Nostr.Article exposing (addressComponentsForArticle, articleFromEvent)
import Nostr.DeletionRequest exposing (deletionEvent)
import Nostr.Event as Event exposing (AddressComponents, Event, ImageMetadata, Kind(..), Tag(..), numberForKind)
import Nostr.External
import Nostr.Nip19 as Nip19 exposing (NIP19Type(..))
import Nostr.Nip27 as Nip27
import Nostr.Nip94 exposing (FileMetadata)
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
import Subscribers
import Svg.Loaders as Loaders
import Tailwind.Breakpoints as Bp
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw
import Task
import TextStats exposing (TextStats, emptyTextStats)
import Time
import Translations.Write as Translations
import Ui.Article
import Ui.Shared exposing (emptyHtml)
import Ui.Styles exposing (Theme(..), darkMode, stylesForTheme)
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
toLayout theme _ =
    Layouts.Sidebar.new
        { theme = theme
        }
        |> Layouts.Sidebar



-- INIT


type alias Model =
    { draftEventId : Maybe EventId -- event ID of draft (when editing one)
    , draftAddressComponents : Maybe AddressComponents -- address components of draft (when editing one)
    , title : Maybe String
    , summary : Maybe String
    , image : Maybe String
    , content : Maybe String
    , milkdown : Milkdown.Model
    , identifier : Maybe String
    , zapWeights : List ( PubKey, RelayUrl, Maybe Int )
    , otherTags : List Tag
    , now : Time.Posix
    , mediaSelector : MediaSelector.Model
    , imageSelection : Maybe ImageSelection
    , imageMetadata : Dict String ImageMetadata
    , publishArticleDialog : PublishArticleDialog.Model
    , publishedAt : Maybe Time.Posix
    , publishDateDialog : PublishDateDialog.Model
    , articleState : ArticleState
    , editorMode : EditorMode
    , loadedContent : LoadedContent Msg
    , modalDialog : ModalDialog
    , languageSelection : Dropdown.Model Language
    , hashtagEditor : HashtagEditor.Model
    , textStats : TextStats
    , debounceStatus : DebounceStatus
    }


type DebounceStatus
    = Inactive
    | Active Int -- Remaining time in milliseconds


type EditorMode
    = Editor
    | Preview


type ModalDialog
    = NoModalDialog
    | NewsletterSentDialog
    | PublishedDialog
    | ErrorDialog


type ArticleState
    = ArticleEmpty
    | ArticleModified
    | ArticleLoadingDraft RequestId
    | ArticleLoadingDraftError String
    | ArticleSavingDraft SendRequestId
    | ArticleDraftSavingError String
    | ArticleDraftSaved
    | ArticlePublishing SendRequestId (Maybe Subscribers.SubscriberEventData)
    | ArticlePublishingError String
    | ArticleSendingNewsletter Int Int
    | ArticleSendingNewsletterError String
    | ArticlePublished
    | ArticleDeletingDraft SendRequestId (Maybe Subscribers.SubscriberEventData)
    | ArticleDeletingDraftError String


type ImageSelection
    = ArticleImageSelection
    | MarkdownImageSelection


init : Auth.User -> Shared.Model -> Route () -> () -> ( Model, Effect Msg )
init user shared route () =
    let
        maybeNip19 =
            Dict.get "a" route.query
                |> Maybe.andThen
                    (\address ->
                        address
                            |> Nip19.decode
                            |> Result.toMaybe
                    )

        createCopy =
            Dict.get "copy" route.query
                |> Maybe.map (\copy -> copy == "true")
                |> Maybe.withDefault False

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
                    let
                        publishedAt =
                            if createCopy then
                                -- published date will equal creation date of article event
                                Nothing
                            else if (article.kind == KindLongFormContent && article.publishedAt == Nothing) then
                                -- assume published date equals creation date of article event
                                Just article.createdAt
                            else
                                -- keep published date if present.
                                -- this allows to keep original published date when publishing older articles from RSS or other sources.
                                article.publishedAt


                        (draftEventId, draftAddressComponents) =
                            if article.kind == KindDraftLongFormContent && not createCopy then
                                (Just article.id, addressComponentsForArticle article)

                            else
                                (Nothing, Nothing)

                        identifier =
                            if createCopy then
                                -- create new identifier for copy
                                Nothing
                            else
                                article.identifier
                    in
                    { draftEventId = draftEventId
                    , draftAddressComponents = draftAddressComponents
                    , title = article.title
                    , summary = article.summary
                    , image = article.image
                    , content = Just article.content
                    , milkdown = Milkdown.init
                    , identifier = identifier
                    , otherTags = article.otherTags
                    , zapWeights =
                        if List.length article.zapWeights > 0 then
                            article.zapWeights

                        else
                            defaultZapWeights user.pubKey
                    , now = Time.millisToPosix 0
                    , mediaSelector = mediaSelector
                    , imageSelection = Nothing
                    , imageMetadata = article.imageMetadata
                    , publishArticleDialog = publishArticleDialog
                    , publishedAt = publishedAt
                    , publishDateDialog = PublishDateDialog.init
                    , articleState = ArticleDraftSaved
                    , editorMode = Editor
                    , loadedContent = { loadedUrls = Set.empty, addLoadedContentFunction = AddLoadedContent }
                    , modalDialog = NoModalDialog
                    , languageSelection = Dropdown.init { selected = article.language }
                    , hashtagEditor = HashtagEditor.init { hashtags = article.hashtags }
                    , textStats = TextStats.compute article.language article.content
                    , debounceStatus = Inactive
                    }

                Nothing ->
                    { draftEventId = Nothing
                    , draftAddressComponents = Nothing
                    , title = Nothing
                    , summary = Nothing
                    , image = Nothing
                    , content = Nothing
                    , milkdown = Milkdown.init
                    , identifier = Nothing
                    , otherTags = []
                    , zapWeights = defaultZapWeights user.pubKey
                    , now = Time.millisToPosix 0
                    , mediaSelector = mediaSelector
                    , imageSelection = Nothing
                    , imageMetadata = Dict.empty
                    , publishArticleDialog = publishArticleDialog
                    , publishedAt = Nothing
                    , publishDateDialog = PublishDateDialog.init
                    , articleState = ArticleEmpty
                    , editorMode = Editor
                    , loadedContent = { loadedUrls = Set.empty, addLoadedContentFunction = AddLoadedContent }
                    , modalDialog = NoModalDialog
                    , languageSelection = Dropdown.init { selected = Just shared.browserEnv.language }
                    , hashtagEditor = HashtagEditor.init { hashtags = [] }
                    , textStats = emptyTextStats
                    , debounceStatus = Inactive
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
    if pubKey == Pareto.paretoClientPubKey then
        [ ( Pareto.paretoClientPubKey, Pareto.paretoRelay, Just 1 )
        ]

    else
        [ ( pubKey, Pareto.paretoRelay, Just 80 )
        , ( Pareto.paretoClientPubKey, Pareto.paretoRelay, Just 20 )
        ]



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
    | HashtagEditorMsg HashtagEditor.Msg
    | HashtagsModified Bool
    | SelectImage ImageSelection
    | ImageSelected ImageSelection MediaSelector.UploadedFile
    | Publish
    | PublishArticle PublishArticleDialog.PublishingInfo
    | SaveDraft
    | Now Time.Posix
    | OpenMediaSelector
    | MediaSelectorSent (MediaSelector.Msg Msg)
    | ReceivedPortMessage IncomingMessage
    | MilkdownSent (Milkdown.Msg Msg)
    | PublishArticleDialogSent (PublishArticleDialog.Msg Msg)
    | PublishDateDialogSent (PublishDateDialog.Msg Msg)
    | UpdatePublishDate Time.Posix
    | ShowPublishDateDialog
    | ModalDialogButtonClicked PublishedDialogButton
    | DropdownSent (Dropdown.Msg Language Msg)
    | LanguageChanged (Maybe Language)
    | StatsComputed (Result Never TextStats)
    | Tick Time.Posix


type PublishedDialogButton
    = OkButton


update : Shared.Model -> Auth.User -> Msg -> Model -> ( Model, Effect Msg )
update shared user msg model =
    case msg of
        EditorChanged newContent ->
            if newContent == "" then
                ( { model
                    | articleState = ArticleModified
                    , content = Nothing
                    , textStats = emptyTextStats
                    , debounceStatus = Inactive
                  }
                , Effect.none
                )

            else
                ( { model
                    | articleState = ArticleModified
                    , content = Just newContent
                    , debounceStatus = Active 500
                  }
                , Effect.none
                )

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
                    ( { model | editorMode = Editor }
                    , model.content
                        |> Maybe.map (loadReferencedNip27Profiles shared.nostr)
                        |> Maybe.withDefault Effect.none
                    )

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

        HashtagEditorMsg innerMsg ->
            HashtagEditor.update
                { msg = innerMsg
                , model = model.hashtagEditor
                , modifiedMsg = Just HashtagsModified
                , toModel = \hashtagEditor -> { model | hashtagEditor = hashtagEditor }
                , toMsg = HashtagEditorMsg
                }

        HashtagsModified modified ->
            if modified then
                ( { model | articleState = ArticleModified }, Effect.none )

            else
                ( model, Effect.none )

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
                                    ( { model | milkdown = Milkdown.setSelectedImage model.milkdown blobDescriptor.url Nothing Nothing }, Effect.none )

                        Nip96File fileMetadata ->
                            case fileMetadata.url of
                                Just url ->
                                    ( { model | milkdown = Milkdown.setSelectedImage model.milkdown url fileMetadata.content fileMetadata.alt }, Effect.none )

                                Nothing ->
                                    ( model, Effect.none )

        Publish ->
            update shared user (PublishArticleDialogSent PublishArticleDialog.ShowDialog) model

        PublishArticle publishingInfo ->
            case publishingInfo of
                ArticleOnly relayUrls ->
                    ( { model | articleState = ArticlePublishing (Nostr.getLastSendRequestId shared.nostr) Nothing }
                    , sendPublishCmd shared model user relayUrls
                    )

                NewsletterOnly subscriberEventData ->
                    ( { model | articleState = ArticleSendingNewsletter subscriberEventData.active subscriberEventData.total }
                    , Subscribers.newsletterSubscribersEvent
                        shared
                        user.pubKey
                        ( KindLongFormContent, user.pubKey, Maybe.withDefault "" model.identifier )
                        { title = Maybe.withDefault "" model.title
                        , summary = Maybe.withDefault "" model.summary
                        , content = Maybe.withDefault "" model.content
                        , imageUrl = Maybe.withDefault "" model.image
                        , language = languageISOCode model
                        }
                        subscriberEventData
                        |> SendApplicationData
                        |> Shared.Msg.SendNostrEvent
                        |> Effect.sendSharedMsg
                    )

                ArticleAndNewsletter relayUrls subscriberEventData ->
                    ( { model | articleState = ArticlePublishing (Nostr.getLastSendRequestId shared.nostr) (Just subscriberEventData) }
                    , sendPublishCmd shared model user relayUrls
                    )

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
                { pubKey = user.pubKey
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
                , testMode = shared.browserEnv.testMode
                }

        PublishDateDialogSent innerMsg ->
            PublishDateDialog.update
                { msg = innerMsg
                , model = model.publishDateDialog
                , toModel = \publishDateDialog -> { model | publishDateDialog = publishDateDialog }
                , toMsg = PublishDateDialogSent
                }

        UpdatePublishDate publishDate ->
            ( { model | publishedAt = Just publishDate }, Effect.none )

        ShowPublishDateDialog ->
            let
                ( publishDateDialog, effect ) =
                    PublishDateDialog.show model.publishDateDialog model.publishedAt
            in
            ( { model | publishDateDialog = publishDateDialog }, Effect.map PublishDateDialogSent effect )

        ModalDialogButtonClicked _ ->
            ( { model | modalDialog = NoModalDialog }, Effect.none )

        DropdownSent innerMsg ->
            Dropdown.update
                { msg = innerMsg
                , model = model.languageSelection
                , toModel = \dropdown -> { model | languageSelection = dropdown, articleState = ArticleModified }
                , toMsg = DropdownSent
                }

        LanguageChanged maybeLanguage ->
            ( { model | textStats = TextStats.compute maybeLanguage (Maybe.withDefault "" model.content) }, Effect.none )

        StatsComputed (Ok textStats) ->
            ( { model | textStats = textStats }, Effect.none )

        StatsComputed (Err _) ->
            ( model, Effect.none )

        Tick _ ->
            case model.debounceStatus of
                Inactive ->
                    -- If debounce is inactive, do nothing
                    ( model, Effect.none )

                Active remainingTime ->
                    if remainingTime <= 0 then
                        -- Time has run out, trigger search
                        ( { model | debounceStatus = Inactive }
                        , TextStats.computeTask (Dropdown.selectedItem model.languageSelection) (Maybe.withDefault "" model.content)
                            |> Task.attempt StatsComputed
                            |> Effect.sendCmd
                        )

                    else
                        -- Decrease remaining time
                        ( { model | debounceStatus = Active (remainingTime - 100) }
                        , Effect.none
                        )


loadReferencedNip27Profiles : Nostr.Model -> String -> Effect Msg
loadReferencedNip27Profiles nostr content =
    Nip27.collectNostrLinks content
        |> Nostr.nip27ProfilesRequest nostr
        |> Maybe.map
            (\eventFilter ->
                RequestUserData eventFilter
                    |> Nostr.createRequest nostr "Referenced Nostr profiles" []
                    |> Shared.Msg.RequestNostrEvents
                    |> Effect.sendSharedMsg
            )
        |> Maybe.withDefault Effect.none


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
                ( Nostr.External.decodeRequestId portMessage.value
                , Nostr.External.decodeEventsKind portMessage.value
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

        "error" ->
            case
                ( model.articleState, Nostr.External.decodeReason portMessage.value )
            of
                ( ArticleSavingDraft _, Ok error ) ->
                    ( { model
                        | articleState = ArticleDraftSavingError error
                        , publishArticleDialog = PublishArticleDialog.hide model.publishArticleDialog
                        , modalDialog = ErrorDialog
                      }
                    , Effect.none
                    )

                ( ArticlePublishing _ _, Ok error ) ->
                    ( { model
                        | articleState = ArticlePublishingError error
                        , publishArticleDialog = PublishArticleDialog.hide model.publishArticleDialog
                        , modalDialog = ErrorDialog
                      }
                    , Effect.none
                    )

                ( ArticleSendingNewsletter _ _, Ok error ) ->
                    ( { model
                        | articleState = ArticleSendingNewsletterError error
                        , publishArticleDialog = PublishArticleDialog.hide model.publishArticleDialog
                        , modalDialog = ErrorDialog
                      }
                    , Effect.none
                    )

                ( ArticleDeletingDraft _ _, Ok error ) ->
                    ( { model
                        | articleState = ArticleDeletingDraftError error
                        , publishArticleDialog = PublishArticleDialog.hide model.publishArticleDialog
                        , modalDialog = ErrorDialog
                      }
                    , Effect.none
                    )

                ( _, _ ) ->
                    -- error message will be collected in Nostr module
                    ( model, Effect.none )

        _ ->
            ( model, Effect.none )


updateModelWithDraftRequest : Model -> Decode.Value -> ( Model, Effect Msg )
updateModelWithDraftRequest model value =
    case Nostr.External.decodeEvents value of
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
                        , draftAddressComponents = addressComponentsForArticle draft
                        , title = draft.title
                        , summary = draft.summary
                        , image = draft.image
                        , content = Just draft.content
                        , identifier = draft.identifier
                        , hashtagEditor = HashtagEditor.init { hashtags = draft.hashtags }
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

        ArticlePublishing sendRequestId maybeSubscriberEventData ->
            if Just sendRequestId == receivedSendRequestId then
                case ( model.draftEventId, maybeSubscriberEventData ) of
                    ( Just _, _ ) ->
                        ( { model
                            | articleState = ArticleDeletingDraft (Nostr.getLastSendRequestId shared.nostr) maybeSubscriberEventData
                          }
                          -- after publishing article, delete draft
                        , sendDraftDeletionCmd shared model user
                        )

                    ( Nothing, Just subscriberEventData ) ->
                        ( { model | articleState = ArticleSendingNewsletter subscriberEventData.active subscriberEventData.total }
                        , Subscribers.newsletterSubscribersEvent
                            shared
                            user.pubKey
                            ( KindLongFormContent, user.pubKey, Maybe.withDefault "" model.identifier )
                            { title = Maybe.withDefault "" model.title
                            , summary = Maybe.withDefault "" model.summary
                            , content = Maybe.withDefault "" model.content
                            , imageUrl = Maybe.withDefault "" model.image
                            , language = languageISOCode model
                            }
                            subscriberEventData
                            |> SendApplicationData
                            |> Shared.Msg.SendNostrEvent
                            |> Effect.sendSharedMsg
                        )

                    ( Nothing, Nothing ) ->
                        ( { model
                            | articleState = ArticlePublished
                            , publishArticleDialog = PublishArticleDialog.hide model.publishArticleDialog
                            , modalDialog = PublishedDialog
                          }
                        , Effect.none
                        )

            else
                ( model, Effect.none )

        ArticleDeletingDraft sendRequestId maybeSubscriberEventData ->
            if Just sendRequestId == receivedSendRequestId then
                case maybeSubscriberEventData of
                    Just subscriberEventData ->
                        ( { model | articleState = ArticleSendingNewsletter subscriberEventData.active subscriberEventData.total }
                        , Subscribers.newsletterSubscribersEvent shared
                            user.pubKey
                            ( KindLongFormContent, user.pubKey, Maybe.withDefault "" model.identifier )
                            { title = Maybe.withDefault "" model.title
                            , summary = Maybe.withDefault "" model.summary
                            , content = Maybe.withDefault "" model.content
                            , imageUrl = Maybe.withDefault "" model.image
                            , language = languageISOCode model
                            }
                            subscriberEventData
                            |> SendApplicationData
                            |> Shared.Msg.SendNostrEvent
                            |> Effect.sendSharedMsg
                        )

                    Nothing ->
                        -- no newsletter to be sent
                        ( { model
                            | articleState = ArticlePublished
                            , publishArticleDialog = PublishArticleDialog.hide model.publishArticleDialog
                            , modalDialog = PublishedDialog
                          }
                        , Effect.none
                        )

            else
                ( model, Effect.none )

        ArticleSendingNewsletter _ _ ->
            ( { model
                | articleState = ArticlePublished
                , publishArticleDialog = PublishArticleDialog.hide model.publishArticleDialog
                , modalDialog = NewsletterSentDialog
              }
            , Effect.none
            )

        _ ->
            ( model, Effect.none )


sendPublishCmd : Shared.Model -> Model -> Auth.User -> List RelayUrl -> Effect Msg
sendPublishCmd shared model user relayUrls =
    let
        publishedAt =
            model.publishedAt
                |> Maybe.withDefault model.now
    in
    eventWithContent shared model user KindLongFormContent (Just publishedAt)
        |> SendLongFormArticle relayUrls
        |> Shared.Msg.SendNostrEvent
        |> Effect.sendSharedMsg


sendDraftCmd : Shared.Model -> Model -> Auth.User -> Effect Msg
sendDraftCmd shared model user =
    eventWithContent shared model user KindDraftLongFormContent model.publishedAt
        |> SendLongFormDraft (Nostr.getDefaultRelays shared.nostr)
        |> Shared.Msg.SendNostrEvent
        |> Effect.sendSharedMsg


sendDraftDeletionCmd : Shared.Model -> Model -> Auth.User -> Effect Msg
sendDraftDeletionCmd shared model user =
    case model.draftEventId of
        Just draftEventId ->
            deletionEvent user.pubKey shared.browserEnv.now draftEventId "Deleting draft after publishing article" model.draftAddressComponents [ KindDraftLongFormContent, KindDraft ]
                |> SendDeletionRequest (Nostr.getDraftRelayUrls shared.nostr draftEventId)
                |> Shared.Msg.SendNostrEvent
                |> Effect.sendSharedMsg

        Nothing ->
            Effect.none


eventWithContent : Shared.Model -> Model -> Auth.User -> Kind -> Maybe Time.Posix -> Event
eventWithContent shared model user kind publishedAt =
    let
        -- NIP-92: media attachments
        imageMetadataList =
            model.content
                |> Maybe.andThen (\content -> Markdown.collectImageUrls content |> Result.toMaybe)
                |> Maybe.withDefault []
                |> List.append (model.image |> Maybe.map List.singleton |> Maybe.withDefault [])
                |> Set.fromList
                |> Set.toList
                |> List.filterMap (getImageMetadata model)
    in
    { pubKey = user.pubKey
    , createdAt = shared.browserEnv.now
    , kind = kind
    , tags =
        []
            |> Event.addTitleTag model.title
            |> Event.addSummaryTag model.summary
            |> Event.addImageTag model.image
            |> Event.addIdentifierTag model.identifier
            |> Event.addHashtagsToTags (HashtagEditor.getHashtags model.hashtagEditor)
            |> Maybe.withDefault identity (publishedAt |> Maybe.map Event.addPublishedAtTag)
            |> Maybe.withDefault identity (languageISOCode model |> Maybe.map (Event.addLabelTags "ISO-639-1"))
            |> Event.addZapTags model.zapWeights
            |> Event.addAltTag (altText model.identifier user.pubKey kind [ Pareto.paretoRelay ])
            |> Event.addImetaTags imageMetadataList
    , content = model.content |> Maybe.withDefault ""
    , id = ""
    , sig = Nothing
    , relays = Nothing
    }


getImageMetadata : Model -> String -> Maybe ImageMetadata
getImageMetadata model imageUrl =
    -- default: check if article has image metadata already
    case Dict.get imageUrl model.imageMetadata of
        Just imageMetadata ->
            Just imageMetadata

        Nothing ->
            -- check if image is in media selector
            MediaSelector.fileMetadataForUrl model.mediaSelector imageUrl
                |> Maybe.andThen imageMetadataFromFileMetadata


imageMetadataFromFileMetadata : FileMetadata -> Maybe ImageMetadata
imageMetadataFromFileMetadata fileMetadata =
    fileMetadata.url
        |> Maybe.map
            (\url ->
                { url = url
                , mimeType = fileMetadata.mimeType
                , blurHash = fileMetadata.blurhash
                , dim = fileMetadata.dim
                , alt = fileMetadata.alt
                , x = fileMetadata.xHash
                , fallbacks = fileMetadata.fallbacks |> Maybe.withDefault []
                }
            )


languageISOCode : Model -> Maybe String
languageISOCode model =
    Dropdown.selectedItem model.languageSelection |> Maybe.map languageToISOCode


altText : Maybe String -> PubKey -> Kind -> List String -> String
altText maybeIdentifier pubKey kind relays =
    case maybeIdentifier of
        Just identifier ->
            case Nip19.encode (Nip19.NAddr { identifier = identifier, pubKey = pubKey, kind = numberForKind kind, relays = relays }) of
                Ok nip19 ->
                    "This is a long form article, you can read it on " ++ Pareto.applicationUrl ++ "/a/" ++ nip19

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
        , PublishArticleDialog.subscriptions model.publishArticleDialog PublishArticleDialogSent
        , debounceSubscription model.debounceStatus
        ]


debounceSubscription : DebounceStatus -> Sub Msg
debounceSubscription debounceStatus =
    case debounceStatus of
        Inactive ->
            Sub.none

        Active _ ->
            Time.every 100 Tick



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
                , Tw.flex
                , Tw.flex_col
                , Tw.gap_2
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
                    [ viewImage shared.browserEnv.translations model
                    ]
                ]
            , viewEditor shared model
            , viewLanguage shared.browserEnv model
            , viewTags shared.theme shared.browserEnv model
            , viewPublishDate shared.browserEnv shared.theme model
            , viewArticleState shared.browserEnv shared.theme model.articleState
            , saveButtons shared.browserEnv shared.theme model
            , TextStats.view shared.browserEnv shared.theme model.textStats
            , viewMediaSelector user shared model
            ]
        , PublishDateDialog.new
            { model = model.publishDateDialog
            , toMsg = PublishDateDialogSent
            , onCommitDate = UpdatePublishDate
            , browserEnv = shared.browserEnv
            , theme = shared.theme
            }
            |> PublishDateDialog.view
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
        , viewModalDialog shared.theme shared.browserEnv model.articleState model.modalDialog
        ]
    }


viewModalDialog : Theme -> BrowserEnv -> ArticleState -> ModalDialog -> Html Msg
viewModalDialog theme browserEnv articleState modalDialog =
    case modalDialog of
        NoModalDialog ->
            emptyHtml

        NewsletterSentDialog ->
            MessageDialog.new
                { onClick = ModalDialogButtonClicked
                , onClose = ModalDialogButtonClicked OkButton
                , title = Translations.newsletterSentMessageBoxTitle [ browserEnv.translations ]
                , content = div [] [ text <| Translations.newsletterSentMessageBoxText [ browserEnv.translations ] ]
                , buttons =
                    [ { style = MessageDialog.PrimaryButton
                      , title = "Ok"
                      , identifier = OkButton
                      }
                    ]
                , theme = theme
                }
                |> MessageDialog.view

        PublishedDialog ->
            MessageDialog.new
                { onClick = ModalDialogButtonClicked
                , onClose = ModalDialogButtonClicked OkButton
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

        ErrorDialog ->
            MessageDialog.new
                { onClick = ModalDialogButtonClicked
                , onClose = ModalDialogButtonClicked OkButton
                , title = Translations.errorMessageBoxTitle [ browserEnv.translations ]
                , content = div [] [ text <| Maybe.withDefault "" (articleStateToString browserEnv articleState) ]
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
            emptyHtml


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

        ArticleDraftSavingError error ->
            Just <| Translations.articleDraftSaveError [ browserEnv.translations ] ++ ": " ++ error

        ArticlePublishing _ _ ->
            Just <| Translations.articlePublishingState [ browserEnv.translations ]

        ArticlePublishingError error ->
            Just <| Translations.articlePublishingError [ browserEnv.translations ] ++ ": " ++ error

        ArticlePublished ->
            Just <| Translations.articlePublishedState [ browserEnv.translations ]

        ArticleSendingNewsletter _ _ ->
            Just <| Translations.sendingNewsletterState [ browserEnv.translations ]

        ArticleSendingNewsletterError error ->
            Just <| Translations.sendingNewsletterError [ browserEnv.translations ] ++ ": " ++ error

        ArticleDeletingDraft _ _ ->
            Just <| Translations.deletingDraftState [ browserEnv.translations ]

        ArticleDeletingDraftError error ->
            Just <| Translations.deletingDraftError [ browserEnv.translations ] ++ ": " ++ error


articleStateProcessIndicator : ArticleState -> Maybe (Html Msg)
articleStateProcessIndicator articleState =
    case articleState of
        ArticleEmpty ->
            Nothing

        ArticleModified ->
            Nothing

        ArticleLoadingDraft _ ->
            Just <| (Loaders.rings [] |> Html.fromUnstyled)

        ArticleLoadingDraftError _ ->
            Nothing

        ArticleSavingDraft _ ->
            Just <| (Loaders.rings [] |> Html.fromUnstyled)

        ArticleDraftSaved ->
            Nothing

        ArticleDraftSavingError _ ->
            Nothing

        ArticlePublishing _ _ ->
            Just <| (Loaders.rings [] |> Html.fromUnstyled)

        ArticlePublishingError _ ->
            Nothing

        ArticleSendingNewsletter _ _ ->
            Just <| (Loaders.rings [] |> Html.fromUnstyled)

        ArticleSendingNewsletterError _ ->
            Nothing

        ArticlePublished ->
            Nothing

        ArticleDeletingDraft _ _ ->
            Just <| (Loaders.rings [] |> Html.fromUnstyled)

        ArticleDeletingDraftError _ ->
            Nothing


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
            emptyHtml


viewTitle : Theme -> BrowserEnv -> Model -> Html Msg
viewTitle _ browserEnv model =
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
viewSubtitle _ browserEnv model =
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


viewImage : I18Next.Translations -> Model -> Html Msg
viewImage translations model =
    let
        styles =
            stylesForTheme ParetoTheme
    in
    case model.image of
        Just image ->
            div
                [ css
                    [ Tw.max_w_72
                    , Tw.cursor_pointer
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
                    , Tw.flex
                    , Tw.bg_color styles.colorB2
                    , Tw.justify_center
                    , Tw.items_center
                    , Tw.cursor_pointer
                    , darkMode [ Tw.bg_color styles.colorB2DarkMode ]
                    ]
                , Events.onClick (SelectImage ArticleImageSelection)
                ]
                [ Html.span
                    [ css
                        [ Tw.text_color Theme.white
                        , Tw.m_2
                        ]
                    ]
                    [ text <| Translations.imageSelectionInstructionalText [ translations ]
                    ]
                ]


viewEditor : Shared.Model -> Model -> Html Msg
viewEditor shared model =
    case model.editorMode of
        Editor ->
            div
                [ css
                    [ Tw.w_full
                    ]
                ]
                [ milkdownEditor model.milkdown shared.browserEnv (model.content |> Maybe.withDefault "")
                ]

        Preview ->
            let
                styles =
                    stylesForTheme shared.theme
            in
            Ui.Article.viewContentMarkdown shared.browserEnv.environment styles (Just model.loadedContent) (Nostr.getProfile shared.nostr) (Maybe.withDefault "" model.content)


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


viewLanguage : BrowserEnv -> Model -> Html Msg
viewLanguage browserEnv model =
    let
        styles =
            Ui.Styles.stylesForTheme ParetoTheme
    in
    div
        [ css
            [ Tw.w_full
            ]
        ]
        [ {- Label -}
          label
            ([ Attr.for "dropdownMenu"
             , css
                [ Tw.block
                , Tw.text_sm
                , Tw.font_medium
                , Tw.mb_2
                ]
             ]
                ++ styles.colorStyleLabel
            )
            [ text <| Translations.languageSelectionLabel [ browserEnv.translations ]
            ]
        , {- Dropdown -}
          Dropdown.new
            { model = model.languageSelection
            , toMsg = DropdownSent
            , choices = Locale.defaultLanguages
            , allowNoSelection = True
            , toLabel = toLabel browserEnv.translations
            }
            |> Dropdown.withOnChange LanguageChanged
            |> Dropdown.view
        ]


toLabel : Translations -> Maybe Language -> String
toLabel translations maybeLanguage =
    case maybeLanguage of
        Just language ->
            languageToString translations language

        Nothing ->
            Translations.noLanguageText [ translations ]


viewTags : Theme -> BrowserEnv -> Model -> Html Msg
viewTags theme browserEnv model =
    HashtagEditor.new
        { model = model.hashtagEditor
        , toMsg = HashtagEditorMsg
        , translations = browserEnv.translations
        , theme = theme
        }
        |> HashtagEditor.view


viewPublishDate : BrowserEnv -> Theme -> Model -> Html Msg
viewPublishDate browserEnv theme model =
    let
        publishDate =
            model.publishedAt
                |> Maybe.map (BrowserEnv.formatDate browserEnv)
                |> Maybe.withDefault (Translations.nowText [ browserEnv.translations ])
    in
    div
        [ css
            [ Tw.flex
            , Tw.justify_between
            , Tw.gap_2
            ]
        ]
        [ Html.text <| Translations.publishingDateLabel [ browserEnv.translations ] { publishDate = publishDate }
        , Button.new
            { label = Translations.changePublishDateButtonTitle [ browserEnv.translations ]
            , onClick = Just ShowPublishDateDialog
            , theme = theme
            }
            |> Button.withTypeSecondary
            |> Button.view
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
    let
        contentComplete =
            (model.title /= Nothing)
                && (model.summary /= Nothing)
                && (model.content /= Nothing)
                && (model.image /= Nothing)
                && (model.identifier /= Nothing)
    in
    case model.articleState of
        ArticleDraftSaved ->
            contentComplete

        ArticlePublishingError _ ->
            contentComplete

        ArticleSendingNewsletterError _ ->
            contentComplete

        ArticleModified ->
            contentComplete

        _ ->
            False


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
    case model.articleState of
        ArticleModified ->
            True

        ArticleDraftSavingError _ ->
            True

        _ ->
            False
