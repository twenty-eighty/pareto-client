module Pages.Write exposing (Model, Msg, page)

import Auth
import BrowserEnv exposing (BrowserEnv)
import Components.Button as Button
import Components.MediaSelector as MediaSelector exposing (UploadedFile(..))
import Components.PublishArticleDialog as PublishArticleDialog exposing (PublishArticleDialog)
import Css
import Dict exposing (Dict)
import Effect exposing (Effect)
import Hex
import Html.Styled as Html exposing (Html, a, aside, button, code, div, h2, h3, h4, img, input, label, node, p, span, text, textarea)
import Html.Styled.Attributes as Attr exposing (class, css, style)
import Html.Styled.Events as Events exposing (..)
import Json.Decode as Decode
import Layouts
import Milkdown.MilkdownEditor as Milkdown
import Murmur3
import Nostr
import Nostr.Article exposing (Article, articleFromEvent)
import Nostr.DeletionRequest exposing (draftDeletionEvent)
import Nostr.Event as Event exposing (Event, Kind(..), Tag(..), buildAddress)
import Nostr.Nip19 as Nip19 exposing (NIP19Type(..))
import Nostr.Request exposing (RequestId, RequestData(..))
import Nostr.Send exposing (SendRequestId, SendRequest(..))
import Nostr.Types exposing (EventId, IncomingMessage, PubKey, RelayUrl)
import Page exposing (Page)
import Pareto
import Ports
import Route exposing (Route)
import Shared
import Shared.Msg
import Tailwind.Breakpoints as Bp
import Tailwind.Utilities as Tw
import Tailwind.Theme as Theme
import Time
import Translations.Write as Translations
import Ui.Styles exposing (Theme)
import View exposing (View)
import Json.Decode as Decode


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
    , zapWeights : List (PubKey, RelayUrl, Maybe Int)
    , otherTags : List Tag
    , now : Time.Posix
    , mediaSelector : MediaSelector.Model
    , imageSelection : Maybe ImageSelection
    , publishArticleDialog : PublishArticleDialog.Model Msg
    , articleState : ArticleState
    }

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
            |> Maybe.andThen (\nip19 ->
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
            case (maybeArticle, maybeNip19) of
                (Nothing, Just (NAddr naddrData)) ->
                    Event.eventFilterForNaddr naddrData
                    |> RequestArticle (Just naddrData.relays)
                    |> (Nostr.createRequest shared.nostr "Article described as NIP-19 for editing" [])
                    |> Shared.Msg.RequestNostrEvents
                    |> Effect.sendSharedMsg

                (_, _) ->
                    Effect.none

        (mediaSelector, mediaSelectorEffect) =
            MediaSelector.init
                { selected = Nothing
                , toMsg = MediaSelectorSent
                , blossomServers = Nostr.getBlossomServers shared.nostr user.pubKey
                , nip96Servers = Nostr.getNip96Servers shared.nostr user.pubKey
                , displayType = MediaSelector.DisplayModalDialog False
                }

        publishArticleDialog =
            PublishArticleDialog.init { }

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
                    , zapWeights = article.zapWeights
                    , now = Time.millisToPosix 0
                    , mediaSelector = mediaSelector
                    , imageSelection = Nothing
                    , publishArticleDialog = publishArticleDialog
                    , articleState = ArticleDraftSaved
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
                    , zapWeights = []
                    , now = Time.millisToPosix 0
                    , mediaSelector = mediaSelector
                    , imageSelection = Nothing
                    , publishArticleDialog = publishArticleDialog
                    , articleState = ArticleEmpty
                    }

    in
    ( model
    , Effect.batch
        [ mediaSelectorEffect
        , effect
        ]
    )

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
            ( { model |
                mediaSelector = MediaSelector.show model.mediaSelector
                , imageSelection = Just imageSelection
                , articleState = ArticleModified
              }, Effect.none )

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
                            ( { model | milkdown = Milkdown.setSelectedImage model.milkdown blobDescriptor.url }, Effect.none )

                        Nip96File fileMetadata ->
                            case fileMetadata.url of
                                Just url ->
                                    ( { model | milkdown = Milkdown.setSelectedImage model.milkdown url }, Effect.none )

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
                    }, Effect.none
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
                (milkdown, cmd) =
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

updateWithPortMessage : Shared.Model -> Model -> Auth.User -> IncomingMessage -> ( Model, Effect Msg )
updateWithPortMessage shared model user portMessage =
    case portMessage.messageType of
        "published" ->
            updateWithPublishedResults shared model user portMessage.value
        
        "events" ->
            -- TODO: This code has not been tested because the a=naddr1 query parameter
            -- is lost while logging in. After preserving the query parameters while logging
            -- in the article should appear here
            case (Decode.decodeValue (Decode.field "requestId" Decode.int) portMessage.value,
                Decode.decodeValue (Decode.field "kind" Event.kindDecoder) portMessage.value,
                model.articleState) of
                (Ok requestId, Ok KindDraftLongFormContent, ArticleLoadingDraft draftRequestId) ->
                    if requestId == draftRequestId then
                        updateModelWithDraftRequest model portMessage.value
                    else
                        (model, Effect.none)

                (_, _, _) ->
                    ( model, Effect.none )
        _ -> 
            ( model, Effect.none )

updateModelWithDraftRequest : Model -> Decode.Value -> (Model, Effect Msg)
updateModelWithDraftRequest  model value =
    case Decode.decodeValue (Decode.field "events" (Decode.list Event.decodeEvent)) value of
        Ok draftEvents ->
            let
                maybeDraft =
                    draftEvents
                    |> List.head
                    |> Maybe.map (articleFromEvent)
                    |> Maybe.andThen (Result.toMaybe)
            in
            case maybeDraft of
                Just draft ->
                    ( {model
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
                    ({model | articleState = ArticleLoadingDraftError "Can't load draft" }, Effect.none)


        Err error ->
            ({model | articleState = ArticleLoadingDraftError (Decode.errorToString error) }, Effect.none)


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
                ({ model
                    | articleState = ArticleDraftSaved
                    , draftEventId = receivedDraftEventId
                }, Effect.none)
            else
                (model, Effect.none)

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
                        }
                        , Effect.none
                        )
            else
                (model, Effect.none)

        ArticleDeletingDraft sendRequestId ->
            if Just sendRequestId == receivedSendRequestId then
                ( { model
                    | articleState = ArticlePublished
                    , publishArticleDialog = PublishArticleDialog.hide model.publishArticleDialog
                  }
                , Effect.none
                )
            else
                (model, Effect.none)

        _ ->
            (model, Effect.none)



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
        [ ]
        |> Event.addTitleTag model.title
        |> Event.addSummaryTag model.summary
        |> Event.addImageTag model.image
        |> Event.addIdentifierTag model.identifier
        |> Event.addTagTags model.tags
        |> Event.addZapTags model.zapWeights
        |> Event.addClientTag Pareto.client Pareto.paretoPubKey Pareto.handlerIdentifier Pareto.paretoRelay
    , content = model.content |> Maybe.withDefault ""
    , id = ""
    , sig = Nothing
    , relay = Nothing
    }

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
                    [ viewTitle shared.browserEnv model
                    , viewSubtitle shared.browserEnv model
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
            , viewEditor shared.browserEnv model
            , viewTags  shared.browserEnv model
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
        ]
    }

viewArticleState : BrowserEnv -> Theme -> ArticleState -> Html Msg
viewArticleState browserEnv theme articleState =
    let
        styles =
            Ui.Styles.stylesForTheme theme
    in
    case articleStateToString browserEnv articleState of
        Just articleStateString ->
            div
                (styles.colorStyleGrayscaleMuted ++ styles.textStyle14 ++
                [
                ])
                [ text articleStateString ]

        Nothing ->
            div [][]
    
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
            div [][]

viewTitle : BrowserEnv -> Model -> Html Msg
viewTitle browserEnv model =
    div
        [ css
            [ Tw.w_full
            ]
        ]
        [ node "auto-resize-textarea"
            [ Attr.attribute "value" (model.title |> Maybe.withDefault "")
            , Attr.attribute "color" "rgb(55,55,55)"
            , Attr.attribute "fontfamily" "Inter"
            , Attr.attribute "fontsize" "36px"
            , Attr.attribute "fontWeight" "700"
            , Attr.attribute "placeholder" ( Translations.editorTitlePlaceholderText [ browserEnv.translations ])
            , on "input-change" (Decode.map UpdateTitle decodeInputChange)
            ]
            []
        ]
    

viewSubtitle : BrowserEnv -> Model -> Html Msg
viewSubtitle browserEnv model =
    div
        [ css
            [ Tw.w_full
            ]
        ]
        [ node "auto-resize-textarea"
            [ Attr.attribute "value" (model.summary |> Maybe.withDefault "")
            , Attr.attribute "color" "rgb(55,55,55)"
            , Attr.attribute "fontfamily" "Inter"
            , Attr.attribute "fontsize" "19px"
            , Attr.attribute "fontWeight" "700"
            , Attr.attribute "placeholder" ( Translations.editorSubtitlePlaceholderText [ browserEnv.translations ])
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

viewEditor : BrowserEnv -> Model -> Html Msg
viewEditor browserEnv model =
            div
                [ css
                    [ Tw.w_full
                    ]
                ]
                [ milkdownEditor model.milkdown browserEnv (model.content |> Maybe.withDefault "")
                ]

statusDiv : Model -> Html msg
statusDiv model =
    div []
        [ code
            [ style "color" "#017575"
            , style "font-family" "Monaco"
            , style "font-weight" ""
            , style "font-size" "12px"
            , style "border-radius" "4px"
            , style "margin-left" "16px"
            ]
            [ text (modelToStatusString model) ]
        ]

modelToStatusString : Model -> String
modelToStatusString model =
    model.content
    |> Maybe.map hexHash
    |> Maybe.withDefault ""

hexHash : Milkdown.Content -> String
hexHash str =
    str
        |> Murmur3.hashString 1234
        |> Hex.toString
        |> (++) "0x"


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

sampleDoc : String
sampleDoc =
    """
# heading 1

some text

## heading 2

* a **list**
* **another** item
* *third*

> quote
> me
> on

```python
for x in range(100):
    print(x)

```"""

viewTags : BrowserEnv -> Model -> Html Msg
viewTags browserEnv model =
    div
        [ css
            [ Tw.w_full
            ]
        ]
        [         {- Label -}
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
        ,         {- Input Field -}
        input
            [ Attr.type_ "text"
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
        [ saveDraftButton browserEnv model theme
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
    (model.articleState == ArticleDraftSaved || model.articleState == ArticleModified) &&
    model.title /= Nothing &&
    model.summary /= Nothing &&
    model.content /= Nothing &&
    model.image /= Nothing &&
    model.identifier /= Nothing


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