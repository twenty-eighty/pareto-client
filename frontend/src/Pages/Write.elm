module Pages.Write exposing (Model, Msg, page)

import Auth
import BrowserEnv exposing (BrowserEnv)
import Components.Button as Button
import Components.MediaSelector as MediaSelector exposing (UploadedFile(..))
import Components.PublishArticleDialog as PublishArticleDialog
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
import Nostr.Event as Event exposing (Event, Kind(..), Tag(..))
import Nostr.Nip19 as Nip19 exposing (NIP19Type(..))
import Nostr.Request exposing (RequestData(..))
import Nostr.Send exposing (SendRequest(..))
import Page exposing (Page)
import Pareto
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
import Nostr.Article exposing (Article)
import Components.PublishArticleDialog exposing (PublishArticleDialog)
import Nostr.Types exposing (RelayUrl)


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
    { title : Maybe String
    , summary : Maybe String
    , image : Maybe String
    , content : Maybe String
    , milkdown : Milkdown.Model
    , identifier : Maybe String
    , tags : Maybe String
    , now : Time.Posix
    , mediaSelector : MediaSelector.Model
    , imageSelectionn : Maybe ImageSelection
    , publishArticleDialog : PublishArticleDialog.Model Msg
    }

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
            PublishArticleDialog.init
                { relays = Nostr.getWriteRelaysForPubKey shared.nostr user.pubKey
                }
        model =
            case maybeArticle of
                Just article ->
                    { title = article.title
                    , summary = article.summary
                    , image = article.image
                    , content = Just article.content
                    , milkdown = Milkdown.init
                    , identifier = article.identifier
                    , tags = tagsString article.hashtags
                    , now = Time.millisToPosix 0
                    , mediaSelector = mediaSelector
                    , imageSelectionn = Nothing
                    , publishArticleDialog = publishArticleDialog
                    }

                Nothing ->
                    { title = Nothing
                    , summary = Nothing
                    , image = Nothing
                    , content = Nothing
                    , milkdown = Milkdown.init
                    , identifier = Nothing
                    , tags = Nothing
                    , now = Time.millisToPosix 0
                    , mediaSelector = mediaSelector
                    , imageSelectionn = Nothing
                    , publishArticleDialog = publishArticleDialog
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
    | MilkdownSent (Milkdown.Msg Msg)
    | PublishArticleDialogSent (PublishArticleDialog.Msg Msg)


update : Shared.Model -> Auth.User -> Msg -> Model -> ( Model, Effect Msg )
update shared user msg model =
    case msg of
        EditorChanged newContent ->
            if newContent == "" then
                ( { model | content = Nothing }, Effect.none )
            else
                ( { model | content = Just newContent }, Effect.none )

        EditorFocused ->
            ( model, Effect.none )

        EditorBlurred ->
            ( model, Effect.none )

        EditorLoaded ->
            ( model, Effect.none )

        UpdateTitle title ->
            if title == "" then
                ( { model | title = Nothing }, Effect.none )
            else
                ( { model | title = Just <| filterTitleChars title }, Effect.none )

        UpdateSubtitle summary ->
            if summary == "" then
                ( { model | summary = Nothing }, Effect.none )
            else
                ( { model | summary = Just <| filterTitleChars summary }, Effect.none )

        UpdateTags tags ->
            if tags == "" then
                ( { model | tags = Nothing }, Effect.none )
            else
                ( { model | tags = Just tags }, Effect.none )

        SelectImage imageSelection ->
            ( { model |
                mediaSelector = MediaSelector.show model.mediaSelector
                , imageSelectionn = Just imageSelection
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
            ( model, sendPublishCmd shared model user relayUrls )

        SaveDraft ->
            ( model, sendDraftCmd shared model user )

        Now now ->
            if model.identifier == Nothing then
                ( { model | now = now, identifier = now |> Time.posixToMillis |> String.fromInt |> Just }, Effect.none )
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
                }

sendPublishCmd : Shared.Model -> Model -> Auth.User -> List RelayUrl -> Effect Msg
sendPublishCmd shared model user relayUrls =
    eventWithContent shared model user KindDraftLongFormContent
    |> SendLongFormArticle relayUrls
    |> Shared.Msg.SendNostrEvent
    |> Effect.sendSharedMsg

sendDraftCmd : Shared.Model -> Model -> Auth.User -> Effect Msg
sendDraftCmd shared model user =
    eventWithContent shared model user KindDraftLongFormContent
    |> SendLongFormDraft Pareto.defaultRelays
    |> Shared.Msg.SendNostrEvent
    |> Effect.sendSharedMsg

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
        |> Event.addClientTag Pareto.client
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
        -- Sub.none
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
            -- , openMediaSelectorButton shared.browserEnv
            , saveButtons shared.browserEnv shared.theme model
            , viewMediaSelector user shared model
            ]
        , PublishArticleDialog.new
            { model = model.publishArticleDialog
            , toMsg = PublishArticleDialogSent
            , onPublish = PublishArticle
            , browserEnv = shared.browserEnv
            , theme = shared.theme
            }
            |> PublishArticleDialog.view
        ]
    }

viewMediaSelector : Auth.User -> Shared.Model -> Model -> Html Msg
viewMediaSelector user shared model =
    case model.imageSelectionn of
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
        [ saveDraftButton browserEnv theme
        , publishButton browserEnv model theme
        ]


publishButton : BrowserEnv -> Model -> Theme -> Html Msg
publishButton browserEnv model theme =
    Button.new
        { label = Translations.publishButtonTitle [ browserEnv.translations ]
        , onClick = Publish
        , theme = theme
        }
        |> Button.withDisabled (not <| articleReadyForPublishing model)
        |> Button.withTypePrimary
        |> Button.view

articleReadyForPublishing : Model -> Bool
articleReadyForPublishing model =
    True

openMediaSelectorButton : BrowserEnv -> Html Msg
openMediaSelectorButton browserEnv =
    button
        [ css
            [ Tw.bg_color Theme.gray_200
            , Tw.py_2
            , Tw.px_4
            , Tw.rounded_full
            , Css.hover
                [ Tw.bg_color Theme.gray_300
                ]
            ]
        , Events.onClick OpenMediaSelector
        ]
        [ text <| "Open Media Selector" ]

saveDraftButton : BrowserEnv -> Theme -> Html Msg
saveDraftButton browserEnv theme =
    Button.new
        { label = Translations.saveDraftButtonTitle [ browserEnv.translations ]
        , onClick = SaveDraft
        , theme = theme
        }
        |> Button.withTypeSecondary
        |> Button.view
